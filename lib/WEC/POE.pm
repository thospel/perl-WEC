package WEC::POE;
# POE based event loop
use 5.006;
use strict;
use warnings;
# Work around some POE::Kernel version destroying $^W
# Won't work if somewhere else POE::Kernel already got used
BEGIN {our $old_w = $^W}
use POE;
BEGIN {$^W = our $old_w}
use POE::Kernel;
use Carp;

$POE::VERSION =~ /^\d+\.(\d+)(?:_(\d+))?$/ ||
    die "Could not parse POE version $POE::VERSION";
croak "POE version 0.29_01 or higher required--this is only version $POE::VERSION" if $1 < 29 || $1 == 29 && (!$2 || $2 < 1);

my %map_type = ("Poll" => "IO_Poll");
my %bad_type = map {$_ => 1} qw(PerlSignals TkCommon TkActiveState);
my @poe_type = map(m!^POE[/\\](?:XS[/\\])?Loop[/\\](.+)\.pm\z! && !$bad_type{$1} ? $map_type{$1} || $1 : (), keys %INC);
die "Could not determine POE event loop from qw(@poe_type)" if 
    @poe_type != 1 && 
    # Next is typical for the case you ask for IO::Poll and 
    # POE falls back to IO::Select anyways
    !(@poe_type == 2 && $poe_type[0] eq "Select" && $poe_type[1] eq "IO_Poll");
our $poe_type = $poe_type[0];

# Force pickup of Time::HiRes
Event->import if $poe_type eq "Event";

use vars qw($VERSION @ISA @EXPORT_OK);
$VERSION = "1.000";
use WEC::Kernel qw(CODE PREVIOUS NEXT delete_work run_work run_idle
                   add_signal delete_signal forget_signals auto_unloop
                   @immediate @loops $work $idle %signals $alive_signal);
@ISA = qw(WEC::Kernel);
@EXPORT_OK = qw(add_read add_write delete_read delete_write
                add_alarm delete_alarm add_work delete_work auto_unloop
                add_idle delete_idle add_signal delete_signal alive_signal
                loop unloop init readable writable);
croak "Kernel type should be '$WEC::kernel_type', not '", __PACKAGE__, "'"
    unless __PACKAGE__ eq $WEC::kernel_type;

my (%read_refs, %write_refs, $work_token, $busy, $idle_id);
my $sig_count = 0;
our ($session, $session_id, $in_poe);
our $alias = "WEC";
our @dies = ();
$alive_signal = 1;
my ($select_read, $select_write, $delay_set, $alarm_remove, $sig);

sub add_read($*$) {
    defined(my $fd = fileno($_[1])) || croak "Not a filehandle";
    croak "Descriptor $fd already selected for read" if $read_refs{$fd};
    $read_refs{$fd} = $_[2];
    $in_poe ? 
        $poe_kernel->select_read($_[1], "readable") :
        $poe_kernel->call($session || croak("Not inited"), 
                          add => $select_read, $_[1], "readable");
}

sub add_write($*$) {
    defined(my $fd = fileno($_[1])) || croak "Not a filehandle";
    croak "Descriptor $fd already selected for write" if $write_refs{$fd};
    $write_refs{$fd} = $_[2];
    $in_poe ? 
        $poe_kernel->select_write($_[1], "writable") :
        $poe_kernel->call($session || croak("Not inited"), 
                          add => $select_write, $_[1],"writable");
}

sub delete_read($*) {
    defined(my $fd = fileno($_[1])) || croak "Not a filehandle";
    croak "Descriptor $fd wasn't selected for read" unless
        delete $read_refs{$fd};
    $in_poe ?
        $poe_kernel->select_read($_[1]) :
        $poe_kernel->call($session || croak("Not inited"), 
                          add => $select_read, $_[1]);
}

sub delete_write($*) {
    defined(my $fd = fileno($_[1])) || croak "Not a filehandle";
    croak "Descriptor $fd wasn't selected for write " unless
        delete $write_refs{$fd};
    $in_poe ? 
        $poe_kernel->select_write($_[1]) :
        $poe_kernel->call($session || croak("Not inited"), 
                          add => $select_write, $_[1]);
}

sub add_alarm {
    return $in_poe ?
        $poe_kernel->delay_set(alarm => $_[1], $_[2]) :
        $poe_kernel->call($session || croak("Not inited"), 
                          add	=> $delay_set,
                          alarm => $_[1], $_[2]);
}

sub delete_alarm {
    $in_poe ?
        $poe_kernel->alarm_remove($_[1]) :
        $poe_kernel->call($session || croak("Not inited"), 
                          add => $alarm_remove, $_[1]);
}

# $work points to the node we've just been running
sub add_work {
    shift;
    return $work = $work->[NEXT] = $work->[NEXT][PREVIOUS] =
        [$work, $work->[NEXT], shift] if $work;
    $work_token ||= ($in_poe ? 
                     $poe_kernel->yield("work") : 
                     $poe_kernel->post($session || croak("Not inited"), "work"), 
                     1);
    return $work->[PREVIOUS] = $work->[NEXT] = $work = [undef, undef, shift];
}

sub try_idle {
    $busy ? $busy = undef : run_idle;
}

sub add_idle {
    shift;
    return $idle = $idle->[NEXT] = $idle->[NEXT][PREVIOUS] =
        [$idle, $idle->[NEXT], shift] if $idle;
    $idle_id = add_work(undef, \&try_idle);
    return $idle->[PREVIOUS] = $idle->[NEXT] = $idle = [undef, undef, shift];
}

sub delete_idle {
    if (&WEC::Kernel::delete_idle) {
        delete_work(undef, $idle_id);
        return 1;
    }
}

sub set_signal {
    !$alive_signal || defined($poe_kernel->refcount_increment($session_id, "s")) || die "Could not inc session: $!";
    ++$sig_count;
    $in_poe ? 
        $poe_kernel->sig(shift, "signal") :
        $poe_kernel->call($session || croak("Not inited"), 
                          add => $sig, shift, "signal");
}

sub unset_signal {
    $in_poe ?
        $poe_kernel->sig(shift) :
        $poe_kernel->call($session || croak("Not inited"), 
                          add => $sig, shift);
    !$alive_signal || defined($poe_kernel->refcount_decrement($session_id, "s")) || die "Could not dec session: $!";
    --$sig_count;
}

sub alive_signal {
    return $alive_signal unless @_;
    my $old = $alive_signal;
    $alive_signal = shift;
    if ($old) {
        if (!$alive_signal && defined($session_id)) {
            defined($poe_kernel->refcount_decrement($session_id, "s")) ||
                die "Could not dec session: $!" for 1..$sig_count;
        }
    } elsif ($alive_signal && defined($session_id)) {
        defined($poe_kernel->refcount_increment($session_id, "s")) ||
            die "Could not inc session: $!" for 1..$sig_count;
    }
    return $old;
}

sub loop {
    local $in_poe = 1;
    $poe_kernel->run();
    $work_token = undef;	# For if we got here due to $poe_kernel->stop
    return shift @loops unless @dies;
    my @d = @dies;
    @dies = ();
    die @d;
}

sub unloop {
    push(@loops, shift);
    # Needs more cleanup, e.g. readable, writable, work etc.
    forget_signals;
    $sig_count = 0;
    $session = $session_id = undef;
    $work_token = 1;	# So it won't get added anymore
    $poe_kernel->stop();
}

my @session_args =
    (inline_states => {
        _start	=> sub {
            $poe_kernel->alias_set($alias);
            $session = $alias;
            $session_id = $_[SESSION]->ID;
            $work_token ||= ($poe_kernel->yield("work"), 1) if $work;
        },
        _stop => sub {
            forget_signals;
            $sig_count = 0;
            $session = $session_id = undef;
        },
        add => sub {
            return $_[ARG0]->($poe_kernel, @_[ARG1..$#_]);
        },
        readable	=> sub {
            $busy ||= 1;
            $read_refs{fileno($_[ARG0])}->();
        },
        writable	=> sub {
            $busy ||= 1;
            $write_refs{fileno($_[ARG0])}->();
        },
        "alarm"	=> sub {
            $busy ||= 1;
            $_[ARG0]->();
        },
        "work" => sub {
            $work_token = undef;
            return unless $work;
            if ($work->[NEXT] != $work) {
                $busy ||= 1;
                run_work;
                run_work if !$busy && $work;
            } else {
                run_work;
            }
            $work_token ||= ($poe_kernel->yield("work"), 1) if $work;
        },
        "signal" => sub {
            my $work = $signals{$_[ARG0]} || return;
            $poe_kernel->sig_handled;
            push @immediate, $work;
            for (my $here = $work->[NEXT];
                 $here != $work; $here = $here->[NEXT]) {
                push @immediate, $here;
            }
            $work->[CODE] && $work->[CODE]->() while $work = shift @immediate;
            return 0;
        },
        _default	=> sub {
            die("calling non existant state ", $_[ARG0]) unless
                substr($_[ARG0], 0, 1) eq "_";
            return;
        },
    });

sub readable {
    return keys %read_refs;
}

sub writable {
    return keys %write_refs;
}

sub init {
    return if $session;
    $WEC::Kernel::add_sig ||= \&set_signal;
    $WEC::Kernel::del_sig ||= \&unset_signal;
    $select_read  ||= $poe_kernel->can("select_read") || die "No select_read";
    $select_write ||= $poe_kernel->can("select_write")|| die "No select_write";
    $delay_set	  ||= $poe_kernel->can("delay_set")   || die "No delay_set";
    $alarm_remove ||= $poe_kernel->can("alarm_remove")|| die "No alarm_remove";
    $sig	  ||= $poe_kernel->can("sig")	      || die "No sig";
    "POE::Session"->create(@session_args);
}

if ($poe_type eq "Tk") {
    $Tk::LangDebug = 0;
    no warnings "redefine";
    *Tk::Error = sub {
        my $window = shift;
        push @dies, shift;

        $poe_kernel->signal($poe_kernel, "UIDESTROY");
    }
}

1;
