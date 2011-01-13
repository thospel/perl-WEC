package WEC::Kernel;
# Event kernel base class
use 5.008;
use strict;
use warnings;
use Carp;
use Time::HiRes;
use AutoLoader qw(AUTOLOAD);
use POSIX qw(SIG_BLOCK SIG_SETMASK sigprocmask sigaction);
use Socket qw(AF_UNIX SOCK_STREAM PF_UNSPEC);
use Errno qw(EWOULDBLOCK EAGAIN EINTR);
use Config qw();

$SIG{PIPE} = "IGNORE";

our $VERSION = "1.000";
require Exporter;
our @ISA = qw(Exporter);
our @EXPORT_OK = qw(add_alarm delete_alarm unloop run_now add_work
                    delete_work run_work run_signals add_idle delete_idle
                    run_idle add_signal delete_signal alive_signal auto_unloop
                    signal2number signal2string forget_signals
                    TIME INDEX CODE PREVIOUS NEXT NAME
                    @loops @alarms @immediate $now $work $idle $signal_rd
                    $signal_wr %got_signal $got_signal %signals $add_sig
                    $del_sig $alive_signal %old_SIG);
our @alarms = (undef);
our (@immediate, @loops, $now, $work, $idle, %signals, %old_SIG,
     $signal_rd, $signal_wr, $add_sig, $del_sig, $alive_signal, %got_signal);
our $got_signal = 0;
*check_signals = \%signals;

# Alarm indices
sub TIME	() { 0 };
sub INDEX	() { 1 };
sub CODE	() { 2 };	# Must come after INDEX

# Work indices
sub PREVIOUS	() { 0 };
sub NEXT	() { 1 };
# sub CODE	() { 2 };	# Already defined
sub NAME	() { 3 };

my ($block_mask, $old_mask, %signal2number, %signal2string);

1;
__END__

sub add_alarm {
    shift;
    $now = Time::HiRes::time();
    my $time = $now+shift || croak "Attempt to put an alarm on epoch start";
    my $i = @alarms;
    $i = ($alarms[$i][INDEX] = $i) >> 1 while
        $i > 1 && $time < ($alarms[$i] = $alarms[$i >> 1])->[TIME];
    return $alarms[$i] = [$time, $i, shift];
}

sub delete_alarm {
    my $i = $_[1][INDEX];
    if (!$i) {
        croak "Not an alarm reference" unless defined($i) && $i eq "0";
        # Could be an alarm sitting on the expired queue in run_now
        $#{$_[1]} = INDEX if @{$_[1]} > INDEX;
        return;
    }
    $_[1][INDEX] = 0;
    # Last element or beyond...
    if ($i >= $#alarms) {
        croak "Not an alarm reference" if $i > $#alarms;
        pop(@alarms);
        return;
    }
    my $time = $alarms[-1][TIME];
    if ($i > 1 && $time < $alarms[$i >> 1][TIME]) {
        # percolate to root
        $i = (($alarms[$i] = $alarms[$i >> 1])->[INDEX] = $i) >> 1;
        $i = ($alarms[$i][INDEX] = $i) >> 1 while
            $i > 1 && $time < ($alarms[$i] = $alarms[$i >> 1])->[TIME];
    } else {
        # percolate to leafs
        my $n = @alarms-2;
        my $l = $i * 2;
        while ($l < $n) {
            if ($alarms[$l][TIME] < $time) {
                if ($alarms[$l+1][TIME] < $alarms[$l][TIME]) {
                    ($alarms[$i] = $alarms[$l+1])->[INDEX] = $i;
                    $i = $l+1;
                } else {
                    ($alarms[$i] = $alarms[$l])->[INDEX] = $i;
                    $i = $l;
                }
            } elsif ($alarms[$l+1][TIME] < $time) {
                ($alarms[$i] = $alarms[$l+1])->[INDEX] = $i;
                $i = $l+1;
            } else {
                last;
            }
            $l = $i * 2;
        }
        if ($l == $n && $alarms[$l][TIME] < $time) {
            ($alarms[$i] = $alarms[$l])->[INDEX] = $i;
            $i = $l;
        }
    }
    ($alarms[$i] = pop @alarms)->[1] = $i;
}

sub run_now {
    run_signals() if $got_signal;

    ($work = $work->[NEXT])->[CODE]->() if $work;

    # @immediate must be persistent so no alarms get lost if a callback dies
    goto EXPIRED if @alarms <= 1 ||
        $alarms[1][TIME] > ($now = Time::HiRes::time);
    while (@alarms > 2) {
        push @immediate, $alarms[1];
        $alarms[1][INDEX] = 0;

        my $time = $alarms[-1][TIME];
        my $n = @alarms-2;
        my $i = 1;
        my $l = 2;
        while ($l < $n) {
            if ($alarms[$l][TIME] < $time) {
                if ($alarms[$l+1][TIME] < $alarms[$l][TIME]) {
                    ($alarms[$i] = $alarms[$l+1])->[INDEX] = $i;
                    $i = $l+1;
                } else {
                    ($alarms[$i] = $alarms[$l])->[INDEX] = $i;
                    $i = $l;
                }
            } elsif ($alarms[$l+1][0] < $time) {
                ($alarms[$i] = $alarms[$l+1])->[INDEX] = $i;
                $i = $l+1;
            } else {
                last;
            }
            $l = $i * 2;
        }
        if ($l == $n && $alarms[$l][TIME] < $time) {
            ($alarms[$i] = $alarms[$l])->[INDEX] = $i;
            $i = $l;
        }
        ($alarms[$i] = pop @alarms)->[INDEX] = $i;
        goto EXPIRED if $alarms[1][TIME] > $now;
    }
    if (@alarms == 2) {
        $alarms[1][INDEX] = 0;
        push @immediate, pop @alarms;
    }
  EXPIRED:
    my $fun;
    $fun->[CODE] && $fun->[CODE]->() while $fun = shift @immediate;
    $now = Time::HiRes::time;
}

# $work points to the node we've just been running
sub add_work {
    shift;
    return $work = $work->[NEXT] = $work->[NEXT][PREVIOUS] =
        [$work, $work->[NEXT], shift] if $work;
    return $work->[PREVIOUS] = $work->[NEXT] = $work = [undef, undef, shift];
}

sub delete_work {
    shift;
    my $node = shift || croak "Not a work reference";
    if (!$node->[NEXT]) {
        croak "Not a work reference" if @$node;
        return;
    }
    croak "Not a work reference" unless @$node >= CODE && $node->[PREVIOUS];
    if ($node == $work) {
        if ($node == $node->[PREVIOUS]) {
            $work = undef;
            @$node = ();
            return 1;
        }
        $work = $node->[PREVIOUS];
    }
    $node->[PREVIOUS][NEXT] = $node->[NEXT];
    $node->[NEXT][PREVIOUS] = $node->[PREVIOUS];
    @$node = ();
    return;
}

sub run_work {
    ($work = $work->[NEXT])->[CODE]->();
}

# $idle points to the node we've just been running
sub add_idle {
    shift;
    return $idle = $idle->[NEXT] = $idle->[NEXT][PREVIOUS] =
        [$idle, $idle->[NEXT], shift] if $idle;
    return $idle->[PREVIOUS] = $idle->[NEXT] = $idle = [undef, undef, shift];
}

sub delete_idle {
    shift;
    my $node = shift || croak "Not a idle reference";
    if (!$node->[NEXT]) {
        croak "Not a idle reference" if @$node;
        return;
    }
    croak "Not a idle reference" unless @$node >= CODE && $node->[PREVIOUS];
    if ($node == $idle) {
        if ($node == $node->[PREVIOUS]) {
            $idle = undef;
            @$node = ();
            return 1;
        }
        $idle = $node->[PREVIOUS];
    }
    $node->[PREVIOUS][NEXT] = $node->[NEXT];
    $node->[NEXT][PREVIOUS] = $node->[PREVIOUS];
    @$node = ();
    return;
}

sub run_idle {
    ($idle = $idle->[NEXT])->[CODE]->();
}

# A classic signal selfpipe
sub signal_pipe {
    croak "Already have signal fds" if
        defined($signal_rd) || defined($signal_wr);
    require WEC::Socket;
    local $^F = -1;
    if ($^O eq 'MSWin32') {
	socketpair($signal_rd, $signal_wr, AF_UNIX, SOCK_STREAM, PF_UNSPEC) ||
	    die "Could not creat signal socketpair: $! (", $!+0, ")";
	# Triggers a connection reset after TIME_WAIT
	# shutdown($signal_rd, 1);	# no more writing for reader
	# shutdown($signal_wr, 0);	# no more reading for writer
    } else {
	pipe($signal_rd, $signal_wr) || die "Could not create signal pipe: $!";
    }
    binmode($signal_rd);
    binmode($signal_wr);
    WEC::Socket::blocking($signal_rd, 0);
    WEC::Socket::blocking($signal_wr, 0);
}

# Only to be called if the masks have been set up
sub run_signals {
    $^O eq "MSWin32" || sigprocmask(SIG_BLOCK, $block_mask, $old_mask) ||
        die "Could not sigprocmask all signals: $!";
    my @names = sort({$got_signal{$b} <=> $got_signal{$a}}
                     grep $got_signal{$_}, keys %got_signal);
    $got_signal{$_} = 0 for @names;
    $got_signal = 0;
    $^O eq "MSWin32" || sigprocmask(SIG_SETMASK, $old_mask) ||
        die "Could not sigprocmask to restore signals: $!";

    my $work;
    for my $name (@names) {
        unless ($work = $signals{$name}) {
            # Signal race. Reraise it
            kill $name, $$;
            next;
        }
        for (my $here = $work->[PREVIOUS];
             $here != $work; $here = $here->[PREVIOUS]) {
            unshift @immediate, $here;
        }
        unshift @immediate, $work;
    }
    $work->[CODE] && $work->[CODE]->() while $work = shift @immediate;
}

sub read_signal_tokens {
    unless (my $rc = sysread($signal_rd, my $buf, 4096)) {
        die "Unexpected EOF in signal pipe" if defined $rc;
        die "Unexpected read error on signal pipe: $! (",$!+0,")" unless
            $! == EWOULDBLOCK || $! == EAGAIN || $! == EINTR;
    }
    unshift @immediate, [];
}

# Old style unsafe signals so that you can break out of XS code
sub set_old_signal {
    my $name = shift;
    my $new_action = POSIX::SigAction->new(shift);
    my $old_action = POSIX::SigAction->new;
    $got_signal{$name} = 0;
    sigaction(signal2number($name), $new_action, $old_action) ||
        croak "Could not sigaction: $!";
    $old_SIG{$name} = $old_action;
}

sub unset_old_signal {
    my $name = shift;
    sigaction(signal2number($name),
              exists ($old_SIG{$name}) ? delete($old_SIG{$name}) :
              die "Assertion failure, no old signal") ||
              croak "Could not sigaction: $!";
    delete $got_signal{$name};
}

sub add_signal {
    shift;
    my $name = signal2string(shift);
    croak "Unknown signal $name" unless exists $SIG{$name};
    my $work = $signals{$name};
    if ($work) {
        $work = $work->[PREVIOUS] = $work->[PREVIOUS][NEXT] =
            [$work->[PREVIOUS], $work, shift];
    } else {
        $signals{$name} = $work->[PREVIOUS] = $work->[NEXT] = $work =
            [undef, undef, shift, $name];
        my $sig_fun = sub {
            # Try to keep this malloc free so it can be used unsafe
            # Don't split this off. The autoloading will be terrible
            if (!$got_signal) {
                $got_signal = 1;
                local $!;
                until (my $rc = syswrite($signal_wr, "1")) {
                    # Length 0 write or interrupt is allowed
                    next if defined $rc || $! == EINTR;
                    # Presumably this means the pipe is full
                    last if $! == EWOULDBLOCK || $! == EAGAIN;
                    die "Unexpected write error on signal pipe: $!";
                }
            }
            # The next line is a race condition under different signals.
            # That just means these recursive signals will randomly ordered.
            # But we can't do better anyways since a recursive signal can
            # come in before any signal blocking mask we might set here
            $got_signal{$name} ||= 1 + keys %got_signal;
        };
        if ($add_sig) {
            $add_sig->($name, $sig_fun);
        } else {
            $got_signal{$name} = 0;
            $old_SIG{$name} = $SIG{$name};
            $SIG{$name} = $sig_fun;
        }
    }
    return $work
}

sub delete_signal {
    shift;
    my $node = shift || croak "Not a signal reference";
    if (!$node->[NEXT]) {
        croak "Not a signal reference" if @$node;
        return;
    }
    croak "Not a signal reference" unless @$node >= CODE && $node->[PREVIOUS];
    if ($node->[NAME]) {
        my $name = $node->[NAME] || die "Assertion failure, no name";
        if ($node == $node->[NEXT]) {
            $node == delete($signals{$name}) ||
                die "Assertion failure, single node is not last node";
            @$node = ();
            if ($del_sig) {
                $del_sig->($name);
            } else {
                # Next line shuts off undef warning on $SIG assign.
                # undef SIG assign is undocumented but *seems* to work.
                no warnings 'uninitialized';
                $SIG{$name} = exists($old_SIG{$name}) ? delete($old_SIG{$name}) :
                    die "Assertion failure, no old signal";
                delete $got_signal{$name};
            }
            return 1;
        }
        ($signals{$name} = $node->[NEXT])->[NAME] = $name;
    }
    $node->[PREVIOUS][NEXT] = $node->[NEXT];
    $node->[NEXT][PREVIOUS] = $node->[PREVIOUS];
    @$node = ();
    return;
}

sub forget_signals {
    for my $name (keys %signals) {
        my $start = $signals{$name} || die "no signal node for $name";
        my $next = $start->[NEXT];
        for (my $here = $next; $here != $start; $here = $next) {
            $next = $here->[NEXT];
            @$here = ();
        }
        @$start = @_;
        delete $signals{$name};
    }
}

sub signal_init {
    return if $old_mask;
    signal_pipe();

    $WEC::kernel_type->add_read($signal_rd, \&read_signal_tokens);
    if ($^O eq 'MSWin32') {
	$old_mask = "dummy";
    } else {
	$block_mask = POSIX::SigSet->new;
	$block_mask->fillset;
	$old_mask   = POSIX::SigSet->new;
    }
}

sub alive_signal {
    return $alive_signal unless @_;
    my $old = $alive_signal;
    $alive_signal = shift;
    if ($old) {
        if (!$alive_signal) {
            *check_signals = {};
        }
    } elsif ($alive_signal) {
        *check_signals = \%signals;
    }
    return $old;
}

sub signal_maps {
    if ($Config::Config{sig_name} && $Config::Config{sig_num}) {
	my @nrs   = split " ", $Config::Config{sig_num};
	my @names = split " ", $Config::Config{sig_name};
	die "Config{sig_num} and Config{sig_names} have inconsistent length" if
	    @nrs != @names;
	for my $i (0..$#nrs) {
	    my $name = $names[$i];
	    my $nr   = $nrs[$i];
            # "normal" names tend to be in front, e.g. SIGCHLD before SIGCLD
	    $signal2string{$nr} ||= $name;
	    $signal2number{$nr} = $signal2number{$name} = $nr;
	}
    }
    # Get any extra ones from POSIX (maybe drop this code ?)
    POSIX->import("signal_h");
    my $nr;
    for my $name (keys %SIG) {
	next if defined $signal2number{$name};
        my $fun = POSIX->can("SIG$name")|| next;
        eval { $nr = $fun->(); 1 }	|| next;
        $signal2string{$nr} = $name;
        $signal2number{$nr} = $signal2number{$name} = $nr;
    }
    croak "Could not find any signals" unless %signal2string;
    # done in two phases in case multiple strings map to the same number
    # This way one of these strings will then become the "official" name
    $signal2string{$_} ||= $signal2string{$signal2number{$_}} for
        keys %signal2number;
}

sub signal2string {
    signal_maps unless %signal2string;
    croak "Can't convert signal '$_[0]' to a string" unless
        exists $signal2string{$_[0]};
    return $signal2string{shift()};
}

sub signal2number {
    signal_maps unless %signal2number;
    croak "Can't convert signal '$_[0]' to a number" unless
        exists $signal2number{$_[0]};
    return $signal2number{shift()};
}

sub unloop {
    push(@loops, shift);
}

sub auto_unloop() {
    return 1;
}

1;
