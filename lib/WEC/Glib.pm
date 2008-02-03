package WEC::Glib;
# Glib based event loop
use 5.006;
use strict;
use warnings;
use Glib;
use Carp;

our $VERSION = "1.000";

use WEC::Kernel qw(CODE PREVIOUS NEXT
                   add_signal delete_signal
                   @loops $work $signal_rd $alive_signal);
our @ISA = qw(WEC::Kernel);
our @EXPORT_OK = qw(loop unloop init alive_signal auto_unloop
                    add_read add_write delete_read delete_write
                    readable writable add_idle delete_idle
                    add_alarm delete_alarm add_work delete_work
                    add_signal delete_signal);

croak "Kernel type should be '$WEC::kernel_type', not '", __PACKAGE__, "'"
    unless __PACKAGE__ eq $WEC::kernel_type;

our $loop;
our @dies = ();
$alive_signal = 1;
my (%read_refs, %write_refs, $work_timer);

my $in = [qw(in err hup nval)];
sub add_read {
    defined(my $fd = fileno($_[1])) || croak "Not a filehandle";
    croak "Descriptor $fd already selected for read" if $read_refs{$fd};
    my $callback = $_[2];
    $read_refs{$fd} = Glib::IO->add_watch($fd, $in, sub { $callback->(); return 1});
}

my $out = [qw(out err hup nval)];
sub add_write {
    defined(my $fd = fileno($_[1])) || croak "Not a filehandle";
    croak "Descriptor $fd already selected for write" if $write_refs{$fd};
    my $callback = $_[2];
    $write_refs{$fd} = Glib::IO->add_watch($fd, $out, sub { $callback->(); return 1});
}

sub delete_read {
    defined(my $fd = fileno($_[1])) || croak "Not a filehandle";
    Glib::Source->remove(delete($read_refs{$fd}) ||
                         croak "Descriptor $fd wasn't selected for read");
}

sub delete_write {
    defined(my $fd = fileno($_[1])) || croak "Not a filehandle";
    Glib::Source->remove(delete($write_refs{$fd}) ||
                         croak "Descriptor $fd wasn't selected for write");
}

sub add_alarm {
    shift;
    return Glib::Timeout->add(shift, do {
        my $callback = shift;
        sub { $callback->(); return };
    });
}

sub delete_alarm {
    Glib::Source->remove($_[1]);
}

sub run_work {
    ($work = $work->[NEXT])->[CODE]->();
    return 1;
}

sub add_work {
    shift;
    return $work = $work->[NEXT] = $work->[NEXT][PREVIOUS] =
        [$work, $work->[NEXT], shift] if $work;
    $work_timer = Glib::Timeout->add(0, \&run_work);
    return $work->[PREVIOUS] = $work->[NEXT] = $work = [undef, undef, shift];
}

sub delete_work {
    if (&WEC::Kernel::delete_work) {
        Glib::Source->remove($work_timer);
        $work_timer = undef;
        return 1;
    }
}

sub add_idle {
    shift;
    my $callback = shift;
    return Glib::Idle->add(sub { $callback->(); return 1});
}

sub delete_idle {
    shift;
    Glib::Source->remove(shift);
}

sub alive_signal {
    return $alive_signal unless @_;
    croak "Cannot turn of alive_signal in WEC::Tk" if !$_[0];
    my $old = $alive_signal;
    $alive_signal = shift;
    return $old;
}

sub auto_unloop() {
    return;
}

sub signal_init {
    WEC::Kernel::signal_init;
      $WEC::kernel_type->delete_read($signal_rd);
      $WEC::kernel_type->add_read($signal_rd, \&WEC::Kernel::run_signals);
}

sub init {
    $WEC::Kernel::add_sig ||= \&WEC::Kernel::set_old_signal;
    $WEC::Kernel::del_sig ||= \&WEC::Kernel::unset_old_signal;
    if (!$loop) {
        $loop = Glib::MainLoop->new;
        Glib->install_exception_handler(sub { 
            push @dies, shift;
            $loop->quit;
            return 1;
        });
    }
    signal_init unless $signal_rd;
}

sub loop {
    $loop->run;
    return shift @loops unless @dies;
    my @d = @dies;
    @dies = ();
    die @d;
}

sub unloop {
    push(@loops, shift);
    $loop->quit;
}

sub readable {
    return keys(%read_refs)-1 unless wantarray;
    my $fd = fileno($signal_rd) . "";
    return grep $_ ne $fd, keys %read_refs;
}

sub writable {
    return keys %write_refs;
}

1;
