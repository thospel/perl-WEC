package WEC::Tk;
# Tk based event loop
use 5.006;
use strict;
use warnings;
use Carp;
use Tk qw(DONT_WAIT WINDOW_EVENTS FILE_EVENTS DoOneEvent);

use WEC::Kernel qw(unloop CODE PREVIOUS NEXT run_work add_signal delete_signal
                   run_signals
                   @immediate $signal_rd $alive_signal %old_SIG
                   $work $idle @loops);
use vars qw($VERSION @ISA @EXPORT_OK);
$VERSION = "1.000";
@ISA = qw(WEC::Kernel);
@EXPORT_OK = qw($event_window destroy auto_unloop
                add_read add_write delete_read delete_write
                add_alarm delete_alarm add_work delete_work
                add_idle delete_idle add_signal delete_signal alive_signal
                loop unloop init readable writable);
croak "Kernel type should be '$WEC::kernel_type', not '", __PACKAGE__, "'"
    unless __PACKAGE__ eq $WEC::kernel_type;

$alive_signal = 1;
my (%read_refs, %write_refs, $work_timer, $idle_driver);
our $event_window;

sub add_read($*$) {
    defined(my $fd = fileno($_[1])) || croak "Not a filehandle";
    croak "Descriptor $fd already selected for read" if $read_refs{$fd};
    $read_refs{$fd} = 1;
    $event_window->fileevent($_[1], "readable", $_[2]);
}

sub add_write($*$) {
    defined(my $fd = fileno($_[1])) || croak "Not a filehandle";
    croak "Descriptor $fd already selected for write" if $write_refs{$fd};
    $write_refs{$fd} = 1;
    $event_window->fileevent($_[1], "writable", $_[2]);
}

sub delete_read($*) {
    defined(my $fd = fileno($_[1])) || croak "Not a filehandle";
    croak "Descriptor $fd wasn't selected for read" unless
        delete $read_refs{$fd};
    # $event_window->fileevent($_[1], "readable", "");
    if ($write_refs{$fd}) {
        (tied *{$_[1]} || die "Handle $_[1] not tied")->
            handler(Tk::Event::IO::READABLE(), "");
    } else {
        $event_window->fileevent($_[1], "readable", "");
    }
}

sub delete_write($*) {
    defined(my $fd = fileno($_[1])) || croak "Not a filehandle";
    croak "Descriptor $fd wasn't selected for write" unless
        delete $write_refs{$fd};
    # $event_window->fileevent($_[1], "writable", "");
    if ($read_refs{$fd}) {
        (tied *{$_[1]} || die "Handle $_[1] not tied")->
            handler(Tk::Event::IO::WRITABLE(), "");
    } else {
        $event_window->fileevent($_[1], "writable", "");
    }
}

sub add_alarm {
    return $event_window->after($_[1] * 1000, $_[2]);
}

sub delete_alarm {
    $event_window->afterCancel($_[1]);
}

# $work points to the node we've just been running
sub add_work {
    shift;
    return $work = $work->[NEXT] = $work->[NEXT][PREVIOUS] =
        [$work, $work->[NEXT], shift] if $work;
    $work_timer = $event_window->repeat(0, \&run_work);
    return $work->[PREVIOUS] = $work->[NEXT] = $work = [undef, undef, shift];
}

sub delete_work {
    if (&WEC::Kernel::delete_work) {
        $event_window->afterCancel($work_timer);
        $work_timer = undef;
        return 1;
    }
}

sub run_idle {
    # Mm, idle handler needs retriggering
    $idle_driver = $event_window->afterIdle(\&run_idle);
    WEC::Kernel::run_idle();
}

# $idle points to the node we've just been running
sub add_idle {
    shift;
    return $idle = $idle->[NEXT] = $idle->[NEXT][PREVIOUS] =
        [$idle, $idle->[NEXT], shift] if $idle;
    $idle_driver = $event_window->afterIdle(\&run_idle);
    return $idle->[PREVIOUS] = $idle->[NEXT] = $idle = [undef, undef, shift];
}

sub delete_idle {
    if (&WEC::Kernel::delete_idle) {
        $event_window->afterCancel($idle_driver);
        $idle_driver = undef;
        return 1;
    }
}

sub destroy {
    croak "No event window" unless $event_window;
    $event_window->destroy;
    # Or should this be done in some callback ?
    $event_window = undef;
}

sub loop {
    # Extra test without alarm handling makes alarm priority normal
    (@immediate && run_signals),
    DoOneEvent(DONT_WAIT | FILE_EVENTS | WINDOW_EVENTS) while
        (@immediate && run_signals), !@loops && DoOneEvent;
    # @immediate && run_signals while !@loops && DoOneEvent;
    return shift @loops;
}

sub readable {
    return keys(%read_refs)-1 unless wantarray;
    my $fd = fileno($signal_rd) . "";
    return grep $_ ne $fd, keys %read_refs;
}

sub writable {
    return keys %write_refs;
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

sub init {
    $WEC::Kernel::add_sig ||= \&WEC::Kernel::set_old_signal;
    $WEC::Kernel::del_sig ||= \&WEC::Kernel::unset_old_signal;
    $event_window	  ||= Tk::MainWindow->new;
    WEC::Kernel::signal_init unless $signal_rd;
}

1;
