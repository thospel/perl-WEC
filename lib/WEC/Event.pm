package WEC::Event;
# Event based event loop
use 5.006;
use strict;
use warnings;
use Carp;
use Event qw(loop unloop PRIO_NORMAL);

use vars qw(@ISA $VERSION @EXPORT_OK);
$VERSION = "0.01";
use WEC::Kernel qw(CODE PREVIOUS NEXT run_work auto_unloop
                   $work $alive_signal);
@ISA = qw(WEC::Kernel);
@EXPORT_OK = qw(add_read add_write delete_read delete_write
                add_alarm delete_alarm add_work delete_work
                add_idle delete_idle add_signal delete_signal alive_signal
                loop unloop init readable writable auto_unloop);
croak "Kernel type should be '$WEC::kernel_type', not '", __PACKAGE__, "'"
    unless __PACKAGE__ eq $WEC::kernel_type;

$alive_signal = 1;
my (%read_refs, %write_refs, $work_timer);

sub add_read($*$) {
    defined(my $fd = fileno($_[1])) || croak "Not a filehandle";
    croak "Descriptor $fd already selected for read" if $read_refs{$fd};
    $read_refs{$fd} = Event->io(fd => $_[1], poll => "r", cb => $_[2]);
}

sub add_write($*$) {
    defined(my $fd = fileno($_[1])) || croak "Not a filehandle";
    croak "Descriptor $fd already selected for write" if $write_refs{$fd};
    $write_refs{$fd} = Event->io(fd => $_[1], poll => "w", cb => $_[2]);
}

sub delete_read($*) {
    defined(my $fd = fileno($_[1])) || croak "Not a filehandle";
    (delete($read_refs{$fd}) ||
     croak "Descriptor $fd wasn't selected for read")->cancel;
}

sub delete_write($*) {
    defined(my $fd = fileno($_[1])) || croak "Not a filehandle";
    (delete($write_refs{$fd}) ||
        croak "Descriptor $fd wasn't selected for write")->cancel;
}

sub add_alarm {
    return Event->timer(after => $_[1], cb => $_[2]);
}

sub delete_alarm {
    $_[1]->cancel;
}

# $work points to the node we've just been running
sub add_work {
    shift;
    return $work = $work->[NEXT] = $work->[NEXT][PREVIOUS] =
        [$work, $work->[NEXT], shift] if $work;
    $work_timer = Event->timer(after => 0, interval => 0, cb => \&run_work);
    return $work->[PREVIOUS] = $work->[NEXT] = $work = [undef, undef, shift];
}

sub delete_work {
    if (&WEC::Kernel::delete_work) {
        $work_timer->cancel;
        $work_timer = undef;
        return 1;
    }
}

sub add_idle {
    return Event->idle(repeat => 1, cb => $_[1]);
}

sub delete_idle {
    $_[1]->cancel;
}

sub add_signal {
    return Event->signal(signal => $_[1], cb => $_[2]);
}

sub delete_signal {
    $_[1]->cancel;
}

sub readable {
    return keys %read_refs;
}

sub writable {
    return keys %write_refs;
}

sub alive_signal {
    return $alive_signal unless @_;
    croak "Cannot turn of alive_signal in WEC::Event" if !$_[0];
    my $old = $alive_signal;
    $alive_signal = shift;
    return $old;
}

sub init {
}

1;
