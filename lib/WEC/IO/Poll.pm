package WEC::IO::Poll;
# IO::Poll based event loop
use 5.006;
use strict;
use warnings;
use POSIX qw(EINTR);
use Carp;
use IO::Poll;

use vars qw(@ISA $VERSION @EXPORT_OK);
$VERSION = "0.01";
use WEC::Kernel qw(unloop add_alarm delete_alarm run_now
                   add_signal delete_signal alive_signal auto_unloop
                   add_work delete_work add_idle delete_idle run_idle
                   @loops @alarms @immediate $now $work $idle 
                   $signal_rd $alive_signal);
@ISA = qw(WEC::Kernel);
@EXPORT_OK = qw(add_read add_write delete_read delete_write 
                add_alarm delete_alarm add_work delete_work auto_unloop
                add_idle delete_idle add_signal delete_signal alive_signal
                loop unloop init readable writable);
croak "Kernel type should be '$WEC::kernel_type', not '", __PACKAGE__, "'"
    unless __PACKAGE__ eq $WEC::kernel_type;
# Older code used fileno as index, but that proved troublesome if 
# the handle gets closed while already selected work is pending

$alive_signal = 1;
*check_signals = \%WEC::Kernel::signals;

my $polls  = IO::Poll->new();
my (%read_refs, %write_refs);

sub add_read($*$) {
    my $fd = $_[1];
    croak "Descriptor $fd already selected for read" if $read_refs{$fd};
    $read_refs{$fd} = $_[2];
    $polls->mask($_[1] => $write_refs{$fd} ? POLLIN | POLLOUT : POLLIN);
}

sub add_write($*$) {
    my $fd = $_[1];
    croak "Descriptor $fd already selected for write" if $write_refs{$fd};
    $write_refs{$fd} = $_[2];
    $polls->mask($_[1] => $read_refs{$fd} ? POLLOUT | POLLOUT : POLLOUT);
}

sub delete_read($*) {
    my $fd = $_[1];
    croak "Descriptor $fd wasn't selected for read" unless 
        delete $read_refs{$fd};
    if ($write_refs{$fd}) {
        $polls->mask($_[1] => POLLOUT);
    } else {
        $polls->remove($_[1]);
    }
}

sub delete_write($*) {
    my $fd = $_[1];
    croak "Descriptor $fd wasn't selected for write " unless 
        delete $write_refs{$fd};
    if ($read_refs{$fd}) {
        $polls->mask($_[1] => POLLIN);
    } else {
        $polls->remove($_[1]);
    }
}

sub loop {
    until (@loops) {
        if (@alarms > 1 || @immediate || $work) {
            run_now();
            # next if !%read_refs && !%write_refs;
            return shift @loops if @loops;
        }
        ($polls->poll($work || $idle ? 0 : 
                      @alarms > 1 ? $alarms[1][0]-$now > 0 ? $alarms[1][0]-$now : 0 : 
                      %write_refs  || keys %read_refs > 1 || %WEC::Kernel::check_signals ? () : last
                      ) || ($idle && !$work && (@alarms <= 1 || $alarms[1][0] > $now) && run_idle, next)) >= 0 or $! == EINTR() ? next : die "Poll failed: $!";
        ($read_refs{$_} || next)->() for 
            $polls->handles(POLLIN|POLLHUP|POLLERR|POLLNVAL);
        ($write_refs{$_} || next)->() for 
            $polls->handles(POLLOUT|POLLHUP|POLLERR|POLLNVAL);
    }
    return shift @loops;
}

sub readable {
    return keys(%read_refs)-1 unless wantarray;
    my $fd = "$signal_rd";
    return grep $_ ne $fd, keys %read_refs;
}

sub writable {
    return keys %write_refs;
}

*init = \&WEC::Kernel::signal_init;

1;
