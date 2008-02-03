package WEC::IO::Select;
# Raw select based event loop
use 5.006;
use strict;
use warnings;
use POSIX qw(EINTR);
use Carp;
use IO::Select;

use WEC::Kernel qw(add_alarm delete_alarm unloop run_now
                   add_signal delete_signal alive_signal auto_unloop
                   add_work delete_work add_idle delete_idle run_idle
                   @loops @alarms @immediate $now $work $idle $signal_rd
                   $alive_signal);
use vars qw($VERSION @ISA @EXPORT_OK);
$VERSION = "1.000";
@ISA = qw(WEC::Kernel);
@EXPORT_OK = qw(add_read add_write delete_read delete_write
                add_alarm delete_alarm add_work delete_work auto_unloop
                add_idle delete_idle add_signal delete_signal alive_signal
                loop unloop init readable writable);
croak "Kernel type should be '$WEC::kernel_type', not '", __PACKAGE__, "'"
    unless __PACKAGE__ eq $WEC::kernel_type;

$alive_signal = 1;
*check_signals = \%WEC::Kernel::signals;

my $reads  = IO::Select->new();
my $writes = IO::Select->new();
# Older code used fileno as index, but that proved troublesome if
# the handle gets closed while already selected work is pending
my (%read_refs, %write_refs);

sub add_read($*$) {
    my $fh = $_[1];
    croak "Descriptor $fh already selected for read" if $read_refs{$fh};
    $read_refs{$fh} = $_[2];
    $reads->add($fh);
}

sub add_write($*$) {
    my $fh = $_[1];
    croak "Descriptor $fh already selected for write" if $write_refs{$fh};
    $write_refs{$fh} = $_[2];
    $writes->add($fh);
}

sub delete_read($*) {
    my $fh = $_[1];
    croak "Descriptor $fh wasn't selected for read" unless
        delete $read_refs{$fh};
    $reads->remove($fh);
}

sub delete_write($*) {
    my $fh = $_[1];
    croak "Descriptor $fh wasn't selected for write " unless
        delete $write_refs{$fh};
    $writes->remove($fh);
}

sub loop {
    until (@loops) {
        if (@alarms > 1 || @immediate || $work) {
            run_now();
            return shift @loops if @loops;
        }
        $! = EINTR;
        my ($r, $w) = IO::Select::select
            ($reads, $writes, undef, 
             $work || $idle ? 0 : @alarms > 1 ? 
             $alarms[1][0]-$now > 0 ? $alarms[1][0]-$now : 0 : 
             %write_refs || keys %read_refs > 1 || %WEC::Kernel::check_signals ? undef : last
             ) or 
             $! == EINTR() ? ($idle && !$work && (@alarms <= 1 || $alarms[1][0] > $now) && run_idle, next) : die "Select failed: $!";
        ($read_refs{ $_} || next)->() for @$r;
        ($write_refs{$_} || next)->() for @$w;
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
