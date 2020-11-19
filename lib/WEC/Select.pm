package WEC::Select;
# raw select based event loop
use 5.008004;
# Otherwise you will sometimes run into bug #30027
use strict;
use warnings;
use POSIX qw(EINTR);
use Carp;

use WEC::Kernel qw(unloop add_alarm delete_alarm run_now
                   add_work delete_work add_idle delete_idle run_idle
                   add_signal delete_signal alive_signal auto_unloop
                   @loops @alarms @immediate $now $work $idle
                   $signal_rd $alive_signal);
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

my $read_mask  = "";
my $write_mask = "";
my (%read_refs, %write_refs);

sub add_read($*$) {
    defined(my $fd = fileno($_[1])) || croak "Not a filehandle";
    croak "Descriptor $fd already selected for read" if $read_refs{$fd};
    $read_refs{$fd} = $_[2];
    vec($read_mask, $fd, 1) = 1;
}

sub add_write($*$) {
    defined(my $fd = fileno($_[1])) || croak "Not a filehandle";
    croak "Descriptor $fd already selected for write" if $write_refs{$fd};
    $write_refs{$fd} = $_[2];
    vec($write_mask, $fd, 1) = 1;
}

sub delete_read($*) {
    defined(my $fd = fileno($_[1])) || croak "Not a filehandle";
    croak "Descriptor $fd wasn't selected for read" unless $read_refs{$fd};
    # This strange assign before delete is to poison the reference @work in
    # sub loop may still have
    $read_refs{$fd} = undef;
    delete $read_refs{$fd};
    vec($read_mask, $fd, 1) = 0;
    $read_mask =~ s/\x00+\z//;
}

sub delete_write($*) {
    defined(my $fd = fileno($_[1])) || croak "Not a filehandle";
    croak "Descriptor $fd wasn't selected for write " unless $write_refs{$fd};
    # This strange assign before delete is to poison the reference @work in
    # sub loop may still have
    $write_refs{$fd} = undef;
    delete $write_refs{$fd};
    vec($write_mask, $fd, 1) = 0;
    $write_mask =~ s/\x00+\z//;
}

sub loop {
    my ($r, $w);
    until (@loops) {
        # return if $read_mask eq "" && $write_mask eq "";
        if (@alarms > 1 || @immediate || $work) {
            run_now();
            return shift @loops if @loops;
        }
        (select($r = $read_mask, $w = $write_mask, undef,
                $work || $idle ? 0 :
                @alarms > 1 ? $alarms[1][0]-$now > 0 ? $alarms[1][0]-$now : 0 :
                %write_refs || keys %read_refs > 1 || %WEC::Kernel::check_signals ? undef : last) ||
         ($idle && !$work && (@alarms <= 1 || $alarms[1][0] > $now) && run_idle, next)) >=0 or $! == EINTR() ? next : die "Select failed: $! (", $!+0, ")";
        $$_ && $$_->() for
            \@read_refs{ grep vec($r, $_, 1), keys %read_refs},
            \@write_refs{grep vec($w, $_, 1), keys %write_refs};
    }
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

*init = \&WEC::Kernel::signal_init;

1;
