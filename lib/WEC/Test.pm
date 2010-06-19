package WEC::Test;
use warnings;
use strict;
use Carp;

use Time::HiRes qw(time);
use WEC qw(api=1 unloop readable writable);
use WEC::Socket qw(unix inet);
use Test::More (import => []);

use Exporter::Tidy
    default => [qw(loop warn_loop plain_loop unloop delta check_fd
                   check_objects make_socket make_pair
                   write_test write_tests
                   $trace_line $hit $client $server $socket_type
                   $socket $destination @warnings)];

BEGIN { $^W = 1 };
our (@warnings, $socket_type, $socket, $destination, $client, $server, $hit);

my $old = time;
sub delta {
    my $new = time;
    my $r = sprintf("%8.6f", $new - $old);
    $old = $new;
    return $r;
}

sub warn_loop {
    @warnings = ();
    local $SIG{__WARN__} = sub { push @warnings, shift };
    return plain_loop(@_);
}

our $trace_line;
my ($class, $client_paths, @parts, $call_package);
my $object_filename	= __FILE__;
my $object_line		= __LINE__;

{
    no warnings "redefine";
    my $import;
    BEGIN {
        $import = *WEC::Test::import{CODE};
    }
    sub import {
        my ($c, %options) = @_;
        ($call_package, $object_filename, $object_line) = caller;
        if (my $prepare = $call_package->can("prepare_loop")) {
            *loop = sub {
                @warnings = ();
                local $SIG{__WARN__} = sub { push @warnings, shift };
                $prepare->();
                my $rc = WEC::loop(@_);
                diag(join("", @warnings)) if @warnings;
                is(@warnings, 0, "No warnings");
                return $rc;
            };
            *plain_loop = sub {
                $prepare->();
                return WEC::loop(@_);
            };
        } else {
            *loop = sub {
                @warnings = ();
                local $SIG{__WARN__} = sub { push @warnings, shift };
                my $rc = WEC::loop(@_);
                diag(join("", @warnings)) if @warnings;
                is(@warnings, 0, "No warnings");
                return $rc;
            };
            *plain_loop = \&WEC::loop;
        }
        $trace_line   = delete $options{TraceLine} if
            exists $options{TraceLine};
        $client_paths = delete $options{ClientPaths} if
            exists $options{ClientPaths};
        defined($class =    delete $options{Class}) || croak "No class";
        defined(my $parts = delete $options{Parts}) || croak "No parts";
        ref($parts) eq "ARRAY" || croak "Parts isn't an array reference";
        @parts = @$parts;
        croak "Unknown option ", join(", ", keys %options) if %options;
        @_ = $c;
        goto &$import;
    }
}

my (@base_fd, $last_filename, $last_line);
sub check_fd {
    my @fd;
    for (0..4) {
        pipe($fd[2*$_], $fd[2*$_+1]) || die "Could not create pipe: $!";
    }
    for (@fd) {
        $_ = fileno($_) || die "Huh ? No fileno ?";
    }

    my @r = WEC->readable();
    diag("Still Readable fds: @r") if @r;
    my @w = WEC->writable();
    diag("Still Writable fds: @w") if @w;

    if (!@base_fd) {
        # diag("Free fds: @fd");
        @base_fd = @fd;
    } elsif ("@base_fd" eq "@fd") {
        pass("No fd leaked");
    } else {
        my %lost;
        @lost{@base_fd} = ();
        delete @lost{@fd};
        my (undef, $filename, $line) = caller;
        diag("Lost fd " . join(",", keys %lost) .
             " between $last_filename line $last_line and $filename line $line");
        @base_fd = @fd;
        @_ = "fd leaked";
        ($last_filename, $last_line) = ($filename, $line);
        goto &fail;
    }
    (undef, $last_filename, $last_line) = caller;
    diag("$last_filename: $last_line ". delta) if $trace_line;
}

my %old_n;
sub check_objects {
    my %expect = @_;
    my $good = 1;
    my (undef, $filename, $line) = caller;
    for (@parts) {
        my $class = $class . "::" . $_;
        next if (my $n = $class->object_count() - ($expect{$_} || 0)) == ($old_n{$_} || 0);
        diag("$n $class objects leaked between $object_filename line $object_line and $filename line $line");
        $good = 0;
        $old_n{$_} = $n;
    }
    is($good, 1, "No objects leaked");
    ($object_filename, $object_line) = ($filename, $line);
}

sub make_socket {
    ($socket, $destination) =
        $socket_type eq "Unix" ? unix() :
        $socket_type eq "Tcp"  ? inet() :
        die "Unknown socket type '$socket_type'";
}

sub make_pair {
    my ($client_opts, $server_opts, $no_dest) = @_;
    $hit = 0;
    WEC->init;
    make_socket;
    $client = "${class}::Client"->new
        ($no_dest ? () : (Destination => $destination),
         $socket_type eq "Unix" && $client_paths ? (Paths =>$destination) : (),
         @{$client_opts||[]});
    $server = "${class}::Server"->new
        (Handle	=> $socket,
         $socket_type eq "Unix" && !$client_paths ? (Paths=>$destination) : (),
         @{$server_opts || []});
    $socket = undef;
    return $client->connect($no_dest ? $destination : ());
}

# Don't need to test for these, they come with perl
my %safe = ("IO::Select" => 1,
            "IO::Poll"   => 1);
sub write_test {
    my ($file, $type) = @_;
    require WEC;
    my %preferred = map { $_ => 1 } @WEC::wec_preferred;
    my $poe_type = $type;
    $poe_type =~ s/^POE:://;
    my $new = "$file.new.$$";
    {
        open(my $fh, ">", $new) || die "Could not create $new: $!";
        local $\;
        eval {
            my $string = "# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl $file'

use warnings;
use strict;

use Test::More;
";
            if ($poe_type !~ s/^([a-z])/\u$1/ && !$safe{$poe_type}) {
                $string .= "unless (eval { require $poe_type }) {
        plan skip_all => \"Can't find the $poe_type module\";
        exit;
}
";
            }
            if ($type =~ /POE/) {
                $string .= "use $poe_type;\n" if $safe{$poe_type};
                $poe_type =~ s/::/_/g;
                $string .= "unless (eval { require POE }) {
        plan skip_all => \"Can't find the POE module\";
        # Make parser happy with more than one use
        \$WEC::POE::poe_type = \$WEC::POE::poe_type;
        exit;
}
";
            }
            $type =~ s/\b([a-z])/\u$1/;
            $string .= "plan 'no_plan';

is(\$WEC::kernel_type, undef, 'No event class set');
";
            if ($type =~ /POE/) {
                $string .= "use_ok('WEC');
ok(\$WEC::kernel_type	eq 'WEC::POE' &&
   \$WEC::POE::poe_type	eq '$poe_type', 'Event class set to $type');\n\n";
            } else {
                if ($preferred{$type}) {
                    $string .= "use_ok('WEC');\n";
                } else {
                    $string .= "use_ok('WEC', qw($type));\n";
                }
                $string .= "is(\$WEC::kernel_type, 'WEC::$type', 'Event class set to WEC::$type');\n\n";
            }
            $string .= "# Set up the main window now so the connection won't count as a lost fd
WEC::Tk->init;
# Try to minimize user interaction
{
    no warnings 'once';
    \$WEC::Tk::event_window->geometry('+10+10');
}

" if $type eq "Tk";
$string .= "sub prepare_loop {
    no warnings 'once';
    \$POE::Kernel::poe_main_window->geometry('+10+10');
}

" if $type eq "POE::Tk";
            $string .= "use_ok('t::TestKernel')\n";
            $string =~ s/use Test::More;\s*plan\s+(.*)/use Test::More $1/g;
            print $fh $string or die "Error writing to $new: $!";
            close($fh) || die "Error closing $new: $!";
            rename($new, $file) || die "Could not rename $new to $file: $!";
        };
    }
    if ($@) {
        unlink($new) || die "Could not unlink $new: $! after $@";
        die $@;
    }
}

my @drivers = qw(select IO::Select IO::Poll Event Tk Glib POE::select POE::IO::Poll POE::Event POE::Tk);

sub write_tests {
    my $num = 2;
    for my $name (@drivers) {
        my $f = $name;
        $f = "Raw\u$name" if $name =~ /^[a-z]/;
        $f =~ s/(::[a-z])/\U$1/g;
        $f =~ s/([A-Z]+)/\u\L$1/g;
        $f =~ s/::/_/g;
        $f =~ s/([a-z])_([A-Z])/$1$2/g;
        my $file = sprintf("t/%02d_%s.t", $num++, $f);
        print STDERR "File $file";
        write_test($file, $name);
    }
}

1;
