package WEC::Server;
use 5.006;
use strict;
use warnings;
use Carp;

use WEC qw(api=1);
use WEC::Socket qw(blocking listener);

our $VERSION = "1.000";

use base qw(WEC::Object);

our %server_options =
    (AutoClean	=> 1,
     PreAccept	=> undef,
     Accept	=> undef,
     Close	=> undef,
     Die	=> undef);

our %flow_options =
    (Abort	=> undef);

our %default_options = (%server_options, %flow_options);

sub server_options {
    return \%server_options;
}

sub flow_options {
    return \%flow_options;
}

sub default_options {
    return \%default_options;
}

sub new {
    croak "$_[0] requires an even number of parameters" unless @_ % 2;
    my ($class, %params) = @_;
    my $server = $class->SUPER::new(delete $params{Parent});

    $server->{connections} = {};
    $server->{handles} = delete $params{Handle};
    $server->{handles} = [$server->{handles}] if defined $server->{handles} &&
        ref $server->{handles} ne "ARRAY";

    my $accepting = exists($params{Accepting}) ? delete $params{Accepting} : 1;

    $server->{options}	= \my %options;
    my $default_options = $server->default_options;
    my $work;
    for (keys %$default_options) {
        $work = exists $params{$_} ? delete $params{$_} : $default_options->{$_};
        $options{$_} = $work if defined $work;
    }

    my %paths;
    if (defined(my $paths = delete $params{Paths})) {
        $paths{$_}++ for ref($paths) ? @$paths : $paths;
    }

    $work = $server->can("init");
    $work->($server, \%params) if $work;

    if (defined(my $listen = delete $params{Listen})) {
        my $dport = $server->can("default_port");
        $dport = $dport->($server) if $dport;
        for (ref($listen) ? @$listen : $listen) {
            my ($socket, $path) = listener
                ($_, defined($dport) ? (DefaultPort => $dport): ());
            $paths{$path}++;
            push @{$server->{handles}}, $socket;
        }
    }

    croak("Unknown options ", join(", ", keys %params)) if %params;

    defined($server->{handles}) || croak "No Handle parameter";
    @{$server->{handles}} || croak "Empty handle list";

    blocking($_, 0) for @{$server->{handles}};
    if ($accepting) {
        $server->connection_class->start_accepting($server, $server->{handles});
        $server->{accepting} = 1;
    }

    # Only register cleanup paths when we know we will succeed
    $server->{paths} = \%paths;
    return $server;
}

sub new_connection {
    my $connections = shift->{connections};
    my $handle = shift;
    die "Assertion failed: socket $handle already registered" if exists
        $connections->{$handle};
    $connections->{$handle} = shift;
}

sub _drop_connection {
    delete shift->{connections}{shift()};
}

sub add_path {
    my $server = shift;
    $server->{paths}{$_}++ for @_;
}

sub connections {
    return values %{shift->{connections}};
}

sub handles {
    return shift->{handles};
}

sub options {
    return shift->{options};
}

sub accepting {
    my $server = shift;
    return $server->{accepting} unless @_;
    my $old = $server->{accepting};
    if ($server->{accepting} = shift) {
        $server->connection_class->start_accepting($server, $server->{handles})
            unless $old;
    } elsif ($old) {
        WEC->delete_read($_) for @{$server->{handles}};
    }
    return $old;
}

sub abort {
    my $server = shift;
    # Check if we already closed down
    croak "Already aborted" unless $server->{handles};
    # Stop accepting
    if ($server->{accepting}) {
        WEC->delete_read($_) for @{$server->{handles}};
    }
    $server->{handles} = undef;
    # Drop all connections
    $_->_close("abort") for values %{$server->{connections}};
    croak "Connections left even after closing them all" if
        %{$server->{connections}};
}

sub DESTROY {
    my $server = shift;
    $server->abort if $server->{handles};
    for my $path (keys %{$server->{paths}}) {
        next if $path !~ s!\Aunix://!!i;
        $path =~ s/\?.*//s;
        $path =~ s/%([0-9A-Fa-f]{2})/chr hex $1/eg;
        unlink($path) || die "Could not unlink '$path': $!";
    }
    $server->SUPER::DESTROY;
}

1;
