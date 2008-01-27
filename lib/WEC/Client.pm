package WEC::Client;
use 5.006;
use strict;
use warnings;
use Carp;

use WEC::Socket qw(start_connect);

our $VERSION = "0.01";
our @CARP_NOT	= qw(WEC::Socket WEC::FieldConnection WEC::Connection);

use base qw(WEC::Object);

our %client_options =
    (AutoClean	=> 1,
     Connect		=> undef,
     ConnectFail	=> undef,
     Close		=> undef,
     Die		=> undef);
our %flow_options = ();
our %default_options = (%client_options, %flow_options);

sub client_options {
    return \%client_options;
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
    my $client = $class->SUPER::new(delete $params{Parent});

    $client->{connections} = {};
    $client->{destination} = delete $params{Destination};

    $client->{options}	= \my %options;
    my $default_options = $client->default_options;
    my $work;
    for (keys %$default_options) {
        $work = exists $params{$_} ? delete $params{$_} : $default_options->{$_};
        $options{$_} = $work if defined $work;
    }

    $work = $client->can("init");
    $work->($client, \%params) if $work;

    croak("Unknown options ", join(", ", keys %params)) if %params;

    return $client;
}

sub connect : method {
    my $client = shift;
    my $destination = shift || $client->{destination} ||
        croak "No destination address specified and no default";
    my ($peer, $errno) = start_connect($destination);
    # nagle($peer, 0) if $destination =~ /^tcp:/;
    my $connection = $client->connection_class->new_client
        ($client, $peer, $client->{options}, $errno, $destination);
    $client->{connections}{$peer} = $connection;
    return $connection;
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

sub connections {
    return values %{shift->{connections}};
}

sub destination {
    return shift->{destination};
}

sub options {
    return shift->{options};
}

sub abort {
    my $client = shift;
    # Drop all connections
    $_->_close("abort") for values %{$client->{connections}};
    croak "Connections left even after closing them all" if
        %{$client->{connections}};
}

sub DESTROY {
    my $client = shift;
    $client->abort;
    $client->SUPER::DESTROY;
}

1;
