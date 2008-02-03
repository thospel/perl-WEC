package WEC::Flow;
use 5.006;
use strict;
use warnings;
use Carp;

use WEC::Object qw(%objects);

our $VERSION = "1.000";

use base qw(WEC::Object);

sub _drop {
    my $flow = shift;
    $flow->{ended} = 1;
    # if ($flow->{parent}) {
    $flow->{parent}->_drop_flow($flow->{id}, $flow);
    delete $flow->{parent};
    # }
    $flow->stop_all_events;
    $flow->{options} = $flow->{user_data} = undef;
}

sub _abort_flow {
    shift->_drop;
}

sub id {
    return shift->{id};
}

sub options {
    return shift->{options};
}

sub connection {
    return shift->{parent};
}

sub server {
    return shift->{parent}->server;
}

sub client {
    return shift->{parent}->client;
}

sub extend {
    my $flow = shift;
    if ($flow->{ended}) {
        croak "Flow $flow->{id} is already ended"    if $flow->{ended} > 0;
        croak "Flow $flow->{id} is already extended" if $flow->{ended} < 0;
        die "Assertion failed: Flow $flow has ended field $flow->{ended}";
    }
    $flow->{ended} = -1;
}

sub id2string {
    return $_[1];
}

1;
