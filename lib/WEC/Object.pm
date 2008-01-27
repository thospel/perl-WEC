package WEC::Object;
use warnings;
use strict;
use Carp;
use Scalar::Util qw(weaken);
use WEC qw(api=1);

our $VERSION = "0.01";

use AutoLoader qw(AUTOLOAD);

require Exporter;
our @ISA = qw(Exporter);
our @EXPORT_OK = qw(%objects);
our @type_list = qw(alarm);

# use fields qw(parent alarms events user_data);
our %objects;
sub object_count {
    return $objects{ref($_[0]) || $_[0]} || 0 if @_;
    my $objects = 0;
    $objects += $_ for values %objects;
    return $objects;
}

sub new {
    my $class = shift;
    $objects{$class}++;
    return bless {
        alarms  => {},
        events => {},	# Should maybe only be on Connection ?
    }, $class unless defined $_[0];
    my $object = bless {
        alarms	=> {},
        events => {},
    }, $class;
    weaken($object->{parent} = shift);
    return $object;
}

sub type_list {
    return @type_list;
}

sub parent {
    return shift->{parent};
}

sub DESTROY {
    my $object = shift;
    $object->stop_all_events;
    $objects{ref $object}--;
}

1;

__END__

sub add_alarm {
    my ($object, $period, $fun, @params) = @_;
    my $alarm;
    weaken($object);
    $alarm = WEC->add_alarm($period, sub {
        delete $object->{alarms}{$alarm};
        $fun->($object, @params);
    });
    $object->{alarms}{$alarm} = $alarm;
    return "$alarm";
}

sub delete_alarm {
    my ($object, $aid) = @_;
    my $alarm = delete($object->{alarms}{$aid}) ||
        croak "There was no alarm called $aid";
    WEC->delete_alarm($alarm);
}

sub delete_event {
    my ($object, $eid) = @_;
    my $event = delete $object->{events}{$eid} ||
        croak "There was no event called $eid";
    # Leaves only COMMAND (maybe should only undef CALLBACK ?)
    $#$event = 0;
}

sub stop_all_events {
    my $object = shift;
    if (%{$object->{alarms}}) {
        my @alarms = values %{$object->{alarms}};
        %{$object->{alarms}} = ();
        WEC->delete_alarm($_) for @alarms;
    }
    if (%{$object->{events}}) {
        my @events = values %{$object->{events}};
        %{$object->{events}} = ();
        # Leaves only COMMAND (maybe should only undef CALLBACK ?)
        $#$_ = 0 for @events;	
    }
}

sub user_data {
    return shift->{user_data} unless @_ > 1;
    my $req = shift;
    my $old = $req->{user_data};
    $req->{user_data} = shift;
    return $old;
}

1;

__END__

=item X<object_count>my $nr_requests = WEC::FastCGI::Request::object_count()

This function is mainly meant for debugging leaks. Whenever a request object
is created, an internal count is increased, and whenever one gets destroyed an
internal count is decreased. This class method allows you to retrieve the
current count, after which you can compare that with what you expected.

=item X<user_data>$user_data = $request->user_data

With every $request object you can associate one scalar of user data
(default undef). This method returns that user data.

=item X<user_data>$old_user_data = $request->user_data($new_user_data)

Set new user data, returning the old value.
