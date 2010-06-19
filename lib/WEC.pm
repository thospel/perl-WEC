package WEC;

use 5.008001;
use strict;
use warnings;
use Carp;

# test for bug #30733
BEGIN {
    my $delete_ok;
    {
        package WEC::Delete;

        sub DESTROY {
            $delete_ok = 1;
        }

        my @a = bless [], "WEC::Delete";
        my $result = delete $a[0];
    }
    *DELETE_BUG = $delete_ok ? sub() { !1 } : sub() { 1 };
}

require Exporter;

our @EXPORT = qw();
our @IMPORT = qw(add_read delete_read add_write delete_write
                 add_alarm delete_alarm add_work delete_work
                 add_idle delete_idle add_signal delete_signal alive_signal
                 loop unloop auto_unloop init readable writable);
our @EXPORT_OK = (@IMPORT, qw(DELETE_BUG kernel_type api));

our $kernel_type;

our $VERSION = '1.000';
our $API     = 1;

our @wec_preferred = qw(POE Event Tk Glib);
our @wec_supported = (@wec_preferred, qw(IO::Poll IO::Select Select));

sub import {
    my $class = shift;
    if (!$kernel_type) {
        my %wec_supported;
        @wec_supported{@wec_supported} = ();
        # First try to find a forced kernel in the import arguments
        my %supported;
        for (@_) {
            $supported{$_}++ if exists $wec_supported{$_};
        }
        if (keys %supported) {
            croak("Requesting multiple event kernels (",
                  join(", ", keys %supported), ")") if keys %supported > 1;
            $kernel_type = "WEC::" . (keys %supported)[0];
        } else {
            # Then look at the already loaded modules
            $kernel_type = "WEC::Select";
            for (@wec_preferred) {
                my $name = "$_.pm";
                $name =~ s!::!/!g;
                if (exists $INC{$name}) {
                    $kernel_type = "WEC::$_";
                    last;
                }
            }
        }
        my $name = "$kernel_type.pm";
        $name =~ s!::!/!g;
        require $name;
    }
    $kernel_type->import(@IMPORT);

    return unless @_;
    my @globals;
    for (@_) {
        if (/^[[:upper:]]/ && $_ ne "DELETE_BUG") {
            my $name = "WEC::$_.pm";
            $name =~ s!::!/!g;
            require $name;
            $kernel_type->import();
        } elsif (/^api=(\d+)\z/) {
            croak "Requesting WEC api $1, but I only support api $API" unless
                $1 == $API;
        } else {
            push @globals, $_;
        }
    }

    return unless @globals;
    @_ = ($class, @globals);
    goto &Exporter::import;
}

sub api {
    return $API;
}

sub kernel_type {
    return $kernel_type;
}

1;

__END__

=head1 NAME

WEC - Wrapped Event Class

=head1 SYNOPSIS

  use WEC qw(api=1 loop unloop)

  WEC->init;

  $alarm_id = WEC->add_alarm($period, $callback);
  WEC->delete_alarm($alarm_id);

  WEC->add_read($handle, $callback);
  WEC->delete_read($handle);
  WEC->add_write($handle, $callback);
  WEC->delete_write($handle);

  $work_id = WEC->add_work($callback);
  WEC->delete_work($work_id);

  $idle_id = WEC->add_idle($callback);
  WEC->delete_idle($idle_id);

  $signal_id = WEC->add_signal($signal, $callback);
  WEC->delete_signal($signal_id);

  $result = loop();

  unloop(?$result?);

  $unloops = auto_unloop;

  $alive_signal = alive_signal
  $old = alive_signal($alive_signal);

=head1 MOTIVATION

WEC stands for C<Wrapped Event Class>. It's a wrapper for event driven kernels
(like L<Event|Event>, L<Tk|Tk> and L<POE|POE>), or it can provide a default
pure perl event driver if you are not using a seperate one to wrap.

The sort of interface provided is comparable to that of L<Event|Event>, but
actually more primitive. So if you need a fast sophisticated pure event kernel,
by all means use L<Event|Event>.

The wrapper is also meant to make it easy to develop object oriented components
that provide certain functionality through L<WEC::Object|WEC::Object>. It is
however very unlikely to grow as sophisticated as L<POE|POE> in that respect.
So if you need an extensive pluggable system with lots of existing
functionality, have a look at L<POE|POE>.

The target public for this module is people who want to write event driven
objects without yet committing to a specific event kernel and people who need
a pure perl event kernel. Both these jobs can actually also be done by
L<POE|POE>, but this module is much more lightweight and puts less restrictions
on your programming model.

The semantics and arguments of the methods haven't been fixed yet, and they may
change incompatibly in a next version of this module. To catch when this
happens, the api described here has been given number 1, and you can and should
check for that by using C<api=1> in the WEC import arguments (as demonstrated
in the L<SYNOPSIS|SYNOPSIS>).

=head1 DESCRIPTION

The WEC module provides a central facility to watch for various types of events
and notice if these events occur. You are then expected to call
L<the event loop|"loop"> which will work through the registered events,
for each of them calling a callback function, wait for that to return and then
go to the next event. If there are no more events, it will start waiting for
events to occur.

=head1 KERNELS

The following kernels have wrappers in the standard WEC distribution:

=over

=item Select

This is the default pure perl event kernel based an plain
L<select|perlfunc/"select">. Quite fast and light.

=item IO::Select

Another pure perl kernel, this time using L<IO::Select|IO::Select>.
There is no real advantage to using this over L<Select|"Select">, it's
mainly meant as a demo for how to wrap kernels. Medium in speed.

=item IO::Poll

Another pure perl kernel, this time using L<IO::Poll|IO::Poll>.
There is no real advantage to using this over L<Select|"Select">, it's
mainly meant as a demo for how to wrap kernels. Medium in speed.

=item Event

This one uses L<Event|Event> as underlying kernel. Extremely fast,
but depends on an external module that isn't pure perl.

=item Tk

A kernel based on L<Tk|Tk>, useful for making GUI programs.
Also reasonably fast.

=item Glib

A kernel based on L<Glib|Glib> utility library. It's also the underlying event
library used by L<Gtk2|Gtk2>, so you can use it in combination with that
to make GUI programs. Medium in speed.

=item POE

A kernel built around the very powerful L<POE|POE> system. L<POE|POE> is itself
actually an event wrapper and will select an underlying kernel type for itself.
It's currently rather slow though.

=back

You can force WEC to use any of these by mentioning the name in the import
list. E.g. to run a L<Tk> based WEC, you would use:

    use WEC qw(api=1 Tk)

If you don't pass an explicit kerneltype, WEC will choose one for itself at
import time by looking at the modules already loaded, and then choosing one
based on that. The order of preference is L<POE|"POE">, L<Tk|"Tk"> and then
L<Event|"Event">. If none of these is loaded, it will fall back to
L<Select|"Select">.

So the following two are roughly equivalent:

    use WEC qw(api=1 Event);

and:

    use Event;
    use WEC qw(api=1);

=head1 METHODS

=over

=item X<init>WEC->init

Some event kernels need some preparation before they can set up event
callbacks. This function does the needed preparation. The method is guaranteed
idempotent, so multiple calls will behave just like one. For example
L<WEC::POE|WEC::POE> uses it to set up a L<POE::Session|POE::Session>,
while L<WEC::Tk|WEC::Tk> uses it to set up a a L<MainWindow|Tk::MainWindow>.

Some kernels need a new init if after leaving L<the event loop|"loop"> you
want to start again.

=item X<add_alarm>$alarm_id = WEC->add_alarm($period, $function_reference)

After (at least) $period seconds have passed since the moment this method was
called, L<loop|"loop"> will execute:

    $function_reference->()

The add_alarm method returns a handle that you can later use to remove the
alarm with L<delete_alarm|"delete_alarm">.

=item X<delete_alarm>WEC->delete_alarm($alarm_id)

Removes the alarm represented by $alarm_id (which must be the result of an
earlier L<add_alarm|"add_alarm">). It's ok to call this method multiple times
or to call it when the alarm has already gone off. The extras will simply be
ignored.

=item X<add_read>WEC->add_read($handle, $function_reference)

Sets up L<the main event loop|"loop"> to call

    $function_reference->()

whenever $handle is readable. This callback will then presumably do some
operation on the handle which will possibly consume whatever was readable,
after which the handle may not be readable anymore (or it may be if anything
was left). For this reason it doesn't make sense to have multiple callbacks
associated with a readability event, so you can only set up one of these
per handle, and trying to set up a second one on a handle will cause an
exception.

It is also possibly (but unlikely) that when you try to operate on a readable
handle some kernel level event stops the operation from going through, so
it's still a good idea to make your handle non-blocking (for example with
L<the WEC::Socket blocking call|WEC::Socket/blocking>) and check for a failing
operation and simply return without error in case $! is EINTR, EWOULDBLOCK or
EAGAIN.

=item X<delete_read>WEC->delete_read($handle)

Since there can only be one readability callback associated with a handle, the
$handle is sufficient information to find and remove such a callback. This
method will throw an exception if there is no such callback.

=item X<add_write>WEC->add_write($handle, $callback)

Sets up L<the main event loop|"loop"> to call

    $function_reference->()

whenever $handle is writable. This callback will then presumably do some
operation on the handle which will then possibly fill up some internal buffer
after which the handle may not be writable anymore (or it may be if there is
still space). For this reason it doesn't make sense to have multiple callbacks
associated with a writability event, so you can only set up one of these per
handle, and trying to set up a second one on a handle will cause an exception.

It is also possibly (but unlikely) that when you try to operate on a writable
handle some kernel level event stops the operation from going through, so
it's still a good idea to make your handle non-blocking (for example with
L<the WEC::Socket blocking call|WEC::Socket/blocking>) and check for a failing
operation and simply return without error in case $! is EINTR or EWOULDBLOCK or
EAGAIN.

=item X<delete_write>WEC->delete_write($handle)

Since there can only be one writability callback associated with a handle, the
$handle is sufficient information to find and remove such a callback. This
method will throw an exception if there is no such callback.

=item X<add_work>$work_id = WEC->add_work($callback)

This method adds a work procedure that will be called from L<loop|"loop"> as

    $callback->()

all the time in a round robin fashion with all other pending work whose time
has come. The add_work method returns a handle that you later use to remove
the work procedure with L<delete_work|"delete_work">.

This method is meant for the case where you have one or more long running CPU
intensive tasks, but you don't want to block access to
L<the main event loop|"loop"> which would stop other tasks from making any
progress. Instead you should now split up the long running task in many small
pieces and run these pieces one at a time in the work procedure and return
from that after each piece. When the task is done, delete the callback. This
of course will probably involve rewriting your code, probably as a state
machine. After running a work procedure other events will be processed before
the next work procedure gets its turn, so you don't have to worry that they get
run in a bunch and together block the eventloop for too long.

For example, this CPU intensive loop:

    sub sum {
        my $n = 0;
        my $max = shift;
        for ($i=0; $i < $max; $i++) {
            $n += $i;
        }
        print $n;
    }

or (written as a while):

    sub sum {
        my $n = 0;
        my $max = shift;
        my $i = 0;
        while ($i < $max) {
            $n += $i;
            $i++;
        }
        print $n;
    }

could be replaced by:

    sub start_sum {
        my $n = 0;
        my $max = shift;
        my $i = 0;
        my $work_id;
        $work_id = WEC->add_work(sub {
            if ($i < $max) {
                $n += $i;
                $i++;
            } else {
                WEC->delete_work($work_id);
                print $n;
            }
        });
    }

=item X<delete_work>WEC->delete_work($work_id)

Removes the work procedure represented by $work_id (which must be the result
of an earlier L<add_work|"add_work">). It's ok to call this method multiple
times. The extras will simply be ignored.

=item X<add_idle>my $idle_id = WEC->add_idle($callback)

This method adds an idle procedure that will be called from L<loop|"loop"> as

    $callback->()

but only if there are no other (non-idle) events going on. Among themselves
idle procedures are called in a round-robin fashion.

=item X<delete_idle>WEC->delete_idle($idle_id)

Removes the idle procedure represented by $idle_id (which must be the result
of an earlier L<add_idle|"add_idle">). It's ok to call this method multiple
times. The extras will simply be ignored.

=item X<add_signal>$signal_id = WEC->add_signal($signal, $callback)

Adds a handler for signal $signal (designated by a string). Signal handling
works by setting a flag for any signal type that comes in, and only calling
the callback from L<the event loop|"loop"> if the flag is set. So the calls
will never be asynchronous. You can have multiple handlers for one signal,
and they will all be called. Don't use L<%SIG|perlvar/"%SIG"> based signal
handling while using this module when the underlying kernel is L<POE|POE>.
For all other kernels, for a given signal you can either use
L<%SIG|perlvar/"%SIG"> based handling or WEC based handling, but not both at
the same time.

The function reference is called like:

    $callback->()

=item X<delete_signal>WEC->delete_signal($signal_id)

Removes the signal callback represented by $signal_id (which must be the
result of an earlier L<add_signal|"add_signal">). It's ok to call this method
multiple times. The extras will simply be ignored. When the last handler for
a given $signal is removed, L<$SIG{$signal}|perlvar/"%SIG"> will be restored
(except in L<POE|POE> based kernels where all signal handling is always under
L<POE|POE> control).

=item X<loop>$result = loop()

This starts the main event loop. It will patiently wait and dispatch registered
events when their time has come.

Most kernels will stop automatically and return undef when there is no more
work, though there may be a warning if there was no work in the first place.
The exception are the GUI kernels (except if used from POE, since that keeps
its own count ignoring gui events), where gui events remain possible so there
is always potential work.

Most kernels can also be stopped by calling unloop (except for POE).

These unfortunate inconsistencies mean that you should either write
your program to run forever (until killed), do an explicit unloop if and
only if there is no more work, or just exit when you decide you're done
(that last case is fortunately very easy and very common).

=item X<unloop>unloop(?$result?)

Causes the mainloop to finish with returncode $result (defaults to undef if
not given). See the comments under L<loop|"loop"> for the current weakness
of unloop.

=item X<auto_unloop>$unloops = auto_unloop

Returns true if L<the event loop|"loop"> automatically exits if there are no
more callbacks, false otherwise.

=item X<alive_signal>$alive_signal = alive_signal

Returns true if the existence of a signal callback will keep
L<the event loop|"loop"> running. Will be true unless changed.

=item $old = alive_signal($alive_signal);

If given a true argument, it will make sure that having alarm handlers will
keep L<the event loop|"loop"> running. If given a false argument, they will
not.

Returns the old value.

Not all event kernels are able to auto unloop when there are no
signal handlers, others cannot auto unloop at all. For these kernels the
function will throw an exception if you give a false argument.

=back

=head1 EXPORT

None by default, but the following ones may be requested:

=over

=item loop

=item unloop

=item api

=item kernel_type

=item auto_unloop

=item alive_signal

=back

=head1 SEE ALSO

L<WEC::Select>,
L<WEC::IO::Select>,
L<WEC::IO::Poll>,
L<WEC::Tk>,
L<WEC::Event>,
L<WEC::POE>,
L<WEC::Object>,
L<WEC::FieldObject>,
L<POE>,
L<Event>

=head1 AUTHOR

Ton Hospel, E<lt>WEC@ton.iguana.beE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2004 by Ton Hospel

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.6.1 or,
at your option, any later version of Perl 5 you may have available.

=cut
