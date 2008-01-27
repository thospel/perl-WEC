# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl d/04_IoPoll.t'

use warnings;
use strict;

use Test::More 'no_plan';
            
is($WEC::kernel_type, undef, 'No event class set');
use_ok('WEC', qw(IO::Poll));
is($WEC::kernel_type, 'WEC::IO::Poll', 'Event class set to WEC::IO::Poll');

use_ok('t::TestKernel')
