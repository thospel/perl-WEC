# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl 02_RawSelect.t'
use warnings;
use strict;

use Test::More 'no_plan';
            
is($WEC::kernel_type, undef, 'No event class set');
use_ok('WEC', qw(Select));
is($WEC::kernel_type, 'WEC::Select', 'Event class set to WEC::Select');

use_ok('t::TestKernel')
