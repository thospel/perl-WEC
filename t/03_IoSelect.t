# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl 03_IoSelect.t'
use strict;
use warnings;

use Test::More 'no_plan';

is($WEC::kernel_type, undef, 'No event class set');
use_ok('WEC', qw(IO::Select));
is($WEC::kernel_type, 'WEC::IO::Select', 'Event class set to WEC::IO::Select');

use_ok('t::TestKernel')
