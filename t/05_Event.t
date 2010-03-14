# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl 05_Event.t'
use warnings;
use strict;

use Test::More;
unless (eval { require Event }) {
        plan skip_all => "Can't find the Event module";
        exit;
}
plan 'no_plan';
            
is($WEC::kernel_type, undef, 'No event class set');
use_ok('WEC');
is($WEC::kernel_type, 'WEC::Event', 'Event class set to WEC::Event');

use_ok('t::TestKernel')
