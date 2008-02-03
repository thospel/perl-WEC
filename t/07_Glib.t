# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl 07_Glib.t'
use warnings;
use strict;

use Test::More;
unless (eval { require Glib }) {
        plan skip_all => "Can't find the Glib module";
        exit;
}
plan 'no_plan';
            
is($WEC::kernel_type, undef, 'No event class set');
use_ok('WEC');
is($WEC::kernel_type, 'WEC::Glib', 'Event class set to WEC::Glib');

use_ok('t::TestKernel')
