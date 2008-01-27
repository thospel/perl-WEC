# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl d/09_PoeIoPoll.t'

use warnings;
use strict;

use Test::More;
use IO::Poll;
unless (eval { require POE }) {
        plan skip_all => "Can't find the POE module";
        # Make parser happy with more than one use
        $WEC::POE::poe_type = $WEC::POE::poe_type;
        exit;
}
plan 'no_plan';
            
is($WEC::kernel_type, undef, 'No event class set');
use_ok('WEC');
if ($WEC::POE::poe_type	eq 'IO_Poll' &&
    $WEC::kernel_type	eq 'WEC::POE') {
    pass("kernel_type $WEC::kernel_type and POE type $WEC::POE::poe_type");
} else {
    if ($WEC::POE::poe_type eq 'Select' && $WEC::kernel_type eq 'WEC::POE') {
	diag("Wrong kernel_type $WEC::kernel_type and POE type $WEC::POE::poe_type. Skipping tests");
    } else {
	diag("Wrong kernel_type $WEC::kernel_type and POE type $WEC::POE::poe_type.");
	fail("Wrong type");
    }
    exit 0;
}

use_ok('t::TestKernel')
