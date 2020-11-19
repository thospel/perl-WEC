# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl 08_PoeSelect.t'
## no critic (UselessNoCritic MagicNumbers)
use strict;
use warnings;

use Test::More;
unless (eval { require POE }) {
        plan skip_all => "Can't find the POE module";
        # Make parser happy with more than one use
        $WEC::POE::poe_type = $WEC::POE::poe_type;
        exit;
}
plan 'no_plan';

is($WEC::kernel_type, undef, 'No event class set');
use_ok('WEC');
ok($WEC::kernel_type	eq 'WEC::POE' &&
   $WEC::POE::poe_type	eq 'Select', 'Event class set to POE::Select');

use_ok('t::TestKernel')
