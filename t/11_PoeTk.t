# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl d/11_PoeTk.t'

use warnings;
use strict;

use Test::More;
unless (eval { require Tk }) {
        plan skip_all => "Can't find the Tk module";
        exit;
}
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
   $WEC::POE::poe_type	eq 'Tk', 'Event class set to POE::Tk');

sub prepare_loop {
    no warnings 'once';
    $POE::Kernel::poe_main_window->geometry('+10+10');
}

use_ok('t::TestKernel')
