# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl d/06_Tk.t'

use warnings;
use strict;

use Test::More;
unless (eval { require Tk }) {
        plan skip_all => "Can't find the Tk module";
        exit;
}
if ($^O eq "MSWin32") {
        plan skip_all => "Should investigate how to control mainwindows in TK";
        exit;
}
plan 'no_plan';
            
is($WEC::kernel_type, undef, 'No event class set');
use_ok('WEC');
is($WEC::kernel_type, 'WEC::Tk', 'Event class set to WEC::Tk');

# Set up the main window now so the connection won't count as a lost fd
WEC::Tk->init;
# Try to minimize user interaction
{
    no warnings 'once';
    $WEC::Tk::event_window->geometry('+10+10');
}

use_ok('t::TestKernel')
