# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl 01_WEC.t'
#########################

use strict;
use Test::More tests => 7;

BEGIN { use_ok('WEC') };
BEGIN { use_ok('WEC::Server') };
BEGIN { use_ok('WEC::Client') };
BEGIN { use_ok('WEC::Connection') };
BEGIN { use_ok('WEC::Flow') };
BEGIN { use_ok('WEC::Test', Class => "WEC", Parts => [qw(Server Client)]) };
is(++$hit, 1);
