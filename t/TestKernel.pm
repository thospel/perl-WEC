use 5.008_001;
use strict;
use warnings;
use Socket qw(IPPROTO_TCP);
use Time::HiRes qw(time);

BEGIN { $^W = 1 };
my $trace_line = 0;

my $old = time;
sub delta {
    my $new = time;
    my $r = sprintf("%8.6f", $new - $old);
    $old = $new;
    return $r;
}
ok($WEC::kernel_type, "Event type set");
is(WEC->kernel_type, $WEC::kernel_type, "And has the expected value");
can_ok("WEC",
       qw(add_read delete_read add_write delete_write add_alarm delete_alarm
          add_work delete_work add_idle delete_idle add_signal delete_signal
          alive_signal loop unloop auto_unloop init readable writable api));
WEC->import(qw(loop unloop readable writable alive_signal auto_unloop api));
can_ok(__PACKAGE__, qw(loop unloop readable writable alive_signal
                       auto_unloop api));
if (__PACKAGE__->can("prepare_loop")) {
    no warnings "redefine";
    *loop = sub {
        prepare_loop();
        return WEC::loop(@_);
    }
}

my @count;
my (@base_fd, $last_filename, $last_line);
sub check_fd {
    my @fd;
    for (0..4) {
        pipe($fd[2*$_], $fd[2*$_+1]) || die "Could not create pipe: $!";
    }
    for (@fd) {
        $_ = fileno($_) || die "Huh ? No fileno ?";
    }

    my @r = WEC->readable();
    diag("Still Readable fds: @r") if @r;
    my @w = WEC->writable();
    diag("Still Writable fds: @w") if @w;

    if (!@base_fd) {
        # diag("Free fds: @fd");
        @base_fd = @fd;
    } elsif ("@base_fd" eq "@fd") {
        pass("No fd leaked");
    } else {
        my %lost;
        @lost{@base_fd} = ();
        delete @lost{@fd};
        my (undef, $filename, $line) = caller;
        diag("Lost fd " . join(",", keys %lost) .
             " between $last_filename line $last_line and $filename line $line");
        @base_fd = @fd;
        @_ = "fd leaked";
        ($last_filename, $last_line) = ($filename, $line);
        goto &fail;
    }
    (undef, $last_filename, $last_line) = caller;
    diag("$last_filename: $last_line ". delta) if $trace_line;
}

my $workers = 5;
my $target;
my $hit = 0;
{
    package Canary;
    sub DESTROY {
        $hit++;
    }
}

sub canary {
    return bless [], "Canary";
}

sub Pipe(**) {
    no strict "refs";
    if ($^O eq 'MSWin32') {
	use Socket qw(AF_UNIX SOCK_STREAM PF_UNSPEC);
	return socketpair($_[0], $_[1], AF_UNIX, SOCK_STREAM, PF_UNSPEC) ||
	    die "Could not creat signal socketpair: $! (", $!+0, ")";
	# shutdown($_[0], 1);	# no more writing for reader
	# shutdown($_[1], 0);	# no more reading for writer
    } else {
	return pipe($_[0], $_[1]);
    }
}

sub self_alarm_kill {
    if ($^O eq 'MSWin32') {
	alarm(1)
    } else {
	kill "ALRM", $$;
    }
}

WEC->init;
check_fd();

# API check
is(api(), 1, "Right API being tested");

# Basic readability testing
ok(Pipe(local *RD, local *WR), "Create pipe");
$hit = 0;
WEC->init;
WEC->add_read(\*RD, sub {
    $hit++;
    WEC->delete_read(\*RD);
    unloop("foo");
}, canary);
is($hit, 1, "Arg ignored");
close WR;
is (loop(), "foo", "Right returncode from loop");
is($hit, 2, "Readable");
close RD;
check_fd();

# It's ok to re-init sessions
ok(Pipe(local *RD, local *WR), "Create pipe");
$hit = 0;
WEC->init;
WEC->init;
WEC->add_read(\*RD, sub {
    $hit++;
    WEC->delete_read(\*RD);
    unloop("foo");
}, canary);
is($hit, 1, "Arg ignored");
close WR;
is (loop(), "foo", "Right returncode from loop");
is($hit, 2, "Readable");
close RD;
check_fd();

# Basic writability testing
ok(Pipe(\*RD, \*WR), "Create pipe");
$hit = 0;
WEC->init;
WEC->add_write(\*WR, sub {
    $hit++;
    WEC->delete_write(\*WR);
    unloop("bar");
}, canary);
is($hit, 1, "Arg ignored");
is (loop(), "bar", "Right returncode from loop");
is($hit, 2, "Writable");
close \*WR;
close \*RD;
check_fd();

# Basic error detection
ok(Pipe(\*RD, \*WR), "Create pipe");
WEC->init;
WEC->add_read(\*RD, sub { });
eval { WEC->add_read(\*RD, sub {}) };
ok($@, "Can't add readability twice");
eval { WEC->delete_read(\*RD) };
is($@, "", "Can stop reading");
eval { WEC->delete_read(\*RD) };
ok($@, "Can't stop reading twice");
WEC->add_write(\*WR, sub { });
eval { WEC->add_write(\*WR, sub {}) };
ok($@, "Can't add writability twice");
eval { WEC->delete_write(\*WR) };
is($@, "", "Can stop writing");
eval { WEC->delete_write(\*WR) };
ok($@, "Can't stop writing twice");
close \*WR;
close \*RD;
check_fd();

# A real datatransfer
ok(Pipe(\*RD, \*WR), "Create pipe");
$hit = 0;
my $rc = "";
WEC->init;
WEC->add_write(\*WR, sub {
    $hit++;
    print WR "a";
    WEC->delete_write(\*WR);
    close WR;
});
WEC->add_read(\*RD, sub {
    $hit++;
    if (defined(my $read = <RD>)) {
        $rc .= $read;
    } else {
        WEC->delete_read(\*RD);
        close RD;
        unloop();
    }
});
is(loop(), undef, "Right returncode from loop");
is($rc, "a", "Piped a char");
is($hit, 3, "Had all events");
check_fd();

# Multiple simultanous events
ok(Pipe(\*RD, \*WR), "Create pipe");
ok(Pipe(local *RD1, local *WR1), "Create pipe");
close WR;
close WR1;
$hit = 0;
$rc = "";
WEC->init;
WEC->add_read(\*RD, sub {
    WEC->delete_read(\*RD);
    unloop() if ++$hit == 2;
});
WEC->add_read(\*RD1, sub {
    WEC->delete_read(\*RD1);
    unloop() if ++$hit == 2;
});
loop();
close(RD); close(RD1);
is($hit, 2, "Event eliminated");
check_fd();

# Dropping a pending event
ok(Pipe(\*RD, \*WR), "Create pipe");
ok(Pipe(local *RD1, local *WR1), "Create pipe");
ok(Pipe(local *RD2, local *WR2), "Create pipe");
close WR;
close WR1;
$hit = 0;
WEC->init;
WEC->add_read(\*RD, sub {
    $hit++;
    WEC->delete_read(\*RD);
    WEC->delete_read(\*RD1);
    close(WR2);
});
WEC->add_read(\*RD1, sub {
    $hit++;
    WEC->delete_read(\*RD);
    WEC->delete_read(\*RD1);
    close(WR2);
});
WEC->add_read(\*RD2, sub {
    $hit++;
    WEC->delete_read(\*RD2);
    unloop();
});
loop();
close(RD); close(RD1); close(RD2);
is($hit, 2, "Event eliminated");
check_fd();

# Timeout testing
$hit = 0;
WEC->init;
WEC->add_alarm(0, sub {
    is(++$hit, 4, "Now comes first") }, canary);
is($hit, 1, "Arg ignored");
WEC->add_alarm(0.1, sub {
    is(++$hit, 5, "0.1 second later") }, canary);
is($hit, 2, "Arg ignored");
WEC->add_alarm(0.2, sub {
    is(++$hit, 6, "0.2 second later");
    unloop("zoem");
}, canary);
is($hit, 3, "Arg ignored");
is(loop(), "zoem", "Right returncode from loop");
is($hit, 6, "No extra hits");
check_fd();

# Testing a delete
$hit = 0;
WEC->init;
my $drop;
WEC->add_alarm(0, sub {
    is(++$hit, 1, "Now comes first") });
WEC->add_alarm(0.1, sub {
    is(++$hit, 2, "0.1 second later");
    WEC->delete_alarm($drop);
    eval { WEC->delete_alarm($drop) };
    is($@, "", "Can redrop dropped alarms");
});
$drop = WEC->add_alarm(0.2, sub {
    fail("Should never be reached due to drop") });
WEC->add_alarm(0.3, sub {
    is(++$hit, 3, "0.3 second later");
    unloop("zoem");
});
is(loop(), "zoem", "Right returncode from loop");
is($hit, 3, "No extra hits");
check_fd();

# Check auto-exit declaration
if ($WEC::kernel_type eq "WEC::Tk" || $WEC::kernel_type eq "WEC::Glib") {
    ok(!auto_unloop(), "Tk and Glib don't auto_unloop");
} else {
    ok(auto_unloop(), "Most modules auto_unloop");
}
# Check auto-exiting of the event loop
SKIP: {
    skip "$WEC::kernel_type has no auto unloop", 3 unless auto_unloop();
    # skip "$WEC::kernel_type autoexit is dangerous", 3 if
    #    $WEC::kernel_type eq "WEC::Event";

    $hit = 0;
    WEC->init;
    WEC->add_alarm(0.5, sub { is(++$hit, 1, "Our only event"); });
    is(loop(), undef, "Auto-exit returns undef");
    is($hit, 1, "Alarm event reached");
};

# Have alarms and a readability generator. Neither should
# livelock the other
$hit = 0;
ok(Pipe(local *RD, local *WR), "Create pipe");
WEC->init;
@count = (0) x ($workers+1);
my @wid;
my $fun = sub {
    $count[shift]++;
    if (++$hit == 1000) {
        WEC->delete_read(\*RD);
        WEC->delete_alarm($wid[$_]) for 1..$workers;
        unloop();
    } elsif ($hit > 1050) {
        fail("Running too much, probably infinite, alarm count=@count");
        exit;
    }
};
for my $n (1..$workers) {
    my $alarm;
    $alarm = sub {
        $wid[$n] = WEC->add_alarm(0, $alarm);
        $fun->($n);
    };
    $wid[$n] = WEC->add_alarm(0, $alarm);
}
WEC->add_read(\*RD, sub { $fun->(0) });
close WR;
is(loop(), undef, "Right returncode from loop");
is($hit, 1000, "No extra runs");
close RD;
diag("r" . " a" x $#count . "=@count");
ok($count[0] < 1000/($workers+1)+5, "Read count not too high");
ok($count[0] > 1000/($workers+1)-5, "Read count not too low");
$target = int((1000-$count[0])/$workers);
for (1..$workers) {
    ok($count[$_] <= $target+1, "Good spread on alarm count");
    ok($count[$_] >= $target,   "Good spread on alarm count");
}
check_fd();

# Basic adding, running and deleteing of a work procedure
$hit = 0;
WEC->init;
my $wid;
$wid = WEC->add_work(sub {
    if (++$hit >= 4) {
        WEC->delete_work($wid);
        unloop("zzz");
    }
}, canary);
is($hit, 1, "Arg ignored");
is(loop(), "zzz", "Right returncode from loop");
is($hit, 4, "Executed 3 times");

# Have workprocedures and a readability generator. Neither should
# livelock the other
$hit = 0;
ok(Pipe(local *RD, local *WR), "Create pipe");
WEC->init;
@count = (0) x ($workers+1);
$fun = sub {
    $count[shift]++;
    if (++$hit == 1000) {
        WEC->delete_read(\*RD);
        WEC->delete_work($wid[$_]) for 1..$workers;
        unloop();
    }
};
for my $n (1..$workers) {
    $wid[$n] = WEC->add_work(sub { $fun->($n)} );
}
WEC->add_read(\*RD, sub { $fun->(0) });
close WR;
is(loop(), undef, "Right returncode from loop");
is($hit, 1000, "No extra runs");
diag("r" . " w" x $#count . "=@count");
close RD;
ok($count[0] < 1000/2+5, "Read count not too high");
ok($count[0] > 1000/2-5, "Read count not too low");
$target = int((1000-$count[0])/$workers);
for (1..$workers) {
    ok($count[$_] <= $target+1, "Good spread on work count");
    ok($count[$_] >= $target,   "Good spread on work count");
}
check_fd();

# Basic adding, running and deleteing of a idle procedure
$hit = 0;
WEC->init;
$wid = WEC->add_idle(sub {
    if (++$hit >= 4) {
        WEC->delete_idle($wid);
        unloop("yyy");
    }
}, canary);
is($hit, 1, "Arg ignored");
is(loop(), "yyy", "Right returncode from loop");
is($hit, 4, "Executed 3 times");

# Have idleprocedures and a readability generator.
# The readability should livelock the idles
$hit = 0;
ok(Pipe(local *RD, local *WR), "Create pipe");
close WR;
# First get rid of any busy flag
WEC->init;
WEC->add_read(\*RD, sub {
    $hit++;
    WEC->delete_read(\*RD);
    unloop();
});
loop();
is($hit, 1, "added and deleted");
$hit=0;
WEC->init;
@count = (0) x ($workers+1);
$fun = sub {
    $count[shift]++;
    if (++$hit == 1000) {
        WEC->delete_read(\*RD);
        WEC->delete_idle($wid[$_]) for 1..$workers;
        unloop();
    }
};
WEC->add_read(\*RD, sub { $fun->(0) });
for my $n (1..$workers) {
    $wid[$n] = WEC->add_idle(sub { $fun->($n)} );
}
is(loop(), undef, "Right returncode from loop");
is($hit, 1000, "No extra runs");
diag("r" . " i" x $#count . "=@count");
close RD;
ok($count[0] >= 999, "Read count not too low");
$target = int((1000-$count[0])/$workers);
for (1..$workers) {
    ok($count[$_] <= $target+1, "Good spread on idle count");
    ok($count[$_] >= $target,   "Good spread on idle count");
}
check_fd();

# Have pure idleprocedures. They should round-robin
$hit = 0;
ok(Pipe(local *RD, local *WR), "Create pipe");
close WR;
# First get rid of any busy flag
WEC->init;
WEC->add_read(\*RD, sub {
    $hit++;
    WEC->delete_read(\*RD);
    close RD;
    unloop();
});
loop();
is($hit, 1, "added and deleted");
$hit=0;
WEC->init;
@count = (0) x $workers;
$fun = sub {
    $count[shift]++;
    if (++$hit == 1000) {
        WEC->delete_idle($wid[$_]) for 0..$workers-1;
        unloop();
    }
};
for my $n (0..$workers-1) {
    $wid[$n] = WEC->add_idle(sub { $fun->($n)} );
}
is(loop(), undef, "Right returncode from loop");
is($hit, 1000, "No extra runs");
diag("i" . " i" x $#count . "=@count");
close RD;
$target = int(1000/$workers);
for (0..$workers-1) {
    ok($count[$_] <= $target+1, "Good spread on idle count");
    ok($count[$_] >= $target,   "Good spread on idle count");
}
check_fd();

SKIP: {
    skip("$WEC::kernel_type cannot easily generate signals to itself",
	 8+2*$workers) if $^O eq 'MSWin32';
    # Basic adding, running and deleteing of a signal handler
    ok(Pipe(\*RD, \*WR), "Create pipe");
    $hit = 0;
    WEC->init;
    $wid = WEC->add_signal("ALRM", sub {
	if (++$hit < 6) {
	    self_alarm_kill();
	} else {
	    WEC->delete_signal($wid);
	    WEC->delete_read(\*RD);
	    unloop("zzz");
	}
    }, canary);
    is($hit, 1, "Arg ignored");
    WEC->add_read(\*RD, sub {
	$hit++;
	fail("Should never be readable");
    });
    self_alarm_kill();
    is(loop(), "zzz", "Right returncode from loop");
    is($hit, 6, "Executed 3 times");
    close \*RD;
    close \*WR;
    check_fd();

    # Have a signalhandler and a readability generator.
    # Signal handler may livelock readability, but not the other way round
    $hit = 0;
    ok(Pipe(local *RD, local *WR), "Create pipe");
    close WR;
    WEC->init;
    @count = (0) x ($workers+1);
    $fun = sub {
	$count[$_[0]]++;
	++$hit;
	if ($hit < 1000) {
	    kill "ALRM", $$ if $_[0] == 5;
	} elsif ($hit == 1000) {
	    WEC->delete_read(\*RD);
	    WEC->delete_signal($wid[$_]) for 1..$workers;
	    unloop();
	} else {
	    diag("Extra hits!!!");
	}
    };
    for my $n (1..$workers) {
	$wid[$n] = WEC->add_signal("ALRM", sub { $fun->($n)} );
    }
    WEC->add_read(\*RD, sub { $fun->(0) });
    self_alarm_kill();
    is(loop(), undef, "Right returncode from loop");
    is($hit, 1000, "No extra runs");
    diag("r" . " s" x $#count . "=@count");
    close \*RD;
    ok($count[0] < 1000/2+5, "Read count not too high");
    # No low test. Signals are allowed to push out read events
    $target = int((1000-$count[0])/$workers);
    for (1..$workers) {
	ok($count[$_] <= $target+1, "Good spread on work count");
	ok($count[$_] >= $target,   "Good spread on work count");
    }
    check_fd();

    ok(alive_signal(), "Signal keep the loop alive by default");

    # Check if signals indeed keep the loop alive.
    # Also check signalhandling while the underlying loop is active
    $hit = 0;
    WEC->init;
    $wid = WEC->add_signal("ALRM", sub {
	$hit++;
	WEC->delete_signal($wid);
	unloop();
    });
    alarm(1);
    loop();
    alarm(0);
    is($hit, 1, "Exit is through alarm");
    check_fd();

    $old = eval { alive_signal(0) };
  SKIP: {
      if ($WEC::kernel_type eq "WEC::Tk" || $WEC::kernel_type eq "WEC::Glib" ||
	  $WEC::kernel_type eq "WEC::Event") {
	  ok($@, "Cannot change alive_signal");
	  ok(alive_signal(), "And the old value is indeed unchanged");
	  skip "$WEC::kernel_type cannot change alive_signal", 5;
      }
      is($@, "", "Can change alive_signal");
      ok($old, "Returned old alive state");
      is(alive_signal(), 0, "And the old value is indeed changed");

      # Check if signals indeed don't keep the loop alive.
      my $wid;
      $hit = 0;
      WEC->init;
      $wid = WEC->add_signal("ALRM", sub {
	  $hit++;
	  WEC->delete_signal($wid);
	  unloop();
      });
      alarm(1);
      loop();
      alarm(0);
      is($hit, 0, "Exit is not through alarm");
      check_fd();

      $old = alive_signal(1);
      is($old, 0, "Returned old alive state");
      is(alive_signal(), 1, "And the old value is indeed changed");
  }
}

SKIP: {
    skip "$WEC::kernel_type late die on Event hangs", 3 if
        $WEC::kernel_type eq "WEC::POE" && $WEC::POE::poe_type eq "Event";
    # die on the last event source
    ok(Pipe(\*RD, \*WR), "Create pipe");
    close WR;
    WEC->init;
    WEC->add_read(\*RD, sub {
        WEC->delete_read(\*RD);
        close RD;
        die "Foop\n";
    });
    eval { loop() };
    is($@, "Foop\n", "proper die message");
    check_fd();
}

# Speed testing
ok(Pipe(\*RD,  \*WR),	"Create pipe");
ok(Pipe(\*RD1, \*WR1),	"Create pipe");
ok(Pipe(\*RD2, \*WR2),	"Create pipe");
close WR; close WR1; close WR2;
$hit=0;
my $now=1+int(time);
1 while $now > time;
$now = 1+time;
sub speed {
    ++$hit;
    if ($now < time) {
        my $speed = int(0.5+$hit / (time()-$now+1));
        diag(" Kernel speed: $speed events per second");
        WEC->delete_read(\*RD);
        WEC->delete_read(\*RD1);
        WEC->delete_read(\*RD2);
        unloop("foo");
    }
}
WEC->init;
WEC->add_read(\*RD, \&speed);
WEC->add_read(\*RD1, \&speed);
WEC->add_read(\*RD2, \&speed);
is(loop(), "foo", "Right returncode from loop");
close RD; close RD1; close RD2;
check_fd();

# Load socket module
use_ok("WEC::Socket");
can_ok("WEC::Socket",
       qw(unix inet listener is_tcp spawn blocking start_connect));
WEC::Socket->import(qw(unix inet listener spawn blocking is_tcp
                       start_connect));
check_fd();

SKIP: {
    skip "Can't query blocking state on windows", 4 if $^O eq 'MSWin32';

    ok(blocking(\*STDIN), "Stdin is blocking");
    blocking(\*STDIN, 0);
    ok(!blocking(\*STDIN), "Stdin isn't blocking");
    blocking(\*STDIN, 1);
    ok(blocking(\*STDIN), "Stdin is blocking");
    eval { blocking(\*FOO) };
    ok($@, "Foo isn't open");
}
ok(!is_tcp(\*STDIN), "STDIN isn't a tcp socket");
ok(!is_tcp(\*FOO), "Foo isn't a tcp_socket");
SKIP: {
    skip "No support for unix sockets on windows", 4 if $^O eq 'MSWin32';

    my ($socket, $name) = unix();
    ok(defined(fileno($socket)), "It's a filehandle");
    ok(!is_tcp($socket), "It's not a tcp socket");
    like($name, qr!^unix://!, "Returned name is like unix:<path>");
    if (my ($file) = $name =~ m!^unix://(.*)!s) {
	$file  =~ s/%([0-9A-Fa-f]{2})/chr hex $1/eg;
	ok(unlink($file), "Could unlink socket");
    } else {
	fail("Could not parse $name");
    }
}

my %args = (LocalAddr => "localhost");
my ($socket, $name) = inet(%args);
ok(defined(fileno($socket)), "It's a filehandle");
ok(is_tcp($socket), "It's a tcp socket");
like($name, qr!^tcp://127\.0\.0\.1:\d+\z!,
     "Returned name is like tcp://<host>:<port>");
#ok(!%args, "Arglist consumed");

%args = (LocalHost => "localhost");
($socket) = inet(%args);
ok(defined(fileno($socket)), "It's a filehandle");
#ok(!%args, "Arglist consumed");

%args = (LocalAddr => "127.0.0.1", foo => "bar");
eval { ($socket) = inet(%args) };
#ok(defined(fileno($socket)), "It's a filehandle");
#is_deeply(\%args, {foo => "bar"}, "Arglist partly consumed");
like($@, qr/Unknown options foo/i, "recognize bad option");

%args = (LocalAddr => "1.2.3.4.5.6" # , foo => "bar"
         );
eval { inet(%args) };
like($@, qr/Bad hostname/i, "Cannot parse 1.2.3.4.5.6");
#is_deeply(\%args, {foo => "bar"}, "Arglist partly consumed");

close $socket;
check_fd();

# Testing listener
($socket, $name) = listener("tcp://localhost");
ok(defined(fileno($socket)), "It's a filehandle");
ok(is_tcp($socket), "It's a tcp socket");
like($name, qr!^tcp://127\.0\.0\.1:\d+\z!,
     "Returned name is like tcp://<host>:<port>");

SKIP: {
    skip "No support for unix sockets on windows", 4 if $^O eq 'MSWin32';
    my ($socket, $name) = listener("unix://");
    ok(defined(fileno($socket)), "It's a filehandle");
    ok(!is_tcp($socket), "It's not a tcp socket");
    like($name, qr!^unix://!, "Returned name is like unix:<path>");
    if (my ($file) = $name =~ m!^unix://(.*)!s) {
	$file  =~ s/%([0-9A-Fa-f]{2})/chr hex $1/eg;
	ok(unlink($file), "Could unlink socket");
    } else {
	fail("Could not parse $name");
    }
}

1;
