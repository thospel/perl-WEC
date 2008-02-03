package WEC::Connection;
use 5.008_001;	# Otherwise you will hit a closure leak
use strict;
use warnings;
use Carp;
use Scalar::Util qw(weaken dualvar);
use Socket qw(sockaddr_family unpack_sockaddr_in unpack_sockaddr_un inet_ntoa
              AF_INET AF_UNIX);
use Errno qw(EINPROGRESS ENOTCONN ECONNRESET EINTR EWOULDBLOCK EAGAIN EINVAL);

use WEC qw(api=1 DELETE_BUG);
use WEC::Socket qw(blocking);

our $VERSION = "1.000";
our @EXPORT_OK = qw(CLIENT SERVER HEADER BODY CALLBACK COMMAND ARG PARENT hex_show);

use base qw(WEC::Object);

use constant {
    # Typically used in $connection->{direction}
    SERVER	=> 1 << 0,
    CLIENT	=> 1 << 1,

    # Typically used in $connection->{in_state}
    HEADER	=> 1,
    BODY	=> 0,

    COMMAND	=> 0,
    CALLBACK	=> 1,
    ARG		=> 2,
    PARENT	=> 3,
};

sub server {
    my $connection = shift;
    croak "Not a server" unless $connection->{direction} & SERVER;
    return $connection->{parent};
}

sub client {
    my $connection = shift;
    croak "Not a client" unless $connection->{direction} & CLIENT;
    return $connection->{parent};
}

sub broken {
    my ($connection, $err, $user_die) = @_;
    $err =~ s/\.\z//;
    warn("Flow processor error (closing connection): $err, \n") unless $user_die;
    $connection->_close("protocol", dualvar(EINVAL, $err));
}

sub data_in {
    my $connection = shift;
    for ($connection->{in_buffer}) {
        my $rc = sysread($connection->{in_handle}, $_, $connection->{in_size}, length);
        if (!defined($rc)) {
            return if $! == EINTR || $! == EWOULDBLOCK || $! == EAGAIN;
            # ECONNRESET is also a sort of eof, though an unclean one,
            # so handle it as a silent error.
            if ($! == ECONNRESET) {
                $connection->_close("eof", $!);
            } else {
                warn("sysread: $!\n");
                $connection->_close("sysread", $!);
            }
            return;
        }
        if ($rc == 0) {
            $connection->_close("eof");
            return;
        }
      retry:
        eval {
            $connection->{in_process}->($connection) while
                length >= $connection->{in_want} && $connection->{in_handle};
        };
        if ($@) {
            my $err = $@;
            $err =~ s/\n\z//;
            if ($connection->{options}{Die}) {
                $connection->broken($connection->{options}{Die}->($connection, $err) || goto retry, 1);
            } else {
                $connection->broken($err, 0);
            }
        }
    }
}

sub data_out {
    my $connection = shift;
    die "Empty buf" if $connection->{out_buffer} eq "";
    if (defined(my $rc = syswrite($connection->{out_handle},
                                  $connection->{out_buffer}))) {
        substr($connection->{out_buffer}, 0, $rc, "");
        if ($connection->{out_buffer} eq "") {
            WEC->delete_write($connection->{out_handle});
            $connection->_close($connection->{close_on_empty}) if
                $connection->{close_on_empty};
        }
    } else {
        return if $! == EINTR || $! == EWOULDBLOCK || $! == EAGAIN;
        warn("Error writing to handle: $!\n");
        $connection->_close("syswrite", $!);
    }
}

sub suspend_input {
    my ($connection) = @_;
    if ($connection->{resuming}) {
	WEC->delete_alarm($connection->{resuming});
	$connection->{resuming} = undef;
	return;
    }
    croak "Already suspended" if $connection->{suspended};
    croak "Cannot suspend pending connect" if !$connection->{peer_address};
    croak "Cannot suspend a closed connection" if !$connection->{in_handle};
    WEC->delete_read($connection->{in_handle});
    $connection->{suspended} = $connection->{in_handle};
    $connection->{in_handle} = undef;
}

sub resuming {
    my ($connection) = @_;
    $connection->{resuming} = undef;
    WEC->add_read($connection->{suspended}, sub { $connection->data_in });
    $connection->{in_handle} = $connection->{suspended};
    $connection->{suspended} = undef;

    for ($connection->{in_buffer}) {
        eval {
            $connection->{in_process}->($connection) while
                length >= $connection->{in_want} && $connection->{in_handle};
        };
        if ($@) {
            my $err = $@;
            $err =~ s/\n\z//;
            if ($connection->{options}{Die}) {
                $connection->broken($connection->{options}{Die}->($connection, $err) || next, 1);
            } else {
                $connection->broken($err, 0);
            }
        }
    }
}

sub resume_input {
    my ($connection) = @_;
    croak "Not suspended" if
	!$connection->{suspended} || $connection->{resuming};
    $connection->{resuming} = WEC->add_alarm(0, sub { $connection->resuming });
}

# no arg for a user close call
# any argument gives the close cause, e.g. "eof" on the connecting socket
sub _close {
    # Make a copy of the arguments since they likely contain things like $!,
    # and we don't want it to change under us due to some failing syscall
    my ($connection, @args) = @_;
    if ($connection->{peer_address}) {
	warn($connection->{direction} & SERVER ? "Server" : "Client",
	     " abandoned ", length($connection->{out_buffer}),
	     " outgoing bytes: ", hex_show($connection->{out_buffer})) if
		 $connection->{out_buffer} ne "";
    } else {
	warn($connection->{direction} & SERVER ? "Server" : "Client",
	     " abandoned ", length($connection->{out_buffer}),
	     " outgoing bytes: ",
	     hex_show(substr($connection->{out_buffer}, 1))) if
		 $connection->{out_buffer} ne "s";
    }

    if ($connection->{resuming}) {
	WEC->delete_alarm($connection->{resuming});
	$connection->{resuming} = undef;
    }
    my $in_handle = $connection->{in_handle} || $connection->{suspended} ||
	croak "Already closed";
    if ($connection->{suspended}) {
	$connection->{suspended} = undef;
    } else {
	$connection->{in_handle} = undef;
	WEC->delete_read($in_handle) if $connection->{peer_address};
    }
    WEC->delete_write($connection->{out_handle}) if
	$connection->{out_buffer} ne "";
    close($in_handle);
    close($connection->{out_handle}) if
        $connection->{out_handle} != $in_handle;
    $connection->{out_handle} = undef;
    $connection->{parent}->_drop_connection($in_handle);
    # Determine unexpected EOF here, so _abort_flow is allowed to change it.
    my $unexpected = @args && $args[0] eq "eof" && !$connection->{ExpectEOF};
    my $err = "";
    if ($connection->{cork}) {
        my $cork = $connection->{cork};
        $connection->{cork} = [];
        for (@$cork) {
            eval {($_->[CALLBACK] || return)->($connection, "-close", @args)};
            $err .= $@ if $@;
            shift @{$connection->{cork}};
        }
    }
    if ($connection->{answers}) {
        my $answers = $connection->{answers};
        $connection->{answers} = undef;
        for (@$answers) {
            eval {($_->[CALLBACK] || return)->($connection, "-close", @args)};
            $err .= $@ if $@;
        }
    }
    for (values %{$connection->{flows}}) {
        eval {
            $_->_abort_flow(@args && $args[0] eq "eof" &&
                            $connection->{options}{EofIsEnd} ? undef:()) if $_;
        };
        $err .= $@ if $@;
    }
    eval {
        die "Close leaves cork" if
            $connection->{cork} && @{$connection->{cork}};
        die "Close leaves pending answers" if $connection->{answers};
        die "Close leaves connections" if %{$connection->{flows}};
        if (!$connection->{peer_address}) {
            if ($connection->{options}{ConnectFail}) {
                $connection->{options}{ConnectFail}->($connection->{parent}, $connection, @args);
            } elsif (@args >= 3 && $args[0] eq "connect") {
                die "Failed to connect to $args[2]: $args[1]\n";
            } else {
		die "Connect terminated @args";
	    }
        } elsif ($connection->{options}{Close}) {
            $connection->{options}{Close}->($connection->{parent}, $connection, @args);
        } elsif ($unexpected) {
            warn($connection->{direction} & SERVER() ? "Unexpected EOF form client\n" :
                 $connection->{direction} & CLIENT() ? "Unexpected EOF from server\n" :
                 "Unexpected EOF");
        }
    };
    $err .= $@ if $@;

    # Drop potential self references
    $connection->{options}	= undef;
    $connection->{parent}	= undef;
    $connection->{user_data}	= undef;
    $connection->{in_process}	= undef;
    die $err if $err;
}

sub kill {
    my $connection = shift;
    if ($connection->{out_buffer} ne "") {
        WEC->delete_write($connection->{out_handle});
        $connection->{out_buffer} = "";
    }
    $connection->_close("killed");
}

sub new_flow {
    my $connection = shift;
    my $flow = shift->new($connection);
    my $id = $flow->init_flow(@_);
    croak "There already is a flow with id ", $flow->id2string($id) if
        $connection->{flows}{$id};
    croak "New flow while there already is one, but I can't multiplex"
        if %{$connection->{flows}} && !$connection->{host_mpx} && defined $connection->{host_mpx};
    croak "New flow while there already is one, but the peer isn't supposed to multiplex" if %{$connection->{flows}} && !$connection->{peer_mpx} && defined $connection->{peer_mpx};
    if (defined $connection->{flows_left}) {
        croak "Attempt to start an extra flow on a one-shot connection" if
            $connection->{flows_left} <= 0;
        $connection->{flows}{$id} = $flow;
        $connection->{flows_left}--;
    } else {
        $connection->{flows}{$id} = $flow;
    }
    $connection->{ExpectEOF} = undef;
    return $flow;
}

sub _drop_flow {
    my $connection = shift;
    my $id = shift;
    croak "Flow was already dropped" unless $connection->{flows}{$id} &&
        $connection->{flows}{$id} == shift;
    delete $connection->{flows}{$id};
    $connection->{ExpectEOF} = 1 if
        !%{$connection->{flows}} && $connection->{direction} & SERVER;
    $connection->close_on_empty if
        defined $connection->{flows_left} && $connection->{flows_left} <= 0 &&
        $connection->{in_handle};
}

sub close : method {
    shift->_close;
}

sub peer_address {
    return shift->{peer_address} || croak "Connection has no peer (yet)";
}

sub handle {
    return shift->{in_handle};
}

sub options {
    return shift->{options};
}

#1;
#__END__

sub new_server {
    my $connection = shift->SUPER::new(shift);

    $connection->{in_handle}	= $connection->{out_handle} = shift;
    $connection->{options}	= shift;
    $connection->{direction}	= SERVER;
    $connection->{in_size}	= 2**16;
    $connection->{in_buffer}	= $connection->{out_buffer} = "";
    $connection->{flows}	= {};
    $connection->{ExpectEOF}	= 1;
    if ($connection->{options}->{OneShot}) {
        $connection->{host_mpx}	  = 0;
        $connection->{flows_left} = 1;
    } else {
        $connection->{host_mpx}	= 1;
    }

    $connection->{parent}->new_connection($connection->{in_handle}, $connection);

    $connection->init_server(@_);
    die "Server state machine not initialized" unless
        defined($connection->{in_want}) && $connection->{in_process};
    return $connection;
}

sub new_client {
    my $connection = shift->SUPER::new(shift);

    $connection->{in_handle}	= $connection->{out_handle} = shift;
    $connection->{options}	= shift;
    $connection->{direction}	= CLIENT;
    $connection->{flows}	= {};
    if ($connection->{options}->{OneShot}) {
        $connection->{host_mpx}	  = 0;
        $connection->{flows_left} = 1;
    } else {
        $connection->{host_mpx}	= 1;
    }
    $connection->{in_size}	= 2**16;
    $connection->{in_buffer}	= "";
    # Placeholder stops future writes from triggering a writability select
    $connection->{out_buffer}	= "s";

    my ($status, $destination) = @_;
    my $conn = $connection;
    weaken($conn) if $connection->{options}{AutoClean};
    # Writability test works both for delayed and for immediate connect
    WEC->add_write($conn->{out_handle}, sub {
        my $address;
        if ($status) {
            if ($status == EINPROGRESS) {
                $address = getpeername($conn->{in_handle});
                if (!$address) {
                    die "getpeername error: $!" unless $! == ENOTCONN;
                    my $rc = sysread($conn->{in_handle}, my $buf, 16);
                    die "Unexpected good read from unconnected socket" if $rc;
                    die "Unexpected EOF on unconnected socket" if defined($rc);
                    # Was a delayed error on connect
                    $conn->_close("connect", $!, $destination);
                    return;
                }
            } else {
                # Was an immediate error on connect
                $conn->_close("connect", $status, $destination);
                return;
            }
        } else {
            $address = getpeername($conn->{in_handle}) ||
                die "getpeername error: $!";
        }
        my $family = sockaddr_family($address);
        if ($family == AF_INET) {
            my ($port, $host) = unpack_sockaddr_in($address);
            $conn->{peer_address} = "tcp://" . inet_ntoa($host) . ":$port";
        } elsif ($family == AF_UNIX) {
            $conn->{peer_address} = "unix://" . unpack_sockaddr_un($address);
        } else {
            die "Unknown address family $family";
        }

        WEC->add_read($conn->{in_handle}, sub { $conn->data_in });
        WEC->delete_write($conn->{out_handle});
        # Remove the "s" placeholder
        substr($conn->{out_buffer}, 0, 1, "");
        WEC->add_write($conn->{out_handle}, sub {
            $conn->data_out;
        }) if $conn->{out_buffer} ne "";

        $conn->{options}{Connect}->($conn, $destination) if
            $conn->{options}{Connect};
        # Connect callback can cause $conn to evaporate
        $conn->data_out if $conn && $conn->{out_buffer} ne "" && $conn->{out_handle};
    });

    eval {
        $connection->init_client(@_);
        die "client state machine not initialized" unless
            $connection->{in_want} && $connection->{in_process};
    };
    if ($@) {
        my $err = $@;
        WEC->delete_write($conn->{out_handle});
        die $err;
    }
    return $connection;
}

# Server side operation
sub start_accepting {
    my ($class, $server, $handles) = @_;
    weaken($server) if $server->options->{AutoClean};

    for my $handle (@$handles) {
        WEC->add_read($handle, sub {
            my $options = $server->options;
            $options->{PreAccept}->($server) if $options->{PreAccept};
            my $s;
            if (my $address = do { local $^F = -1; accept($s, $handle)}) {
                my $family = sockaddr_family($address);
                if ($family == AF_INET) {
                    my ($port, $host) = unpack_sockaddr_in($address);
                    # We assume stream sockets here, otherwise use inet://...
                    $address = "tcp://" . inet_ntoa($host) . ":$port";
                    if ($options->{IpAccept} && !$options->{IpAccept}{$host}) {
                        close $s;
                        warn("Accepted and immediately dropped connection from $address (not whitelisted in IpAccept)\n");
                        return;
                    }
                } elsif ($family == AF_UNIX) {
                    $address = "unix://" . unpack_sockaddr_un($address);
                } else {
                    die "Unknown address family $family";
                }
                binmode($s);
                blocking($s, 0);
                my $connection = $class->new_server($server, $s, $options);
                $connection->{peer_address} = $address;
                WEC->add_read($s, sub { $connection->data_in });
                $options->{Accept}->($server, $connection) if
		    $options->{Accept};
                if ($connection->{in_want}<= length $connection->{in_buffer}) {
                    # Normally only because $connection->{in_want} == 0,
		    # meaning we want to send some initial greeting after
		    # Accept callback
                    for ($connection->{in_buffer}) {
                      retry:
                        eval {
                            $connection->{in_process}->($connection) while
                                length >= $connection->{in_want} && $connection->{in_handle};
                        };
                        if ($@) {
                            my $err = $@;
                            $err =~ s/\n\z//;
                            if ($connection->{options}{Die}) {
                                $connection->broken($connection->{options}{Die}->($connection, $err) || goto retry, 1);
                            } else {
                                $connection->broken($err, 0);
                            }
                        }
                    }
                }
            } else {
                warn "Accept: $!\n";
            }
        });
    }
}

# Call as $conn->send($content)
sub send : method {
    return if $_[1] eq "";
    die "Message is utf8" if utf8::is_utf8($_[1]);
    weaken(my $connection = shift);
    WEC->add_write($connection->{out_handle}  ||
                   die("Attempt to send on a closed Connection"), sub {
                       $connection->data_out;
                   }) if $connection->{out_buffer} eq "";
    $connection->{out_buffer} .= shift;
    return;
}

sub send0 {
    weaken(my $connection = shift);
    WEC->add_write($connection->{out_handle} ||
                   die("Attempt to send on a closed Connection"),
                   sub {
                       $connection->data_out;
                   }) if $connection->{out_buffer} eq "";
}

sub eat_input {
    my $connection = shift;
    $connection->{in_want} = 10;
    my $buffer = \$connection->{in_buffer};
    $connection->{in_process}  = sub { $$buffer = "" };
}

# Server side operation
sub close_on_empty {
    my $connection = shift;
    if ($connection->{out_buffer} eq "") {
        $connection->_close(shift || "empty");
        return 1;
    } else {
        # Maybe drop readability here
        $connection->{close_on_empty} = shift || "empty";
        return 0;
    }
}

sub send_close {
    my $connection = shift;
    $connection->send(@_) if @_;
    $connection->eat_input;
    $connection->close_on_empty;
}

sub _callback {
    my $connection = shift;
    my $answer = shift @{$connection->{answers}} ||
        croak "No callback pending";
    if ($answer->[PARENT]) {
        my $parent = $answer->[PARENT];
        delete $parent->{events}{$answer};
        ($answer->[CALLBACK] || return)->($parent, $connection, @_);
        return;
    }
    ($answer->[CALLBACK] || return)->($connection, @_);
}

sub hex_show {
    my $len = $_[1] || 16;
    my $str = substr($_[0], 0, $len);
    my $hex = unpack("H*", $str);
    $hex =~ s/(..)\B/$1 /g;
    $str =~ tr/\x00-\x1f\x80-\x9f/./;
    my $more = length($_[0]) > $len ? "..." : "";
    return "$str$more ($hex$more)";
}

sub flows {
    return values %{shift->{flows}};
}

sub flows_ids {
    return keys %{shift->{flows}};
}

sub one_shot {
    my $connection = shift;
    return defined($connection->{flows_left}) unless @_;
    my $old = defined($connection->{flows_left});
    if (shift) {
        $connection->{flows_left} = 1 unless defined $connection->{flows_left};
    } else {
        $connection->{flows_left} = undef;
    }
    return $old;
}

sub expect_eof {
    my $connection = shift;
    return $connection->{ExpectEOF} unless @_;
    my $old = $connection->{ExpectEOF};
    $connection->{ExpectEOF} = shift;
    return $old;
}

sub cork {
    my $connection = shift;
    croak "Already corked" if $connection->{cork};
    $connection->{cork} = [];
}

sub uncork {
    my $connection = shift;
    croak "Not corked" unless $connection->{cork};
    croak "Cannot uncork while handshake still in progress" if
        $connection->{handshaking};
    my $cork = $connection->{cork};
    $connection->{cork} = undef;
    return if !@$cork;
    $connection->send0 if $connection->{out_buffer};
    for (@$cork) {
        push @{$connection->{answers}}, $_;
        $connection->{out_buffer} .= $_->[ARG];
        DELETE_BUG() ? $_->[ARG] = undef : delete $_->[ARG];
    }
    return;
}

sub corked {
    return shift->{cork} ? 1 : 0;
}

sub begin_handshake {
    my $connection = shift;
    croak "Already handshaking" if $connection->{handshaking};
    $connection->{handshaking} = 1;
    $connection->cork;
}

sub end_handshake {
    my $connection = shift;
    croak "Not handshaking" unless $connection->{handshaking};
    delete $connection->{handshaking};
    $connection->{options}{Greeting}->($connection, @_) if
        $connection->{options}{Greeting};
    return 0 unless $connection->{in_handle};
    $connection->uncork;
    return 1;
}

1;
