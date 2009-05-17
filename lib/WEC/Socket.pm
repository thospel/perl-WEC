package WEC::Socket;
use 5.006;
use strict;
use warnings;
use Carp;
use Socket qw(TCP_NODELAY SOCK_STREAM PF_UNIX PF_INET PF_UNSPEC SOL_SOCKET
              SO_REUSEADDR SO_REUSEPORT INADDR_ANY IPPROTO_TCP
              pack_sockaddr_un pack_sockaddr_in unpack_sockaddr_in
              inet_ntoa inet_aton);
use POSIX  qw(tmpnam _exit F_GETFL F_SETFL O_NONBLOCK FD_CLOEXEC);
use Errno  qw(ECONNREFUSED EADDRINUSE EINTR);

our $VERSION = "1.000";
require Exporter;
our @ISA = qw(Exporter);
our @EXPORT_OK = qw($default_listen $default_bind_attempts
                    unix inet listener self_inet nagle spawn blocking is_tcp
                    start_connect);

our $default_listen = 5;
our $default_bind_attempts = 10;

sub unix {
    my %params = @_;

    my $p = delete $params{Path};
    my $auto_clean = delete $params{AutoClean};
    my $abstract = delete $params{Abstract} ? "\x00" : "";
    my $pumask = delete $params{PathUmask};
    my $bind_attempts = defined($p) && $p ne "" ? 1 :
        delete $params{BindAttempts} || $default_bind_attempts;
    my $listen = delete $params{Listen};
    my $blocking = exists $params{Blocking} ? delete $params{Blocking} : 1;
    my $clo_exec = exists $params{CloExec} ? delete $params{CloExec} : 1;

    croak("Unknown options ", join(", ", keys %params)) if %params;

    $listen = $default_listen unless defined($listen);

    # UNIX socket binding doesn't work if the target already exists
    # (even if it followed symlinks, which it does not), so the normal
    # security problems of tmpnam don't apply.
    my $path = defined($p) && $p ne "" ? $p : $abstract . tmpnam();
    my ($oldmask, $listener);
    $oldmask = umask($pumask) if defined($pumask);
    eval {
        {
            # by default CLO_EXEC for everyone except if we hit fd 0
            # local $^F = !defined($clo_exec) ? 0 : $clo_exec ? -1 : 2**31-1;
            # default CLO_EXEC, otherwise do as asked
            local $^F = $clo_exec ? -1 : 2**31-1;
            socket($listener, PF_UNIX, SOCK_STREAM, PF_UNSPEC) ||
                croak "Could not open UNIX socket: $!";
        }
        for (1..$bind_attempts) {
            my $sun = pack_sockaddr_un($path) || croak"Could not pack '$path'";
            last if bind($listener, $sun);
            croak "Could not bind '$path': $!" if defined($p) && $p ne "" ||
                $! != EADDRINUSE || $_ == $bind_attempts;
            $path = $abstract . tmpnam();
        }
        eval {
            blocking($listener, 0) if !$blocking;
            listen($listener, $listen) || 
                croak "Could not listen on socket: $!";
        };
        if ($@) {
            $abstract || unlink($path) || 
                croak "Could not unlink '$path' ($!) after $@";
            die $@;
        }
    };
    umask($oldmask) if defined($pumask);
    die $@ if $@;
    if ($auto_clean && !$abstract) {
        my $p = $path;
        bless ${*$listener}{auto_clean} = \$p, "FTP::Proxy::Utils::AutoClean";
    }
    return $listener unless wantarray;
    # Added / to the characters that don't get escaped
    $path =~ s{([^A-Za-z0-9_.!~*()\'/-])}{sprintf "%%%02X", $1}eg;
    return $listener, "unix://$path";
}

sub inet {
    my %params = @_;

    my $listen = delete $params{Listen};
    $listen = $default_listen unless defined($listen);
    my $reuse_addr = delete $params{defined($params{ReuseAddr}) ? "ReuseAddr" : "Reuse"};
    my $reuse_port = delete $params{ReusePort};
    # my $nagle = exists($params{Nagle}) ? delete($params{Nagle}) : 1;

    # Do the parsing in the style of IO::Socket::INET
    my $address = delete $params{defined($params{LocalAddr}) ? "LocalAddr" : "LocalHost"};
    my $port;
    if (defined($address) && $address =~ s!:([\w\(\)/]+)$!!) {
        $port = $1;
    } else {
        $port = delete($params{LocalPort}) || 0;
    }
    my $blocking = exists $params{Blocking} ? delete $params{Blocking} : 1;
    my $clo_exec = exists $params{CloExec} ? delete $params{CloExec} : 1;

    croak("Unknown options ", join(", ", keys %params)) if %params;

    my $default_port = $port =~ s!\((\d+)\)$!! ? $1 : undef;
    if ($port =~ /\D/) {
        defined($port = getservbyname($port, "tcp") || $default_port) ||
            croak "Could not find any port named $port/tcp";
    }
    my $laddress = defined($address) ? inet_aton($address) : INADDR_ANY;
    defined($laddress) || croak "Bad hostname '$address'";
    my $bind = pack_sockaddr_in($port, $laddress);

    my $listener;
    {
        # by default CLO_EXEC for everyone except if we hit fd 0
        # local $^F = !defined($clo_exec) ? 0 : $clo_exec ? -1 : 2**31-1;
        # default CLO_EXEC, otherwise do as asked
        local $^F = $clo_exec ? -1 : 2**31-1;
        socket($listener, PF_INET, SOCK_STREAM, IPPROTO_TCP) ||
            croak "Could not open INET socket: $!";
    }
    !$reuse_addr || setsockopt($listener, SOL_SOCKET, SO_REUSEADDR, 1) ||
        croak "Could not set reuse address option: $!";
    !$reuse_port || setsockopt($listener, SOL_SOCKET, SO_REUSEPORT, 1) ||
        croak "Could not set reuse port option: $!";
    bind($listener, $bind) || croak "Could not bind to '", defined($address) ? $address : "0.0.0.0", ":$port': $!";
    if ($port == 0) {
        my $laddr = getsockname($listener) ||
            croak "Could not getsockname: $!";
        ($port, $laddress) = unpack_sockaddr_in($laddr);
    }
    blocking($listener, 0) if !$blocking;
    listen($listener, $listen) || croak "Could not listen on bound socket: $!";
    # No uri-escaping is needed since the string won't contain anything unsafe
    return wantarray ? ($listener, "tcp://".inet_ntoa($laddress) . ":$port") :
        $listener;
}

sub listener {
    my ($target, %params) = @_;
    my ($scheme, $where, $options) = $target =~ m{^(\w+)://([^?]*)(.*)}s or
        croak "Listener '$target' is not of the form scheme://target";
    if ($options ne "") {
        croak "Listener doesn't accept options (yet)";
        # Add option parsing here
    }
    my $dport = delete $params{DefaultPort};
    if ($scheme eq "tcp") {
        croak "Parameters for listener '$target' should not contain key 'LocalAddr'" if exists $params{LocalAddr};
        my $port = $dport;
        if ($where =~ s/:(.+)\z//s) {
            $port = $1;
            $port  =~ s/%([0-9A-Fa-f]{2})/chr hex $1/eg;
        }
        croak "Parameters for listener '$target' should not contain key 'LocalPort'" if defined $port && exists $params{LocalPort};
        $where =~ s/%([0-9A-Fa-f]{2})/chr hex $1/eg;
        # Order matters,
        # we want the LocalPort in %options to win if $port is undef
        return inet(LocalAddr => $where,
                    LocalPort => $port, %params);
    } elsif ($scheme eq "unix") {
        $where =~ s/%([0-9A-Fa-f]{2})/chr hex $1/eg;
        croak "Parameters for listener '$target' should not contain key 'Path'"
            if exists $params{Path};
        return unix(Path => $where, %params);
    } else {
        croak "unknown scheme '$scheme'";
    }
}

sub is_tcp(*) {
    my $fh = shift;
    no warnings;
    return getsockopt($fh, IPPROTO_TCP, TCP_NODELAY) ? 1 : 0;
}

sub nagle(*;$) {
    my $fh = shift;
    if (!@_) {
        return unpack("I", getsockopt($fh, IPPROTO_TCP, TCP_NODELAY) ||
               croak "Could not get Nagle state: $!") ? 0 : 1;
    }
    if (shift) {
        setsockopt($fh, IPPROTO_TCP, TCP_NODELAY, 0) ||
            croak "Couldn't enable Nagle's algorithm: $!";
    } else {
        setsockopt($fh, IPPROTO_TCP, TCP_NODELAY,1) ||
            croak "Couldn't disable Nagle's algorithm: $!";
    }
}

sub spawn {
    my $listener = shift;
    defined(my $fd = fileno($listener)) || croak "Filehandle argument invalid";
    @_ || croak "No program argument";

    local $^F=0;	# CLO_EXEC for pipe (write side), none for STDIN
    pipe(my $rdr, my $wrt) || croak "Could not create pipe: $!";

    defined(my $pid = fork()) || die "Could not fork: $!";
    if ($pid == 0) {
        # Child
        eval {
            close($rdr);
            if ($fd) {
                # Normal case, socket not on fd 0 yet
                open(STDIN, "<&$fd") ||
                    die "Could not dup socket to STDIN: $!";
                close($listener);
            } else {
                clo_exec($listener, 0);
            }
            exec(@_) || die "Could not exec $_[0]: $!";
        };
        select($wrt);
        $|=1;
        print $@;
        _exit(1);
    }
    # parent
    close($wrt);
    close($listener);
    if (my @errors = <$rdr>) {
        die @errors;
    }
    return $pid;
}

sub blocking(*$) {
    my $handle = shift;
    no warnings;
    if ($^O eq 'MSWin32' || $^O eq 'VMS') {
	# There seems to be no way to query the state
	return undef unless @_;

	# FIONBIO enables non-blocking sockets on windows and vms.
	# FIONBIO is (0x80000000|(4<<16)|(ord('f')<<8)|126),
	# as per winsock.h, ioctl.h
	my $fionbio = 0x8004667e;
	my $val = pack("L!", shift() ? 0 : 1);
	ioctl($handle, $fionbio, $val) || croak "Can't set ioctl flags: $!";
    } else {
	my $flags = fcntl($handle, F_GETFL, 0) ||
	    croak "Can't get fcntl flags: $!\n";
	return $flags & O_NONBLOCK() ? 0 : 1 unless @_;
	fcntl($handle, F_SETFL,
	      shift() ?
	      $flags & O_NONBLOCK() ? $flags & ~O_NONBLOCK : return :
	      $flags & O_NONBLOCK() ? return : $flags | O_NONBLOCK) or
	      croak "Can't set fcntl flags: $!";
    }
}

sub clo_exec(*$) {
    my $handle = shift;
    no warnings;
    my $flags = fcntl($handle, F_GETFL, 0) ||
        croak "Can't get fcntl flags: $!\n";
    return $flags & FD_CLOEXEC() ? 1 : () unless @_;
    fcntl($handle, F_SETFL,
          shift() ?
          $flags & FD_CLOEXEC() ? return : $flags | FD_CLOEXEC :
          $flags & FD_CLOEXEC() ? $flags & ~FD_CLOEXEC : return) or
          croak "Can't set fcntl flags: $!\n";
}

sub start_connect {
    my ($target, %params) = @_;
    my ($scheme, $where, $options) = $target =~ m{^(\w+)://([^?]+)(.*)}s or
        croak "Connection target '$target' is not of the form scheme://target";
    if ($options ne "") {
        croak "Start_connect doesn't accept options (yet)";
        # Add option parsing here
    }
    $where =~ s/%([0-9A-Fa-f]{2})/chr hex $1/eg;

    my $blocking = delete $params{Blocking};
    my $buffered = delete $params{Buffered};
    croak("Unknown options ", join(", ", keys %params)) if %params;

    my ($peer, $to);
    if ($scheme eq "unix") {
        socket($peer, PF_UNIX, SOCK_STREAM, PF_UNSPEC) ||
            croak "Could not open UNIX socket: $!";
        $to = pack_sockaddr_un($where) || croak "Could not pack '$where'";
    } elsif ($scheme eq "tcp") {
        socket($peer, PF_INET, SOCK_STREAM, IPPROTO_TCP) ||
            croak "Could not open INET socket: $!";
        my ($address, $port) = $where =~ /^(.+):(\d+)$/s or
            croak "Could not parse target '$where' as address:port";
        my $addr = inet_aton($address) || croak "Could not resolve '$address'";
        $addr = inet_aton("127.0.0.1") if $addr eq INADDR_ANY;
        $to = pack_sockaddr_in($port, $addr);
    } else {
        croak "unknown scheme '$scheme'";
    }
    binmode($peer);
    blocking($peer, 0) if !$blocking;
    if (!$buffered) {
        my $fh = select($peer);
        $| = 1;
        select($fh);
    }
    $! = 0 if connect($peer, $to);
    return wantarray ? ($peer, $!) : $peer;
}

# Using this doesn't make any sense if your OS supports
# socketpair(..AF_INET, SOCK_STREAM..)
sub self_inet {
    for my $n (1..10) {
	# no strict "refs";
	my ($lsocket, $laddress) = listener("tcp://127.0.0.1");
	my ($lport) = $laddress =~ /:(\d+)\z/ or
	    croak "No port in listening address $laddress";
	blocking($lsocket, 0);

        socket($_[0], PF_INET, SOCK_STREAM, IPPROTO_TCP) ||
            croak "Could not open INET socket: $!";
	binmode($_[0]);
	my $to = pack_sockaddr_in($lport, inet_aton("127.0.0.1"));
	if (!connect($_[0], $to)) {
	    croak "Could not connect to $laddress: $! (",$!+0,")" if
		$! != ECONNREFUSED;
	    # Somebody is playing silly buggers
	    next;
	}
        my $caddr = getsockname($_[0]) || croak "Could not getsockname: $!";
        my ($cport, $caddress) = unpack_sockaddr_in($caddr);

	my $now = time;
	while (1) {
	    my $n = time;
	    croak "Huh? Can't even accept a known connection in two seconds"
		if $n > $now+2;
	    $now = $n if $n < $now;
	    my $adress = accept($_[1], $lsocket);
	    if (!$adress) {
		next if $! == EINTR;
		croak "Could not accept known pending connection: $!";
	    }
	    my ($aport, $aadress) = unpack_sockaddr_in($adress);
	    next if $cport != $aport || $aadress ne $caddress;
	    close($lsocket);
	    blocking($_[1], 1);
	    shutdown($_[0], 1);        # no more writing for reader
	    shutdown($_[1], 0);        # no more reading for writer
	    return;
	}
    }
    croak "Could not selfconnect. Someone is DOSsing you ?";
}

1;

__END__

=head1 NAME

WEC::Socket - Networking helper functions for WEC

=head1 SYNOPSIS

 # You must export the methods you plan to use
 use WEC::Socket qw(...);

 $socket = unix(%options);
 ($socket, $address) = unix(%options);

 $socket = inet(%options);
 ($socket, $address) = inet(%options);

 $socket = listener($name, %options);
 ($socket, $address) = listener($name, %options);

 $boolean = is_tcp($handle);

 $nagle = nagle($socket);
 $old_nagle = nagle($socket, $nagle);

 $blocking = blocking($handle);
 blocking($handle, $blocking);

 $clo_exec = clo_exec($handle);
 clo_exec($handle, $clo_exec);

 $socket = start_connect($address, %options);
 ($socket, $errno) = start_connect($address, %options);

 $pid = spawn($socket, $program, @args)

=head1 DESCRIPTION

=head1 EXPORT

All functions raise an exception in case of error.

Nothing is exported by default, but the following ones may be requested:

=over

=item X<unix>$socket = unix(%options)

Creates a listening unix domain socket based on the list of tag/value pairs
given as %options. Tags can be:

=over

=item X<unix_Path>Path => $filename

Where in the filesystem the socket should be created.
If not given, a temporary filename will be automatically chosen by using
L<POSIX::tmpnam|POSIX/"tmpnam">.

=item X<unix_Abstract>Abstract => $boolean

If given a true argument, the unix socket will be abstract, meaning it won't
appear in the normal filesystem.

=item X<unix_AutoClean>AutoClean => $boolean

If given a true argument the listening unix socket will be automatically 
removed when the listening socket is closed. This however depends on the 
destructor method getting to run, so if for example the program is killed hard
enough or the socket becomes part of a circular datastructure the cleanup action
will not happen. Consider using abstract sockets (on operating systems that
support them) for guaranteed cleanup.

=item X<unix_PathUmask>PathUmask => $umask

If given, the socket will be created using the given $umask.
Defaults to using the current global L<umask|perlfunc/"umask">.

=item X<unix_CloExec>CloExec => $boolean

If given a true value, the returned socket will remain open across
L<exec|perlfunc/exec>. It will get closed in the execed program otherwise.

Defaults to true.

=item X<unix_BindAttempts>BindAttempts => $attempts

If no L<Path|"unix_Path"> is given and the system choses a filename,
it can get unlucky and still choose an already existing name. This determines
how many times it will try different names before giving up.

Defaults to 10.

=item X<unix_Listen>Listen => $queue_size

The value that will be used as QUEUESIZE for the L<listen|perlfunc/"listen">
call on the created socket.

Defaults to 5.

=item X<unix_Blocking>Blocking => $boolean

If set to true the socket will be blocking, otherwise it will be non-blocking.

The default is true.

=back

The function returns the newly created socket.

You can also use L<IO::Socket::UNIX|IO::Socket::UNIX> to create unix sockets.

=item ($socket, $address) = unix(%options)

Same as the scalar context version, but it also returns a string that can be
used as argument to L<start_connect|"start_connect"> later if you want to
connect to the newly created socket.

=item X<inet>$socket = inet(%options)

Creates a listening internet domain socket based on the list of tag/value pairs
given as %options. Tags can be:

=over

=item X<inet_LocalAddr>LocalAddr => $hostname[:$port]

Gives the hostname or ip-address of an interface to bind to. Defaults to
L<INADDR_ANY|Socket/"INADDR_ANY">, meaning it will listen on all addresses of
the current host. May optionally be followed by a port to bind to (otherwise
it will choose a free port for you). The port can either be numeric or a
logical name.

If you give a hostname, that will cause a temporary block for a DNS lookup,
which may be a problem in an event-driven program. In that case, do an
asynchronous DNS lookup and only call this function with the returned
ip-address as LocalAddr.

=item X<inet_LocalHost>LocalHost => $hostname[:$port]

Synonym for L<LocalAddr|"inet_LocalAddr">

=item X<inet_LocalPort>LocalPort => $port

If given, will bind to the given port. The port can either be numeric or a
logical name. If not given, the system will choose a free port.

=item X<inet_ReuseAddr>ReuseAddr => $boolean

Sets SO_REUSEADDR before binding if given a true value

=item X<inet_Reuse>Reuse => $boolean

Synonym for L<ReuseAddr|"inet_ReuseAddr">

=item X<inet_ReusePort>ReusePort => $boolean

Sets SO_REUSEPORT before binding if given a true value

=item X<inet_CloExec>CloExec => $boolean

If given a true value, the returned socket will remain open across
L<exec|perlfunc/exec>. It will get closed in the execed program otherwise.

=item X<inet_Listen>Listen => $queue_size

The value that will be used as QUEUESIZE for the L<listen|perlfunc/"listen">
call on the created socket.

Defaults to 5.

=item X<inet_Blocking>Blocking => $boolean

If set to true the socket will be blocking, otherwise it will be non-blocking.

The default is true.

=back

The function returns the newly created socket.

You can also use L<IO::Socket::INET|IO::Socket::INET> to create internet
sockets.

=item ($socket, $address) = inet(%options)

Same as the scalar context version, but it also returns a string that can be
used as argument to L<start_connect|"start_connect"> later if you want to
connect to the newly created socket.

=item X<listener>$socket = listener($name, %options)

This is a convenience funtion to start a listening socket by name. $name can
be of two forms:

=over

=item X<listener_unix>unix://path

This will call L<unix|"unix"> with the uridecoded and query-parameter stripped
form of path as L<the Path parameter|"unix_Path"> added to %options.
The only characters in path that B<have> to be escaped are C<?> and C<%>.

=item X<listener_tcp>tcp://host

=item tcp://host:port

This will call L<inet|"inet"> with the uridecoded and query-parameter stripped
form of host as L<the LocalAddr parameter|"inet_LocalAddr"> and port as
L<the LocalPort parameter|"inet_LocalPort"> added to %options. If there is no
port it will use L<the DefaultPort parameter|"listener_DefaultPort"> if there
is one and pick a free port otherwise.
The only characters in host and port that B<have> to be escaped are C<?>, C<%>
and C<:> (none of these will appear in normal host/port combination, so
probably you can ignore this issue).

=back

The following are removed from %options before it's passed to L<unix|"unix"> or
L<inet|"inet">:

=over

=item L<listener_DefaultPort>DefaultPort => port

This is what will be used as default port when you ask for a tcp listening
socket, but it's always valid to pass, even if it doesn't get used in the end.

=back

=item ($socket, $address) = listener($name, %options)

Same as the scalar context version, but it also returns a string that can be
used as argument to L<start_connect|"start_connect"> later if you want to
connect to the newly created socket. (This string will probably be very simular
to what you entered as $name)

=item X<is_tcp>$boolean = is_tcp($handle)

Returns true if the given handle is a tcp socket, false otherwise.

=item X<nagle>$nagle = nagle($socket)

Returns true if Nagle's algorithm is turned on on the given socket.

=item $old_nagle = nagle($socket, $nagle)

Turns on Nagle's algorithm on the given $socket if $nagle is true. Turns it
off otherwise. Returns the old setting.

=item X<blocking>$blocking = blocking($handle)

Returns true if the given $handle is blocking, false otherwise.

=item blocking($handle, $blocking)

Sets the given $handle to blocking if $blocking is true, makes it unblocking
otherwise.

=item X<clo_exec>$clo_exec = blocking($handle)

Returns true if the given $handle will be closed across L<exec|perlfunc/exec>,
false otherwise.

=item clo_exec($handle, $clo_exec)

Sets the given $handle to close across L<exec|perlfunc/exec> if $clo_exec is
true, makes it not closing across L<exec|perlfunc/exec> otherwise.

=item X<start_connect>$socket = start_connect($address, %options)

Creates a new non-blocking socket of the type implied by $address and then
initiates a connection attempt to address. Returns the new socket while you
can check the L<connect|perlfunc/"connect"> returncode in L<$!|perlvar/"$!">.

A returncode might be:

=over

=item 0

The connection immediately succeeded. Normally you cannot be sure that a
successful systemcall sets L<$!|perlvar/"$!">, but the code forces it in
this case.

=item L<EINPROGRESS|POSIX/"EINPROGRESS">

The connection did not succeed or fail immediately. You are supposed to
select on the socket for writability and then check if the connection
succeeded (for example by using L<getsockopt|perlfunc/"getsockopt"> to read
the SO_ERROR option at level SOL_SOCKET, or by doing a
L<getpeername|perlfunc/"getpeername"> to see if the socket is indeed connected
and do a L<sysread|perlfunc/"sysread"> otherwise to pick up the errorcode).

=item Anything else

The connection attempt failed and L<$!|perlvar/"$!"> actually gives the reason.

=back

%options is a list of tag/value pairs. Tags can be:

=over

=item X<start_connect_Blocking>Blocking => $boolean

If given a true value the connect will be blocking and the function will wait
until the connect either succeeded or failed. This can take a long time and
should therefore usually not be used in an event driven program.

Defaults to false.

=item X<start_connect_Buffered>Buffered => $boolean

Buffered I/O on this socket will be autoflushed if this option is false.
Otherwise output will only be flushed when the internal buffer reaches a given
size.

The default is false which is usually what is wanted for sockets.

=back

=item ($socket, $errno) = start_connect($address, %options)

Same as the scalar context version, but also returns the L<$!|perlvar/"$!">
error code in $errno.

=item X<spawn>$pid = spawn($socket, $program, @args)

Executes $program with @args inside a new process with STDIN and STDOUT
set to $socket. Returns the pid of the new process. You retain responsibility
to reap the child when it dies.

=back

=head1 SEE ALSO

L<WEC>,
L<Socket>,
L<IO::Socket>,
L<IO::Socket::INET>,
L<IO::Socket::UNIX>,
L<perlipc>

=head1 AUTHOR

Ton Hospel, E<lt>WEC@ton.iguana.beE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2004 by Ton Hospel

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.2 or,
at your option, any later version of Perl 5 you may have available.

=cut
