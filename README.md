Gerlp
=====
This is a distributed grep implemented as an Erlang/OTP application.

Build
-----
You need Erlang and [rebar](https://github.com/basho/rebar). To generate a release, you also need [relx](https://github.com/erlware/relx).

To build the application:

    $ make

This will create a compact binary called `gerlp`.

To create an OTP release:

    $ vim config/sys.config # edit configuration file
    $ vim config/vm.args    # edit VM config
    $ relx

This will create a self-contained release in `_rel`.

Usage
-----
You can either use the binary, or the generated release.

You need to create an Erlang hosts file. It is usually located in `~/.hosts.erlang`. Example:

    'slave01.mynetwork.com'.
    'slave02.mynetwork.com'.
    'slave03.mynetwork.com'.

This will use three slaves to do the work. You need ssh access to the slaves and Erlang/OTP installed on them. If there is a firewall or NAT between the machine where `gerlp` runs (the master) and the slaves, then you might need to open it up and forward a few ports in. See [this](http://learnyousomeerlang.com/distribunomicon#firewalls) for more information.

Using the `gerlp` binary:

    $ ./gerlp

See `gerlp --help` for possible command line flags.

Why?
----
`Gerlp` is not very useful - even for very large files, a local `grep` will be around one order of magnitude faster. It was made to show how easy it is to do distributed computing with Erlang/OTP.
