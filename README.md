Rmq_shovel
==========

This is a RabbitMQ shovel application. It reuses the rabbitmq_shovel plugin, just repackages it into a standalone Erlang app.

Shovel is a great tool, however it can be very inconvenient that the broker needs to be restarted for any configuration change to take effect. That's why I created a simple wrapper around it - you can even reuse the same configuration snippets, and it's enough to restart this application if you change anything.

Build
-----
You need Erlang and [rebar](https://github.com/basho/rebar).

    $ rebar get-deps compile escriptize

This should fetch dependencies, compile everything, and create an executable escript called `rmq_shovel`.

You can also use `make` to drive the build:

    $ make

in the top level directory should build the app.

Usage
-----
Put your shovel definitions into a configuration file, and

    $ ./rmq_shovel -f shovel.conf

to start the application.

Shovels are configured via Erlang terms, one per instance, each ending with a dot. An example config file `shovel.conf` is provided.

For possible options, see `-h`/`--help`.

For more information on shovel, see the [RabbitMQ Shovel plugin page](http://www.rabbitmq.com/shovel.html).
