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
There are two possibilities.

The first one is to put your shovel definitions into a configuration file, and start the application like this:

    $ ./rmq_shovel -f shovel.conf

Shovels are configured via Erlang terms, one per instance, each ending with a dot. An example config file `shovel.conf` is provided. You can use multiple configuration files (by specifying `-f <config>` multiple times).

The other possibility is to provide parameters via command line options. E.g.:

    $ ./rmq_shovel -s amqp://localhost -d amqp://localhost -q src -p '{exchange, <<"">>}' -p '{routing_key, <<"dst">>}' --src_declare "{'queue.declare', [{queue, <<\"src\">>}, durable]}" --dst_declare "{'queue.declare', [{queue, <<\"dst\">>}, durable]}"

This will start shoveling from the queue `src` on the broker running on localhost, to the same broker, but to the queue `dst`. Both `src` and `dst` is declared first as durable queues.

If you provide a configuration file via `-f`, no other command line option will be used.

Example: you want to move messages from the broker running on localhost, from queue `src` to queue `dst`, which is bound to the exchange `dstexchange` via `dstkey`. Everything is declared already. You can:

    rmq_shovel -s amqp://guest:guest@mag-fh-queue02:5672/%2f -d amqp://guest:guest@mag-fh-queue02:5672/%2f -q src -p '{exchange, <<"dstexchange">>}' -p '{routing_key, <<"dstkey">>}'

If you want to control how fast messages are moved you can use the `--prefetch` (or the short form `-n`). This is the number of documents prefetched from the input queue without acknowledging.

For possible options, see `-h`/`--help`.

For more information on shovel, see the [RabbitMQ Shovel plugin page](http://www.rabbitmq.com/shovel.html).
