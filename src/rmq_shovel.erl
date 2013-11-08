%% rmq_shovel escript entry

-module(rmq_shovel). % needed for rebar

-export([main/1]).

-define(PROG, atom_to_list(?MODULE)).

%% ===================================================================
%% API
%% ===================================================================

main(Args) ->
    OptSpecList =
    [{source, $s, "source", {string, "amqp://guest:guest@localhost:5672/%2f"},
      "RabbitMQ source AMQP URI."},
     {destination, $d, "destination",
      {string, "amqp://guest:guest@localhost:5673/%2f"}, "RabbitMQ source "
      "AMQP URI."},
     {queue, $q, "queue", string, "Queue to consume from. Mandatory."},
     {prefetch, $p, "prefetch", integer, "Prefetch this number of messages."},
     {reconnect_delay, $r, "reconnect_delay", integer, "Reconnect delay."},
     {ack_mode, $a, "ack_mode", string, "Acknowledgement mode: no_ack, "
      "on_publish or on_confirm."},
     {config, $f, "config", {string, undefined}, "Configuration file to read "
      "shovel definitions from. This should contain your shovels, as a tuple "
      "ending with a dot for each. If provided, any other command line "
      "option will be ignored."},
     {help, $h, "help", undefined, "Show usage info."}],
    {ok, {Props, Leftover}} = getopt:parse(OptSpecList, Args),
    Help = proplists:get_value(help, Props),
    if Help =/= undefined; length(Leftover) =/= 0 -> getopt:usage(OptSpecList,
                                                                  ?PROG),
                                                     halt(0);
       Help =:= undefined, length(Leftover) =:= 0 -> start(Props)
    end.

%% ===================================================================
%% Private
%% ===================================================================

start(Props) ->
    case proplists:get_value(config, Props) of
        undefined -> start_with_cmdline(Props);
        ConfigFile -> start_with_configfile(ConfigFile)
    end.

start_with_configfile(Config) ->
    {ok, Shovels} = file:consult(Config),
    start_shovels(Shovels).

start_with_cmdline(Props) ->
    {error, Props}. % TODO

start_shovels(Shovels) ->
    io:format("starting shovel(s) ~p~n", [Shovels]),
    ok = application:start(sasl),
    ok = application:start(amqp_client),
    ok = application:load(rabbitmq_shovel),
    ok = application:set_env(rabbitmq_shovel, shovels, Shovels),
    ok = application:start(rabbitmq_shovel),
    loop().

loop() ->
    Pid = erlang:whereis(rabbitmq_shovel_sup),
    Ref = erlang:monitor(process, Pid),
    receive
        {'DOWN', Ref, process, Pid, shutdown} ->
            halt(0);
        {'DOWN', Ref, process, Pid, Reason} ->
            io:format("error: ~p~n", [Reason]),
            halt(1);
        _ ->
            ok
    end,
    loop().
