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
      "RabbitMQ source AMQP URI. Can be used multiple times to consume from "
      "multiple brokers."},
     {destination, $d, "destination",
      {string, "amqp://guest:guest@localhost:5673/%2f"}, "RabbitMQ "
      "destination AMQP URI."},
     {src_declare, undefined, "src_declare", string, "Any declaration in "
      "the source broker. Can be used multiple times."},
     {dst_declare, undefined, "dst_declare", string, "Any declaration in "
      "the destination broker. Can be used multiple times."},
     {queue, $q, "queue", binary, "Queue to consume from. Mandatory."},
     {prefetch, $n, "prefetch", integer, "Prefetch this number of messages."},
     {reconnect_delay, $r, "reconnect_delay", integer, "Reconnect delay."},
     {ack_mode, $a, "ack_mode", atom, "Acknowledgement mode: no_ack, "
      "on_publish or on_confirm."},
     {publish_field, $p, "publish_field", string, "By default Shovel will "
      "keep message fields (fields in the basic.publish method used to "
      "re-publish messages, e.g. exchange, routing key, etc) intact. Use "
      "this option (multiple times if needed) to override any of these "
      "fields."},
     {config, $f, "config", string, "Configuration file to read "
      "shovel definitions from. This should contain your shovels, as a tuple "
      "ending with a dot for each. If provided, any other command line "
      "option will be ignored. Can be specified multiple times to use more "
      "than one shovel configuration file (but the names of shovels must be "
      "unique)."},
     {version, $v, "version", undefined, "Show version info."},
     {help, $h, "help", undefined, "Show usage info."}],
    {ok, {Props, Leftover}} = getopt:parse(OptSpecList, Args),
    case proplists:is_defined(version, Props) of
        true -> show_version(),
                halt(0);
        false -> ok
    end,
    Help = proplists:get_value(help, Props),
    if Help =/= undefined; length(Leftover) =/= 0 -> getopt:usage(OptSpecList,
                                                                  ?PROG),
                                                     halt(0);
       Help =:= undefined, length(Leftover) =:= 0 -> start(Props)
    end.

%% ===================================================================
%% Private
%% ===================================================================

show_version() ->
    ok = application:load(rmq_shovel),
    {ok, Vsn} = application:get_key(rmq_shovel, vsn),
    io:format("rmq_shovel v~ts~n", [Vsn]),
    ok.

start(Props) ->
    case proplists:get_all_values(config, Props) of
        [] -> start_with_cmdline(Props);
        Configs -> start_with_configfile(Configs)
    end.

start_with_configfile(Configs) ->
    start_with_configfile(Configs, []).

start_with_configfile([H|T], Shovels) ->
    {ok, Shovel} = file:consult(H),
    start_with_configfile(T, Shovel ++ Shovels);

start_with_configfile([], Shovels) ->
    start_shovels(Shovels).

start_with_cmdline(Props) ->
    Source = proplists:get_all_values(source, Props),
    Destination = proplists:get_value(destination, Props),
    SrcDeclare = to_term(proplists:get_all_values(src_declare, Props)),
    DstDeclare = to_term(proplists:get_all_values(dst_declare, Props)),
    Queue = proplists:get_value(queue, Props),
    Fields = to_term(proplists:get_all_values(publish_field, Props)),
    Prefetch = proplists:get_value(prefetch, Props, 0),
    AckMode = proplists:get_value(ack_mode, Props, on_confirm),
    Delay = proplists:get_value(reconnect_delay, Props, 5),
    Shovel = {shovel, [{sources, [{brokers, Source},
                                  {declarations, SrcDeclare}]},
                       {destinations, [{broker, Destination},
                                       {declarations, DstDeclare}]},
                       {queue, Queue},
                       {publish_fields, Fields},
                       {prefetch_count, Prefetch},
                       {ack_mode, AckMode},
                       {reconnect_delay, Delay}
                      ]},
    start_shovels([Shovel]).

to_term(List) ->
    to_term(List, []).

to_term([], Acc) ->
    Acc;

to_term([H|T], Acc) ->
    {ok, Tokens, _} = erl_scan:string(H ++ "."),
    {ok, Term} = erl_parse:parse_term(Tokens),
    to_term(T, [Term|Acc]).

start_shovels(Shovels) ->
    error_logger:info_msg("starting shovel(s) ~p~n", [Shovels]),
    ok = application:start(amqp_client),
    ok = application:load(rabbitmq_shovel),
    ok = application:set_env(rabbitmq_shovel, shovels, Shovels),
    ok = application:start(rabbitmq_shovel),
    Pid = erlang:whereis(rabbitmq_shovel_sup),
    Ref = erlang:monitor(process, Pid),
    loop(Pid, Ref).

loop(Pid, Ref) ->
    receive
        {'DOWN', Ref, process, Pid, shutdown} ->
            halt(0);
        {'DOWN', Ref, process, Pid, Reason} ->
            error_logger:error_msg("shovel stopped: ~p~n", [Reason]),
            halt(1);
        _ ->
            ok
    end,
    loop(Pid, Ref).
