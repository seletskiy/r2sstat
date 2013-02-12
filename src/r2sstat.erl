%% @author Stanislav Seletskiy <s.seletskiy@gmail.com>
%% @doc Entry for console r2s-viewer app.
-module(r2sstat).
-created('Date: 12/02/2013').
-created_by('Stanislav Seletskiy <s.seletskiy@gmail.com>').

-export([
    main/1
]).

-define(DEFAULT_TIMEOUT,  10000).
-define(DEFAULT_WAIT,     5000).
-define(DEFAULT_CHANNELS, [100, 700, 600, 610, 601, 715]).

-record(state, {
    channels :: [integer()],
    wait :: integer(),
    timeout :: integer(),
    master :: umb_device:device(),
    device :: umb_device:device(),
    bus    :: atom(),
    line   :: integer()
}).

%% @doc Main entry to script.
-spec main([]) -> ok.
main([]) ->
    usage();
main(RawArgs) ->
    application:start(umb),
    case getopt:parse(get_opts_spec(), RawArgs) of
        {ok, {Opts, Args}} ->
            case validate(Opts, Args) of
                {ok, Action, VArgs} ->
                    do_action(Action, Opts, VArgs);
                {error, Reason, Data} ->
                    show_error(Reason, Data)
            end;
        {error, {Reason, Data}} ->
            show_error(Reason, Data)
    end.

validate([help | _], _) ->
    {ok, help, []};
validate([_ | Tail], Args) ->
    validate(Tail, Args);
validate([], [Host, Port | Channels]) ->
    {ok, view, [
        Host, list_to_integer(Port) |
        lists:map(fun(X) -> list_to_integer(X) end, Channels)]};
validate([], _) ->
    {error, host_and_port, "<host> and <port> are required"}.

do_action(help, _, _) ->
    usage();
do_action(view, Opts, [Host, Port | []]) ->
    do_action(view, Opts, [Host, Port | ?DEFAULT_CHANNELS]);
do_action(view, Opts, [Host, Port | Channels]) ->
    Timeout = proplists:get_value(timeout, Opts, ?DEFAULT_TIMEOUT),
    Wait    = proplists:get_value(wait,    Opts, ?DEFAULT_WAIT),
    State = #state{
        channels = Channels,
        timeout  = Timeout,
        wait     = Wait,
        line     = element(2, io:rows()),
        master   = umb_device:new(15, 1),
        device   = umb_device:new(2,  1),
        bus      = element(2, umb_bus:connect(r2s, umb_transport_tcp, [
            Host, Port, [
                {inet, [{send_timeout, Timeout}]},
                {recv_timeout, Timeout}]]))
    },
    view_loop(State).

view_loop(State) ->
    {Result, State2} = request(State, umb_request:multichannel([
        umb_request:online_data(Channel) || Channel <- State#state.channels])),
    State3 = State2#state{line = State2#state.line + 1},
    State4 = case Result of
        {ok, Frame} ->
            case umb_request:payload(Frame) of
                {ok, Payload} ->
                    print_line(State3, Payload);
                {error, Reason} ->
                    io:format(standard_error, "Error: ~p~n", [Reason]),
                    State3
            end;
        {error, Reason} ->
            io:format(standard_error, "Error: ~p~n", [Reason]),
            State3
    end,
    timer:sleep(State4#state.wait),
    view_loop(State4).

request(State, Request) ->
    Result = umb_bus:request(
        State#state.bus, State#state.master, State#state.device,
        Request),
    {Result, State}.

print_line(State, Data) ->
    Format = make_format(State#state.channels),
    State2 = case State#state.line >= element(2, io:rows()) - 1 of
        true ->
            print_header(Format),
            State#state{line = 1};
        false ->
            State
    end,
    print_data(Format, Data),
    State2.

make_format(Channels) ->
    make_format(Channels, []).
make_format([], Format) ->
    [{length(print_date()), "timestamp", undefined}] ++ Format;
make_format([Channel | Tail], Format) ->
    Item = case lists:keyfind(Channel, 1, known_channels()) of
        false ->
            Name = integer_to_list(Channel),
            {length(Name), Name, fun(X) -> X end};
        {Channel, Name, _, Fmt} ->
            {length(Name), Name, Fmt}
    end,
    make_format(Tail, Format ++ [Item]).

print_date() ->
    {{Y, M, D}, {Hour, Min, Sec}} = erlang:localtime(),
    lists:flatten(
        io_lib:format("~2..0B/~2..0B/~2..0B ~2..0B:~2..0B:~2..0B", [
            Y rem 1000, M, D, Hour, Min, Sec])).

print_header([]) ->
    io:format("~n");
print_header([FormatItem | FormatTail]) ->
    {Len, Name, _}  = FormatItem,
    FmtString = lists:flatten(io_lib:format("~~~Bs ", [Len])),
    io:format(FmtString, [Name]),
    print_header(FormatTail).

print_data([], []) ->
    io:format("~n");
print_data([{_, "timestamp", _} | FormatTail], Data) ->
    io:format("~s ", [print_date()]),
    print_data(FormatTail, Data);
print_data([FormatItem | FormatTail], [DataItem | DataTail]) ->
    {Len, _, Fmt}  = FormatItem,
    FmtString = lists:flatten(io_lib:format("~~~Bs ", [Len])),
    io:format(FmtString, [(Fmt(DataItem))]),
    print_data(FormatTail, DataTail).

show_error(Reason, Data) ->
    io:format(standard_error, "Error: ~s ~p~n~n", [Reason, Data]),
    usage().

usage() ->
    getopt:usage(get_opts_spec(), atom_to_list(?MODULE),
        "<host> <port> [channel ...]", [
            {"<host>", "Host to connect to"},
            {"<port>", "TCP port to connect to"},
            {"channel", "one or more channels to fetch; " ++
                "if not specified default set will be used"}]).

known_channels() ->
    [
        {100, "a.temp", "Ambient temperature (Celsium)", fun format_celsium/1},
        {700, "precip", "Precipation type", fun format_precipation/1},
        {600, "a.ltr/m", "Accumulated litres per meter^2", fun format_float/1},
        {610, "a.flm/m", "Accumulated film level per meter^2 (mm)", fun format_float/1},
        {601, "flm/m", "Film level per meter^2 since last request (mm)", fun format_float/1},
        {715, "sflk/s", "Snowflakes per second", fun format_int/1}
    ].

format_precipation(P) ->
    io_lib:format("~s", [P]).

format_int(I) ->
    io_lib:format("~B", [I]).

format_celsium(C) ->
    io_lib:format("~.1fC", [C]).

format_float(F) ->
    io_lib:format("~.2f", [F]).

%% @doc Returns list of console options.
get_opts_spec() ->
    %% {Name,     ShortOpt,  LongOpt,   ArgSpec,   HelpMsg}
    [
        {help,     $h,        "help",    undefined, "Show this help"},
        {timeout,  $t,        undefined, integer,   "Timeout to network operations (ms)"},
        {wait,     $w,        undefined, integer,   "Wait between requests (ms)"},
        {noheader, $n,        undefined, undefined, "Do not display columns header"}
    ].
