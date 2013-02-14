%% @author Stanislav Seletskiy <s.seletskiy@gmail.com>
%% @doc Console viewer for R2S-UMB that display info like iostat.
-module(r2sstat).
-created('Date: 12/02/2013').
-created_by('Stanislav Seletskiy <s.seletskiy@gmail.com>').

-include("umb.hrl").

-export([
    main/1
]).

-define(DEFAULT_TIMEOUT,  10000).
-define(DEFAULT_WAIT,     5000).
-define(DEFAULT_CHANNELS, [
    ?UMB_CNL_R2S_TEMP_C,
    ?UMB_CNL_R2S_PREC_TYPE,
    ?UMB_CNL_R2S_LITRES,
    ?UMB_CNL_R2S_LITRES_SINCE,
    ?UMB_CNL_R2S_FILM,
    ?UMB_CNL_R2S_FILM_SINCE,
    ?UMB_CNL_R2S_DRIZZLES,
    ?UMB_CNL_R2S_RDROPS,
    ?UMB_CNL_R2S_SFLAKES,
    ?UMB_CNL_R2S_HAILS,
    ?UMB_CNL_R2S_DRIZZLE_F,
    ?UMB_CNL_R2S_RAIN_F,
    ?UMB_CNL_R2S_SNOW_F,
    ?UMB_CNL_R2S_HAIL_F
]).

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
            case parse_args(Opts, Args) of
                {ok, Action, VArgs} ->
                    do_action(Action, Opts, VArgs);
                {error, Reason, Data} ->
                    show_error(Reason, Data)
            end;
        {error, {Reason, Data}} ->
            show_error(Reason, Data)
    end.

%% @doc Parse required arguments and choose action to do.
parse_args([help | _], _) ->
    {ok, help, []};
parse_args([list_chan | _], [Host, Port]) ->
    {ok, list_chan, [Host, list_to_integer(Port)]};
parse_args([list_chan | _], _) ->
    parse_args([], []);
parse_args([_ | Tail], Args) ->
    parse_args(Tail, Args);
parse_args([], [Host, Port | Channels]) ->
    {ok, view, [Host, list_to_integer(Port) |
        lists:map(fun(X) -> list_to_integer(X) end, Channels)]};
parse_args([], _) ->
    {error, host_and_port, "<host> and <port> are required"}.

%% @doc Do selected action.
do_action(help, _, _) ->
    usage();
do_action(list_chan, Opts, [Host, Port]) ->
    State = make_state(Opts, Host, Port, []),
    case list_channels(State) of
        {ok, List} -> print_channels(List);
        Error      -> show_error(Error, "can not get channels list", no_usage)
    end;
do_action(view, Opts, [Host, Port | []]) ->
    do_action(view, Opts, [Host, Port | ?DEFAULT_CHANNELS]);
do_action(view, Opts, [Host, Port | Channels]) ->
    view_loop(make_state(Opts, Host, Port, Channels)).

%% @doc Creating internal program state for later usage.
make_state(Opts, Host, Port, Channels) ->
    Timeout = proplists:get_value(timeout, Opts, ?DEFAULT_TIMEOUT),
    Wait    = proplists:get_value(wait,    Opts, ?DEFAULT_WAIT),
    State = #state{
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
    {ok, ChannelsInfo} = list_channels_full_info(known_channels(),
        Channels, State),
    State#state{channels = ChannelsInfo}.

%% @doc Action: infinite loop for obtaining data from device.
view_loop(State) ->
    Result = request(State, umb_request:multichannel([
        umb_request:online_data(proplists:get_value(channel, Channel)) ||
            Channel <- State#state.channels])),
    State2 = State#state{line = State#state.line + 1},
    State3 = case Result of
        {ok, Payload} ->
            print_line(State2, Payload);
        {error, Reason} ->
            show_error(Reason, "failed to obtain data", no_usage),
            State2
    end,
    timer:sleep(State3#state.wait),
    view_loop(State3).

%% @doc Shorthand for sending requests via umb api.
request(State, Request) ->
    Result = umb_bus:request(
        State#state.bus, State#state.master, State#state.device,
        Request),
    case Result of
        {ok, Frame} -> umb_request:payload(Frame);
        Error       -> Error
    end.

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
    [{length(print_date()), "timestamp"} | lists:reverse(Format)];
make_format([Channel | Tail], Format) ->
    Num = proplists:get_value(channel, Channel),
    Var = proplists:get_value(variable, Channel),
    Name = abbrev(Num, Var),
    make_format(Tail, [{length(Name), Name} | Format]).

print_date() ->
    {{Y, M, D}, {Hour, Min, Sec}} = erlang:localtime(),
    lists:flatten(
        io_lib:format("~2..0B/~2..0B/~2..0B ~2..0B:~2..0B:~2..0B", [
            Y rem 1000, M, D, Hour, Min, Sec])).

print_header([]) ->
    io:format("~n");
print_header([{Len, Name} | FormatTail]) ->
    FmtString = lists:flatten(io_lib:format("~~~Bs ", [Len])),
    io:format(FmtString, [Name]),
    print_header(FormatTail).

print_data([], []) ->
    io:format("~n");
print_data([{_, "timestamp"} | FormatTail], Data) ->
    io:format("~s ", [print_date()]),
    print_data(FormatTail, Data);
print_data([{Len, _} | FormatTail], [DataItem | DataTail]) ->
    FmtString = lists:flatten(io_lib:format("~~~Bs ", [Len])),
    io:format(FmtString, [(format_item(DataItem))]),
    print_data(FormatTail, DataTail).

show_error(Reason, Data, no_usage) ->
    io:format(standard_error, "Error: ~w ~p~n", [Reason, Data]).
show_error(Reason, Data) ->
show_error(Reason, Data, no_usage),
    usage().

usage() ->
    getopt:usage(get_opts_spec(), atom_to_list(?MODULE),
        "<host> <port> [channel ...]", [
            {"<host>", "Host to connect to"},
            {"<port>", "TCP port to connect to"},
            {"channel", "one or more channels to fetch; " ++
                "if not specified default set will be used"}]).

print_channels([]) ->
    ok;
print_channels([Channel | Tail]) ->
    Num  = proplists:get_value(channel, Channel),
    Var  = proplists:get_value(variable, Channel),
    Unit = proplists:get_value(unit, Channel),
    Type = proplists:get_value(data_type, Channel),
    io:format("~5B [~10s] ~6s ~-22s ~-15s~n", [Num, abbrev(Num, Var), Type,
        Var, Unit]),
    print_channels(Tail).

list_channels(State) ->
    case request(State, umb_request:info_cnl_num()) of
        {ok, {_, BlocksNum}} -> {ok, list_channels_block(BlocksNum, State)};
        Error                -> Error
    end.

list_channels_block(Count, State) ->
    list_channels_block(0, Count, [], State).
list_channels_block(N, N, Result, _State) ->
    Result;
list_channels_block(N, Count, Result, State) ->
    case request(State, umb_request:info_cnl_list(N)) of
        {ok, List} ->
            case list_channels_full_info([], List, State) of
                {ok, Info} ->
                    list_channels_block(N + 1, Count,
                        lists:append(Info, Result), State);
                Error -> Error
            end;
        Error
            -> Error
    end.

list_channels_full_info(Known, List, State) ->
    list_channels_full_info(Known, List, [], State).
list_channels_full_info(_, [], Result, _) ->
    {ok, lists:reverse(Result)};
list_channels_full_info(Known, [Channel | Tail], Result, State) ->
    Info = case lists:keyfind(Channel, 1, Known) of
        {Channel, I} ->
            I;
        false ->
            case request(State, umb_request:info_cnl_full(Channel)) of
                {ok, I} -> I;
                _Error     -> undefined
            end
    end,
    case Info of
        undefined ->
            list_channels_full_info(Known, Tail, Result, State);
        Info ->
            list_channels_full_info(Known, Tail, [Info | Result], State)
    end.

abbrev(?UMB_CNL_R2S_TEMP_C, _) ->
    "a.temp";
abbrev(?UMB_CNL_R2S_LITRES, _) ->
    "a.ltr/m";
abbrev(?UMB_CNL_R2S_LITRES_SINCE, _) ->
    "ltr/m";
abbrev(?UMB_CNL_R2S_FILM, _) ->
    "a.flm/m";
abbrev(?UMB_CNL_R2S_FILM_SINCE, _) ->
    "flm/m";
abbrev(?UMB_CNL_R2S_DRIZZLES, _) ->
    "d/s";
abbrev(?UMB_CNL_R2S_RDROPS, _) ->
    "r/s";
abbrev(?UMB_CNL_R2S_SFLAKES, _) ->
    "s/s";
abbrev(?UMB_CNL_R2S_HAILS, _) ->
    "h/s";
abbrev(?UMB_CNL_R2S_DRIZZLE_F, _) ->
    "d";
abbrev(?UMB_CNL_R2S_RAIN_F, _) ->
    "r";
abbrev(?UMB_CNL_R2S_SNOW_F, _) ->
    "s";
abbrev(?UMB_CNL_R2S_HAIL_F, _) ->
    "h";
abbrev(?UMB_CNL_R2S_PREC_INT, _) ->
    "p.ints";
abbrev(?UMB_CNL_R2S_PREC_TYPE, _) ->
    "prec.type";
abbrev(_Num, Name) ->
    Name.

known_channels() ->
    [{Num, [{channel, Num}, {name, abbrev(Num, none)}]} ||
        Num <- ?DEFAULT_CHANNELS].

format_item(P) when is_atom(P) ->
    io_lib:format("~s", [P]);
format_item(I) when is_integer(I) ->
    io_lib:format("~B", [I]);
format_item(C) when is_float(C) ->
    io_lib:format("~.2f", [C]).

%% @doc Returns list of console options.
get_opts_spec() ->
    %% {Name,      ShortOpt,  LongOpt,   ArgSpec,   HelpMsg}
    [
        {help,      $h,        "help",     undefined, "Show this help"},
        {list_chan, $c,        "channels", undefined, "List channels from a device"},
        {timeout,   $t,        undefined,  integer,   "Timeout to network operations (ms)"},
        {wait,      $w,        undefined,  integer,   "Wait between requests (ms)"},
        {noheader,  $n,        undefined,  undefined, "Do not display columns header"}
    ].
