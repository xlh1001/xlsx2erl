%%%----------------------------------------------
%%% @author VG_xiao
%%% @doc
%%% 
%%% @end
%%% Created 2023-08-18 11:56:20
%%%----------------------------------------------
-module(xlsx2erl_worker).

-behaviour(gen_server).

%% API
-export([start_link/2, work/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%%%===================================================================
%%% API
%%%===================================================================

start_link(Name, Args) ->
    gen_server:start_link({local, Name}, ?MODULE, Args, []). 

work(Worker, ExcelName) ->
    gen_server:cast(Worker, {work, ExcelName}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
init(Args) ->
    try
        do_init(Args)
    catch
        Type : Reason : StackTrace ->
            io:format("init: ~w, error: ~w, reason: ~w, stacktrace: ~p~n", [Args, Type, Reason, StackTrace]),
            {stop, Reason}
    end.

%% @private
%% @doc Handling call messages
handle_call(Request, From, State) ->
    try
        do_handle_call(Request, From, State)
    catch
        Type:Reason:StackTrace ->
            io:format("handle_call:~p, error:~p, reason:~p, stacktrace:~p", [Request, Type, Reason, StackTrace]),
            {reply, {false, 1}, State}
    end.

%% @private
%% @doc Handling cast messages
handle_cast(Request, State) ->
    try
        do_handle_cast(Request, State)
    catch
        Type:Reason:StackTrace ->
            io:format("handle_cast:~p, error:~p, reason:~p, stacktrace:~p~n", [Request, Type, Reason, StackTrace]),
            {noreply, State}
    end.


%% @private
%% @doc Handling all non call/cast messages
handle_info(Info, State) ->
    try
        do_handle_info(Info, State)
    catch
        Type:Reason:StackTrace ->
            io:format("handle_info:~p, error:~p, reason:~p, stacktrace:~p~n", [Info, Type, Reason, StackTrace]),
            {noreply, State}
    end.

%% @private
terminate(Reason, State) ->
    do_terminate(Reason, State).

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


do_init([ExcelPath, PathMap]) ->
    HeaderList = xlsx2erl_tool:config_header(),
    Mods = xlsx2erl_tool:config_callback(),
    ExcelMap = maps:put(excel_path, ExcelPath, PathMap),
    HeaderMap = maps:put(header_def, lists:keysort(1, HeaderList), ExcelMap),
    {ok, maps:put(callback_mod, Mods, HeaderMap)}.

do_handle_call(_Request, _From, State) ->
    info("======  unhandle call :~p~n",[_Request]),
    {reply, ok, State}.


do_handle_cast({work, ExcelName}, #{excel_path := ExcelPath} = State) ->
    FileName = lists:concat([ExcelPath, "/", ExcelName]),
    case catch xlsx2erl_one(State, FileName) of
        ok ->
            xlsx2erl:success(self(), ExcelName);
        {false, Mod, miss_path} ->
            info("lose path ! Mod:~p~n",[Mod]),
            xlsx2erl:fail(self(), ExcelName);
        {false, atom, filed_value_err} ->
            info("字段类型错误 ! atom 只能是小写字符开头，由小写字母以及数字组成 ~n",[]),
            xlsx2erl:fail(self(), ExcelName);
        {false, Key, filed_value_err} ->
            info("字段类型错误 ! 类型：~p ~n",[Key]),
            xlsx2erl:fail(self(), ExcelName);
        {false, Key, key_filed_lost} ->
            info("关键字段~p缺失! ~n",[Key]),
            xlsx2erl:fail(self(), ExcelName);
        {false, Row, lost_key_cloumn} ->
            info("第~p行主键缺失! ~n",[Row]),
            xlsx2erl:fail(self(), ExcelName);
        {false, Arity, fun_args_not_exist} ->
            info("导出函数中所需字段~s不存在! ~n",[Arity]),
            xlsx2erl:fail(self(), ExcelName);
        {false, {Name, FunName}, fun_miss_args} ->
            info("导出函数~s中所需字段~s不存在! ~n",[FunName, Name]),
            xlsx2erl:fail(self(), ExcelName);
        _Err ->
            info("导出失败 reason :~p ~n",[_Err]),
            xlsx2erl:fail(self(), ExcelName)
    end,
    {noreply, State};

do_handle_cast(Request, State) ->
    info("======  unhandle cast :~p~n",[Request]),
    {noreply, State}.

do_handle_info({update_path, Key, Value}, State) ->
    {noreply, maps:put(Key, Value, State)};

do_handle_info(stop, State) ->
    {stop, normal, State};

do_handle_info(Request, State) ->
    info("======  unhandle info :~p~n",[Request]),
    {noreply, State}.

do_terminate(Reason, _State) ->
    info("====== Module:~p stop reason:~p~n",[?MODULE, Reason]),
    ok.


xlsx2erl_one(State = #{header_def := HeaderList, callback_mod := Mods}, File) ->
    SheetList = xlsx2erl_read:read_xlsx(File),
    case xlsx2erl_oprator:oprate(SheetList, HeaderList, Mods) of
        {ok, NewList} ->
            write_to_file(Mods, State, File, NewList);
        {false, Key, Code} ->
            {false, Key, Code}
    end.

write_to_file([], _State, _File, _NewList) -> ok;
write_to_file([{_ExportKey, _FunKey, CallBacMod, _PathKeyList}|T], State, File, NewList) ->
    ok = CallBacMod:write_to_file(State, File, NewList),
    write_to_file(T, State, File, NewList).

info(Str, Args) ->
    xlsx2erl:info(Str, Args).