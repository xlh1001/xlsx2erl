%%%----------------------------------------------
%%% @author VG_xiao
%%% @doc
%%% 
%%% @end
%%% Created 2023-08-18 11:56:20
%%%----------------------------------------------
-module(xlsx2erl_worker).

-behaviour(gen_server).

-include("xlsx2erl.hrl").

%% API
-export([start_link/2, do_oprate/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%%%===================================================================
%%% API
%%%===================================================================

start_link(Name, Args) ->
    gen_server:start_link({local, Name}, ?MODULE, Args, []). 

cancel_export(SheetId) ->
    gen_server:cast(?MODULE, {cancel_export, SheetId}).
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
    ModMap = maps:put(callback_mod, Mods, HeaderMap),
    {ok, maps:put(export_sheets, [], ModMap)}.

do_handle_call(_Request, _From, State) ->
    info("======  unhandle call :~p~n",[_Request]),
    {reply, ok, State}.

do_handle_cast({cancel_export, SheetId}, State = #{export_sheets := SheetIds}) ->
    NewSheetIds = lists:delete(SheetId, SheetIds),
    {noreply, maps:put(export_sheets, NewSheetIds, State)};

do_handle_cast(Request, State) ->
    info("======  unhandle cast :~p~n",[Request]),
    {noreply, State}.

do_handle_info({update_path, Key, Value}, State) ->
    {noreply, maps:put(Key, Value, State)};

do_handle_info(stop, State) ->
    {stop, normal, State};

do_handle_info({export, all}, State) ->
    EtsSheetList = xlsx2erl_loader:get_sheet_list(),
    SheetIds = maps:get(export_sheets, State, []),
    NewSheetIds = do_work(EtsSheetList, State, SheetIds),
    {noreply, maps:put(export_sheets, NewSheetIds, State)};

do_handle_info({export, ExcelIdList}, State) ->
    EtsSheetList = xlsx2erl_loader:get_sheet_list(ExcelIdList),
    SheetIds = maps:get(export_sheets, State, []),
    NewSheetIds = do_work(EtsSheetList, State, SheetIds),
    {noreply, maps:put(export_sheets, NewSheetIds, State)};

do_handle_info(Request, State) ->
    info("======  unhandle info :~p~n",[Request]),
    {noreply, State}.

do_terminate(Reason, _State) ->
    info("====== Module:~p stop reason:~p~n",[?MODULE, Reason]),
    ok.

do_work([], _State, SheetIds) -> SheetIds;
do_work([#ets_sheet{id = SheetId, sheet = Sheet} | T], State, SheetIds) ->
    spawn(fun() -> 
        ExcelName = xlsx2erl_loader:get_excel_name(SheetId),
        SheetName = Sheet#excel_sheet.name,
        try
            Time1 = erlang:system_time(millisecond),
            #{callback_mod := Mods} = State,
            write_to_file(Mods, maps:put(excel_name, ExcelName, State), [Sheet]),
            Time2 = erlang:system_time(millisecond),
            info("~ts配置中的子表【~s】导出成功，耗时【~p】ms~n",[ExcelName, SheetName, Time2 - Time1])
        catch
            throw : Reason : _StackTrace ->
                handle_export_res(ExcelName, SheetName, Reason);
            _Type : Reason : _StackTrace ->
                info("~ts配置中的子表【~s】导出失败 :~p StackTrace:~p~n",[ExcelName, SheetName, Reason, _StackTrace])
        after
            cancel_export(SheetId)
        end
    end),
    do_work(T, State, [SheetId | lists:delete(SheetId, SheetIds)]).

write_to_file([], _State, _NewList) -> ok;
write_to_file([{_ExportKey, _FunKey, CallBacMod, _PathKeyList}|T], State, NewList) ->
    ok = CallBacMod:write_to_file(State, NewList),
    write_to_file(T, State, NewList).

info(Str, Args) ->
    xlsx2erl:info(Str, Args).

handle_export_res(ExcelName, SheetName, Res) ->
    case Res of
        {false, Mod, miss_path} ->
            info("~ts配置中的子表【~s】导出失败：路径配置缺失 ! Mod:~p~n",[ExcelName, SheetName, Mod]);
        {false, atom, filed_value_err} ->
            info("~ts配置中的子表【~s】导出失败：字段类型错误 ! atom 只能是小写字符开头，由小写字母以及数字组成 ~n",[ExcelName, SheetName]);
        {false, Key, filed_value_err} ->
            info("~ts配置中的子表【~s】导出失败：字段类型错误 ! 类型：~p ~n",[ExcelName, SheetName, Key]);
        {false, Key, key_filed_lost} ->
            info("~ts配置中的子表【~s】导出失败：关键字段~p缺失! ~n",[ExcelName, SheetName, Key]);
        {false, Row, lost_key_cloumn} ->
            info("~ts配置中的子表【~s】导出失败：第~p行主键缺失! ~n",[ExcelName, SheetName, Row]);
        {false, Arity, fun_args_not_exist} ->
            info("~ts配置中的子表【~s】导出失败：导出函数中所需字段~s不存在! ~n",[ExcelName, SheetName, Arity]);
        {false, {Name, FunName}, fun_miss_args} ->
            info("~ts配置中的子表【~s】导出失败：导出函数~s中所需字段~s不存在! ~n",[ExcelName, SheetName, FunName, Name]);
        _Err ->
            info("~ts配置中的子表【~s】导出失败：导出失败 reason :~p ~n",[ExcelName, SheetName, _Err])
    end.

do_oprate(Sheet, ExcelName) ->
    HeaderList = get_header(config_header),
    Mods = get_header(config_callback),
    case xlsx2erl_oprator:oprate([Sheet], HeaderList, Mods) of
        {ok, [NewSheet]} ->
            NewSheet;
        Res ->
            SheetName = Sheet#excel_sheet.name,
            handle_export_res(ExcelName, SheetName, Res),
            false
    end.

get_header(Fun) ->
    case get(Fun) of
        undefined ->
            Value = xlsx2erl_tool:Fun(),
            put(Fun, Value),
            Value;
        Value -> ok
    end,
    Value.