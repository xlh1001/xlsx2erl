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
-export([start_link/1, start_link/2, do_oprate/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([
    do_init/1,
    do_handle_call/3,
    do_handle_cast/2,
    do_handle_info/2,
    do_terminate/2
]).

-define(CATCH_ERROR, 
    try
        erlang:apply(?MODULE, Fun, Args)
    catch
        Type:Reason:StackTrace ->
            format_error(Fun, Request, Type, Reason, StackTrace),
            do_catch_error(Fun, Reason, State)
    end).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []). 

start_link(Name, Args) ->
    gen_server:start_link({local, Name}, ?MODULE, Args, []). 

cancel_export(SheetId) ->
    gen_server:cast(?MODULE, {cancel_export, SheetId}).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================


%% @private
%% @doc Initializes the server
init(TmpArgs) ->
    catch_error(do_init, [TmpArgs]).

%% @private
%% @doc Handling call messages
handle_call(Request, From, State) ->
    catch_error(do_handle_call, [Request, From, State]).

%% @private
%% @doc Handling cast messages
handle_cast(Request, State) ->
    catch_error(do_handle_cast, [Request, State]).

%% @private
%% @doc Handling all non call/cast messages
handle_info(Request, State) ->
    catch_error(do_handle_info, [Request, State]).

%% @private
terminate(Reason, State) ->
    catch_error(do_terminate, [Reason, State]).

catch_error(Fun, Args = [_]) ->
    Request = null, State = null,
    ?CATCH_ERROR;
catch_error(Fun, Args = [Request, State]) ->
    ?CATCH_ERROR;
catch_error(Fun, Args = [Request, _From, State]) ->
    ?CATCH_ERROR.

do_catch_error(do_terminate, _Reason, _State) -> 
    ok;
do_catch_error(do_handle_call, _Reason, State) -> 
    {reply, error, State};
do_catch_error(do_init, Reason, _State) -> 
    {stop, Reason};
do_catch_error(_, _Reason, State) -> 
    {noreply, State}.

format_error(do_terminate, _, Type, Reason, StackTrace) ->
    info("~w error type:~w, reason:~w ~n, stacktrace: ~w~n", 
        [do_terminate, Type, Reason, StackTrace]),
    ok;
format_error(Fun, Request, Type, Reason, StackTrace) ->
    info("~w error type:~w, reason:~w Request:~w~n, stacktrace: ~w~n", 
        [Fun, Type, Reason, Request, StackTrace]),
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


do_init([ExcelPath, PathMap]) ->
    HeaderList = xlsx2erl_tool:config_header(),
    Mods = xlsx2erl_tool:config_callback(),
    Map = #{
        excel_path => ExcelPath,
        header_def => HeaderList,
        callback_mod => Mods,
        export_sheets => []
    },
    State = maps:merge(PathMap, Map),
    {ok, State}.

do_handle_call(_Request, _From, State) ->
    info("======  unhandle call :~p~n",[_Request]),
    {reply, ok, State}.

do_handle_cast({cancel_export, SheetId}, State) ->
    SheetIds = maps:get(export_sheets, State, []),
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
    lists:member(SheetId, SheetIds) == false andalso 
        spawn(fun() -> 
            do_work_core(SheetId, Sheet, State)
        end),
    do_work(T, State, [SheetId | lists:delete(SheetId, SheetIds)]).

do_work_core(SheetId, Sheet, State) ->
    ExcelName = xlsx2erl_loader:get_excel_name(SheetId),
    #{callback_mod := Mods} = State,
    ok = write_to_file(Mods, maps:put(excel_name, ExcelName, State), Sheet, 0),
    cancel_export(SheetId).

write_to_file([], _State, _Sheet, 0) -> ok;
write_to_file([], State, Sheet, Num) -> 
    receive
        do_exported ->
            write_to_file([], State, Sheet, Num - 1)
    end;
write_to_file([Record | T], State, Sheet, Num) ->
    Parent = self(),
    spawn(fun() -> callback_do_work(Parent, Record, State, Sheet) end),
    write_to_file(T, State, Sheet, Num + 1).

callback_do_work(Parent, Record, State, Sheet) ->
    #{excel_name := ExcelName} = State,
    SheetName = Sheet#excel_sheet.name,
    try
        Time1 = erlang:system_time(millisecond),
        CallBackMod = xlsx2erl_tool:config_callback_mod(Record),
        CallBackMod:check_export_path(State, Record),
        CallBackMod:write_erl(State, Record, Sheet),
        Time2 = erlang:system_time(millisecond),
        Success = {true, CallBackMod, Time2 - Time1},
        handle_export_res(Success, ExcelName, SheetName)
    catch
        throw : Reason : _StackTrace ->
            handle_export_res(Reason, ExcelName, SheetName);
        _Type : Reason : StackTrace ->
            Res = {false, {Reason, StackTrace}, server_error},
            handle_export_res(Res, ExcelName, SheetName)
    after
        Parent ! do_exported
    end.

info(Str, Args) ->
    xlsx2erl:info(Str, Args).

do_oprate(Sheet, ExcelName) ->
    HeaderList = get_header(config_header),
    Mods = get_header(config_callback),
    OprateRes = xlsx2erl_oprator:oprate([Sheet], HeaderList, Mods),
    do_oprate(OprateRes, Sheet#excel_sheet.name, ExcelName).

do_oprate({ok, [NewSheet]}, _SheetName, _ExcelName) -> 
    NewSheet;
do_oprate(OprateRes, SheetName, ExcelName) -> 
    handle_export_res(OprateRes, ExcelName, SheetName).

get_header(Fun) ->
    get_header_helper(get(Fun), Fun).

get_header_helper(undefined, Fun) ->
    Value = xlsx2erl_tool:Fun(),
    put(Fun, Value),
    Value;
get_header_helper(Value, _) ->
    Value.


handle_export_res({false, atom, filed_value_err}, ExcelName, SheetName) ->
    info("~ts配置中的子表【~s】字段类型错误 ! atom 由小写字母开头，数字、下划线、小写字母组成 ~n",[ExcelName, SheetName]);
handle_export_res({false, Key, filed_value_err}, ExcelName, SheetName) ->
    info("~ts配置中的子表【~s】字段类型错误 ! 类型：~p ~n",[ExcelName, SheetName, Key]);
handle_export_res({false, Key, key_filed_lost}, ExcelName, SheetName) ->
    info("~ts配置中的子表【~s】关键字段~p缺失! ~n",[ExcelName, SheetName, Key]);
handle_export_res({false, Row, lost_key_cloumn}, ExcelName, SheetName) ->
    info("~ts配置中的子表【~s】第~p行主键缺失! ~n",[ExcelName, SheetName, Row]);
handle_export_res({false, Arity, fun_args_not_exist}, ExcelName, SheetName) ->
    info("~ts配置中的子表【~s】导出函数中所需字段~s不存在! ~n",[ExcelName, SheetName, Arity]);
handle_export_res({false, Mod, miss_path}, ExcelName, SheetName) ->
    info("~ts配置中的子表【~s】导出失败：路径配置缺失 ! Mod:~p~n",[ExcelName, SheetName, Mod]);
handle_export_res({false, {CallBackMod, Name, FunName}, fun_miss_args}, ExcelName, SheetName) ->
    info("~ts配置中的子表【~s】，~p导出失败：导出函数~s中所需字段~s不存在! ~n",[ExcelName, SheetName, CallBackMod, FunName, Name]);
handle_export_res({false, {Reason, StackTrace}, server_error}, ExcelName, SheetName) ->
    info("~ts配置中的子表【~s】导出失败：~p~n StackTrace:~w~n",[ExcelName, SheetName, Reason, StackTrace]);
handle_export_res({true, CallBackMod, Time}, ExcelName, SheetName) ->
    info("~ts配置中的子表【~s】，~p执行成功，耗时【~p】ms~n",[ExcelName, SheetName, CallBackMod, Time]);
handle_export_res(_Err, ExcelName, SheetName) ->
    info("~ts配置中的子表【~s】错误 reason :~w ~n",[ExcelName, SheetName, _Err]).

