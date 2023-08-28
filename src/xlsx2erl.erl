%%%----------------------------------------------
%%% @author VG_xiao
%%% @doc
%%% 
%%% @end
%%% Created 2023-08-04 14:32:52
%%%----------------------------------------------

-module(xlsx2erl).

-behaviour(gen_server).

-include("xlsx2erl.hrl").
-include_lib("wx/src/wxe.hrl").
%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([success/2, fail/2, update_user_data/2, info/2]).



-define(DETS_XLSX2ERL, dets_xlsx2erl).
-define(WORKER_FREE, 1). %% 空闲
-define(WORKER_BUSY, 2). %% 繁忙

-record(state, {
    worker = []
    ,undo_task = []
    
    ,excel = []
    ,excel_path = ""
    
    ,wx_frame = undefined
    
    ,path_map = #{}
    
    ,tags = []
    ,excel_tags = #{} 
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []). 

success(WorkerPid, FileName) ->
    gen_server:cast(?MODULE, {xlsx2erl_success, WorkerPid, FileName}).

fail(WorkerPid, ExcelName) ->
    gen_server:cast(?MODULE, {xlsx2erl_fail, WorkerPid, ExcelName}).

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
    try
        do_terminate(Reason, State)
    catch
        Type : Reason:StackTrace ->
            io:format("terminate: ~w, error: ~w, reason: ~w, stacktrace: ~w", [Reason, Type, Reason, StackTrace]),
            ok
    end.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

do_init([]) ->
    load_user_data("."),
    process_flag(trap_exit, true),

    PathKeyList = get_all_path_key(xlsx2erl_tool:config_callback(), []),
    State = init_state(PathKeyList),
    WorkerNum = user_data(worker_num, 15),
    WorkerList = start_worker(0, WorkerNum, [State#state.excel_path, State#state.path_map], []),

    WxFramPid = start_wx(State, WorkerNum),

    NewState = State#state{worker = WorkerList, wx_frame = WxFramPid},
    {ok, NewState}.


do_handle_call(_Request, _From, State) ->
    info("======  unhandle call :~p~n",[_Request]),
    {reply, ok, State}.


do_handle_cast({xlsx2erl_success, WorkerPid, ExcelName}, State) ->
    {noreply, handle_work_res(State, WorkerPid, ExcelName, success)};

do_handle_cast({xlsx2erl_fail, WorkerPid, ExcelName}, State) ->
    {noreply, handle_work_res(State, WorkerPid, ExcelName, fail)};

do_handle_cast(Request, State) ->
    info("======  unhandle cast :~p~n",[Request]),
    {noreply, State}.

do_handle_info({export, all}, State = #state{excel = ExcelList, worker = WorkerList}) ->
    {NewWorkerList, UndoExcelList} = lists:foldl(
        fun
            ({Worker, ?WORKER_FREE}, {AccWorkerL, AccExcelList}) ->
                [{_, ExcelName}|NewAccExcelL] = AccExcelList,
                xlsx2erl_worker:work(Worker, ExcelName),
                NewWorkerList = lists:keystore(Worker, 1, AccWorkerL, {Worker, ?WORKER_BUSY}),
                {NewWorkerList, NewAccExcelL};
            (_, Acc) -> Acc
        end, {WorkerList, ExcelList}, WorkerList),
    {noreply, State#state{worker = NewWorkerList, undo_task = [TmpExcelName || {_Id, TmpExcelName} <- UndoExcelList]}};

do_handle_info({export, ExcelNameList}, State = #state{worker = WorkerList, undo_task = UndoTask}) ->
    {ExcelNameList0, NewWorkerList} = divide_worker(ExcelNameList, WorkerList, []),
    UndoTask0 = UndoTask -- ExcelNameList0,
    NewUndoTask = ExcelNameList0 ++ UndoTask0,
    {noreply, State#state{worker = NewWorkerList, undo_task = NewUndoTask}};


do_handle_info({update_path, Key, Path}, State) ->
    #state{worker = WorkerList, path_map = PathMap} = State,
    notify_worker(WorkerList, Key, Path),
    update_user_data(Key, Path),
    {noreply, State#state{path_map = maps:put(Key, Path, PathMap)}};

do_handle_info({update_user_data, excel_path, NewValue}, State) ->
    #state{worker = WorkerList, wx_frame = WxFramPid} = State,
    {_, ExcelList} = xlsx2erl_tool:make_excel(NewValue),
    notify_worker(WorkerList, excel_path, NewValue),
    notify_wxFrame(WxFramPid, {update_excel, ExcelList}),
    update_user_data(excel_path, NewValue),
    {noreply, State#state{excel_path = NewValue, excel = ExcelList}};
do_handle_info({update_user_data, worker_num, NewValue}, State) ->
    #state{excel_path = ExcelPath, worker = WorkerList, path_map = PathMap} = State,
    Len = erlang:length(WorkerList),
    NewLen = list_to_integer(NewValue),
    NewWorkerList = 
        if
            NewLen > Len ->
                AddWorkerList = start_worker(Len, NewLen, [ExcelPath, PathMap], []),
                AddWorkerList ++ WorkerList;
            true ->
                {NewList, DelList} = lists:split(NewLen, WorkerList),
                [Worker ! stop || {Worker, _} <- DelList],
                NewList
        end,
    update_user_data(worker_num, NewLen),
    {noreply, State#state{worker = NewWorkerList}};

do_handle_info({tags, AddTag, ExcelName}, State) ->
    #state{tags = Tags, excel_tags = TagMap} = State,
    NewTags = [AddTag|lists:delete(AddTag, Tags)],
    ExcelList = maps:get(AddTag, TagMap, []),
    NewMap = maps:put(AddTag, [ExcelName|lists:delete(ExcelName, ExcelList)], TagMap),
    update_user_data(tags, NewTags),
    update_user_data(excel_tags, NewMap),
    {noreply, State#state{tags = NewTags, excel_tags = NewMap}};

do_handle_info({del_self_tags, ExcelName}, State) ->
    #state{excel_tags = TagMap} = State,
    NewTagMap = maps:map(fun(_, ExcelList) ->
        lists:delete(ExcelName, ExcelList)
    end, TagMap),
    update_user_data(excel_tags, NewTagMap),
    {noreply, State#state{excel_tags = NewTagMap}};

do_handle_info({del_tag, Tag}, State) ->
    #state{tags = Tags, excel_tags = TagMap} = State,
    NewTags = lists:delete(Tag, Tags),
    NewMap = maps:remove(Tag, TagMap),
    update_user_data(tags, NewTags),
    update_user_data(excel_tags, NewMap),
    {noreply, State#state{tags = NewTags, excel_tags = NewMap}};

do_handle_info(stop, State = #state{worker = WorkerList}) ->
    notify_worker(WorkerList, stop),
    {stop, normal, State};

do_handle_info(Request, State) ->
    info("======  unhandle_info :~p~n",[Request]),
    {noreply, State}.

do_terminate(_Reason, _State) ->
    dets:close(?DETS_XLSX2ERL),
    ok.

do_work(WorkerPid, WorkerList, [ExcelName | T]) ->
    xlsx2erl_worker:work(WorkerPid, ExcelName),
    NewWorkerList = lists:keystore(WorkerPid, 1, WorkerList, {WorkerPid, ?WORKER_BUSY}),
    {NewWorkerList, T}.

handle_work_res(State = #state{undo_task = []}, WorkerPid, ExcelName, Flag) ->
    #state{worker = WorkerList, wx_frame = WxFramPid} = State,
    notify_wxFrame(WxFramPid, {export_res, ExcelName, Flag}),
    NewWorkerList = lists:keystore(WorkerPid, 1, WorkerList, {WorkerPid, ?WORKER_FREE}),
    State#state{worker = NewWorkerList};
handle_work_res(State, WorkerPid, ExcelName, Flag) ->
    #state{worker = WorkerList, undo_task = UndoTask, wx_frame = WxFramPid} = State,
    notify_wxFrame(WxFramPid, {export_res, ExcelName, Flag}),
    {NewWorkerList, NewUndoTask} = do_work(WorkerPid, WorkerList, UndoTask),
    State#state{worker = NewWorkerList, undo_task = NewUndoTask}.

load_user_data(Path) ->
    File = lists:concat([Path, "/xlsx2erl.dets"]),
    dets:open_file(?DETS_XLSX2ERL, [{file, File}, {keypos, #xlsx2erl_user_data.key}, {ram_file, true}, {type, set}]),
    ok.

update_user_data(Key, Value) ->
    dets:insert(?DETS_XLSX2ERL, #xlsx2erl_user_data{key = Key, value = Value}).

%% return: List
user_data(Key, Default) ->
    case dets:lookup(?DETS_XLSX2ERL, Key) of
        [] -> Default;
        [#xlsx2erl_user_data{value = Value}] -> Value
    end.

get_all_path_key([], Acc) -> Acc;
get_all_path_key([{_ExportKey, _, _CallBacMod, PathKeyList}|T], Acc) ->
    get_all_path_key(T, PathKeyList ++ Acc).

init_state(PathKeyList) ->
    ExcelPath = user_data(excel_path, ""),
    {_, ExcelList} = xlsx2erl_tool:make_excel(ExcelPath),
    PathList = [{PathKey, user_data(PathKey, "")} || PathKey <- PathKeyList],
    
    Tags = user_data(tags, []),
    TagMap = user_data(excel_tags, #{}),
    #state{
        excel = ExcelList
        ,excel_path = user_data(excel_path, "")
        ,path_map = maps:from_list(PathList)
        ,tags = Tags
        ,excel_tags = TagMap
    }.

start_wx(State, WorkerNum) ->
    #state{
        excel_path = ExcelPath
        ,path_map = PathMap
        ,tags = Tags
        ,excel_tags = TagMap
    } = State,
    WxOption = [
        {excel_path, ExcelPath}
        , {tags, Tags}
        , {path_map, PathMap}
        , {worker_num, WorkerNum}
        , {tag_map, TagMap}
        , {server, self()}
    ],
    #wx_ref{state = WxFramPid} = xlsx2erl_wx:start_link(WxOption),
    WxFramPid.

start_worker(Start, Start, _Args, Acc) -> Acc;
start_worker(Start, WorkerNum, Args, Acc) ->
    Name = erlang:list_to_atom(lists:concat(["xlsx2erl_worker_", WorkerNum])),
    {ok, Pid} = xlsx2erl_worker:start_link(Name, Args),
    start_worker(Start, WorkerNum - 1, Args, [{Pid, ?WORKER_FREE} | Acc]).

divide_worker(ExcelNames, [], Acc) -> {ExcelNames, Acc};
divide_worker([ExcelName | T], [{Worker, ?WORKER_FREE} | WorkerList], Acc) ->
    xlsx2erl_worker:work(Worker, ExcelName),
    divide_worker(T, WorkerList, [{Worker, ?WORKER_BUSY} | Acc]);
divide_worker(ExcelNames, [H | WorkerList], Acc) ->
    divide_worker(ExcelNames, WorkerList, [H | Acc]).

info(Str, Args) ->
    notify_wxFrame(whereis(xlsx2erl_wx), {info, io_lib:format(Str, Args)}).

notify_wxFrame(WxFramPid, Msg) ->
    WxFramPid ! Msg.

notify_worker(WorkerList, Msg) ->
    [Worker ! Msg || {Worker, _} <- WorkerList].

notify_worker(WorkerList, Key, NewValue) ->
    [Worker ! {update_path, Key, NewValue} || {Worker, _} <- WorkerList].