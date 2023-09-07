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

-export([update_user_data/2, info/2, notify_wxFrame/1]).



-define(DETS_XLSX2ERL, dets_xlsx2erl).
-define(WORKER_FREE, 1). %% 空闲
-define(WORKER_BUSY, 2). %% 繁忙

-record(state, {
    worker = undefined
    
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
    WxFramPid = start_wx(State),
    {ok, Pid} = xlsx2erl_worker:start_link(xlsx2erl_worker, [State#state.excel_path, State#state.path_map]),
    xlsx2erl_loader:start_link(State#state.excel_path),
    
    NewState = State#state{worker = Pid, wx_frame = WxFramPid},
    {ok, NewState}.


do_handle_call(_Request, _From, State) ->
    info("======  unhandle call :~p~n",[_Request]),
    {reply, ok, State}.



do_handle_cast(Request, State) ->
    info("======  unhandle cast :~p~n",[Request]),
    {noreply, State}.

do_handle_info({export, Args}, State = #state{worker = WorkerPid}) ->
    notify_worker(WorkerPid, {export, Args}),
    {noreply, State};


do_handle_info({update_path, Key, Path}, State) ->
    #state{worker = WorkerPid, path_map = PathMap} = State,
    notify_worker(WorkerPid, {update_path, Key, Path}),
    update_user_data(Key, Path),
    {noreply, State#state{path_map = maps:put(Key, Path, PathMap)}};

do_handle_info({update_user_data, excel_path, NewValue}, State = #state{worker = WorkerPid}) ->
    notify_worker(WorkerPid, {update_path, excel_path, NewValue}),
    xlsx2erl_loader:change_excel_path(NewValue),
    update_user_data(excel_path, NewValue),
    {noreply, State#state{excel_path = NewValue}};

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

do_handle_info(stop, State = #state{worker = WorkerPid}) ->
    notify_worker(WorkerPid, stop),
    {stop, normal, State};

do_handle_info(Request, State) ->
    info("======  unhandle_info :~p~n",[Request]),
    {noreply, State}.

do_terminate(_Reason, _State) ->
    dets:close(?DETS_XLSX2ERL),
    ok.

% do_work(WorkerPid, WorkerList, [ExcelName | T]) ->
%     xlsx2erl_worker:work(WorkerPid, ExcelName),

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
    PathList = [{PathKey, user_data(PathKey, "")} || PathKey <- PathKeyList],
    
    Tags = user_data(tags, []),
    TagMap = user_data(excel_tags, #{}),
    #state{
        excel_path = ExcelPath
        ,path_map = maps:from_list(PathList)
        ,tags = Tags
        ,excel_tags = TagMap
    }.

start_wx(State) ->
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
        , {tag_map, TagMap}
        , {server, self()}
    ],
    #wx_ref{state = WxFramPid} = xlsx2erl_wx:start_link(WxOption),
    WxFramPid.


info(Str, Args) ->
    notify_wxFrame(whereis(xlsx2erl_wx), {info, io_lib:format(Str, Args)}).

notify_wxFrame(Msg) ->
    notify_wxFrame(whereis(xlsx2erl_wx), Msg).
notify_wxFrame(WxFramPid, Msg) ->
    WxFramPid ! Msg.

notify_worker(Worker, Msg) ->
    Worker ! Msg.
