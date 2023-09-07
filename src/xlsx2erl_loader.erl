%%%----------------------------------------------
%%% @author VG_xiao
%%% @doc
%%% 
%%% @end
%%% Created 2023-09-04 19:46:13
%%%----------------------------------------------
-module(xlsx2erl_loader).

-behaviour(gen_server).

-include_lib("kernel/include/file.hrl").
-include("xlsx2erl.hrl").

%% API
-export([start_link/1]).

-export([
	refresh/0
	,change_excel_path/1
	,get_excel_list/0
	,get_sheet_list/0
	,get_sheet_list/1
	,get_excel_name/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
	max_id = 1
	, excel_path = ""
	, load_time = 0
	, reload_timer = 0
	, excel_list = []
	, reload_flag = 0
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(ExcelPath) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [ExcelPath], []). 

change_excel_path(Path) ->
	gen_server:cast(?MODULE, {change_excel_path, Path}). 

refresh() ->
	gen_server:cast(?MODULE, refresh). 

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

do_init([ExcelPath]) ->
	ets:new(?ETS_EXCEL_FILE, [named_table, public, set, {keypos, #ets_excel.id}, {read_concurrency, true}]),
	ets:new(?ETS_SHEET_INFO, [named_table, public, set, {keypos, #ets_sheet.id}, {read_concurrency, true}]),

	Timer = erlang:send_after(500, self(), reload),
	{ok, #state{excel_path = ExcelPath, reload_timer = Timer, reload_flag = init}}.



do_handle_call(Request, _From, State) ->
	io:format("do_handle_call unmatch : ~w", [Request]),
	{reply, ok, State}.

do_handle_cast({change_excel_path, Path}, State) ->
	#state{reload_timer = OldTimer} = State,
	xlsx2erl_tool:cancel_timer(OldTimer),
	Timer = erlang:send_after(100, self(), reload),
	NewState = State#state{
		excel_path = Path, 
		max_id = 1, 
		excel_list = [], 
		reload_timer = Timer
	},
	{noreply, NewState};

do_handle_cast(refresh, State) ->
	#state{reload_timer = OldTimer} = State,
	xlsx2erl_tool:cancel_timer(OldTimer),
	Timer = erlang:send_after(50, self(), reload),
	{noreply, State#state{load_time = 0, reload_timer = Timer, reload_flag = refresh}};

do_handle_cast(Request, State) ->
	io:format("do_handle_cast unmatch : ~w", [Request]),
	{noreply, State}.

do_handle_info(reload, State) ->
	#state{
		excel_path = ExcelPath 
		,load_time = LoadTime
		,max_id = MaxId
		,reload_timer = Timer
		,excel_list = OldExcelList
		,reload_flag = Flag
	} = State,
	xlsx2erl_tool:cancel_timer(Timer),

	{_, ExcelList} = xlsx2erl_tool:make_excel(ExcelPath),
	ExcelNameList = [ExcelName || {_, ExcelName} <- ExcelList],
	OldExcelNameList = [ExcelName || {_, ExcelName} <- OldExcelList],
	Flag =/= normal andalso xlsx2erl:info("所有excel配置表格式检查开始~n", []),
	MaxSheetNum = xlsx2erl_tool:config_sheet_num(),
	%% 处理excel新增或者变更
	{NewMaxId, NewExcelList} = do_reload(ExcelList, MaxId, MaxSheetNum, LoadTime, ExcelPath, OldExcelList),
	%% 删除excel
	DelList = OldExcelNameList -- ExcelNameList,
	[ets:delete(?ETS_EXCEL_FILE, Id) || {Id, Name} <- OldExcelList, lists:member(Name, DelList)],
	(DelList =/= [] orelse NewMaxId =/= MaxId) andalso xlsx2erl:notify_wxFrame(update_excel),
	Flag =/= normal andalso xlsx2erl:info("所有excel配置表格式检查完成~n", []),
	RefreshTime = xlsx2erl_tool:config_refresh_time(),
	NewTimer = erlang:send_after(RefreshTime * 1000, self(), reload),
	RealExcelList = [{Id, Name} || {Id, Name} <- NewExcelList, lists:member(Name, DelList) == false],
	NewState = State#state{
		max_id = NewMaxId, 
		reload_timer = NewTimer, 
		load_time = erlang:localtime()
		,excel_list = RealExcelList
		,reload_flag = normal
	},
	{noreply, NewState};

do_handle_info(Info, State) ->
	io:format("do_handle_info unmatch : ~w", [Info]),
	{noreply, State}.

do_terminate(_Reason, _State) ->
	ok.

do_reload([], MaxId, _MaxSheetNum, _LoadTime, _ExcelPath, OldExcelList) -> 
	{MaxId, OldExcelList};
do_reload([{_, FileName} | T], MaxId, MaxSheetNum, LoadTime, ExcelPath, OldExcelList) ->
	File = lists:concat([ExcelPath, "/", FileName]),
	FileInfo = file:read_file_info(File),
	FindRes = lists:keyfind(FileName, 2, OldExcelList),
	NewMaxId = load_sheet(FindRes, FileInfo, FileName, MaxId, MaxSheetNum, LoadTime, File),
	if
		NewMaxId =/= MaxId ->
			NewExcelList = lists:keystore(MaxId, 1, OldExcelList, {MaxId, FileName});
		true ->
			NewExcelList = OldExcelList
	end,
	do_reload(T, NewMaxId, MaxSheetNum, LoadTime, ExcelPath, NewExcelList).

%% 旧配置表变更
load_sheet({NewId, _}, {ok, #file_info{mtime = WriteTime}}, FileName, MaxId, MaxSheetNum, LoadTime, File) when WriteTime > LoadTime ->
	do_load_sheet(NewId, FileName, MaxSheetNum, File),
	MaxId;
%% 新增配置表
load_sheet(false, _, FileName, MaxId, MaxSheetNum, _LoadTime, File) ->
	do_load_sheet(MaxId, FileName, MaxSheetNum, File),
	MaxId + 1;
load_sheet(_, _, _FileName, MaxId, _MaxSheetNum, _LoadTime, _ExcelPath) -> MaxId.

do_load_sheet(NewId, FileName, MaxSheetNum, File) ->
	SheetList = xlsx2erl_read:read_xlsx(File),
	case insert_sheet(SheetList, FileName, NewId, MaxSheetNum, []) of
		[] ->
			false;
		SheetIdList ->
			Excel = #ets_excel{id = NewId, file_name = FileName, sheet_ids = SheetIdList},
			ets:insert(?ETS_EXCEL_FILE, Excel)
	end.

insert_sheet([], _FileName, _Id, _MaxSheetNum, Acc) -> Acc;
insert_sheet([Sheet = #excel_sheet{id = SheetId} | T], FileName, Id, MaxSheetNum, Acc) ->
	case xlsx2erl_worker:do_oprate(Sheet, FileName) of
		#excel_sheet{name = SheetName} = NewSheet ->
			xlsx2erl:info("~ts配置中的子表【~s】格式检查通过~n", [FileName, SheetName]),
			NewSheetId = Id * MaxSheetNum + SheetId,
			ets:insert(?ETS_SHEET_INFO, #ets_sheet{id = NewSheetId, sheet = NewSheet}), 
			NewAcc = [NewSheetId | Acc];
		_ ->
			NewAcc = Acc
	end,
	insert_sheet(T, FileName, Id, MaxSheetNum, NewAcc).

get_excel_list() ->
    List = ets:tab2list(?ETS_EXCEL_FILE),
    [{Id, FileName} || #ets_excel{id = Id, file_name = FileName} <- List].

get_sheet_list() ->
	ets:tab2list(?ETS_SHEET_INFO).

get_sheet_list(ExcelIdList) ->
	SheetIdList = lists:foldl(
		fun(ExcelId, Acc) -> 
			case ets:lookup(?ETS_EXCEL_FILE, ExcelId) of
			 	[#ets_excel{sheet_ids = SheetIdList}] ->
			 		Acc ++ SheetIdList;
			 	_ -> Acc
			end 
		end, [], ExcelIdList),

	lists:foldl(
		fun(SheetId, Acc) -> 
			Acc ++ ets:lookup(?ETS_SHEET_INFO, SheetId) 
		end, [], SheetIdList).

get_excel_name(SheetId) ->
	MaxSheetNum = xlsx2erl_tool:config_sheet_num(),
	ExcelId = SheetId div MaxSheetNum,
	[#ets_excel{file_name = FileName}] = ets:lookup(?ETS_EXCEL_FILE, ExcelId),
	FileName.