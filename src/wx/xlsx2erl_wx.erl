%%%----------------------------------------------
%%% @author VG_xiao
%%% @doc
%%% 
%%% @end
%%% Created 2023-08-09 17:07:26
%%%----------------------------------------------
-module(xlsx2erl_wx).


-include("xlsx2erl_wx.hrl").

-behaviour(wx_object).
-export([start/0, start/1, start_link/0, start_link/1,  
     init/1, terminate/2,  code_change/3,
     handle_info/2, handle_call/3, handle_cast/2, handle_event/2, handle_sync_event/3]).


-record(state, {
    frame = null
    ,popupmenu = null
    ,tips_ctrl
    ,logs_ctrl
    ,choice_ctrl
    ,excel_ctrl

    ,logs_len = 0

    ,excel_path = ""
    ,tags = []
    ,path_map = #{}   %% Id => {Key, Path}
    ,server = undefined
    ,excel_tags = #{}   %% xx => [xxx.xlsx]
    ,extra = #{}
}).

start() ->
    start([]).

start(Debug) ->
    wx_object:start({local, ?MODULE}, ?MODULE, Debug, []).

start_link() ->
    start_link([]).

start_link(Debug) ->
    wx_object:start_link({local, ?MODULE}, ?MODULE, Debug, []).

%% -----------------------------
%% |导表工具
%% |设置  SVN HELP
%% 

%    WxOption = [
%         {excel_path, ExcelPath}
%         , {tags, Tags}
%         , {path_map, PathMap}
%         , {tag_map, TagMap}
%         , {server, self()}
%     ],

init(Options) ->
    %% 初始化wx环境
    wx:new(Options),
    process_flag(trap_exit, true),
    OptionMap = maps:from_list(Options),
    #{
        excel_path := ExcelPath
        ,tags := Tags
        ,tag_map := TagMap
        ,server := Server
        ,path_map := PathMap
    } = OptionMap,
    % erlang:group_leader(whereis(user), self()),
    
    %% 创建一个 “导表工具” 窗口,宽设置为500像素，高设置为800像素
    Frame = wxFrame:new(wx:null(), ?wxID_ANY, "导表工具", [{size,{600,800}}]),
    %% 创建菜单栏 
    NewPathMap = xlsx2erl_wx_tool:create_wxMenuBar(Frame, maps:to_list(PathMap)),
    %% 底部状态栏
    _SB = wxFrame:createStatusBar(Frame, []),

    %% pathmap 格式转换成 Id => {Key, Path}
    NewOpMap = maps:put(path_map, NewPathMap, OptionMap),
    {ChoiceCtrl, ExcelCtrl, TipsCtrl, LogsCtrl} =
            xlsx2erl_wx_tool:create_panel(Frame, NewOpMap, []),
    
    wxFrame:show(Frame),
    put(excel_list, []),
    PopupMenu = xlsx2erl_wx_tool:create_popupMenu(Tags),

    State = #state{
        frame = Frame
        ,popupmenu = PopupMenu 
        
        ,tips_ctrl = TipsCtrl
        ,logs_ctrl = LogsCtrl
        ,choice_ctrl = ChoiceCtrl % {LBPanel, Choice}
        ,excel_ctrl = ExcelCtrl % {ExcelParent, Sizer}

        ,excel_path = ExcelPath
        ,tags = Tags
        ,excel_tags = TagMap
        ,path_map = NewPathMap
        ,server = Server
    },
    {Frame, State}.




handle_info({info, LogStr}, State) ->
    format(State, LogStr),
    {noreply, clear_log(State)};

%% Handled as in normal gen_server callbacks
handle_info(update_excel, State) ->
    #state{excel_ctrl = {ExcelParent, Sizer}, extra = ExtraMap} = State,
    ExcelList = xlsx2erl_loader:get_excel_list(),
    % format(State, io_lib:format("update_excel ExcelList ~p~n",[ExcelList])),
    put(excel_list, ExcelList),
    xlsx2erl_wx_tool:recreate_excel_choice(ExcelParent, Sizer, ExcelList),
    {noreply, State#state{extra = maps:remove(?EXTRA_MAP_KEY_EXCELIDS, ExtraMap)}};

handle_info({'EXIT',_, wx_deleted}, State) ->
    {noreply,State};
handle_info({'EXIT',_, shutdown}, State) ->
    {noreply,State};
handle_info({'EXIT',_, normal}, State) ->
    {noreply,State};
handle_info(Msg, State) ->
    format(State, io_lib:format("~p Got Unhandle Info ~p~n",[?MODULE, Msg])),
    {noreply,State}.

handle_call(Msg, _From, State) ->
    format(State, io_lib:format("~p Got Unhandle call ~p~n",[?MODULE, Msg])),
    {reply,ok,State}.

handle_cast(Msg, State) ->
    format(State, io_lib:format("~p Got Unhandle cast ~p~n",[?MODULE, Msg])),
    {noreply,State}.

handle_event(Ev = #wx{event = #wxCommand{type = command_menu_selected}}, State) ->
    handle_menu_select_event(Ev, State);


handle_event(#wx{event=#wxClose{}}, State = #state{frame = Frame}) ->
    ok = wxFrame:setStatusText(Frame, "Closing...",[]),
    {stop, normal, State};

%% 右键
handle_event(#wx{event = #wxContextMenu{type = context_menu}, userData = FileBaseName}, State) ->
    #state{frame = Frame, excel_tags = TagMap, popupmenu = PopupMenu, extra = ExtraMap} = State,
    TagStr = maps:fold(
        fun(Tag, ExcelList, Acc) -> 
            case lists:member(FileBaseName, ExcelList) of
                true ->
                    lists:concat([Acc, Tag]);
                _ -> Acc
            end
        end, "", TagMap),
    NewTagStr = if TagStr == "" -> "无";true -> TagStr end,
    wxMenu:setLabel(PopupMenu, ?MY_wxMenuId_SELFTAG, lists:concat(["当前标签：", NewTagStr])),
    wxWindow:popupMenu(Frame, PopupMenu),
    {noreply, State#state{extra = maps:put(?EXTRA_MAP_KEY_EXCELNAME, FileBaseName, ExtraMap)}};


handle_event(#wx{id = ?MY_wxChoice_CHOOSETAG, event = #wxCommand{type = command_choice_selected, cmdString = Tag}}, State) ->
    #state{excel_tags = TagMap, excel_ctrl = {ExcelParent, Sizer}} = State,
    ExcelNameList = maps:get(Tag, TagMap, []),
    {_, ExcelList} = lists:foldl(fun(Name, {Seq, Acc}) -> {Seq+1, [{Seq, Name}|Acc]} end, {1, []}, ExcelNameList),
    ExcelList =/= [] andalso xlsx2erl_wx_tool:recreate_excel_choice(ExcelParent, Sizer, ExcelList),
    {noreply, State};

handle_event(#wx{id = ?MY_wxTextCtrl_SEARCH, event = #wxCommand{type = Type, cmdString = SearchStr}}, State) 
                                        when Type == command_text_updated 
                                        orelse Type == command_text_enter ->

    #state{excel_ctrl = {ExcelParent, Sizer}} = State,
    ExcelList = get(excel_list),
    if
        SearchStr == [] ->
            NewExcelList = ExcelList;
        true ->
            NewExcelList = lists:foldl(fun(H = {_, ExcelName}, Acc) ->
                case string:str(ExcelName, SearchStr) of
                    0 -> Acc;
                    _ -> [H|Acc]
                end
            end, [], ExcelList)
    end,
    NewExcelList =/= [] andalso xlsx2erl_wx_tool:recreate_excel_choice(ExcelParent, Sizer, NewExcelList),
    {noreply, State};

handle_event(#wx{id = ?BTN_ID_EXPORT_SELECT, event = #wxCommand{type = command_button_clicked}}, State) ->
    #state{extra = ExtraMap, server = Server} = State,
    SelectIds = maps:get(?EXTRA_MAP_KEY_EXCELIDS, ExtraMap, []),
    notify_server(Server, {export, SelectIds}),
    {noreply, State};


handle_event(#wx{id = ?BTN_ID_EXPORT_ALL, event = #wxCommand{type = command_button_clicked}}, State = #state{server = Server}) ->
    notify_server(Server, {export, all}),
    {noreply, State};

handle_event(#wx{id = ?BTN_ID_CHOICE_CLEAR, event = #wxCommand{type = command_button_clicked}}, State) ->
    #state{extra = ExtraMap, excel_ctrl = {ExcelParent, Sizer}} = State,
    ExcelList = get(excel_list),
    xlsx2erl_wx_tool:recreate_excel_choice(ExcelParent, Sizer, ExcelList),
    {noreply, State#state{extra = maps:remove(?EXTRA_MAP_KEY_EXCELIDS, ExtraMap)}};

handle_event(#wx{id = ExcelId, event = #wxCommand{type = command_checkbox_clicked, commandInt = Flag}}, State) ->
    #state{extra = ExtraMap} = State,
    List = maps:get(?EXTRA_MAP_KEY_EXCELIDS, ExtraMap, []),
    if
        Flag == 1 ->
            NewExtraMap = maps:put(?EXTRA_MAP_KEY_EXCELIDS, [ExcelId | lists:delete(ExcelId, List)], ExtraMap);
        true ->
            NewExtraMap = maps:put(?EXTRA_MAP_KEY_EXCELIDS, lists:delete(ExcelId, List), ExtraMap)
    end,
    {noreply, State#state{extra = NewExtraMap}};

handle_event(Ev, State) ->
    format(State, io_lib:format("~p LINE:~p Got event ~p ~n",[?MODULE, ?LINE, Ev])),
    {noreply, State}.

handle_sync_event(Request, _Ref, State) ->
    format(State, io_lib:format("~p Got Request ~p ~n",[?MODULE, Request])),
    {noreply, State}.

code_change(_, _, State) ->
    {stop, not_yet_implemented, State}.

terminate(_Reason, _State = #state{frame = Frame, server = Server}) ->
    % catch wx_object:call(State#state.example, shutdown),
    wxFrame:destroy(Frame),
    notify_server(Server, stop),
    wx:destroy(),
    init:stop().

change_tips(State) ->
    #state{
        excel_path = ExcelPath, path_map = PathMap, tips_ctrl = TipsCtrl
    } = State,   
    wxTextCtrl:clear(TipsCtrl),
    wxTextCtrl:appendText(TipsCtrl, xlsx2erl_wx_tool:tips_str(ExcelPath, PathMap)),
    ok.
    

handle_menu_select_event(#wx{id = ?wxID_ABOUT}, State = #state{frame = Frame}) ->
    AboutString = "author-VG_xiao\n\n地址:https://github.com/xlh1001/xlsx2erl\n\n如果觉得有帮助请给我点个小星星吧！",
    Dialog = wxMessageDialog:new(Frame, AboutString, [{style, ?wxOK bor ?wxICON_INFORMATION bor ?wxSTAY_ON_TOP}, {caption, "About"}]),
    wxMessageDialog:showModal(Dialog),
    wxMessageDialog:destroy(Dialog),
    {noreply, State};
handle_menu_select_event(#wx{id = ?wxID_EXIT}, State) ->
    {stop, normal, State};
handle_menu_select_event(#wx{id = ?MY_wxMenuId_XSLXDIR}, State) ->
    #state{frame = Frame, excel_path = ExcelPath, server = Server} = State,
    Dialog = wxDirDialog:new(Frame, [{'title', "选择excel文件所在目录"},{'defaultPath', ExcelPath}]),
    NewExcelPath = xlsx2erl_wx_tool:handle_dialog(wxDirDialog, Dialog, fun wxDirDialog:getPath/1, ExcelPath),
    NewPath = xlsx2erl_tool:change_path(NewExcelPath),
    notify_server_update(Server, excel_path, NewPath, ExcelPath),
    NewState = State#state{excel_path = NewPath},
    change_tips(NewState),
    {noreply, NewState};
handle_menu_select_event(#wx{id = ?MY_wxMenuId_OPENFILE}, State) ->
    #state{excel_path = ExcelPath, extra = #{?EXTRA_MAP_KEY_EXCELNAME := ExcelName}} = State,
    ExcelName =/= false andalso os:cmd(io_lib:format("start excel \"~s/~ts\"",[ExcelPath, ExcelName])),
    {noreply, State};
handle_menu_select_event(#wx{id = ?MY_wxMenuId_DIR}, State = #state{excel_path = ExcelPath}) ->
    os:cmd(lists:concat(["start ", ExcelPath])),
    {noreply, State};
handle_menu_select_event(#wx{id = ?MY_wxMenuId_EXPORT}, State) ->
    #state{extra = #{?EXTRA_MAP_KEY_EXCELNAME := ExcelName}, server = Server} = State,
    notify_server(Server, {export, [ExcelName]}),
    {noreply, State};
handle_menu_select_event(#wx{id = ?MY_wxMenuId_DELETE}, State) ->
    #state{excel_path = ExcelPath, extra = #{?EXTRA_MAP_KEY_EXCELNAME := ExcelName}} = State,
    ok = file:delete(lists:concat([ExcelPath, "/", ExcelName])),
    {noreply, State};
handle_menu_select_event(#wx{id = ?MY_wxMenuId_REFRESH}, State) ->
    xlsx2erl_loader:refresh(),
    {noreply, State};
handle_menu_select_event(#wx{id = ?MY_wxMenuId_SVNUPDATE}, State = #state{excel_path = ExcelPath}) ->
    os:cmd(lists:concat(["call svn update ", ExcelPath])),
    {noreply, State};
handle_menu_select_event(#wx{id = ?MY_wxMenuId_SVNCOMMIT}, State = #state{excel_path = ExcelPath}) ->
    os:cmd(lists:concat(["call svn commit ", ExcelPath, " -m 提交配置"])),
    {noreply, State};
handle_menu_select_event(#wx{id = ?MY_wxMenuId_DELSELFTAG}, State) ->
    #state{excel_tags = TagMap, server = Server, extra = #{?EXTRA_MAP_KEY_EXCELNAME := ExcelName}} = State,
    NewTagMap = maps:map(fun(_, ExcelList) ->
        lists:delete(ExcelName, ExcelList)
    end, TagMap),
    notify_server(Server, {del_self_tags, ExcelName}),
    {noreply, State#state{excel_tags = NewTagMap}};

handle_menu_select_event(#wx{id = TagId, userData = {select_tag, StartId, ?MY_wxMenuId_SETTAG}}, State) ->
    #state{excel_tags = TagMap, server = Server, tags = Tags, extra = #{?EXTRA_MAP_KEY_EXCELNAME := ExcelName}} = State,
    Tag = lists:nth(TagId - StartId, Tags),
    ExcelList = maps:get(Tag, TagMap, []),
    NewMap = maps:put(Tag, [ExcelName|lists:delete(ExcelName, ExcelList)], TagMap),
    notify_server(Server, {tags, Tag, ExcelName}),
    {noreply, State#state{excel_tags = NewMap}};
handle_menu_select_event(#wx{id = TagId, userData = {select_tag, StartId, ?MY_wxMenuId_DELTAG}}, State) ->
    #state{excel_tags = TagMap, server = Server, tags = Tags, choice_ctrl = {LBPanel, Choice}} = State,
    Tag = lists:nth(TagId - StartId, Tags),
    NewTags = lists:delete(Tag, Tags),
    NewMap = maps:remove(Tag, TagMap),
    notify_server(Server, {del_tag, Tag}),
    PopupMenu = xlsx2erl_wx_tool:create_popupMenu(NewTags),
    NewChoice = xlsx2erl_wx_tool:refresh_choice(LBPanel, Choice, NewTags),
    {noreply, State#state{popupmenu = PopupMenu, tags = NewTags, excel_tags = NewMap, choice_ctrl = {LBPanel, NewChoice}}};

handle_menu_select_event(#wx{id = ?MY_wxMenuId_SETNEWTAG}, State) ->
    #state{
        frame = Frame, tags = Tags, excel_tags = TagMap, 
        server = Server, popupmenu = OldPopupMenu, 
        extra = #{select_excel := ExcelName},
        choice_ctrl = {LBPanel, Choice}
    } = State,
    Dialog = wxTextEntryDialog:new(Frame, "添加标签", []),
    AddTag = xlsx2erl_wx_tool:handle_dialog(wxTextEntryDialog, Dialog, fun wxTextEntryDialog:getValue/1, ""),

    wxMenu:destroy(OldPopupMenu),
    ExcelList = maps:get(AddTag, TagMap, []),
    NewTags = [AddTag|lists:delete(AddTag, Tags)],
    NewMap = maps:put(AddTag, [ExcelName|lists:delete(ExcelName, ExcelList)], TagMap),
    notify_server(Server, {tags, AddTag, ExcelName}),
    PopupMenu = xlsx2erl_wx_tool:create_popupMenu(NewTags),
    NewChoice = xlsx2erl_wx_tool:refresh_choice(LBPanel, Choice, NewTags),
    {noreply, State#state{popupmenu = PopupMenu, tags = NewTags, excel_tags = NewMap, choice_ctrl = {LBPanel, NewChoice}}};

handle_menu_select_event(Ev = #wx{id = Id}, State = #state{path_map = PathMap}) ->
    case maps:get(Id, PathMap, null) of
        null ->
            format(State, io_lib:format("~p LINE:~p Got event ~p ~n",[?MODULE, ?LINE, Ev])),
            NewState = State;
        {Key, Path} ->
            #state{frame = Frame, path_map = PathMap, server = Server} = State,
            Dialog = wxDirDialog:new(Frame, [{'title', io_lib:format("选择存放~p文件目录",[Key])},{'defaultPath', Path}]),
            NewPath = xlsx2erl_wx_tool:handle_dialog(wxDirDialog, Dialog, fun wxDirDialog:getPath/1, Path),
            RealPath = xlsx2erl_tool:change_path(NewPath),
            RealPath =/= Path andalso notify_server(Server, {update_path, Key, RealPath}),
            NewState = State#state{path_map = maps:put(Id, {Key, RealPath}, PathMap)},
            change_tips(NewState)
    end,
    {noreply, NewState}.



notify_server_update(_Server, _Key, Value, Value) -> ok;
notify_server_update(Server, Key, NewValue, _Value) ->
    notify_server(Server, {update_user_data, Key, NewValue}).

notify_server(undefined, _) -> skip;
notify_server(Server, Msg) ->
    Server ! Msg.


format(#state{logs_ctrl = LogsCtrl}, LogStr) ->
    format(LogsCtrl, LogStr);
format(LogsCtrl, LogStr) ->
    wxTextCtrl:appendText(LogsCtrl, LogStr),
    ok.

clear_log(State = #state{logs_len = Len}) when Len =< 50 -> State;
clear_log(State = #state{logs_ctrl = LogsCtrl}) ->
    wxTextCtrl:clear(LogsCtrl),
    State#state{logs_len = 0}.