%%%----------------------------------------------
%%% @author VG_xiao
%%% @doc
%%% 
%%% @end
%%% Created 2023-08-18 16:58:54
%%%----------------------------------------------
-module(xlsx2erl_wx_tool).

-include("xlsx2erl_wx.hrl").

-export([create_popupMenu/1, create_wxMenuBar/2, create_panel/3, handle_dialog/4]).

-export([tips_str/3, format_wxTextCtrl/2, recreate_excel_choice/3, refresh_choice/3, log_str/2]).

% -----------------------------------------------------------------

%% excel右键弹窗
create_popupMenu(Tags) ->
	
    {_, TagList} = xlsx2erl_tool:make_tag_menus(Tags),

    Menu = wxMenu:new([]),
    SubMenu  = wxMenu:new([]),
    DelMenu  = wxMenu:new([]),

    wxMenu:append(Menu, ?MY_wxMenuId_OPENFILE, "打开文件", []), % os:cmd("start E:/test/Erlang/game/tool/xsl2erl/test/test-测试.xlsx").
    wxMenu:append(Menu, ?MY_wxMenuId_DIR, "所在文件夹", []),    % os:cmd("start E:/test/Erlang/game/tool/xsl2erl/test/").
    wxMenu:append(Menu, ?MY_wxMenuId_EXPORT, "导出文件", []),
    wxMenu:append(Menu, ?MY_wxMenuId_DELETE, "删除文件", []),
    wxMenu:append(Menu, ?MY_wxMenuId_REFRESH, "刷新", []),
    wxMenu:appendSeparator(Menu),
    wxMenu:append(Menu, ?MY_wxMenuId_SVNUPDATE, "更新文件", []),
    wxMenu:append(Menu, ?MY_wxMenuId_SVNCOMMIT, "提交文件", []),
    wxMenu:appendSeparator(Menu),
    <<SubTagId:16>> = <<1:4,1:12>>,
    [wxMenu:append(SubMenu, TagId + SubTagId, TagName, []) || {TagId, TagName} <- TagList],
    wxMenu:append(Menu, ?MY_wxMenuId_SELFTAG, "", []),
    wxMenu:append(Menu, ?MY_wxMenuId_SETTAG, "添加到已有标签", SubMenu, []),
    wxMenu:append(Menu, ?MY_wxMenuId_SETNEWTAG, "添加到指定标签", []),
    wxMenu:append(Menu, ?MY_wxMenuId_DELSELFTAG, "清除文件标签", []),
    % 这里TagId与SubMenu中的一样，实际上并不会新增，只是复用了，因此DelMenu事件不会触发
    % [wxMenu:append(DelMenu, TagId, TagName, []) || { TagId, TagName} <- TagList],
    <<DelTagId:16>> = <<2:4,1:12>>,
    [wxMenu:append(DelMenu, TagId + DelTagId, TagName, []) || {TagId, TagName} <- TagList],
    wxMenu:append(Menu, ?MY_wxMenuId_DELTAG, "删除标签", DelMenu, []),

    wxMenu:connect(Menu, command_menu_selected),
    wxMenu:connect(SubMenu, command_menu_selected, [{userData, {select_tag, SubTagId, ?MY_wxMenuId_SETTAG}}]),
    wxMenu:connect(DelMenu, command_menu_selected, [{userData, {select_tag, DelTagId, ?MY_wxMenuId_DELTAG}}]),
    Menu.

% -----------------------------------------------------------------

create_wxMenuBar(Frame, PathList) ->
	% 新建菜单栏（设置 svn help）
	MB = wxMenuBar:new(),
	% 添加设置菜单
	{_, MenuList, PathMap} = lists:foldl(
		fun({Key, Path}, {Id, Acc, AccMap}) -> 
			NewMap = maps:put(Id, {Key, Path}, AccMap),
			NewAcc = [{Id, lists:concat(["设置", Key ,"目录"]) } | Acc],
			{Id+1, NewAcc, NewMap}
		end, {?MY_wxMenuId_UNKNOWNDIR, [], #{}}, PathList),
	SettingMenuList = MenuList ++ [
		{?MY_wxMenuId_XSLXDIR, "&设置excel目录"}, 
		% {?MY_wxMenuId_WORKERNUM, "&设置工作进程数量"}
	],
	create_wxMenu(MB, "&设置", SettingMenuList),
	create_wxMenu(MB, "&文件", [{?wxID_EXIT, "&退出"}]),
	create_wxMenu(MB, "&说明", [{?wxID_ABOUT, "&工具信息"}]), %% {?wxID_HELP, "&帮助"}, 
	% 将菜单栏附加到窗口中
	wxFrame:setMenuBar(Frame, MB),
	%% 添加事件 command_menu_selected、close_window
	wxFrame:connect(Frame, command_menu_selected),
	wxFrame:connect(Frame, close_window),
	PathMap.

create_wxMenu(MB, MenuLable, SubMenuList) ->
	% 添加xx菜单
	Menu = wxMenu:new([]),
	% 设置-设置工作目录功能
	[wxMenu:append(Menu, Id, Lable) || {Id, Lable} <- SubMenuList],
	% 将菜单添加到菜单栏中
	wxMenuBar:append(MB, Menu, MenuLable),
	ok.


% -----------------------------------------------------------------

%% 界面创建逻辑
create_panel(Frame, OptionMap = #{tags := Tags}, ExcelList) ->
	%% 创建一个窗口，指定样式 wxSP_NOBORDER 
	%% 其他样式参考 wxSplitterWindow:new/2 
	LeftSplitter   = wxSplitterWindow:new(Frame, [{style, ?wxSP_NOBORDER}, {size, {200,800}}]),    %{size, {500,800}}
	RightSplitter   = wxSplitterWindow:new(LeftSplitter, [{style, ?wxSP_NOBORDER}]),    %{size, {500,800}}
	
	%% 给左边的窗口加控件
	{LBPanel, List} = create_left_panel(LeftSplitter, Tags, ExcelList),
    % {sashPosition, Value} Value值是正的则表示指定左边的窗口（LBPanel）大小，值为负数则指定的是右边窗口的大小
    wxSplitterWindow:splitVertically(LeftSplitter, LBPanel, RightSplitter, [{sashPosition,200}]),

    %% 给左边的窗口加控件
	{TipsCtrl, LogsCtrl} = create_right_panel(RightSplitter, OptionMap),

    % 0.0 - only the bottom/right window is automatically resized
    % 0.5 - both windows grow by equal size
    % 1.0 - only left/top window grows 
	wxSplitterWindow:setSashGravity(LeftSplitter,   0.0),
	wxSplitterWindow:setSashGravity(RightSplitter,   0.0),
    %% 设置最小为50，防止拖拽到0
    wxSplitterWindow:setMinimumPaneSize(LeftSplitter, 100),
    wxSplitterWindow:setMinimumPaneSize(RightSplitter, 100),

    [{_, Choice}, {ExcelParent, Sizer} | _] = List,
    {{LBPanel, Choice}, {ExcelParent, Sizer}, TipsCtrl, LogsCtrl}.

%% 左边界面：搜索框，标签筛选框，excel显示区域，按钮区域
create_left_panel(LeftSplitter, TagChoices, ExcelList) ->
	CreateCb = fun(Parent) ->
		OtherSz = wxStaticBoxSizer:new(?wxHORIZONTAL, Parent, [{label, ""}]),
		create_search(Parent, OtherSz),
		Choice = create_choice(Parent, OtherSz, TagChoices),
		{Panel, Sizer} = create_ecxel_choice(Parent, ExcelList),
    	[{OtherSz, Choice}, {Panel, Sizer}]
	end,
	%% 创建excel列表窗口
    {LBPanel, [List], Sizer} = create_subwindow(LeftSplitter, "excel", [CreateCb]),
    %% 创建导出按钮
	create_btn(LBPanel, Sizer),
	% Map = #{tag_choices => {ChoiceSizer, TagChoice}, excel_box => {ExcelPanel, ExcelBoxList}},
    {LBPanel, List}.

%% 右边界面添加2个显示文字的窗口
create_right_panel(RightSplitter, OptionMap) ->
	#{
        excel_path := ExcelPath
        ,path_map := PathMap
        ,worker_num := WorkerNum
    } = OptionMap,
	%% 创建一个文字控件（显示、编辑文字）
    AddEvent = fun(Parent, Str) ->
	    EventText = wxTextCtrl:new(Parent, ?wxID_ANY,  [{style, ?wxTE_DONTWRAP bor ?wxTE_MULTILINE bor ?wxTE_READONLY}]),
	    wxTextCtrl:appendText(EventText, Str),
	    EventText
	end,

	%% 创建一个tips窗口
	StrTips = tips_str(ExcelPath, PathMap, WorkerNum),
    {EvPanel, [TipsCtrl],_} = create_subwindow(RightSplitter, "tips", [{AddEvent, StrTips}]),
    
    {LogPanel, [LogEvCtrl],_} = create_subwindow(RightSplitter, "log", [{AddEvent, "welcome"}]),
    %% {sashPosition, Value} Value值是正的则表示指定上方的窗口（EvPanel）大小，值为负数则指定的是下窗口的大小
    wxSplitterWindow:splitHorizontally(RightSplitter, EvPanel, LogPanel, [{sashPosition, 150}]),
    {TipsCtrl, LogEvCtrl}.

%% 给左边界面底部加2个按钮
create_btn(LBPanel, Sizer) ->
	CreateBtn = fun(Parent) ->
		OtherSz = wxStaticBoxSizer:new(?wxHORIZONTAL, Parent, [{label, "btns"}]),
		BtnList = [?BTN_EXPORT_SELECT, ?BTN_EXPORT_ALL, ?BTN_EXPORT_CLEAR],
		Btns = [
			begin 
				Button = wxButton:new(Parent, BtnId, [{label, BtnLabel}, {style, ?wxNO_BORDER}, {size, {60,30}}]),
				wxButton:setToolTip(Button,TipString),
				Button
			end|| {BtnId, BtnLabel, TipString} <- BtnList], %
		[wxSizer:add(OtherSz, Button, [{proportion, 1},{flag, ?wxALIGN_BOTTOM bor ?wxBOTTOM}]) || Button <- Btns], %  
		OtherSz
	end,
    BtnSizer = CreateBtn(LBPanel),
    %% 设置BtnSizer在Sizer wxBOTTOM-底部，wxALIGN_BOTTOM-底部对齐，wxEXPAND-自动填充
    %% proportion 值为0表示不可更改 border 边框宽度
    wxSizer:add(Sizer, BtnSizer, [{proportion, 1}, {border, 1}, {flag, ?wxALIGN_BOTTOM bor ?wxBOTTOM bor ?wxEXPAND}]),
    wxWindow:connect(LBPanel, command_button_clicked),
    ok.

%% 创建一个勾选框
create_ecxel_choice(Parent, ExcelList) ->
	CreateCbCore = fun(TmpParent) ->
		% Mods = [filename:rootname(F) || F <- filelib:wildcard("*.xlsx", "../test")],
		[begin 
			Ctrl = wxCheckBox:new(TmpParent, Id, FileName, [{size, {0, 18}}]),
			%% 左键点击
	    	wxCheckBox:connect(Ctrl, command_checkbox_clicked),
	    	%% 右键菜单
			wxCheckBox:connect(Ctrl, context_menu, [{userData, FileName}]),
			Ctrl
		end || {Id, FileName} <- ExcelList]
	end,

	{Panel, [_Ctrls], Sizer} = create_subwindow(Parent, wxScrolledWindow, "", [CreateCbCore], []),
	wxScrolledWindow:setScrollRate(Panel, 5, 5),
	wxScrolledWindow:setMinSize(Panel, {200, 580}),
	{Panel, Sizer}.

recreate_excel_choice(Parent, Sizer, ExcelList) ->
	% [wxCheckBox:destroy(ExcelBox) || ExcelBox <- ExcelBoxList],
	wxSizer:clear(Sizer, [{delete_windows, true}]),
	%% 点击选择下方的item会有一个偏移值，每个新增的item需要减去这个偏移值
	{0, StartY} = wxScrolledWindow:getViewStart(Parent),

	%% 手动给所有的item赋一个偏移坐标，没有设置坐标会出现所有item都在一行的情况
	lists:foldl(fun({Id, FileName}, Y) ->
			Ctrl = wxCheckBox:new(Parent, Id, FileName, [{pos, {4, Y - StartY}}, {size, {200, 18}}]),
			wxSizer:add(Sizer, Ctrl, [{flag, ?wxEXPAND}]), 
			%% 这里确保新增的item能够正常显示
			wxSizer:fitInside(Sizer, Ctrl),
	    	%% 左键点击
	    	wxCheckBox:connect(Ctrl, command_checkbox_clicked),
	    	%% 右键菜单
			wxCheckBox:connect(Ctrl, context_menu, [{userData, FileName}]),
			Y+18
	end, 9, ExcelList),
	%% https://forums.wxwidgets.org/viewtopic.php?t=18283，refresh不要和update一起出现
	%% 刷新下页面
    wxScrolledWindow:refresh(Parent),
    % wxWindow:update(Parent),
    ok.

% setMinSize(This,Width,Height)
%% 创建一个搜索框
create_search(Parent, Sizer) ->
	%% 输入框 指定style wxTE_PROCESS_ENTER（只有设定该样式才能使得事件command_text_enter生效）
	%% {value, 输入框默认值}
	TextCtrl  = wxTextCtrl:new(Parent, ?MY_wxTextCtrl_SEARCH, []), %% {style, ?wxTE_PROCESS_ENTER}
	%% 鼠标悬停提示
	wxTextCtrl:setToolTip(TextCtrl, "ENTER键触发搜索"),
	% 输入字符串并按下enter
	% wxTextCtrl:connect(TextCtrl, command_text_enter),
	wxTextCtrl:connect(TextCtrl, command_text_updated),
	% % 输入框字符串变更事件 
	% wxTextCtrl:connect(TextCtrl, command_text_updated),
	wxSizer:add(Sizer, TextCtrl, [{border,4}, {flag, ?wxALL}]),
	ok.

%% 创建一个选择栏
create_choice(Parent, Sizer, Tags) ->
	Choice = wxChoice:new(Parent, ?MY_wxChoice_CHOOSETAG, [{choices, Tags}]),
    %% 鼠标悬停提示
    wxChoice:setToolTip(Choice, "选择标签"),
    %% 监听命令command_choice_selected
    wxChoice:connect(Choice, command_choice_selected),
    %% Add to sizers
    wxSizer:add(Sizer, Choice, [{border,4}, {flag, ?wxALL}]),
    Choice.

refresh_choice(Parent, Choice, Tags) ->
	wxChoice:clear(Choice),
	wxChoice:create(Choice, Parent, ?MY_wxChoice_CHOOSETAG, wxChoice:getPosition(Choice), wxChoice:getSize(Choice), Tags),
	wxChoice:refresh(Choice),
	Choice.

%% 创建一个子窗口 
%% BoxLabel:名称
%% Funs:
create_subwindow(Parent, BoxLabel, Funs) ->
	create_subwindow(Parent, wxPanel, BoxLabel, Funs, []).

create_subwindow(Parent, WindowsMod, BoxLabel, Funs, CreateOptions) ->
	%% 创建一个面板，用来挂载 controls 
    Panel = WindowsMod:new(Parent, CreateOptions),
    %% 添加一个静态框 wxStaticBoxSizer，用来显示这个子窗口的名字
    Sz    = wxStaticBoxSizer:new(?wxVERTICAL, Panel, [{label, BoxLabel}]),
    %% 将这个静态框挂载到面板上
    WindowsMod:setSizer(Panel, Sz),
    %% 执行函数对应的创建逻辑
    Ctrls = do_apply(Funs, Panel),
    %% Funs创建的内容放到静态框中
    lists:foreach(
    	fun(CtrlList) ->
    		create_subwindow_helper(Sz, CtrlList)
    	end, Ctrls),
    {Panel, Ctrls, Sz}.

create_subwindow_helper(_Sz, []) -> skip;
create_subwindow_helper(Sz, [{Ctrl, _}|T]) ->
	wxSizer:add(Sz, Ctrl, [{flag, ?wxEXPAND}]),
	create_subwindow_helper(Sz, T);
create_subwindow_helper(Sz, [{Ctrl, _, _}|T]) ->
	wxSizer:add(Sz, Ctrl, [{flag, ?wxEXPAND}]),
	create_subwindow_helper(Sz, T);
create_subwindow_helper(Sz, [Ctrl | T]) ->
	wxSizer:add(Sz, Ctrl, [{flag, ?wxEXPAND}]),
	create_subwindow_helper(Sz, T);
create_subwindow_helper(Sz, Ctrl) ->
	wxSizer:add(Sz, Ctrl, [{proportion, 1}, {flag, ?wxEXPAND}]).

do_apply(Funs, Panel) ->
	Res = lists:foldl(
		fun ({Fun, Arg}, Acc) -> 
				[Fun(Panel, Arg) | Acc];
			(Fun, Acc) ->
				[Fun(Panel) | Acc]
		end, [], Funs),
	lists:reverse(Res).


% -----------------------------------------------------------------

handle_dialog(Module, Dialog, Fun, Default) ->
	Return = case Module:showModal(Dialog) of
		?wxID_OK ->
			Fun(Dialog);
		_ ->
			Default
	end,
	Module:destroy(Dialog),
	Return.
% -----------------------------------------------------------------

format_wxTextCtrl(LogCtrl, Str) ->
	wxTextCtrl:appendText(LogCtrl, Str),
    ok.

tips_str(ExcelPath, PathMap, WorkerNum) ->
	PathList = maps:to_list(PathMap),

	Str = lists:foldl(fun({_Id, {Key, Path}}, Acc) ->
		lists:concat([Acc, "\n", Key, "目录:", filename:absname(Path)])
	end, "", PathList),


	lists:concat([
		"当前配置\nexcel目录:", filename:absname(ExcelPath), 
		Str,
		"\n工作进程数量:", WorkerNum, "\n添加新的文件后选择一个excel文件右键刷新！"
	]).

log_str(ExcelName, success) ->
	lists:concat(["导出", ExcelName, "配置成功!\n"]);
log_str(ExcelName, fail) ->
	lists:concat(["导出", ExcelName, "配置失败，请检查格式!\n"]).