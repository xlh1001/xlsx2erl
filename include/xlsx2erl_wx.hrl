-include_lib("wx/include/wx.hrl").
-include_lib("wx/src/wxe.hrl").

-define(MY_wxMenuId_UNKNOWNDIR, 450).
% -define(MY_wxMenuId_ERLDIR, 	502).
-define(MY_wxMenuId_XSLXDIR, 	503).
-define(MY_wxMenuId_WORKERNUM, 	504).
-define(MY_wxMenuId_OPENFILE, 	505).
-define(MY_wxMenuId_EXPORT, 	506).
-define(MY_wxMenuId_DELETE, 	507).
-define(MY_wxMenuId_SVNUPDATE, 	508).
-define(MY_wxMenuId_SVNCOMMIT, 	509).
-define(MY_wxMenuId_SETTAG,     510).
-define(MY_wxMenuId_SETNEWTAG,  511).
-define(MY_wxMenuId_DIR,        512).
-define(MY_wxMenuId_REFRESH,    513).
-define(MY_wxMenuId_DELTAG,     514).
-define(MY_wxMenuId_DELSELFTAG, 515).
-define(MY_wxMenuId_SELFTAG,    516).

-define(MY_wxChoice_CHOOSETAG,  1).

-define(MY_wxTextCtrl_SEARCH,  1).

-define(BTN_ID_EXPORT_SELECT, 	1). %% 选择导出
-define(BTN_ID_EXPORT_ALL, 		2). %% 全部导出
-define(BTN_ID_CHOICE_CLEAR,    3). %% 清除选中

-define(BTN_EXPORT_SELECT, 	    {?BTN_ID_EXPORT_SELECT, "select", "导出选中的文件"}). %% 选择导出
-define(BTN_EXPORT_ALL, 		{?BTN_ID_EXPORT_ALL, "all", "全部导出"}). %% 全部导出
-define(BTN_EXPORT_CLEAR, 		{?BTN_ID_CHOICE_CLEAR, "clear", "清除选中效果"}). %% 清除选中


-define(EXTRA_MAP_KEY_EXCELNAME, select_excel).
-define(EXTRA_MAP_KEY_EXCELIDS, select_excel_list).

%% id分配规则  
%%		wxCheckBox  
%%    		1 - 2000  excel文件
%% 		wxMenu  
%% 			1 - 500 标签 Id
%%      