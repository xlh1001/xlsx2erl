
-record(excel_cell, {
	r = 0, 	%% 行 
	c = 0,  %% 列
	v = "",  %% 值
	str = "" %% "B1"
}).

-record(excel_sheet,{
	id = 0, 		%% sheet id
	name = "",   
	content = #{}, 	%% #{Row => Cells} 
	column = [],    %% [{Column, #{Key => Value}}]
	fun_map = #{}   %% 函数列表
}).

-record(excel_fun, {
	fun_name = none, 	%% 函数名
	args = [],     	%% 参数：字段名
	values = [] 	%% 值：字段名
}).


-define(IF(Expr, True, False), 
	if 
		Expr -> True; 
		true -> False
	end).


-define(DEFAULT_EXPORT_FUN, get).
-define(EXPORT, 1).
-define(UNEXPORT, 0).

-define(KEY, 1).
-define(NOTKEY, 0).


-define(ETS_EXCEL_FILE, ets_excel_file).
-define(ETS_SHEET_INFO, ets_sheet_info).

-record(ets_sheet, {
	id = 0,
	sheet = undefined
}).

-record(ets_excel, {
	id = 0,
	sheet_ids = [],
	file_name = ""
}).


%% 用户数据
-record(xlsx2erl_user_data, {
	key = 0,
	value = []
}).
