%%%----------------------------------------------
%%% @author VG_xiao
%%% @doc
%%% 
%%% @end
%%% Created 2023-08-07 20:10:28
%%%----------------------------------------------
-module(xlsx2erl_oprator).


-include("xlsx2erl.hrl").


-export([oprate/3 ]).

oprate(SheetList, HeaderList, Mods) ->
	case catch oprate(SheetList, HeaderList, Mods, []) of
        {false, Key, Code} -> {false, Key, Code};
        HandleList -> {ok, HandleList}
    end.
 
oprate([], _, _Mods, Acc) -> Acc;
oprate([Sheet = #excel_sheet{}|T], HeaderList, Mods, Acc) ->
	NewSheet = update_sheet(Sheet, HeaderList, Mods),
	oprate(T, HeaderList, Mods, [NewSheet|Acc]).

update_sheet(Sheet = #excel_sheet{content = ContentList}, HeaderList, Mods) ->
	{BeginRow, CloumnList, NewContentList} = make_column_field(ContentList, HeaderList, []),
	FuncMap = make_func(Mods, CloumnList, NewContentList, HeaderList, #{}),
	NewList = check_row(BeginRow, CloumnList, NewContentList),
	% io:format("==========OldMap ~p~nNewMap:~p~n",[NewContentList, NewList]),
	Sheet#excel_sheet{column = CloumnList, content = NewList, fun_map = FuncMap}.

% -----------------------------------------------------------------------------

make_column_field(ContentList, [{Row, data_begin, _}], Acc) -> 
	{Row, Acc, ContentList};
make_column_field(ContentList, [{Row, Key, ValueType}|T], []) ->
	{Cells, NewContentList} = get_cells(Row, ContentList),
	Fun = fun
		(#excel_cell{c = Cloumn, v = Value}, {Acc, Before}) when Cloumn == Before + 1 ->
			{[{Cloumn, #{Key => handle_value(ValueType, Value)}}|Acc], Cloumn};
		(_Cell, Acc) -> Acc
	end, 
	{CloumnList, _} = lists:foldl(Fun, {[], 0}, Cells),
	make_column_field(NewContentList, T, CloumnList);
make_column_field(ContentList, [{Row, Key, ValueType}|T], CloumnList) ->
	{Cells, NewContentList} = get_cells(Row, ContentList),
	Fun = fun({Cloumn, Map}, Acc) ->
		FindRes = lists:keyfind(Cloumn, #excel_cell.c, Cells),
		NewMap = make_column_field_core(FindRes, Key, ValueType, Map),
		[{Cloumn, NewMap} | Acc]
	end,
	NewCloumnList = lists:foldl(Fun, [], CloumnList),
	make_column_field(NewContentList, T, NewCloumnList).

get_cells(Row, ContentList) ->
	case proplists:get_value(Row, ContentList, null) of
		null -> 
			Cells = [],
			NewContentList = lists:keysort(1, [{Row, Cells}|ContentList]);
		Cells ->
			NewContentList = ContentList
	end,
	{Cells, NewContentList}.

handle_value(atom, [First|T] = Value) -> %% atom 由小写字母开头，数字下划线小写字母组成
	(First < 97 orelse First > 122) andalso throw({false, atom, filed_value_err}),
	List = [H || H <- T, H < 48 orelse ((H > 57 andalso H < 95) orelse H == 96) orelse  H > 122],
	List =/= [] andalso throw({false, atom, filed_value_err}),
	erlang:list_to_atom(Value);
handle_value(int, Value) -> %% 确保数据是数字
	List = [H || H <- Value, H < 48 orelse H > 57],
	List =/= [] andalso throw({false, int, filed_value_err}),
	erlang:list_to_integer(Value);
handle_value(list, Value) -> % "[xxx]" => [xxx]
	xlsx2erl_tool:string_to_term(Value);
handle_value(_, Value) -> [H || H <- Value, H > 31].


make_column_field_core(false, Key, _ValueType, _Map) 
				when Key == comment orelse 
					Key == name orelse Key == data_type ->
	throw({false, Key, key_filed_lost});
make_column_field_core(false, _Key, _ValueType, Map) -> 
	Map;
make_column_field_core(#excel_cell{v = Value}, Key, ValueType, Map) ->
	maps:put(Key, handle_value(ValueType, Value), Map).

% ---------------------------------------------------------------------
make_func([], _CloumnList, _ContentList, _HeaderList, AccMap) -> AccMap;
make_func([H | T], CloumnList, ContentList, HeaderList, AccMap) ->
	{ExportKey, FunKey, _CallBacMod, _PathList} = H,
	{Row, _, _} = lists:keyfind(FunKey, 2, HeaderList),
	Cells = proplists:get_value(Row, ContentList, []),
	Fun = fun(#excel_cell{v = Value}, Acc) ->
		FunList = xlsx2erl_tool:string_to_term(Value),
		Funs = make_func_core(CloumnList, FunList),
		Funs ++ Acc
	end, 
	List = lists:foldl(Fun, [], Cells),
	DefaultFun = default_fun(CloumnList, ExportKey),
	% xlsx2erl:info("========= DefaultFun:~p~n", [DefaultFun]),
	NewMap = maps:put(FunKey, [DefaultFun | List], AccMap),
	make_func(T, CloumnList, ContentList, HeaderList, NewMap).

make_func_core(CloumnList, List) when is_list(List) ->
	[make_func_core(CloumnList, {TmpFunName, TmpArity, TmpReturn}) || {TmpFunName, TmpArity, TmpReturn} <- List];
make_func_core(CloumnList, {TmpFunName, TmpArity, TmpReturn}) ->
	#excel_fun{
		fun_name = TmpFunName, 
		args = transform_args(CloumnList, TmpArity, []), 
		values = transform_args(CloumnList, TmpReturn, [])
	}.

default_fun(CloumnList, ExportKey) -> 
	Fun = fun({Cloumn, FieldMap}, {AccKey, Acc}) ->
		#{name := Name, data_type := DataType} = FieldMap,
		% xlsx2erl:info("========= FieldMap:~p~n", [FieldMap]),
		IsKey = maps:get(is_key, FieldMap, ?NOTKEY),
		IsExport = maps:get(ExportKey, FieldMap, ?EXPORT),
		NewAccKey = ?IF(IsKey == ?KEY, [{Cloumn, Name, DataType} | AccKey], AccKey),
		NewAcc = ?IF(IsExport == ?EXPORT, [{Cloumn, Name, DataType} | Acc], Acc),
		{NewAccKey, NewAcc}
	end,
	{Arity, Return} = lists:foldl(Fun, {[], []}, CloumnList),
	#excel_fun{fun_name = ?DEFAULT_EXPORT_FUN, args = lists:reverse(Arity), values = lists:reverse(Return)}.

transform_args(_CloumnList, [], Acc) -> lists:reverse(Acc);
transform_args(CloumnList, [Arg | T], Acc) ->
	Cloumn = transform_args_core(CloumnList, Arg),
	transform_args(CloumnList, T, [Cloumn | Acc]).

transform_args_core([], Arity) -> 
	throw({false, Arity, fun_args_not_exist});
transform_args_core([{Cloumn, #{name := Name, data_type := DataType}} | _], Name) ->
	{Cloumn, Name, DataType};
transform_args_core([_ | CloumnList], Arity) ->
	transform_args_core(CloumnList, Arity).

% ---------------------------------------------------------------------

check_row(BeginRow, Cloumns, ContentList) ->
	KeyCloumn = [ 
		Cloumn || {Cloumn, #{is_key := IsKey}} <- Cloumns, IsKey == 1
	],
	% xlsx2erl:info("========= ContentList:~p~n", [ContentList]),
	%% 按行排序
	check_row_core(ContentList, BeginRow, KeyCloumn, []).

check_row_core([], _BeginRow, _KeyCloumn, Acc) -> lists:keysort(1, Acc);
check_row_core([{Row, Cells} | T], BeginRow, KeyCloumn, Acc) when Row =< BeginRow ->
	check_row_core(T, BeginRow, KeyCloumn, [{Row, Cells}|Acc]);
check_row_core([{Row, [#excel_cell{r = Row}|_] = Cells} | T], BeginRow, KeyCloumn, Acc) ->
	CheckRes = check_cloumn_exists(KeyCloumn, Cells),
	CheckRes ==	false andalso throw({false, Row, lost_key_cloumn}),
	check_row_core(T, BeginRow, KeyCloumn, [{Row, Cells}|Acc]);
%% 空行标志数据读取结束
check_row_core([_ | _T], _BeginRow, _KeyCloumn, AccMap) -> 
	AccMap.


check_cloumn_exists([], _Cells) -> true;
check_cloumn_exists([Cloumn|T], Cells) ->
	case lists:keyfind(Cloumn, #excel_cell.c, Cells) of
		#excel_cell{} ->
			check_cloumn_exists(T, Cells);
		_ ->
			false
	end.


