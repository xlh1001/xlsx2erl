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

%% return: list() | {false, Key, filed_value_err} |
% 	{false, Key, key_filed_lost} | {false, Row, lost_key_cloumn} ->
oprate(SheetList, HeaderList, Mods) ->
	case catch oprate(SheetList, HeaderList, Mods, []) of
        {false, Key, Code} -> {false, Key, Code};
        HandleList -> {ok, HandleList}
    end.
 
oprate([], _, _Mods, Acc) -> Acc;
oprate([Sheet = #excel_sheet{}|T], HeaderList, Mods, Acc) ->
	NewSheet = update_sheet(Sheet, HeaderList, Mods),
	oprate(T, HeaderList, Mods, [NewSheet|Acc]).

update_sheet(Sheet = #excel_sheet{content = ContentMap}, HeaderList, Mods) ->
	{BeginRow, CloumnList} = make_column_field(ContentMap, HeaderList, []),
	FuncMap = make_func(Mods, CloumnList, ContentMap, HeaderList, #{}),
	NewMap = check_row(BeginRow, CloumnList, ContentMap),
	Sheet#excel_sheet{column = CloumnList, content = NewMap, fun_map = FuncMap}.



% -----------------------------------------------------------------------------

make_column_field(_ContentMap, [{Row, data_begin, _}], Acc) -> {Row, Acc};
make_column_field(ContentMap, [{Row, Key, ValueType}|T], []) ->
	Cells = maps:get(Row, ContentMap),
	Fun = fun
		(#excel_cell{c = Cloumn, v = Value}, {Acc, Before}) when Cloumn == Before + 1 ->
			{[{Cloumn, #{Key => handle_value(ValueType, Value)}}|Acc], Cloumn};
		(_Cell, Acc) -> Acc
	end, 
	{CloumnList, _} = lists:foldl(Fun, {[], 0}, Cells),
	make_column_field(ContentMap, T, CloumnList);
make_column_field(ContentMap, [{Row, Key, ValueType}|T], CloumnList) ->
	Cells = maps:get(Row, ContentMap),
	Fun = fun({Cloumn, Map}, Acc) ->
		FindRes = lists:keyfind(Cloumn, #excel_cell.c, Cells),
		NewMap = make_column_field_core(FindRes, Key, ValueType, Map),
		[{Cloumn, NewMap} | Acc]
	end,
	NewCloumnList = lists:foldl(Fun, [], CloumnList),
	make_column_field(ContentMap, T, NewCloumnList).


handle_value(atom, [First|T] = Value) -> %% atom 只能是小写字符开头，由小写字母以及数字组成
	(First < 97 orelse First > 122) andalso throw({false, atom, filed_value_err}),
	List = [H || H <- T, H < 48 orelse (H > 57 andalso H < 97) orelse  H > 122],
	List =/= [] andalso throw({false, atom, filed_value_err}),
	erlang:list_to_atom(Value);
handle_value(int, Value) -> %% 确保数据是数字
	List = [H || H <- Value, H < 48 orelse H > 57],
	List =/= [] andalso throw({false, int, filed_value_err}),
	erlang:list_to_integer(Value);
handle_value(list, Value) -> % "[xxx]" => [xxx]
	xlsx2erl_tool:string_to_term(Value);
handle_value(_, Value) -> Value.


make_column_field_core(false, Key, _ValueType, _Map) 
				when Key == comment orelse 
					Key == name orelse Key == data_type ->
	throw({false, Key, key_filed_lost});
make_column_field_core(false, _Key, _ValueType, Map) -> 
	Map;
make_column_field_core(#excel_cell{v = Value}, Key, ValueType, Map) ->
	maps:put(Key, handle_value(ValueType, Value), Map).

% ---------------------------------------------------------------------
make_func([], _CloumnList, _ContentMap, _HeaderList, AccMap) -> AccMap;
make_func([H | T], CloumnList, ContentMap, HeaderList, AccMap) ->
	{ExportKey, FunKey, _CallBacMod, _PathList} = H,
	{Row, _, _} = lists:keyfind(FunKey, 2, HeaderList),
	Cells = maps:get(Row, ContentMap),
	Fun = fun(#excel_cell{v = Value}, Acc) ->
		FunList = xlsx2erl_tool:string_to_term(Value),
		Funs = make_func_core(FunList),
		Funs ++ Acc
	end, 
	List = lists:foldl(Fun, [], Cells),
	DefaultFun = default_fun(CloumnList, ExportKey),
	% xlsx2erl:info("========= DefaultFun:~p~n", [DefaultFun]),
	NewMap = maps:put(FunKey, [DefaultFun | List], AccMap),
	make_func(T, CloumnList, ContentMap, HeaderList, NewMap).

make_func_core(List) when is_list(List) ->
	[make_func_core({TmpFunName, TmpArity, TmpReturn}) || {TmpFunName, TmpArity, TmpReturn} <- List];
make_func_core({TmpFunName, TmpArity, TmpReturn}) ->
	#excel_fun{
		fun_name = TmpFunName, 
		args = TmpArity, 
		values = TmpReturn
	}.

default_fun(CloumnList, ExportKey) -> 
	Fun = fun({_Cloumn, #{name := Name} = FieldMap}, {AccKey, Acc}) ->
		% xlsx2erl:info("========= FieldMap:~p~n", [FieldMap]),
		IsKey = maps:get(is_key, FieldMap, ?NOTKEY),
		IsExport = maps:get(ExportKey, FieldMap, ?EXPORT),
		NewAccKey = ?IF(IsKey == ?KEY, [Name | AccKey], AccKey),
		NewAcc = ?IF(IsExport == ?EXPORT, [Name | Acc], Acc),
		{NewAccKey, NewAcc}
	end,
	{Arity, Return} = lists:foldl(Fun, {[], []}, CloumnList),
	#excel_fun{fun_name = ?DEFAULT_EXPORT_FUN, args = Arity, values = Return}.

% ---------------------------------------------------------------------

check_row(BeginRow, Cloumns, ContentMap) ->
	KeyCloumn = [ 
		Cloumn || {Cloumn, #{is_key := IsKey}} <- Cloumns, IsKey == 1
	],
	% xlsx2erl:info("========= ContentMap:~p~n", [ContentMap]),
	List = maps:to_list(ContentMap),
	check_row_core(List, BeginRow, KeyCloumn, #{}).

check_row_core([], _BeginRow, _KeyCloumn, AccMap) -> AccMap;
check_row_core([{Row, Cells} | T], BeginRow, KeyCloumn, AccMap) when Row =< BeginRow ->
	check_row_core(T, BeginRow, KeyCloumn, maps:put(Row, Cells, AccMap));
check_row_core([{Row, [#excel_cell{r = Row}|_] = Cells} | T], BeginRow, KeyCloumn, AccMap) ->
	CheckRes = check_cloumn_exists(KeyCloumn, Cells),
	CheckRes ==	false andalso throw({false, Row, lost_key_cloumn}),
	check_row_core(T, BeginRow, KeyCloumn, maps:put(Row, Cells, AccMap));
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

