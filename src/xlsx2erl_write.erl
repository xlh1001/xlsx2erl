%%%----------------------------------------------
%%% @author VG_xiao
%%% @doc
%%% 
%%% @end
%%% Created 2023-08-08 14:51:37
%%%----------------------------------------------
-module(xlsx2erl_write).

-include("xlsx2erl.hrl").


-export([write_to_file/3]).


write_to_file(StateMap, File, SheetList) ->
	#{callback_mod := Mods} = StateMap,
	{ExportKey, FunKey, _CallBacMod, PathKeyList} = lists:keyfind(?MODULE, 3, Mods),
	CheckPath = lists:member(hrl_path, PathKeyList) andalso lists:member(erl_path, PathKeyList),
	CheckPath == false andalso throw({false, ?MODULE, miss_path}),
	[write_erl(ExportKey,FunKey, StateMap, File, Sheet) || Sheet <- SheetList],
	ok.

write_erl(ExportKey,FunKey, Map, File, Sheet) ->
	#{
        hrl_path := HrlPath 
        ,erl_path := ErlPath
        ,header_def := HeaderList
    } = Map,
	{BeginRow, _, _} = lists:keyfind(data_begin, 2, HeaderList),
	FileBaseName = filename:basename(File),
	make_hrl(ExportKey, HrlPath, Sheet),
	make_erl(BeginRow, FunKey, ErlPath, FileBaseName, Sheet),
	ok.

record_name(NameStr) ->
	list_to_atom(lists:concat(["cfg_",NameStr])).

make_file_name(Path, RecordName, hrl) ->
	lists:concat([Path, "/", RecordName, ".hrl"]);
make_file_name(Path, RecordName, erl) ->
	lists:concat([Path, "/", RecordName, ".erl"]).


make_hrl(ExportKey, HrlPath, Sheet) ->
	#excel_sheet{name = NameStr, column = CloumnList} = Sheet,
	RecordName = record_name(NameStr),
	Data = make_hrl_data(ExportKey, CloumnList, ""),
	RecordDef = lists:concat(["-record(", RecordName, ", {", Data, "\n})."]),
	FileName = make_file_name(HrlPath, RecordName, hrl),
	% file:delete(FileName)
	write_data(FileName, unicode:characters_to_binary(RecordDef), [binary]),
	ok.

make_hrl_data(_ExportKey, [], Acc) -> Acc;
make_hrl_data(ExportKey, [{_Cloumn, FieldMap} | CloumnList], Acc) ->
	IsExport = maps:get(ExportKey, FieldMap, ?EXPORT),
	NewAcc = make_hrl_data_core(IsExport, FieldMap, Acc),
	make_hrl_data(ExportKey, CloumnList, NewAcc).


make_hrl_data_core(?EXPORT, FieldMap, "") ->
	Str = make_hrl_data_helper(FieldMap),
	lists:concat(["\n\t", Str]);
make_hrl_data_core(?EXPORT, FieldMap, Acc) ->
	Str = make_hrl_data_helper(FieldMap),
	concat_acc(Acc, Str, "\n\t,");
make_hrl_data_core(_, _Map, Acc) ->
	Acc.

make_hrl_data_helper(FieldMap) ->
	#{comment := Comment, name := Name, data_type := Type} = FieldMap,
	DefaultValue = default_value(Type),
	lists:concat([Name, " = ", DefaultValue, " 	%% ", Comment]).


default_value(int) -> "0";
default_value(list) -> "[]";
default_value(string) -> "\"\"".


make_erl(BeginRow, FunKey, ErlPath, FileBaseName, Sheet) ->
	#excel_sheet{
		name = NameStr, content = ContentMap, fun_map = FuncMap
	} = Sheet,
	RecordName = record_name(NameStr),
	FunList = maps:get(FunKey, FuncMap, []),
	Header = file_header(RecordName, FileBaseName, FunList, NameStr),
	FileName = make_file_name(ErlPath, RecordName, erl),
	WriteHeader = binary_to_list(unicode:characters_to_binary(Header)),
	write_data(FileName, WriteHeader, [binary]),
	FunData = make_fun(FunList, BeginRow, ContentMap, RecordName, ""),
	write_data(FileName, FunData, [binary, append]),
	ok.

file_header(RecordName, FileBaseName, FuncMap, SheetName) ->
	Note = "%% 自动生成，不要手动修改 ！！\n",
	FileName = io_lib:format("~ts",[FileBaseName]),
	Comment = lists:concat([Note, "%% excel:", FileName, "\n%% sheet:", SheetName,"\n\n"]),
	Mod = lists:concat([Comment, "-module(", RecordName, ")."]),
	Include = lists:concat([Mod, "\n-include(\"", RecordName, ".hrl\").\n\n"]),
	FunData = export_fun(FuncMap, ""),
	lists:concat([Include, "\n-export([",FunData,"\n]).\n"]).

export_fun([], NewAcc) -> NewAcc;
export_fun([#excel_fun{fun_name = FunName, args = Arity} | FuncList], "") ->
	Acc = lists:concat(["\n\t", FunName, "/", erlang:length(Arity)]),
	export_fun(FuncList, Acc);
export_fun([#excel_fun{fun_name = FunName, args = Arity} | FuncList], Acc) ->
	NewAcc = lists:concat([Acc, "\n\t ,", FunName, "/", erlang:length(Arity)]),
	export_fun(FuncList, NewAcc).

make_fun([], _Row, _Content, _Record, Acc) -> Acc;
make_fun([#excel_fun{fun_name = ?DEFAULT_EXPORT_FUN} = Func|T], Row, Content, Record, Acc) ->
	FunData = make_defate_fun(Func, Row, Content, Record),
	NewAcc = lists:concat([Acc, FunData]),
	make_fun(T, Row, Content, Record, NewAcc);
make_fun([Func|T], Row, Content, Record, Acc) ->
	FunData = make_normal_fun(Func, Row, Content),
	NewAcc = lists:concat([Acc, FunData]),
	make_fun(T, Row, Content, Record, NewAcc).

make_defate_fun(Func, BeginRow, ContentMap, RecordName) ->
	#excel_fun{fun_name = FunName, args = Arity, values = Values} = Func,
	Fun = fun
		(Row, Cells, Acc) when Row >= BeginRow ->
			ArgsData = make_defalt_fun_args(Arity, Cells, ""),
			ValueData = make_defalt_fun_value(Values, Cells, ""),
			concat_fun(FunName, Acc, ArgsData, RecordName, ValueData);
		(_Row, _Cells, Acc) -> Acc
	end,
	FunData = maps:fold(Fun, "\n\n", ContentMap),
	Default = get_defate_fun(FunName, Arity, "false."),
	lists:concat([FunData, Default]).
	
make_defalt_fun_args([], _Cells, Acc) -> Acc;
make_defalt_fun_args([{Cloumn, _Name, DataType} | ArgsPos], Cells, Acc) ->
	Value = handle_value(DataType, lists:keyfind(Cloumn, #excel_cell.c, Cells)),
	make_defalt_fun_args(ArgsPos, Cells, concat_acc(Acc, Value)).

make_defalt_fun_value([], _Cells, Acc) -> Acc;
make_defalt_fun_value([{Cloumn, Name, DataType} | ValuePos], Cells, Acc) ->
	Value = handle_value(DataType, lists:keyfind(Cloumn, #excel_cell.c, Cells)),
	if
		Value == none ->
			make_defalt_fun_value(ValuePos, Cells, Acc);
		true ->
			Data = lists:concat([Name, " = ", Value]),
			make_defalt_fun_value(ValuePos, Cells, concat_acc(Acc, Data))
	end.

make_normal_fun(Func, BeginRow, ContentMap) ->
	#excel_fun{fun_name = FunName, args = Arity, values = Values} = Func,
	% xlsx2erl:info("========= Func:~p~n", [Func]),
	Fun = fun
		(Row, Cells, Acc) when Row >= BeginRow ->
			OutArgs = make_fun_args(Arity, FunName, Cells, []),
			List = maps:get(OutArgs, Acc, []),
			OutValues = make_normal_fun_core(Values, Cells, List, []),
			maps:put(OutArgs, OutValues, Acc);
		(_Row, _Cells, Acc) -> Acc
	end,
	FunDataMap = maps:fold(Fun, #{}, ContentMap),
	
	Fun2 = fun(TmpArgs, TmpValues, Acc) ->
		ArgStr = xlsx2erl_tool:list_to_str(TmpArgs),
		ValStr = xlsx2erl_tool:list_to_str(TmpValues),
		concat_fun(FunName, Acc, ArgStr, ValStr)
	end,
	FunData = maps:fold(Fun2, "\n\n", FunDataMap),
	Default = get_defate_fun(FunName, Arity, "[]."),
	lists:concat([FunData, Default]).


make_fun_args([], _FunName, _Cells, Acc) -> 
	lists:reverse(Acc);
make_fun_args([{Cloumn, Name, DataType}|T], FunName, Cells, Acc) ->
	Value = handle_value(DataType, lists:keyfind(Cloumn, #excel_cell.c, Cells)),
	Value == none andalso throw({false, {Name, FunName}, fun_miss_args}),
	make_fun_args(T, FunName, Cells, [Value | Acc]).

make_normal_fun_core([], _Cells, List, Acc) -> 
	List ++ lists:reverse(Acc);
make_normal_fun_core([{Cloumn, _Name, DataType} | ValuePos], Cells, List, Acc) ->
	case handle_value(DataType, lists:keyfind(Cloumn, #excel_cell.c, Cells)) of
		none ->
			make_normal_fun_core(ValuePos, Cells, List, Acc);
		Value -> 
			case lists:member(Value, List) of
				false -> NewAcc = [Value | Acc];
				_ -> NewAcc = Acc
			end,
			make_normal_fun_core(ValuePos, Cells, List, NewAcc)
	end.

%% 没有参数的函数就不需要默认匹配选项了
concat_fun(FunName, Acc, "", ValueData) ->
	lists:concat([Acc, "\n", FunName, "() -> [", ValueData, "]."]);
concat_fun(FunName, Acc, ArgsData, ValueData) ->
	lists:concat([Acc, "\n", FunName, "(", ArgsData, ") -> [", ValueData, "];"]).


concat_fun(FunName, Acc, ArgsData, RecordName, ValueData) ->
	lists:concat([Acc, "\n", FunName, "(", ArgsData, ") -> #", RecordName, "{", ValueData, "}; "]).

get_defate_fun(_FunName, [], _Default) -> "\n";
get_defate_fun(FunName, ArgsPos, Default) ->
	lists:concat(["\n", FunName, "(", make_defate_fun_args(ArgsPos, ""), ") -> ", Default]).

make_defate_fun_args([], Acc) -> Acc;
make_defate_fun_args([{_Cloumn, Name, _DataType} | ArgsPos], Acc) ->
	NewAcc = concat_acc(Acc, lists:concat(["_", Name])),
	make_defate_fun_args(ArgsPos, NewAcc).

concat_acc(Acc, Data) ->
	concat_acc(Acc, Data, ", ").

concat_acc(Acc, none, _InsertStr) -> Acc;
concat_acc("", Data, _InsertStr) ->
	lists:concat([Data]);
concat_acc(Acc, Data, InsertStr) ->
	lists:concat([Acc, InsertStr, Data]).


handle_value(_, false) -> none;
handle_value(int, #excel_cell{v = ""}) -> 
	0; 
handle_value(int, #excel_cell{v = Value}) ->  
	list_to_integer(Value);
handle_value(list, #excel_cell{v = ""}) -> 
	"[]"; 
handle_value(list, #excel_cell{v = Value}) -> 
	io_lib:format("~w", [xlsx2erl_tool:string_to_term(Value)]);
handle_value(string, #excel_cell{v = ""}) -> 
	"";
handle_value(string, #excel_cell{v = Value}) -> 
	NewVal = binary_to_list(unicode:characters_to_binary(Value)),
	io_lib:format("<<~s/utf8>>", [NewVal]).

write_data(FileName, Data, Option) ->
	ok = file:write_file(FileName, Data, Option),
	ok.