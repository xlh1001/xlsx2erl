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
		name = NameStr, content = ContentMap, column = CloumnList, fun_map = FuncMap
	} = Sheet,
	RecordName = record_name(NameStr),
	FunList = maps:get(FunKey, FuncMap, []),
	Header = file_header(RecordName, FileBaseName, FunList, NameStr),
	FileName = make_file_name(ErlPath, RecordName, erl),
	WriteHeader = binary_to_list(unicode:characters_to_binary(Header)),
	write_data(FileName, WriteHeader, [binary]),
	FunData = make_fun(BeginRow, ContentMap, CloumnList, RecordName, FunList, ""),
	write_data(FileName, FunData, [binary, append]),
	ok.
	

write_data(FileName, Data, Option) ->
	ok = file:write_file(FileName, Data, Option),
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


make_defate_fun(Func, BeginRow, ContentMap, RecordName) ->
	#excel_fun{fun_name = FunName, args = Arity, values = Values} = Func,
	Fun = fun
		(Row, Cells, Acc) when Row >= BeginRow ->
			ArgsData = make_fun_args(Arity, Cells, ""),
			ValueData = make_fun_value(FunName, Values, Cells, ""),
			concat_fun(FunName, Acc, ArgsData, RecordName, ValueData);
		(_Row, _Cells, Acc) -> Acc
	end,
	FunData = maps:fold(Fun, "\n\n", ContentMap),
	Default = get_defate_fun(FunName, ArgsPos),
	lists:concat([FunData, Default]).
	



















make_fun(_BeginRow, _ContentMap, _CloumnList, _RecordName, [], Acc) -> Acc;
make_fun(BeginRow, ContentMap, CloumnList, RecordName, [Func | FuncList], Acc) ->
	TmpData = make_fun_data(BeginRow, ContentMap, CloumnList, RecordName, Func),
	make_fun(BeginRow, ContentMap, CloumnList, RecordName, FuncList, lists:concat([Acc, TmpData])).

make_fun_data(BeginRow, ContentMap, CloumnList, RecordName, Func) ->
	#excel_fun{fun_name = FunName, args = Arity, values = Values} = Func,
	ArgsPos = transform_args(CloumnList, Arity, []),
	ValuePos = transform_args(CloumnList, Values, []),
	% xlsx2erl:info("========= Func:~p~nArgsPos:~p~nValuePos:~p~n", [Func, ArgsPos, ValuePos]),
	Fun = fun
		(Row, Cells, Acc) when Row >= BeginRow ->
			ArgsData = make_fun_args(ArgsPos, Cells, ""),
			ValueData = make_fun_value(FunName, ValuePos, Cells, ""),
			concat_fun(FunName, Acc, ArgsData, RecordName, ValueData);
		(_Row, _Cells, Acc) -> Acc
	end,
	FunData = maps:fold(Fun, "\n\n", ContentMap),
	Default = get_defate_fun(FunName, ArgsPos),
	lists:concat([FunData, Default]).

concat_fun(?DEFAULT_EXPORT_FUN, Acc, ArgsData, RecordName, ValueData) ->
	lists:concat([Acc, "\n", ?DEFAULT_EXPORT_FUN, "(", ArgsData, ") -> #", RecordName, "{", ValueData, "}; "]);
concat_fun(FunName, Acc, ArgsData, _RecordName, ValueData) ->
	lists:concat([Acc, "\n", FunName, "(", ArgsData, ") -> [", ValueData, "];"]).


get_defate_fun(?DEFAULT_EXPORT_FUN, ArgsPos) ->
	lists:concat(["\n", ?DEFAULT_EXPORT_FUN, "(", make_defate_fun_args(ArgsPos, ""), ") -> false."]);
get_defate_fun(FunName, ArgsPos) ->
	lists:concat(["\n", FunName, "(", make_defate_fun_args(ArgsPos, ""), ") -> []."]).

make_defate_fun_args([], Acc) -> Acc;
make_defate_fun_args([{_, #{name := Name}} | ArgsPos], Acc) ->
	make_defate_fun_args(ArgsPos, concat_acc(Acc, lists:concat(["_", Name]))).

make_fun_args([], _Cells, Acc) -> Acc;
make_fun_args([{Cloumn, _Name, DataType} | ArgsPos], Cells, Acc) ->
	Value = handle_value(DataType, lists:keyfind(Cloumn, #excel_cell.c, Cells)),
	make_fun_args(ArgsPos, Cells, concat_acc(Acc, Value)).

make_fun_value(_, [], _Cells, Acc) -> Acc;
make_fun_value(?DEFAULT_EXPORT_FUN, [{Cloumn, Name, DataType} | ValuePos], Cells, Acc) ->
	Value = handle_value(DataType, lists:keyfind(Cloumn, #excel_cell.c, Cells)),
	if
		Value == none ->
			make_fun_value(?DEFAULT_EXPORT_FUN, ValuePos, Cells, Acc);
		true ->
			Data = lists:concat([Name, " = ", Value]),
			make_fun_value(?DEFAULT_EXPORT_FUN, ValuePos, Cells, concat_acc(Acc, Data))
	end;
make_fun_value(_F, [{Cloumn, #{data_type := DataType}} | ValuePos], Cells, Acc) ->
	Value = handle_value(DataType, lists:keyfind(Cloumn, #excel_cell.c, Cells)),
	make_fun_value(_F, ValuePos, Cells, concat_acc(Acc, Value)).

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
	io_lib:format("<<~s/utf8>>", [binary_to_list(unicode:characters_to_binary(Value))]).

transform_args(_CloumnList, [], Acc) -> Acc;
transform_args(CloumnList, [Arg | T], Acc) ->
	case transform_args_core(CloumnList, Arg) of
		false -> transform_args(CloumnList, T, Acc);
		Element -> transform_args(CloumnList, T, [Element | Acc])
	end.

transform_args_core([], _Arity) -> false;
transform_args_core([{_Cloumn, #{name := Arity}} = H | _], Arity) ->
	H;
transform_args_core([_ | CloumnList], Arity) ->
	transform_args_core(CloumnList, Arity).
