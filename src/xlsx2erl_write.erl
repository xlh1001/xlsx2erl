%%%----------------------------------------------
%%% @author VG_xiao
%%% @doc
%%% 
%%% @end
%%% Created 2023-08-08 14:51:37
%%%----------------------------------------------
-module(xlsx2erl_write).

-include("xlsx2erl.hrl").


-export([check_export_path/2, write_erl/3]).

% StateMap  #{
% 	excel_path := ExcelPath,
%     header_def := HeaderList,
%     callback_mod := Mods,
%     excel_name := SheetName,
%     % 加上config中的配置的对应所有配置的key及对应路径{mod, [CallbackCfg...]}
%     % CallbackCfg = {_, _, _, PathKey(eg: [erl_path, hrl_path] )} 
%     % eg:
%     erl_path := ErlPath, 
%     hrl_path := HrlPath
%     ...
% }
%%
-callback check_export_path(StateMap :: map(), Record :: tuple()) -> ok.
-callback write_erl(StateMap :: map(), Record :: tuple(), Sheet :: #excel_sheet{}) -> ok.

check_export_path(StateMap, Record) ->
	PathKeyList = xlsx2erl_tool:config_pathkey(Record),
	CheckPath = (PathKeyList -- [erl_path, hrl_path]) == [],
	CheckPath == false andalso throw({false, ?MODULE, miss_path}),
	do_check_export_path(StateMap, PathKeyList),
	ok.

do_check_export_path(_StateMap, []) -> ok;
do_check_export_path(StateMap, [PathKey | T]) ->
	Path = maps:get(PathKey, StateMap, false),
	Path == false andalso throw({false, ?MODULE, miss_path}),
	do_check_export_path(StateMap, T).

write_erl(Map, Record, Sheet) ->
	#{
        hrl_path := HrlPath 
        ,erl_path := ErlPath
        ,header_def := HeaderList
        ,excel_name := FileBaseName
    } = Map,
    {ExportKey, FunKey} = xlsx2erl_tool:config_export_key(Record),
	{BeginRow, _, _} = lists:keyfind(data_begin, 2, HeaderList),
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
		name = NameStr, content = ContentList, fun_map = FuncMap
	} = Sheet,
	RecordName = record_name(NameStr),
	FunList = maps:get(FunKey, FuncMap, []),
	Header = file_header(RecordName, FileBaseName, FunList, NameStr),
	FileName = make_file_name(ErlPath, RecordName, erl),
	WriteHeader = binary_to_list(unicode:characters_to_binary(Header)),
	write_data(FileName, WriteHeader, [binary]),
	make_fun(FunList, FileName, BeginRow, ContentList, RecordName),
	ok.

file_header(RecordName, FileBaseName, FuncMap, SheetName) ->
	Note = "%% 自动生成，不要手动修改 ！！\n",
	FileName = io_lib:format("~ts",[FileBaseName]),
	Comment = lists:concat([Note, "%% excel:", FileName, "\n%% sheet:", SheetName,"\n\n"]),
	Mod = lists:concat([Comment, "-module(", RecordName, ")."]),
	Include = lists:concat([Mod, "\n-include(\"", RecordName, ".hrl\").\n\n"]),
	FunData = export_fun(FuncMap, ""),
	lists:concat([Include, "\n-export([",FunData,"\n]).\n\n"]).

export_fun([], NewAcc) -> NewAcc;
export_fun([#excel_fun{fun_name = FunName, args = Arity} | FuncList], "") ->
	Acc = lists:concat(["\n\t", FunName, "/", erlang:length(Arity)]),
	export_fun(FuncList, Acc);
export_fun([#excel_fun{fun_name = FunName, args = Arity} | FuncList], Acc) ->
	NewAcc = lists:concat([Acc, "\n\t ,", FunName, "/", erlang:length(Arity)]),
	export_fun(FuncList, NewAcc).

make_fun([], _FileName, _Row, _Content, _Record) -> ok;
make_fun([Func = #excel_fun{fun_name = ?DEFAULT_EXPORT_FUN}|T], FileName, Row, Content, Record) ->
	make_defate_fun(FileName, Func, Row, Content, Record),
	make_fun(T, FileName, Row, Content, Record);
make_fun([Func|T], FileName, Row, Content, Record) ->
	make_normal_fun(FileName, Func, Row, Content),
	make_fun(T, FileName, Row, Content, Record).

make_defate_fun(FileName, Func, BeginRow, ContentList, RecordName) ->
	#excel_fun{fun_name = FunName, args = Arity, values = Values} = Func,
	Fun = fun
		({Row, Cells}) when Row >= BeginRow ->
			ArgsData = make_defalt_fun_args(Arity, Cells, ""),
			ValueData = make_defalt_fun_value(Values, Cells, ""),
			Str = concat_fun(FunName, ArgsData, RecordName, ValueData),
			write_data(FileName, Str, [binary, append]);
		(_) -> skip
	end,
	lists:foreach(Fun, ContentList),
	Default = get_defate_fun(FunName, Arity, "false.\n\n"),
	write_data(FileName, Default, [binary, append]),
	ok.
	
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

make_normal_fun(FileName, Func, BeginRow, ContentList) ->
	#excel_fun{fun_name = FunName, args = Arity, values = Values} = Func,
	% xlsx2erl:info("========= Func:~p~n", [Func]),
	Fun = fun
		({Row, Cells}, Acc) when Row >= BeginRow ->
			OutArgs = make_fun_args(Arity, FunName, Cells, []),
			List = proplists:get_value(OutArgs, Acc, []),
			OutValues = make_normal_fun_core(Values, Cells, List, []),
			lists:keystore(OutArgs, 1, Acc, {OutArgs, OutValues});
		(_, Acc) -> Acc
	end,
	FunDataList = lists:foldl(Fun, [], ContentList),
	
	Fun2 = fun({TmpArgs, TmpValues}) ->
		ArgStr = xlsx2erl_tool:list_to_str(TmpArgs),
		ValStr = xlsx2erl_tool:list_to_str(TmpValues),
		Str = concat_fun(FunName, ArgStr, ValStr),
		write_data(FileName, Str, [binary, append])
	end,
	lists:foreach(Fun2, FunDataList),
	Default = get_defate_fun(FunName, Arity, "[].\n\n"),
	write_data(FileName, Default, [binary, append]),
	ok.


make_fun_args([], _FunName, _Cells, Acc) -> 
	lists:reverse(Acc);
make_fun_args([{Cloumn, Name, DataType}|T], FunName, Cells, Acc) ->
	Value = handle_value(DataType, lists:keyfind(Cloumn, #excel_cell.c, Cells)),
	Value == none andalso throw({false, {?MODULE, Name, FunName}, fun_miss_args}),
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
concat_fun(FunName, "", ValueData) ->
	lists:concat(["\n", FunName, "() -> [", ValueData, "]."]);
concat_fun(FunName, ArgsData, ValueData) ->
	lists:concat(["\n", FunName, "(", ArgsData, ") -> [", ValueData, "];"]).


concat_fun(FunName, ArgsData, RecordName, ValueData) ->
	lists:concat(["\n", FunName, "(", ArgsData, ") -> #", RecordName, "{", ValueData, "}; "]).

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