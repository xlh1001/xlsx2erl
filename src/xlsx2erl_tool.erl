%%%----------------------------------------------
%%% @author VG_xiao
%%% @doc
%%% 
%%% @end
%%% Created 2023-08-08 19:17:18
%%%----------------------------------------------
-module(xlsx2erl_tool).

-export([
    string_to_term/1, 
    make_erl_file/2, 
    make_excel/1, 
    make_tag_menus/1, 
    excel_name/2, 
    change_path/1, 
    list_to_str/1
    ,now/0
    ,cancel_timer/1
]).

-export([
    config_header/0, 
    config_callback/0, 
    config_sheet_num/0,
    config_refresh_time/0
]).

%% term反序列化，string转换为term，e.g., "[{a},1]"  => [{a},1]
string_to_term(String) ->
    %% 将字符串扫描成Token
    case erl_scan:string(String++".") of
    %% 之所以加"."是因为Tokens必须以点标记结束才能被解析函数接受
        {ok, Tokens, _} ->
            case erl_parse:parse_term(Tokens) of
                {ok, Term} -> Term;
                _Err -> undefined
            end;
        _Error ->
            undefined
    end.


make_erl_file(BinPath, SavePath) ->
    file:make_dir(SavePath),
    {ok, NameList} = file:list_dir_all(BinPath),
    make_erl_file(BinPath, SavePath, NameList).

make_erl_file(_BinPath, _SavePath, []) -> ok;
make_erl_file(BinPath, SavePath, [Name|T]) ->
    [ModName|_] = string:tokens(Name, "."),
    FileName = lists:concat([BinPath, "/", Name]),
    case beam_lib:chunks(FileName, [abstract_code]) of
        {ok, {_, [{abstract_code, {_, AC}}]}} ->
            Content = io_lib:format("~ts", [erl_prettypr:format(erl_syntax:from_list(AC))]),
            SaveContent = unicode:characters_to_binary(Content),
            file:write_file(lists:concat([SavePath, ModName, ".erl"]), SaveContent, [binary]);
        _ ->
            io:format("~p no abstract_code ~n",[ModName])
    end,
    make_erl_file(BinPath, SavePath, T).


%% return: [{Id, FileName}|...]
make_excel(ExcelPath) ->
    {MaxId, List} = lists:foldl(fun(File, {Id, Acc}) ->
        case lists:member($~, File) of
            false ->
                {Id + 1, [{Id + 1, filename:basename(File)} | Acc]};
            true ->
                {Id, Acc}
        end
    end, {0, []}, filelib:wildcard("*.xlsx", ExcelPath)),
    {MaxId, lists:reverse(List)}. 

make_tag_menus(Tags) ->
    {MaxId, List} = lists:foldl(fun(Tag, {Id, Acc}) ->
        {Id + 1, [{Id + 1, Tag} | Acc]}
    end, {0, []}, Tags),
    {MaxId, lists:reverse(List)}.

excel_name(Id, ExcelList) ->
    case lists:keyfind(Id, 1, ExcelList) of
        {_, Name} -> Name;
        _ -> false
    end.

change_path(Path) ->
    List = string:replace(Path,"\\","/", all),
    lists:concat(List).



config_header() ->
    List = [
        {1, comment,        string}
        ,{2, name,          string}
        ,{3, data_type,     atom}
        ,{4, is_key,        integer}
        ,{5, export_erl,    integer}
        ,{6, erl_funs,      list}
        ,{7, data_begin,    null}
    ],
    get_config(header_def, List).

config_callback() ->
    List = [
        {export_erl, erl_funs, xlsx2erl_write, [erl_path, hrl_path]}
    ],
    get_config(mod, List).

config_sheet_num() ->
    get_config(sheet_num, 100).

config_refresh_time() ->
    get_config(refresh_time, 5).

get_config(Key, Default) ->
    case application:get_env(xlsx2erl, Key) of
        {ok, Data} -> Data;
        _ -> Default
    end.

list_to_str(List) ->
    list_to_str(List, 0, "").

list_to_str([], _, Str) -> Str;
list_to_str([H | T], Count, "") ->
    list_to_str(T, Count + 1, H);
list_to_str([H | T], Count, Str) when Count < 10 ->
    NewStr = lists:concat([Str, ", ", H]),
    list_to_str(T, Count + 1, NewStr);
list_to_str([H | T], _Count, Str) ->
    NewStr = lists:concat([Str, ",\n ", H]),
    list_to_str(T, 0, NewStr).

now() ->
    erlang:system_time(second).

cancel_timer(Timer) when is_reference(Timer) -> 
    erlang:cancel_timer(Timer);
cancel_timer(_) -> ok.