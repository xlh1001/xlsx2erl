%%%----------------------------------------------
%%% @author VG_xiao
%%% @doc
%%% 
%%% @end
%%% Created 2023-08-07 17:23:20
%%%----------------------------------------------
-module(xlsx2erl_read).

%% 参考：https://github.com/ngqlaw/noisy_tools

-include_lib("xmerl/include/xmerl.hrl").
-include("xlsx2erl.hrl").


-export([read_xlsx/1]).


%% return： [#excel_sheet{}]
read_xlsx(File) ->
	{ok, ContentBin} = file:read_file(File),
	{ok, ExcelData} = zip:unzip(ContentBin, [memory]),
	SheetInfos = make_sheet_info(ExcelData),
    SharedTable = make_share_table(ExcelData),
    make_sheet(SheetInfos, ExcelData, SharedTable).

transform_excel(StrKey, Str, ExcelData) ->
	WorkbookBinary = proplists:get_value(StrKey, ExcelData),
    {WorkbookDoc, _} = xmerl_scan:string(erlang:binary_to_list(WorkbookBinary)),
    xmerl_xpath:string(Str, WorkbookDoc).

make_sheet_info(ExcelData) ->
	[#xmlElement{content = SheetsXML}] = transform_excel("xl/workbook.xml", "/workbook/sheets", ExcelData),
	[sheet_info(SheetXML) || SheetXML <- SheetsXML].

sheet_info(#xmlElement{attributes = Attrs}) ->
	{value, #xmlAttribute{value = SheetName}} = lists:keysearch(name, #xmlAttribute.name, Attrs),
    {value, #xmlAttribute{value = SheetIdStr}} = lists:keysearch(sheetId, #xmlAttribute.name, Attrs),
    #excel_sheet{id = list_to_integer(SheetIdStr), name = SheetName}.

make_share_table(ExcelData) ->
	SharedStringXML = transform_excel("xl/sharedStrings.xml", "/sst/si/t", ExcelData),
	%% shared table其中的字符串序号从0开始
	share_table(SharedStringXML, #{}, 0).

share_table([], Map, _) -> Map;
share_table([#xmlElement{content = []} | T], Map, Index) ->
	NewMap = maps:put(Index, "", Map),
	share_table(T, NewMap, Index + 1);
share_table([#xmlElement{content = Content} | T], Map, Index) ->
	[#xmlText{value = Value}|_] = Content,
	NewMap = maps:put(Index, Value, Map),
	share_table(T, NewMap, Index + 1).

make_sheet(SheetInfos, ExcelData, SharedTable) ->
	make_sheet(SheetInfos, ExcelData, SharedTable, []).

make_sheet([], _ExcelData, _SharedTable, SheetList) -> SheetList;
make_sheet([#excel_sheet{id = SheetId} = Sheet | T], ExcelData, SharedTable, SheetList) ->
	KeyStr = lists:concat(["xl/worksheets/sheet", SheetId, ".xml"]),
	[#xmlElement{content = RowsXML}] = transform_excel(KeyStr, "/worksheet/sheetData", ExcelData),
	% file:write_file("../src/RowsXML.txt", io_lib:format("~p", [RowsXML]), [binary, append]),
	Fun = fun(RowXML = #xmlElement{attributes = Attrs}, Acc) ->
		#xmlAttribute{value = RowStr} = lists:keyfind(r, #xmlAttribute.name, Attrs),
		Row = erlang:list_to_integer(RowStr),
		Cells = make_row(RowXML, SharedTable),
		[{Row, Cells} | Acc]
	end,
	List = lists:foldl(Fun, [], RowsXML),
	make_sheet(T, ExcelData, SharedTable, [Sheet#excel_sheet{content = lists:reverse(List)}|SheetList]).

make_row(#xmlElement{content = CellsXML}, SharedTable) ->
 	Fun = fun
 		(#xmlElement{content = []}, Acc) ->
 			Acc;
 		(CellXML, Acc) ->
 			Cell = make_cell(CellXML, SharedTable),
 			[Cell | Acc]
 	end,
 	lists:reverse(lists:foldl(Fun, [], CellsXML)).


make_cell(#xmlElement{attributes = Attrs, content = Content}, SharedTable) ->
	#xmlElement{
	    content = [#xmlText{value = V}]
	} = lists:keyfind(v, #xmlElement.name, Content),

	#xmlAttribute{value = PosStr} = lists:keyfind(r, #xmlAttribute.name, Attrs),
	{Row, Cell} = row_cloumn(PosStr),
	Value =
	    case lists:keyfind(t, #xmlAttribute.name, Attrs) of
	        #xmlAttribute{value = "s"} ->
	            maps:get(list_to_integer(V), SharedTable);
	        _ ->
	            V
	    end,
	#excel_cell{r = Row, c = Cell, v = Value, str = PosStr}.


row_cloumn(PosStr) ->
	{Row, Cloumn} = row_cloumn(PosStr, [], []),
	RowVal = calc_pos(Row, $0, 0, 0),
	CloumnVal = calc_pos(Cloumn, $A, 0, 0),
	{RowVal, CloumnVal}.

row_cloumn([], Row, Cloumn) -> {Row, Cloumn};
row_cloumn([H|T], Row, Cloumn) when H >= $A andalso H =< $Z ->
	row_cloumn(T, Row, [H | Cloumn]);
row_cloumn([H|T], Row, Cloumn) when H >= $0 andalso H =< $9 ->
	row_cloumn(T, [H | Row], Cloumn).

%% 转换成10进制行列坐标	
calc_pos([], _, _, Acc) -> erlang:floor(Acc);
calc_pos([H|T], $A, Index, Acc) ->  
	calc_pos(T, $A, Index + 1, Acc + (H - $A + 1)*math:pow(26, Index));
calc_pos([H|T], $0, Index, Acc) ->  
	calc_pos(T, $0, Index + 1, Acc + (H - $0) * math:pow(10, Index)).