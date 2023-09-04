%% 自动生成，不要手动修改 ！！
%% excel:test-测试.xlsx
%% sheet:reward

-module(cfg_reward).
-include("cfg_reward.hrl").


-export([
	get/1
	 ,get_mul/1
	 ,get_value1/0
	 ,get_value1/1
	 ,get_value2/2
]).



get(1) -> #cfg_reward{lev = 1, award = [{1001,5},{1002,5}], tips = <<"这是字符串1"/utf8>>, value1 = 1, value2 = 3, value3 = 4}; 
get(2) -> #cfg_reward{lev = 2, award = [{1001,6},{1002,6}], tips = <<"这是字符串2"/utf8>>, value1 = 3, value2 = 7, value3 = 24}; 
get(3) -> #cfg_reward{lev = 3, award = [{1001,5},{1002,6}], tips = <<"这是字符串3"/utf8>>, value1 = 5, value2 = 11, value3 = 60}; 
get(4) -> #cfg_reward{lev = 4, award = [{1001,6},{1002,7}], tips = <<"这是字符串4"/utf8>>, value1 = 7, value2 = 15, value3 = 112}; 
get(5) -> #cfg_reward{lev = 5, award = [{1001,5},{1002,7}], tips = <<"这是字符串5"/utf8>>, value1 = 9, value2 = 19, value3 = 180}; 
get(6) -> #cfg_reward{lev = 6, award = [{1001,6},{1002,8}], tips = <<"这是字符串6"/utf8>>, value1 = 11, value2 = 23, value3 = 264}; 
get(7) -> #cfg_reward{lev = 7, award = [{1001,5},{1002,8}], tips = <<"这是字符串7"/utf8>>, value1 = 13, value2 = 27, value3 = 364}; 
get(8) -> #cfg_reward{lev = 8, award = [{1001,6},{1002,9}], tips = <<"这是字符串8"/utf8>>, value1 = 15, value2 = 31, value3 = 480}; 
get(9) -> #cfg_reward{lev = 9, award = [{1001,5},{1002,9}], tips = <<"这是字符串9"/utf8>>, value1 = 17, value2 = 35, value3 = 612}; 
get(10) -> #cfg_reward{lev = 10, award = [{1001,6},{1002,10}], tips = <<"这是字符串10"/utf8>>, value1 = 19, value2 = 39, value3 = 760}; 
get(11) -> #cfg_reward{lev = 11, award = [{1001,5},{1002,10}], tips = <<"这是字符串11"/utf8>>, value1 = 21, value2 = 43, value3 = 924}; 
get(12) -> #cfg_reward{lev = 12, award = [{1001,6},{1002,11}], tips = <<"这是字符串12"/utf8>>, value1 = 23, value2 = 47, value3 = 1104}; 
get(13) -> #cfg_reward{lev = 13, award = [{1001,5},{1002,11}], tips = <<"这是字符串13"/utf8>>, value1 = 25, value2 = 51, value3 = 1300}; 
get(14) -> #cfg_reward{lev = 14, award = [{1001,6},{1002,12}], tips = <<"这是字符串14"/utf8>>, value1 = 27, value2 = 55, value3 = 1512}; 
get(15) -> #cfg_reward{lev = 15, award = [{1001,5},{1002,12}], tips = <<"这是字符串15"/utf8>>, value1 = 29, value2 = 59, value3 = 1740}; 
get(16) -> #cfg_reward{lev = 16, award = [{1001,6},{1002,12}], tips = <<"aadada"/utf8>>, value1 = 31, value2 = 63, value3 = 1984}; 
get(_lev) -> false.


get_mul(1) -> [3, 4];
get_mul(3) -> [7, 24];
get_mul(5) -> [11, 60];
get_mul(7) -> [15, 112];
get_mul(9) -> [19, 180];
get_mul(11) -> [23, 264];
get_mul(13) -> [27, 364];
get_mul(15) -> [31, 480];
get_mul(17) -> [35, 612];
get_mul(19) -> [39, 760];
get_mul(21) -> [43, 924];
get_mul(23) -> [47, 1104];
get_mul(25) -> [51, 1300];
get_mul(27) -> [55, 1512];
get_mul(29) -> [59, 1740];
get_mul(31) -> [63, 1984];
get_mul(_value1) -> [].


get_value1() -> [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31].



get_value1(3) -> [1];
get_value1(7) -> [3];
get_value1(11) -> [5];
get_value1(15) -> [7];
get_value1(19) -> [9];
get_value1(23) -> [11];
get_value1(27) -> [13];
get_value1(31) -> [15];
get_value1(35) -> [17];
get_value1(39) -> [19];
get_value1(43) -> [21];
get_value1(47) -> [23];
get_value1(51) -> [25];
get_value1(55) -> [27];
get_value1(59) -> [29];
get_value1(63) -> [31];
get_value1(_value2) -> [].


get_value2(1, 4) -> [3];
get_value2(3, 24) -> [7];
get_value2(5, 60) -> [11];
get_value2(7, 112) -> [15];
get_value2(9, 180) -> [19];
get_value2(11, 264) -> [23];
get_value2(13, 364) -> [27];
get_value2(15, 480) -> [31];
get_value2(17, 612) -> [35];
get_value2(19, 760) -> [39];
get_value2(21, 924) -> [43];
get_value2(23, 1104) -> [47];
get_value2(25, 1300) -> [51];
get_value2(27, 1512) -> [55];
get_value2(29, 1740) -> [59];
get_value2(31, 1984) -> [63];
get_value2(_value1, _value3) -> [].