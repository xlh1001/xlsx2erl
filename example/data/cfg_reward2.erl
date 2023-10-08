%% 自动生成，不要手动修改 ！！
%% excel:test-测试.xlsx
%% sheet:reward2

-module(cfg_reward2).
-include("cfg_reward2.hrl").


-export([
	get/1
]).


get(1) -> #cfg_reward2{lev = 1, award = [{1001,5},{1002,5}], tips = <<"这是字符串1"/utf8>>}; 
get(2) -> #cfg_reward2{lev = 2, award = [{1001,6},{1002,6}], tips = <<"这是字符串2"/utf8>>}; 
get(3) -> #cfg_reward2{lev = 3, award = [{1001,5},{1002,6}], tips = <<"这是字符串3"/utf8>>}; 
get(4) -> #cfg_reward2{lev = 4, award = [{1001,6},{1002,7}], tips = <<"这是字符串4"/utf8>>}; 
get(5) -> #cfg_reward2{lev = 5, award = [{1001,5},{1002,7}], tips = <<"这是字符串5"/utf8>>}; 
get(6) -> #cfg_reward2{lev = 6, award = [{1001,6},{1002,8}], tips = <<"这是字符串6"/utf8>>}; 
get(7) -> #cfg_reward2{lev = 7, award = [{1001,5},{1002,8}], tips = <<"这是字符串7"/utf8>>}; 
get(8) -> #cfg_reward2{lev = 8, award = [{1001,6},{1002,9}], tips = <<"这是字符串8"/utf8>>}; 
get(9) -> #cfg_reward2{lev = 9, award = [{1001,5},{1002,9}], tips = <<"这是字符串9"/utf8>>}; 
get(10) -> #cfg_reward2{lev = 10, award = [{1001,6},{1002,10}], tips = <<"这是字符串10"/utf8>>}; 
get(11) -> #cfg_reward2{lev = 11, award = [{1001,5},{1002,10}], tips = <<"这是字符串11"/utf8>>}; 
get(12) -> #cfg_reward2{lev = 12, award = [{1001,6},{1002,11}], tips = <<"这是字符串12"/utf8>>}; 
get(13) -> #cfg_reward2{lev = 13, award = [{1001,5},{1002,11}], tips = <<"这是字符串13"/utf8>>}; 
get(14) -> #cfg_reward2{lev = 14, award = [{1001,6},{1002,12}], tips = <<"这是字符串14"/utf8>>}; 
get(15) -> #cfg_reward2{lev = 15, award = [{1001,5},{1002,12}], tips = <<"这是字符串15"/utf8>>}; 
get(_lev) -> false.

