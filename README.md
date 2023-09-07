xlsx2erl
=====

将满足一定格式的excel表格转换成erlang的配置表（学习wxwidgets练手项目）

如果觉得对你有帮助，请给我颗小星星吧！

![image](效果图.png)

Build
-----

    ./rebar3 compile


Run
-----
    ./rebar3 shell


Config配置说明：
-----
    [
        {
            xlsx2erl, [
                {header_def, [
                    {1, comment,        string}
                    ,{2, name,          atom}
                    ,{3, data_type,     atom}
                    ,{4, is_key,        int}
                    ,{5, export_erl,    int}
                    ,{6, erl_funs,      list} 
                    ,{7, data_begin,    null}
                ]}
                ,{mod, [
                    {export_erl, erl_funs, xlsx2erl_write, [erl_path, hrl_path]}
                ]}
                ,{sheet_num, 10}
                ,{refresh_time, 2}
            ]
        }
    ].    

定义excel配置表格式：
-------------------
    {header_def, [{行数，标识， 数据类型（该行的数据类型）}]}
    
        {1,comment,string}： 第一行配置表字段为注释，数据类型为string
        {2,name,atom}：      第二行配置表字段名，数据类型为atom，
        {3,data_type,atom}： 第三行配置表字段对应的数据类型（该列的数据类型），数据类型为atom
        {4,is_key,int}：     第四行配置表的主键字段标识，1为主键，0或者不填表示普通字段
        {5,export_erl,int}： 第五行是否导出erlang文件标识，0否1是，默认是1
        {6,erl_funs,list}：  第六行erlang文件生成特殊函数，格式：[{FunName, ArgList, ReturnList},...] ArgList: [字段名1,字段名2...] ReturnList: [字段名1,字段名2...]
        {7,data_begin,null}：第七行数据开始标识，数据从这行开始读取
    
        注意：comment name data_type is_key data_begin这些标识不可以缺失
        数据读取以空行结束，数据中间不能有任何空行
    
    {mod, []} 定义不同导出规则，以及对应的回调模块
    
        {export_erl, erl_funs, xlsx2erl_write, [erl_path, hrl_path]}
        export_erl：对应header_def中的标识，表示由这个字段控制是否导出erlang配置，可以自由定义
        erl_funs：对应导出规则中的特殊生成规则（函数）
        xlsx2erl_write：回调模块，由xlsx2erl_write模块控制生成erlang配置
        [erl_path, hrl_path]：生成文件存放路径key，ui模块读取这个列表生成按钮设置存放路径，具体使用规则由回调模块定义

    {sheet_num, 10} 表示每个excel表只能有10个sheet，超过了会导致数据丢失

    {refresh_time, 2} 表示每隔2秒扫描excel所在路径，读取变更的excel文件



效率：
    10000行左右，只导出默认函数，耗时1-2秒；
    10000行左右，添加2个导出函数，耗时5-6秒；












