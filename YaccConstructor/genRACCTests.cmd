Main\bin\Release\Main.exe -f YardFrontend -g RACCGenerator -t test_seq.yrd --testpath ..\Tests\RACC\test_seq\
Main\bin\Release\Main.exe -f YardFrontend -g RACCGenerator -t test_alt.yrd --testpath ..\Tests\RACC\test_alt\
Main\bin\Release\Main.exe -f YardFrontend -g RACCGenerator -t test_cls.yrd --testpath ..\Tests\RACC\test_cls\
Main\bin\Release\Main.exe -f YardFrontend -g RACCGenerator -t test_alt_in_cls.yrd --testpath ..\Tests\RACC\test_alt_in_cls\
Main\bin\Release\Main.exe -f YardFrontend -g RACCGenerator -t test_cls_with_tail.yrd --testpath ..\Tests\RACC\test_cls_with_tail\
Main\bin\Release\Main.exe -f YardFrontend -g RACCGenerator -t test_cls_with_head.yrd --testpath ..\Tests\RACC\test_cls_with_head\
Main\bin\Release\Main.exe -f YardFrontend -g RACCGenerator -t test_arithm_glr.yrd --testpath ..\Tests\RACC\test_arithm_glr\
Main\bin\Release\Main.exe -f YardFrontend -g RACCGenerator -t test_l_attr.yrd --testpath ..\Tests\RACC\test_l_attr\
Main\bin\Release\Main.exe -f YardFrontend -g RACCGenerator -t test_simple_checker.yrd --testpath ..\Tests\RACC\test_simple_checker\
Main\bin\Release\Main.exe -f YardFrontend -g RACCGenerator -t test_checker_on_glr.yrd --testpath ..\Tests\RACC\test_checker_on_glr\
Main\bin\Release\Main.exe -f YardFrontend -g RACCGenerator -t test_summator_1.yrd --testpath ..\Tests\RACC\test_summator_1\

RD /S /Q RACCGeneratedTests
mkdir RACCGeneratedTests
move ..\Tests\RACC\test_seq\*.fs  RACCGeneratedTests/
move ..\Tests\RACC\test_alt\*.fs  RACCGeneratedTests/
move ..\Tests\RACC\test_cls\*.fs  RACCGeneratedTests/
move ..\Tests\RACC\test_alt_in_cls\*.fs  RACCGeneratedTests/
move ..\Tests\RACC\test_cls_with_tail\*.fs  RACCGeneratedTests/
move ..\Tests\RACC\test_cls_with_head\*.fs  RACCGeneratedTests/
move ..\Tests\RACC\test_arithm_glr\*.fs  RACCGeneratedTests/
move ..\Tests\RACC\test_l_attr\*.fs  RACCGeneratedTests/
move ..\Tests\RACC\test_simple_checker\*.fs  RACCGeneratedTests/
move ..\Tests\RACC\test_checker_on_glr\*.fs  RACCGeneratedTests/
move ..\Tests\RACC\test_summator_1\*.fs  RACCGeneratedTests/

del RACCCore.Test\test_*

call BatchSubstitute.cmd RACC.Actions RACC.Actions_Alt RACCGeneratedTests\test_alt.yrd.actions.fs >> RACCCore.Test\test_alt.yrd.actions.fs
call BatchSubstitute.cmd RACCGenerator.Tables RACCGenerator.Tables_Alt RACCGeneratedTests\test_alt.yrd.tables.fs >> RACCCore.Test\test_alt.yrd.tables.fs

call BatchSubstitute.cmd RACC.Actions RACC.Actions_Seq RACCGeneratedTests\test_seq.yrd.actions.fs >> RACCCore.Test\test_seq.yrd.actions.fs
call BatchSubstitute.cmd RACCGenerator.Tables RACCGenerator.Tables_Seq RACCGeneratedTests\test_seq.yrd.tables.fs >> RACCCore.Test\test_seq.yrd.tables.fs

call BatchSubstitute.cmd RACC.Actions RACC.Actions_Cls RACCGeneratedTests\test_cls.yrd.actions.fs >> RACCCore.Test\test_cls.yrd.actions.fs
call BatchSubstitute.cmd RACCGenerator.Tables RACCGenerator.Tables_Cls RACCGeneratedTests\test_cls.yrd.tables.fs >> RACCCore.Test\test_cls.yrd.tables.fs

call BatchSubstitute.cmd RACC.Actions RACC.Actions_alt_in_cls RACCGeneratedTests\test_alt_in_cls.yrd.actions.fs >> RACCCore.Test\test_alt_in_cls.yrd.actions.fs
call BatchSubstitute.cmd RACCGenerator.Tables RACCGenerator.Tables_alt_in_cls RACCGeneratedTests\test_alt_in_cls.yrd.tables.fs >> RACCCore.Test\test_alt_in_cls.yrd.tables.fs

call BatchSubstitute.cmd RACC.Actions RACC.Actions_Cls_head RACCGeneratedTests\test_cls_with_head.yrd.actions.fs >> RACCCore.Test\test_cls_with_head.yrd.actions.fs
call BatchSubstitute.cmd RACCGenerator.Tables RACCGenerator.Tables_Cls_head RACCGeneratedTests\test_cls_with_head.yrd.tables.fs >> RACCCore.Test\test_cls_with_head.yrd.tables.fs

call BatchSubstitute.cmd RACC.Actions RACC.Actions_Cls_tail RACCGeneratedTests\test_cls_with_tail.yrd.actions.fs >> RACCCore.Test\test_cls_with_tail.yrd.actions.fs
call BatchSubstitute.cmd RACCGenerator.Tables RACCGenerator.Tables_Cls_tail RACCGeneratedTests\test_cls_with_tail.yrd.tables.fs >> RACCCore.Test\test_cls_with_tail.yrd.tables.fs

call BatchSubstitute.cmd RACC.Actions RACC.Actions_Aritm_glr RACCGeneratedTests\test_arithm_glr.yrd.actions.fs >> RACCCore.Test\test_arithm_glr.yrd.actions.fs
call BatchSubstitute.cmd RACCGenerator.Tables RACCGenerator.Tables_Aritm_glr RACCGeneratedTests\test_arithm_glr.yrd.tables.fs >> RACCCore.Test\test_arithm_glr.yrd.tables.fs

call BatchSubstitute.cmd RACC.Actions RACC.Actions_L_attr RACCGeneratedTests\test_l_attr.yrd.actions.fs >> RACCCore.Test\test_l_attr.yrd.actions.fs
call BatchSubstitute.cmd RACCGenerator.Tables RACCGenerator.Tables_L_attr RACCGeneratedTests\test_l_attr.yrd.tables.fs >> RACCCore.Test\test_l_attr.yrd.tables.fs

call BatchSubstitute.cmd RACC.Actions RACC.Actions_Simple_checker RACCGeneratedTests\test_simple_checker.yrd.actions.fs >> RACCCore.Test\test_simple_checker.yrd.actions.fs
call BatchSubstitute.cmd RACCGenerator.Tables RACCGenerator.Tables_Simple_checker RACCGeneratedTests\test_simple_checker.yrd.tables.fs >> RACCCore.Test\test_simple_checker.yrd.tables.fs

call BatchSubstitute.cmd RACC.Actions RACC.Actions_Checker_on_glr RACCGeneratedTests\test_checker_on_glr.yrd.actions.fs >> RACCCore.Test\test_checker_on_glr.yrd.actions.fs
call BatchSubstitute.cmd RACCGenerator.Tables RACCGenerator.Tables_Checker_on_glr RACCGeneratedTests\test_checker_on_glr.yrd.tables.fs >> RACCCore.Test\test_checker_on_glr.yrd.tables.fs

call BatchSubstitute.cmd RACC.Actions RACC.Actions_Summator_1 RACCGeneratedTests\test_summator_1.yrd.actions.fs >> RACCCore.Test\test_summator_1.yrd.actions.fs
call BatchSubstitute.cmd RACCGenerator.Tables RACCGenerator.Tables_Summator_1 RACCGeneratedTests\test_summator_1.yrd.tables.fs >> RACCCore.Test\test_summator_1.yrd.tables.fs