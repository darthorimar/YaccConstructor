@echo off
..\..\bin\Release\v%1\YaccConstructor.exe  -g FParsecGenerator  -i ..\..\Tests\FParsec\calc.yrd > log.txt
..\..\bin\Release\v%1\YaccConstructor.exe  -g FParsecGenerator  -i ..\..\Tests\FParsec\literals.yrd > log1.txt