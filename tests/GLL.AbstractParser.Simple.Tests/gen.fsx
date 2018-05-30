#I @"../../Bin/Release/v40"

#r @"YC.Common.dll"
#r @"YC.GLLGenerator.dll"
#r @"YC.YardFrontend.dll"
#r @"YC.Conversions.dll"

open Yard.Generators.GLL
open Yard.Frontends.YardFrontend
open Yard.Core.Conversions.ExpandMeta 

module GLLAbstractParserSimpleTests =
    let fe = new YardFrontend()
    let gen = new GLL()
    let meta = new ExpandMeta()

    let generate() = 
        let ilFirst = fe.ParseGrammar "StaticAnalysisV.yrd"
        {ilFirst with grammar = meta.ConvertGrammar(ilFirst.grammar)}
        gen.Generate(ilFirst, true, "-pos int -token int -module GLL.StaticAnalysisV -o StaticAnalysisV.yrd.fs") |> ignore
        
        let ilSecond = fe.ParseGrammar "StaticAnalysisM.yrd"
        {ilSecond with grammar = meta.ConvertGrammar(ilFirst.grammar)}
        gen.Generate(ilSecond, true, "-pos int -token int -module GLL.StaticAnalysisM -o StaticAnalysisM.yrd.fs") |> ignore

GLLAbstractParserSimpleTests.generate()
