﻿//  RNGLRGenerator.fs contains implementation of interface Generator
//
//  Copyright 2011-2012 Avdyukhin Dmitry
//
//  This file is part of YaccConctructor.
//
//  YaccConstructor is free software:you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.

namespace Yard.Generators.RNGLR

open Yard.Core
open IL
open Constraints
open Yard.Generators.RNGLR
open InitialConvert
open Yard.Generators.RNGLR.FinalGrammar
open Yard.Generators.YardPrinter
open States
open Printer
open TranslatorPrinter
open Option

type RNGLR() = 
    inherit Generator()
        override this.Name = "RNGLRGenerator"
        override this.Constraints = [|noEbnf; noMeta; noInnerAlt; noLiterals; noInnerAlt; noBrackets; needAC; singleModule|]
        override this.Generate (definition, args) =
            let start = System.DateTime.Now
            let args = args.Split([|' ';'\t';'\n';'\r'|]) |> Array.filter ((<>) "")
            let pairs = Array.zeroCreate <| args.Length / 2
            for i = 0 to pairs.Length-1 do
                pairs.[i] <- args.[i * 2], args.[i * 2 + 1]
            let mutable moduleName = ""
            let mutable tokenType = ""
            let mutable table = LALR
            let mutable fullPath = false
            let mutable positionType = "Microsoft.FSharp.Text.Lexing.Position"
            let mutable needTranslate = true
            let mutable light = true
            let mutable printInfiniteEpsilonPath = ""
            let mutable isRelaxed = false
            let mutable printAnlysis = false
            let mutable output = definition.info.fileName + ".fs"
            let mutable targetLanguage = FSharp
            for opt, value in pairs do
                match opt with
                | "-module" -> moduleName <- value
                | "-token" -> tokenType <- value
                | "-pos" -> positionType <- value
                | "-o" -> if value.Trim() <> "" then output <- value
                | "-table" ->
                    match value with
                    | "LALR" -> table <- LALR
                    | "LR" -> table <- LR
                    | x -> failwith "Unexpected table type %s" x
                | "-fullpath" ->
                    if value = "true" then fullPath <- true
                    elif value = "false" then fullPath <- false
                    else failwith "Unexpected fullPath value %s" value
                | "-translate" ->
                    if value = "true" then needTranslate <- true
                    elif value = "false" then needTranslate <- false
                    else failwith "Unexpected translate value %s" value
                | "-light" ->
                    if value = "on" then light <- true
                    elif value = "off" then light <- false
                    else failwith "Unexpected light value %A" value
                | "-infEpsPath" -> printInfiniteEpsilonPath <- value
                | "-lang" ->
                    targetLanguage <-
                        match value.ToLowerInvariant() with
                        | "fsharp" -> FSharp
                        | "scala" -> Scala
                        | s -> "Language " + s + "is not supported" |> failwith
                | "-relaxed" ->
                    if value = "true" then isRelaxed <- true
                    elif value = "false" then isRelaxed <- false
                    else failwith "Unexpected relaxed value"
                | "-printAnalysis" ->
                    if value = "true" then printAnlysis <- true
                    elif value = "false" then printAnlysis <- false
                    else failwith "Unexpected printAnalysis value"
                // In other cases causes error
                | _ -> failwithf "Unknown option %A" opt
            let newDefinition = initialConvert definition
            let grammar = new FinalGrammar(newDefinition.grammar.[0].rules);

            let printRules () =
                let printSymbol (symbol : int) =
                    if symbol < grammar.indexator.nonTermCount then
                        grammar.indexator.indexToNonTerm symbol
                    elif symbol >= grammar.indexator.termsStart && symbol <= grammar.indexator.termsEnd then
                        grammar.indexator.indexToTerm symbol
                    else grammar.indexator.indexToLiteral symbol
                printfn "\nrules:"
                for i = 0 to grammar.rules.rulesCount-1 do
                    printf "%4d: %s = " i <| printSymbol (grammar.rules.leftSide i)
                    for j = 0 to grammar.rules.length i - 1 do
                        printf "%s " <| printSymbol (grammar.rules.symbol i j)
                    printfn ""
            printRules ()

            if grammar.EpsilonCyclicNonTerms.Length > 0 then
                eprintfn "Grammar contains non-terminals, which can infinitely infer epsilon:"
                grammar.EpsilonCyclicNonTerms
                |> List.map (String.concat " <- ")
                |> List.iter (eprintfn "%s")
                eprintfn ""
                if printInfiniteEpsilonPath <> "" then
                    System.IO.Directory.CreateDirectory printInfiniteEpsilonPath |> ignore
                    for cycle in grammar.EpsilonCyclicNonTerms do
                        let nonTerm = List.head cycle
                        grammar.epsilonTrees.[grammar.indexator.nonTermToIndex nonTerm].AstToDot
                            grammar.indexator.indexToNonTerm (fun _ -> 0) grammar.rules.leftSideArr
                            (System.IO.Path.Combine (printInfiniteEpsilonPath, nonTerm + ".dot"))
                grammar.epsilonTrees |> Array.iter (fun t -> if t <> null then t.EliminateCycles())
            let statesInterpreter = buildStates table grammar
            let mutable tables = new Tables(grammar, statesInterpreter)

            let grammarAnalyzis(table:RelaxedTables) : string  = 
                let mutable result = "// Grammar analyzis \n"
                let pushes = table.attendedPushes
                for i in 0 .. pushes.Length - 1 do
                    if pushes.[i] <> (0, List.Empty) then
                            let symbol = grammar.indexator.indexToTerm (fst pushes.[i])
                            result <- result +  "// State " + (string i) + " accepst only "
                            + symbol + " symbol; \t defalut: push and goto to " + string (snd pushes.[i]) + " \n"
                result
            let mutable grammarAnalyze = ""
            let mutable attendedPushes = null
            let mutable attendedReduces = null
            if isRelaxed then
                let relaxedTable = new RelaxedTables(grammar, statesInterpreter)
                tables.reduces <- relaxedTable.attendedRed
                attendedPushes <- relaxedTable.attendedPushes
                attendedReduces <- relaxedTable.attendedReduces
                if printAnlysis then
                   grammarAnalyze <- grammarAnalyzis(relaxedTable);
            use out = new System.IO.StreamWriter (output)
            let res = new System.Text.StringBuilder()
            let dummyPos = char 0
            let println (x : 'a) =
                Printf.kprintf (fun s -> res.Append(s).Append "\n" |> ignore) x
            let print (x : 'a) =
                Printf.kprintf (fun s -> res.Append(s) |> ignore) x
            let package, _class  =
                        match moduleName with
                        | "" -> "RNGLR","Parse"
                        | s when s.Contains "." -> s.Split '.' |> Array.rev |> (fun a -> a.[0], String.concat "." a.[1..])
                        | s -> "RNGLR",s
            let printHeaders moduleName fullPath light output targetLanguage =
                let fsHeaders() = 
                    println "%s" <| getPosFromSource fullPath dummyPos (defaultSource output)
                    println "module %s"
                    <|  match moduleName with
                        | "" -> "RNGLR.Parse"
                        | s -> s
                    if not light then
                        println "#light \"off\""
                    println "#nowarn \"64\";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type"

                    println "open Yard.Generators.RNGLR.Parser"
                    println "open Yard.Generators.RNGLR"
                    println "open Yard.Generators.RNGLR.AST"

                    match definition.head with
                    | None -> ()
                    | Some (s : Source.t) ->
                        println "%s" <| getPosFromSource fullPath dummyPos s
                        println "%s" <| s.text + getPosFromSource fullPath dummyPos (defaultSource output)

                let scalaHeaders () =

                    println "package %s" package
                    println "//import Yard.Generators.RNGLR.Parser"
                    println "//import Yard.Generators.RNGLR"
                    println "//import Yard.Generators.RNGLR.AST"

                match targetLanguage with
                | FSharp -> fsHeaders()
                | Scala -> scalaHeaders()

            let mutable relaxedPushes : Option<array<int * int>> = Option.None
            let mutable relaxedReduces : Option<array<int*int>> = Option.None
            if isRelaxed then
                relaxedPushes <- Option.Some(Array.create (attendedPushes.GetLength 0) (0,0))
                relaxedReduces <- Option.Some(Array.create (attendedReduces.GetLength 0) (0,0))
                for i = 0 to attendedPushes.Length-1 do
                    relaxedReduces.Value.[i] <- attendedReduces.[i]
                    if not (snd attendedPushes.[i]).IsEmpty then 
                        relaxedPushes.Value.[i] <- fst attendedPushes.[i], (snd attendedPushes.[i]).Head
                    else relaxedPushes.Value.[i] <- 0, 0
            printHeaders moduleName fullPath light output targetLanguage
            let tables = printTables grammar definition.head tables moduleName tokenType res relaxedPushes relaxedReduces targetLanguage _class
            let res = if not needTranslate || targetLanguage = Scala then tables
                        else tables + printTranslator grammar newDefinition.grammar.[0].rules
                                        positionType fullPath output dummyPos

            let res = 
                match definition.foot with
                | None -> res
                | Some (s : Source.t) ->
                    res + (getPosFromSource fullPath dummyPos s + "\n"
                                + s.text + getPosFromSource fullPath dummyPos (defaultSource output) + "\n")
            let res =
                match targetLanguage with
                | FSharp ->
                    let init = res.Replace("\r\n", "\n")
                    let curLine = ref 1// Must be 2, but there are (maybe) some problems with F# compiler, causing to incorrect warning
                    let res = new System.Text.StringBuilder(init.Length * 2)
                    for c in init do
                        match c with
                        | '\n' -> incr curLine; res.Append System.Environment.NewLine
                        | c when c = dummyPos -> res.Append (string !curLine)
                        | x -> res.Append x
                        |> ignore
                    res.ToString()
                | Scala -> res + "\n}"
            out.WriteLine res
            if printAnlysis && isRelaxed then
                out.WriteLine grammarAnalyze
            out.Close()
            eprintfn "Generation time: %A" <| System.DateTime.Now - start
            //(new YardPrinter()).Generate newDefinition
            box ()
        override this.Generate definition = this.Generate (definition, "")
