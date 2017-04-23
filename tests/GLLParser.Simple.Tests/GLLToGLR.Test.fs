module GLLToGLR
open NUnit.Framework
open YaccConstructor.API
open Yard.Generators.GLL.ParserCommon
open AbstractAnalysis.Common
open Yard.Generators.GLL.AbstractParser
open Microsoft.FSharp.Reflection
open Yard.Generators.Common


let grammarFilesPath = @"C:/Users/ilya/Documents/projects/YaccConstructor/tests/GLLParser.Simple.Tests/"


let getParserSource grammarFile =    
    generate grammarFile
                "YardFrontend" "GLLGenerator" 
                (Some "-translate true")
                ["ExpandEbnf"; "ExpandMeta"; "ExpandInnerAlt"; "AddDefaultAC"; "Linearize"] 
                [] :?> ParserSourceGLL
   
let private tokenToString (token : 'a) =
    match FSharpValue.GetUnionFields(token, typeof<'a>) with
        | case, _ -> case.Name.ToUpper()

let getInput tokens stringToGLLToken withErrors =
    let notEof =
        tokenToString >> ((<>) "RNGLR_EOF")
    let stringifiedTokens = 
        tokens
        |> Array.filter notEof
        |> Array.map (tokenToString >> stringToGLLToken)
    if withErrors
    then
        new LinearIputWithErrors(stringifiedTokens, stringToGLLToken "ERROR") :> IParserInput
    else
        new LinearInput(stringifiedTokens) :> IParserInput



let lbl tokenId = tokenId
let edg f t l = new ParserEdge<_>(f,t,lbl l)

let toLinearGraph(tokens: _ seq) = 
    let qGraph = new SimpleInputGraph<_>(0, Seq.length tokens, SimpleCalcWithErrors.tokenToNumber)
    do tokens
        |> Seq.mapi (fun i t -> edg i (i + 1) t)
        |> qGraph.AddVerticesAndEdgeRange
        |> ignore
    qGraph



let testConversion grmmarFile tokens expected (createErrorToken: _ option) rules translate withErrors =
    let qGraph = toLinearGraph <| Seq.ofArray tokens
    match SimpleCalcWithErrors.buildAstAbstract qGraph with 
        | Yard.Generators.ARNGLR.Parser.ParseResult.Success(tree) ->
            printf "AST before:\n"
            printfn "%s" <| tree.StringRepr()
            tree.ChooseSingleAst()
            printf "AST after:\n"
            printfn "%s" <| tree.StringRepr()
            let translateArgs : AST.TranslateArguments<_,_> = {        
                tokenToRange = fun _ -> 0, 0
                zeroPosition = 0
                clearAST = false
                filterEpsilons = true
            }
            let errorDict = new AST.ErrorDictionary<_>()
            let translated = SimpleCalcWithErrors.translate translateArgs tree errorDict
            printfn "Translated %A" translated
        | Yard.Generators.ARNGLR.Parser.ParseResult.Error(_, t, err) -> Assert.Fail(err) 



    let parser = getParserSource <| grammarFilesPath + grmmarFile
    let input = getInput tokens parser.StringToToken withErrors
    let tree = buildAst parser input
    printfn "GLL before:"
    printfn "%s" <|  tree.StringRepr(fun i -> if i = -1 then "-1" else parser.IntToString.[i])
    if withErrors
    then tree.ChooseSingleAst <| (=)(parser.StringToToken "ERROR")

    printfn "GLL after:"
    printfn "%s" <|  tree.StringRepr(fun i -> if i = -1 then "-1" else parser.IntToString.[i])

    let transArguments : ASTGLLFSA.TranslateArguments<_,_,_> = {
        tokenToRange = fun _ -> 0, 0
        zeroPosition = 0
        createErrorToken = createErrorToken
        intToString = fun i -> parser.IntToString.[i]
        rulesInfo = parser.RulesInfo
        rules = rules
        translate = translate
        withErrors = withErrors
    }

    let translated = tree.Translate tokens transArguments :> string list |> List.head
    Assert.AreEqual (expected, translated)
        
    
[<TestFixture>]
type ``GLL to RNGLR conversion test`` () =
//    [<Test>]
//    member test.``Simple calc without errors in input``() =     
//        let tokens = [
//                    SimpleCalcWithoutErrors.NUM "1"
//                    SimpleCalcWithoutErrors.TIMES ""
//                    SimpleCalcWithoutErrors.NUM "2"
//                    SimpleCalcWithoutErrors.TIMES ""
//                    SimpleCalcWithoutErrors.NUM "3"
//                    SimpleCalcWithoutErrors.PLUS ""
//                    SimpleCalcWithoutErrors.NUM "4"
//                    SimpleCalcWithoutErrors.TIMES ""
//                    SimpleCalcWithoutErrors.NUM "5"
//                    SimpleCalcWithoutErrors.RNGLR_EOF ""] |> Array.ofList
//        testConversion
//            @"SimpleCalcWithoutErrors.yrd"
//            tokens
//            "1 * 2 * 3 + 4 * 5"
//            None
//            SimpleCalcWithoutErrors.parserSource.Rules
//            SimpleCalcWithoutErrors.translate
//            false

    [<Test>]
    member test.``Simple calc with errors in input``() =     
        let tokens = [
                    SimpleCalcWithErrors.NUM "1"
                    SimpleCalcWithErrors.TIMES ""
                    SimpleCalcWithErrors.NUM "2"
                    SimpleCalcWithErrors.PLUS ""
                    SimpleCalcWithErrors.NUM "3"
                    SimpleCalcWithErrors.TIMES ""
                    SimpleCalcWithErrors.NUM "4"
                    SimpleCalcWithErrors.PLUS "4"
                    SimpleCalcWithErrors.NUM "3"
                    SimpleCalcWithErrors.TIMES ""
                    SimpleCalcWithErrors.NUM "4"
                    SimpleCalcWithErrors.RNGLR_EOF ""] |> Array.ofList
        testConversion
            @"SimpleCalcWithErrors.yrd"
            tokens
            "1 * 2 + 3 * 4 + 3 * 4"
            (Some SimpleCalcWithErrors.ERROR)
            SimpleCalcWithErrors.parserSource.Rules
            SimpleCalcWithErrors.translate
            true
