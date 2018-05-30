module YC.GLL.Abstarct.Tests.StaticAnalysisBenchmark

open FSharp.Data
open FSharp.Data.JsonExtensions


open QuickGraph
open AbstractAnalysis.Common
open Yard.Generators.GLL.AbstractParser
open Yard.Generators.Common.ASTGLL
open Yard.Generators.GLL.ParserCommon
open NUnit.Framework

let files =
    [|
      "gzip.json"
      "bzip2.json"
      "ls.json"
      "pr.json"
      "wc.json"
     |]

let parseJson(file) =
  JsonValue.Load("/home/ilya/code/Meerkat/core/src/test/resources/static_analysis/" + file)
  
let convert(file) = 
    let json = parseJson file
    let nodes = [for n in json?nodes -> (n?id.AsInteger(), n?value.AsString())] 
    let edges = [for e in json?edges -> (e?from.AsInteger(), e?label.AsString().ToUpper(), e.GetProperty("to").AsInteger())] 
    (nodes, edges)
    
let toGraph file tokenizer = 
    let edg f t (l: string) = 
         new ParserEdge<_>(f, t, tokenizer l) 
    let (nodes, edges) = convert(file)
    let allVs = nodes |> List.map (fun (i,_) -> i * 1<positionInInput>) |> Array.ofList
    let eofV = allVs.Length       
    let g = new SimpleInputGraph<_>(allVs, id)
    
    [|for (f,l,t) in edges -> edg f t l |]
    |> g.AddVerticesAndEdgeRange
    |> ignore
    
   
    g, nodes.Length
        

  
//[<EntryPoint>]
//let main args =
[<TestFixture>]
type ``GLL abstract padrser tests``() =
   [<Test>]
   member this._01_SimpleSPPFTest() = 
       let gV, nodes = toGraph "pr.json" (fun x -> GLL.StaticAnalysisV.stringToToken.[x] |> int)
       let cnt = 1
       let start = System.DateTime.Now
       let rootV =
           [for i in 0..cnt-1 ->
               Yard.Generators.GLL.AbstractParser.getAllRangesForStartState GLL.StaticAnalysisV.parserSource gV
               |> Set.ofSeq
               |> Seq.length]
       printfn "%A" rootV
       
       let timeV = (System.DateTime.Now - start).TotalMilliseconds / (float cnt)
       printfn "%A" timeV
