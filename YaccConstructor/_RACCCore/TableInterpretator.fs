﻿// TableInterpretator.fs
//
// Copyright 2009-2010 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

namespace Yard.Generators._RACCGenerator


open Yard.Generators._RACCGenerator.AST

type Item<'state, 'symbol, 'tree> =
    {
        state  : 'state
        symbol : 'symbol        
    }

module  TableInterpreter = 
    let goto tables states symbol = 
        Set.fold 
            (fun buf state -> 
                try 
                    Set.filter (fun (x,y) -> x = hash ((state.itemName,state.position),DSymbol(symbol.name))) tables.gotoSet
                    |> Set.map snd
                    |> Set.map (fun gt -> Set.add  {itemName = fst gt; position = snd gt; forest=state.forest; sTrace = state.sTrace} buf)
                    |> Set.unionMany
                with _ -> buf)
            Set.empty
            states

    let private getDFA tables state = tables.automataDict.[state.itemName]

    let buildItem state= 
        Set.map
            (fun rule -> 
                {
                    state =  {state with position = rule.FromStateID}
                    symbol = 
                        match rule.Symbol with
                        | DSymbol (s) -> s
                        | _           -> failwith "Error 01"
                })

    let getItems tables smb state =
        (getDFA tables state).DRules
        |> Set.filter (fun rule -> rule.FromStateID = state.position && rule.Symbol = DSymbol smb)
        |> buildItem state

    let getPrevItems tables smb state =
        (getDFA tables state).DRules
        |> Set.filter (fun rule -> rule.ToStateID = state.position && rule.Symbol = DSymbol smb)
        |> buildItem state

    let memoize f = 
        let t = new System.Collections.Generic.Dictionary<_,_>()
        fun tables parserState ->        
            let id = hash(parserState)
            let key = parserState
            if t.ContainsKey(key)       
            then             
                t.[key] 
            else     
                let res = f tables parserState
                t.Add(key,res)
                res                     

    let print ps =
        printfn "ParseState:\n"
        printfn "     i = %A\n" ps.i
        printfn "     symbol = %A\n" ps.inpSymbol
        printfn "     statesSet = [\n" 
        Set.iter 
            (fun s -> 
                printfn "         State:\n"
                printfn "             item = %A\n" s.itemName 
                printfn "             position = %A\n" s.position
                printfn "             forest = <<\n"
                List.iter PrintTree s.forest
                printfn "\n             >>\n")
            ps.statesSet
        printfn "     ]\n" 

    let rec climb() = 
        memoize
            (fun tables parserState ->
#if DEBUG
                printfn "\n Climb \n" 
                print parserState
#endif                
                if parserState.inpSymbol.name = "s" && parserState.lexer.Get(parserState.i).name = "EOF" 
                then
                 Set.map
                    (fun  state ->                                                                        
                        {
                            rItem  = state
                            rI     = parserState.i
                            rLexer = parserState.lexer
                        }                    
                    )                    
                    parserState.statesSet
                else    
                let gotoSet = goto tables parserState.statesSet parserState.inpSymbol        
                
                let getTrace state inpSymbol fromID toID = 
                    let dfa = getDFA tables state
                    dfa.DRules
                    |> Set.filter
                           (fun rule -> rule.FromStateID = fromID && rule.ToStateID = toID && rule.Symbol = DSymbol(inpSymbol))
                    |> Set.maxElement
                    |> fun rule -> 
                        if Seq.exists ((=)rule.ToStateID) dfa.DFinaleStates
                        then rule.Label + (Set.filter (fun r -> r.FromStateID = rule.ToStateID) dfa.DRules |> Set.maxElement |> fun x -> x.Label)
                        else rule.Label


                (parse()) tables {parserState with statesSet = gotoSet}
                |> Set.map
                    (fun res ->                                
                        getPrevItems tables parserState.inpSymbol.name res.rItem
                        |> Set.fold
                            (fun buf itm ->                                
                                let _val trace =
                                    {
                                        id    = itm.state.itemName
                                        trace = trace//itm.state.trace
                                        value = NodeV 1
                                    }
                                let node trace forest = (forest , itm.state.itemName, _val trace) |> Node
                                if itm.state.position > 0
                                then
                                    parserState.statesSet            
                                    |> Set.map
                                        (fun state ->
                                            {
                                                rItem  = {itm.state with forest = state.forest @ res.rItem.forest; sTrace = itm.state.sTrace @ [getTrace itm.state parserState.inpSymbol.name itm.state.position res.rItem.position]}
                                                rI     = res.rI
                                                rLexer = parserState.lexer
                                            })                                        
                                    |> Set.union buf
                                else                                             
                                    {
                                        parserState with
                                            inpSymbol = {name = res.rItem.itemName; value = ""} 
                                            i = res.rI                                           
                                            statesSet = 
                                                
                                                Set.map 
                                                    (fun stt -> 
                                                        let trace = [getTrace itm.state parserState.inpSymbol.name itm.state.position res.rItem.position] @ stt.sTrace
                                                        {stt with forest = [stt.forest @ res.rItem.forest |> node trace]
                                                                  sTrace = trace})
                                                    parserState.statesSet
                                    }
                                    |> (climb()) tables                                    
                                    |> Set.union  buf
                            )
                            Set.empty
                    )                    
                |> Set.unionMany)
        
    and parse ()= 
        memoize
            (fun tables parserState ->
#if DEBUG
                printfn "\n Parse \n" 
                print parserState
#endif                        
                let isFinaleState state= 
                    let dfa = tables.automataDict.[state.itemName]
                    Set.exists ((=) (state.position)) dfa.DFinaleStates
                let resPart1 =
                    let buildResult item =                        
                        {
                            rItem  = {item with forest = []}
                            rI     = parserState.i
                            rLexer = parserState.lexer
                        }
                    Set.filter isFinaleState parserState.statesSet
                    |> Set.map buildResult
                        
                let resPart2 =                                                                               
                    let nextLexeme =  parserState.lexer.Get(parserState.i)                        
                    if nextLexeme.name = "EOF"
                    then 
                        Set.empty
                    else
                        let _val item =
                            {
                                id    = item.itemName
                                trace = []
                                value = LeafV nextLexeme
                            }
                        let leaf item = [(nextLexeme.name, _val item) |> Leaf]
                                                        
                        {
                            parserState with 
                                statesSet = 
                                    parserState.statesSet 
                                    |> Set.map (fun stt -> {stt with forest =  leaf stt})
                                inpSymbol = nextLexeme
                                i         = parserState.i + 1
                        }
                        |> (climb()) tables 
                        |> Set.filter (fun res -> not (isFinaleState res.rItem))
                resPart1 + resPart2)

        
    let run lexer tables= 
        let res = 
            (parse()) tables
                {
                    statesSet = Set.singleton {itemName = "s"; position = 0; forest=[]; sTrace = []}
                    inpSymbol = {name = "";value =""}                                    
                    i         = 1
                    lexer     = lexer
                }
        Set.iter 
            (fun x -> 
                printfn "\n result %A \n\n pos: %A \n" x.rItem.itemName x.rI
                List.iter PrintTree x.rItem.forest)
            res
        res