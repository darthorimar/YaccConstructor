// Implementation file for parser generated by fsyacc
#light "off"
module Yard.Frontends.FsYaccFrontend.Parser
#nowarn "64";; // turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Core
open Microsoft.FSharp.Text.Lexing
open Microsoft.FSharp.Text.Parsing.ParseHelpers
# 1 "Parser.fsy"


open Yard.Core.IL
open Yard.Core.IL.Production


# 15 "Parser.fs"
// This type is the type of tokens accepted by the parser
type token = 
  | HEAD of (string)
  | ACTION_CODE of (string)
  | IDENT of (string)
  | COLON
  | BAR
  | TYPE_KW
  | TYPE of (string)
  | START_KW
  | ASSOC_KW
  | TOKEN_KW
  | DOUBLE_PERC
  | EOF
// This type is used to give symbolic names to token indexes, useful for error messages
type tokenId = 
    | TOKEN_HEAD
    | TOKEN_ACTION_CODE
    | TOKEN_IDENT
    | TOKEN_COLON
    | TOKEN_BAR
    | TOKEN_TYPE_KW
    | TOKEN_TYPE
    | TOKEN_START_KW
    | TOKEN_ASSOC_KW
    | TOKEN_TOKEN_KW
    | TOKEN_DOUBLE_PERC
    | TOKEN_EOF
    | TOKEN_end_of_input
    | TOKEN_error
// This type is used to give symbolic names to token indexes, useful for error messages
type nonTerminalId = 
    | NONTERM__starts
    | NONTERM_alternative
    | NONTERM_yard_list_13
    | NONTERM_yard_item_2
    | NONTERM_rule
    | NONTERM_yard_list_12
    | NONTERM_yard_option_11
    | NONTERM_yard_item_1
    | NONTERM_typedef
    | NONTERM_start
    | NONTERM_assoc
    | NONTERM_token
    | NONTERM_yard_nlist_10
    | NONTERM_yard_option_9
    | NONTERM_s
    | NONTERM_yard_option_8
    | NONTERM_yard_list_7
    | NONTERM_yard_nlist_6
    | NONTERM_yard_list_5
    | NONTERM_yard_list_4
    | NONTERM_yard_option_3

// This function maps tokens to integers indexes
let tagOfToken (t:token) = 
  match t with
  | HEAD _ -> 0 
  | ACTION_CODE _ -> 1 
  | IDENT _ -> 2 
  | COLON  -> 3 
  | BAR  -> 4 
  | TYPE_KW  -> 5 
  | TYPE _ -> 6 
  | START_KW  -> 7 
  | ASSOC_KW  -> 8 
  | TOKEN_KW  -> 9 
  | DOUBLE_PERC  -> 10 
  | EOF  -> 11 

// This function maps integers indexes to symbolic token ids
let tokenTagToTokenId (tokenIdx:int) = 
  match tokenIdx with
  | 0 -> TOKEN_HEAD 
  | 1 -> TOKEN_ACTION_CODE 
  | 2 -> TOKEN_IDENT 
  | 3 -> TOKEN_COLON 
  | 4 -> TOKEN_BAR 
  | 5 -> TOKEN_TYPE_KW 
  | 6 -> TOKEN_TYPE 
  | 7 -> TOKEN_START_KW 
  | 8 -> TOKEN_ASSOC_KW 
  | 9 -> TOKEN_TOKEN_KW 
  | 10 -> TOKEN_DOUBLE_PERC 
  | 11 -> TOKEN_EOF 
  | 14 -> TOKEN_end_of_input
  | 12 -> TOKEN_error
  | _ -> failwith "tokenTagToTokenId: bad token"

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
let prodIdxToNonTerminal (prodIdx:int) = 
  match prodIdx with
    | 0 -> NONTERM__starts 
    | 1 -> NONTERM_alternative 
    | 2 -> NONTERM_yard_list_13 
    | 3 -> NONTERM_yard_list_13 
    | 4 -> NONTERM_yard_item_2 
    | 5 -> NONTERM_rule 
    | 6 -> NONTERM_yard_list_12 
    | 7 -> NONTERM_yard_list_12 
    | 8 -> NONTERM_yard_option_11 
    | 9 -> NONTERM_yard_option_11 
    | 10 -> NONTERM_yard_item_1 
    | 11 -> NONTERM_typedef 
    | 12 -> NONTERM_start 
    | 13 -> NONTERM_assoc 
    | 14 -> NONTERM_token 
    | 15 -> NONTERM_yard_nlist_10 
    | 16 -> NONTERM_yard_nlist_10 
    | 17 -> NONTERM_yard_option_9 
    | 18 -> NONTERM_yard_option_9 
    | 19 -> NONTERM_s 
    | 20 -> NONTERM_yard_option_8 
    | 21 -> NONTERM_yard_option_8 
    | 22 -> NONTERM_yard_list_7 
    | 23 -> NONTERM_yard_list_7 
    | 24 -> NONTERM_yard_nlist_6 
    | 25 -> NONTERM_yard_nlist_6 
    | 26 -> NONTERM_yard_list_5 
    | 27 -> NONTERM_yard_list_5 
    | 28 -> NONTERM_yard_list_4 
    | 29 -> NONTERM_yard_list_4 
    | 30 -> NONTERM_yard_option_3 
    | 31 -> NONTERM_yard_option_3 
    | _ -> failwith "prodIdxToNonTerminal: bad production index"

let _fsyacc_endOfInputTag = 14 
let _fsyacc_tagOfErrorTerminal = 12

// This function gets the name of a token as a string
let token_to_string (t:token) = 
  match t with 
  | HEAD _ -> "HEAD" 
  | ACTION_CODE _ -> "ACTION_CODE" 
  | IDENT _ -> "IDENT" 
  | COLON  -> "COLON" 
  | BAR  -> "BAR" 
  | TYPE_KW  -> "TYPE_KW" 
  | TYPE _ -> "TYPE" 
  | START_KW  -> "START_KW" 
  | ASSOC_KW  -> "ASSOC_KW" 
  | TOKEN_KW  -> "TOKEN_KW" 
  | DOUBLE_PERC  -> "DOUBLE_PERC" 
  | EOF  -> "EOF" 

// This function gets the data carried by a token as an object
let _fsyacc_dataOfToken (t:token) = 
  match t with 
  | HEAD _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | ACTION_CODE _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | IDENT _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | COLON  -> (null : System.Object) 
  | BAR  -> (null : System.Object) 
  | TYPE_KW  -> (null : System.Object) 
  | TYPE _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | START_KW  -> (null : System.Object) 
  | ASSOC_KW  -> (null : System.Object) 
  | TOKEN_KW  -> (null : System.Object) 
  | DOUBLE_PERC  -> (null : System.Object) 
  | EOF  -> (null : System.Object) 
let _fsyacc_gotos = [| 0us; 65535us; 2us; 65535us; 9us; 10us; 15us; 16us; 3us; 65535us; 4us; 5us; 9us; 2us; 15us; 2us; 3us; 65535us; 4us; 4us; 9us; 4us; 15us; 4us; 2us; 65535us; 35us; 40us; 40us; 40us; 2us; 65535us; 10us; 11us; 12us; 13us; 1us; 65535us; 8us; 9us; 2us; 65535us; 10us; 12us; 12us; 12us; 2us; 65535us; 33us; 42us; 42us; 42us; 1us; 65535us; 32us; 33us; 2us; 65535us; 31us; 44us; 44us; 44us; 2us; 65535us; 30us; 46us; 46us; 46us; 5us; 65535us; 18us; 19us; 20us; 21us; 22us; 23us; 25us; 26us; 27us; 28us; 1us; 65535us; 24us; 25us; 1us; 65535us; 0us; 1us; 1us; 65535us; 36us; 37us; 2us; 65535us; 35us; 36us; 40us; 41us; 2us; 65535us; 33us; 34us; 42us; 43us; 2us; 65535us; 31us; 32us; 44us; 45us; 2us; 65535us; 30us; 31us; 46us; 47us; 1us; 65535us; 0us; 30us; |]
let _fsyacc_sparseGotoTableRowOffsets = [|0us; 1us; 4us; 8us; 12us; 15us; 18us; 20us; 23us; 26us; 28us; 31us; 34us; 40us; 42us; 44us; 46us; 49us; 52us; 55us; 58us; |]
let _fsyacc_stateToProdIdxsTableElements = [| 1us; 0us; 1us; 0us; 1us; 1us; 1us; 1us; 1us; 3us; 1us; 3us; 1us; 4us; 1us; 5us; 1us; 5us; 1us; 5us; 1us; 5us; 1us; 5us; 1us; 7us; 1us; 7us; 1us; 8us; 1us; 10us; 1us; 10us; 1us; 11us; 1us; 11us; 1us; 11us; 1us; 12us; 1us; 12us; 1us; 13us; 1us; 13us; 1us; 14us; 1us; 14us; 1us; 14us; 2us; 15us; 16us; 1us; 16us; 1us; 17us; 1us; 19us; 1us; 19us; 1us; 19us; 1us; 19us; 1us; 19us; 1us; 19us; 1us; 19us; 1us; 19us; 1us; 19us; 1us; 20us; 1us; 23us; 1us; 23us; 2us; 24us; 25us; 1us; 25us; 1us; 27us; 1us; 27us; 1us; 29us; 1us; 29us; 1us; 30us; |]
let _fsyacc_stateToProdIdxsTableRowOffsets = [|0us; 2us; 4us; 6us; 8us; 10us; 12us; 14us; 16us; 18us; 20us; 22us; 24us; 26us; 28us; 30us; 32us; 34us; 36us; 38us; 40us; 42us; 44us; 46us; 48us; 50us; 52us; 54us; 57us; 59us; 61us; 63us; 65us; 67us; 69us; 71us; 73us; 75us; 77us; 79us; 81us; 83us; 85us; 88us; 90us; 92us; 94us; 96us; 98us; |]
let _fsyacc_action_rows = 49
let _fsyacc_actionTableElements = [|1us; 16415us; 0us; 48us; 0us; 49152us; 1us; 32768us; 1us; 3us; 0us; 16385us; 1us; 16386us; 2us; 6us; 0us; 16387us; 0us; 16388us; 1us; 32768us; 3us; 8us; 1us; 16393us; 4us; 14us; 1us; 16386us; 2us; 6us; 1us; 16390us; 4us; 15us; 0us; 16389us; 1us; 16390us; 4us; 15us; 0us; 16391us; 0us; 16392us; 1us; 16386us; 2us; 6us; 0us; 16394us; 1us; 32768us; 6us; 18us; 1us; 32768us; 2us; 27us; 0us; 16395us; 1us; 32768us; 2us; 27us; 0us; 16396us; 1us; 32768us; 2us; 27us; 0us; 16397us; 1us; 16402us; 6us; 29us; 1us; 32768us; 2us; 27us; 0us; 16398us; 1us; 16399us; 2us; 27us; 0us; 16400us; 0us; 16401us; 1us; 16412us; 9us; 24us; 1us; 16410us; 8us; 22us; 1us; 32768us; 7us; 20us; 1us; 32768us; 5us; 17us; 1us; 32768us; 10us; 35us; 1us; 16406us; 2us; 7us; 1us; 16405us; 10us; 39us; 1us; 32768us; 11us; 38us; 0us; 16403us; 0us; 16404us; 1us; 16406us; 2us; 7us; 0us; 16407us; 1us; 16408us; 5us; 17us; 0us; 16409us; 1us; 16410us; 8us; 22us; 0us; 16411us; 1us; 16412us; 9us; 24us; 0us; 16413us; 0us; 16414us; |]
let _fsyacc_actionTableRowOffsets = [|0us; 2us; 3us; 5us; 6us; 8us; 9us; 10us; 12us; 14us; 16us; 18us; 19us; 21us; 22us; 23us; 25us; 26us; 28us; 30us; 31us; 33us; 34us; 36us; 37us; 39us; 41us; 42us; 44us; 45us; 46us; 48us; 50us; 52us; 54us; 56us; 58us; 60us; 62us; 63us; 64us; 66us; 67us; 69us; 70us; 72us; 73us; 75us; 76us; |]
let _fsyacc_reductionSymbolCounts = [|1us; 2us; 0us; 2us; 1us; 5us; 0us; 2us; 1us; 0us; 2us; 3us; 2us; 2us; 3us; 1us; 2us; 1us; 0us; 9us; 1us; 0us; 0us; 2us; 1us; 2us; 0us; 2us; 0us; 2us; 1us; 0us; |]
let _fsyacc_productionToNonTerminalTable = [|0us; 1us; 2us; 2us; 3us; 4us; 5us; 5us; 6us; 6us; 7us; 8us; 9us; 10us; 11us; 12us; 12us; 13us; 13us; 14us; 15us; 15us; 16us; 16us; 17us; 17us; 18us; 18us; 19us; 19us; 20us; 20us; |]
let _fsyacc_immediateActions = [|65535us; 49152us; 65535us; 16385us; 65535us; 16387us; 16388us; 65535us; 65535us; 65535us; 65535us; 16389us; 65535us; 16391us; 16392us; 65535us; 16394us; 65535us; 65535us; 16395us; 65535us; 16396us; 65535us; 16397us; 65535us; 65535us; 16398us; 65535us; 16400us; 16401us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 16403us; 16404us; 65535us; 16407us; 65535us; 16409us; 65535us; 16411us; 65535us; 16413us; 16414us; |]
let _fsyacc_reductions ()  =    [| 
# 187 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'a)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (Microsoft.FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : '_starts));
# 196 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'yard_list_13)) in
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 27 "Parser.fsy"
                                    let lst=_1 in let ac=_2 in  PSeq(List.map (fun prod -> {new Production.elem<Source.t, Source.t> with omit=false and rule=prod and binding=None and checker=None}) lst, Some(ac,(0,0))) 
                   )
# 27 "Parser.fsy"
                 : 'alternative));
# 208 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 29 "Parser.fsy"
                                      [] 
                   )
# 29 "Parser.fsy"
                 : 'yard_list_13));
# 218 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'yard_item_2)) in
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'yard_list_13)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 31 "Parser.fsy"
                                     let yard_h=_1 in let yard_t=_2 in  yard_h::yard_t 
                   )
# 31 "Parser.fsy"
                 : 'yard_list_13));
# 230 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 33 "Parser.fsy"
                                          let s=_1 in  PRef((s,(0,0)), None) 
                   )
# 33 "Parser.fsy"
                 : 'yard_item_2));
# 241 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'yard_option_11)) in
            let _4 = (let data = parseState.GetInput(4) in (Microsoft.FSharp.Core.Operators.unbox data : 'alternative)) in
            let _5 = (let data = parseState.GetInput(5) in (Microsoft.FSharp.Core.Operators.unbox data : 'yard_list_12)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 36 "Parser.fsy"
                             let name=_1 in let alt=_4 in let opt_alts=_5 in  { new Rule.t<Source.t, Source.t> with 
                                 name=name 
                                 and args=[] 
                                 and body=if opt_alts=[] then alt else PAlt(alt, List.reduce (fun acc prod -> PAlt(acc, prod)) opt_alts)
                                 and _public=false
                                 and metaArgs=[] } 
                   )
# 36 "Parser.fsy"
                 : 'rule));
# 260 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 43 "Parser.fsy"
                                      [] 
                   )
# 43 "Parser.fsy"
                 : 'yard_list_12));
# 270 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'yard_item_1)) in
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'yard_list_12)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 45 "Parser.fsy"
                                     let yard_h=_1 in let yard_t=_2 in  yard_h::yard_t 
                   )
# 45 "Parser.fsy"
                 : 'yard_list_12));
# 282 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 47 "Parser.fsy"
                                            
                   )
# 47 "Parser.fsy"
                 : 'yard_option_11));
# 292 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 48 "Parser.fsy"
                                        
                   )
# 48 "Parser.fsy"
                 : 'yard_option_11));
# 302 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'alternative)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 50 "Parser.fsy"
                                                    let alt=_2 in alt
                   )
# 50 "Parser.fsy"
                 : 'yard_item_1));
# 313 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'yard_nlist_10)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 52 "Parser.fsy"
                                                            
                   )
# 52 "Parser.fsy"
                 : 'typedef));
# 325 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'yard_nlist_10)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 54 "Parser.fsy"
                                                     let name=_2 in  name 
                   )
# 54 "Parser.fsy"
                 : 'start));
# 336 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'yard_nlist_10)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 56 "Parser.fsy"
                                                      
                   )
# 56 "Parser.fsy"
                 : 'assoc));
# 347 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'yard_option_9)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'yard_nlist_10)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 58 "Parser.fsy"
                                                                   let tokens=_3 in  tokens 
                   )
# 58 "Parser.fsy"
                 : 'token));
# 359 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 60 "Parser.fsy"
                                            let yard_h=_1 in  [yard_h] 
                   )
# 60 "Parser.fsy"
                 : 'yard_nlist_10));
# 370 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'yard_nlist_10)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 62 "Parser.fsy"
                                      let yard_h=_1 in let yard_t=_2 in  yard_h::yard_t 
                   )
# 62 "Parser.fsy"
                 : 'yard_nlist_10));
# 382 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 64 "Parser.fsy"
                                           let yard_item=_1 in  Some yard_item 
                   )
# 64 "Parser.fsy"
                 : 'yard_option_9));
# 393 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 65 "Parser.fsy"
                                       None 
                   )
# 65 "Parser.fsy"
                 : 'yard_option_9));
# 403 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'yard_option_3)) in
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'yard_list_4)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'yard_list_5)) in
            let _4 = (let data = parseState.GetInput(4) in (Microsoft.FSharp.Core.Operators.unbox data : 'start)) in
            let _5 = (let data = parseState.GetInput(5) in (Microsoft.FSharp.Core.Operators.unbox data : 'yard_nlist_6)) in
            let _7 = (let data = parseState.GetInput(7) in (Microsoft.FSharp.Core.Operators.unbox data : 'yard_list_7)) in
            let _8 = (let data = parseState.GetInput(8) in (Microsoft.FSharp.Core.Operators.unbox data : 'yard_option_8)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 69 "Parser.fsy"
                          let h=_1 in let tokens=_2 in let st=_4 in let rules=_7 in  h, List.concat tokens, st, rules 
                   )
# 69 "Parser.fsy"
                 : 'a));
# 420 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 71 "Parser.fsy"
                                                   
                   )
# 71 "Parser.fsy"
                 : 'yard_option_8));
# 430 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 72 "Parser.fsy"
                                        
                   )
# 72 "Parser.fsy"
                 : 'yard_option_8));
# 440 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 74 "Parser.fsy"
                                     [] 
                   )
# 74 "Parser.fsy"
                 : 'yard_list_7));
# 450 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'rule)) in
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'yard_list_7)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 76 "Parser.fsy"
                                    let yard_h=_1 in let yard_t=_2 in  yard_h::yard_t 
                   )
# 76 "Parser.fsy"
                 : 'yard_list_7));
# 462 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'typedef)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 78 "Parser.fsy"
                                             let yard_h=_1 in  [yard_h] 
                   )
# 78 "Parser.fsy"
                 : 'yard_nlist_6));
# 473 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'typedef)) in
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'yard_nlist_6)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 80 "Parser.fsy"
                                     let yard_h=_1 in let yard_t=_2 in  yard_h::yard_t 
                   )
# 80 "Parser.fsy"
                 : 'yard_nlist_6));
# 485 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 82 "Parser.fsy"
                                     [] 
                   )
# 82 "Parser.fsy"
                 : 'yard_list_5));
# 495 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'assoc)) in
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'yard_list_5)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 84 "Parser.fsy"
                                    let yard_h=_1 in let yard_t=_2 in  yard_h::yard_t 
                   )
# 84 "Parser.fsy"
                 : 'yard_list_5));
# 507 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 86 "Parser.fsy"
                                     [] 
                   )
# 86 "Parser.fsy"
                 : 'yard_list_4));
# 517 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'token)) in
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'yard_list_4)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 88 "Parser.fsy"
                                    let yard_h=_1 in let yard_t=_2 in  yard_h::yard_t 
                   )
# 88 "Parser.fsy"
                 : 'yard_list_4));
# 529 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 90 "Parser.fsy"
                                           let yard_item=_1 in  Some yard_item 
                   )
# 90 "Parser.fsy"
                 : 'yard_option_3));
# 540 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 91 "Parser.fsy"
                                       None 
                   )
# 91 "Parser.fsy"
                 : 'yard_option_3));
|]
# 551 "Parser.fs"
let tables () : Microsoft.FSharp.Text.Parsing.Tables<_> = 
  { reductions= _fsyacc_reductions ();
    endOfInputTag = _fsyacc_endOfInputTag;
    tagOfToken = tagOfToken;
    dataOfToken = _fsyacc_dataOfToken; 
    actionTableElements = _fsyacc_actionTableElements;
    actionTableRowOffsets = _fsyacc_actionTableRowOffsets;
    stateToProdIdxsTableElements = _fsyacc_stateToProdIdxsTableElements;
    stateToProdIdxsTableRowOffsets = _fsyacc_stateToProdIdxsTableRowOffsets;
    reductionSymbolCounts = _fsyacc_reductionSymbolCounts;
    immediateActions = _fsyacc_immediateActions;
    gotos = _fsyacc_gotos;
    sparseGotoTableRowOffsets = _fsyacc_sparseGotoTableRowOffsets;
    tagOfErrorTerminal = _fsyacc_tagOfErrorTerminal;
    parseError = (fun (ctxt:Microsoft.FSharp.Text.Parsing.ParseErrorContext<_>) -> 
                              match parse_error_rich with 
                              | Some f -> f ctxt
                              | None -> parse_error ctxt.Message);
    numTerminals = 15;
    productionToNonTerminalTable = _fsyacc_productionToNonTerminalTable  }
let engine lexer lexbuf startState = (tables ()).Interpret(lexer, lexbuf, startState)
let s lexer lexbuf : 'a =
    Microsoft.FSharp.Core.Operators.unbox ((tables ()).Interpret(lexer, lexbuf, 0))
