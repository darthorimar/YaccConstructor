//this file was generated by RACC
//source grammar:..\Tests\RACC\test_checker_on_glr\\test_checker_on_glr.yrd
//date:2/12/2011 10:57:00 AM

module RACC.Actions_Checker_on_glr

open Yard.Generators.RACCGenerator

let getUnmatched x expectedType =
    "Unexpected type of node\nType " + x.ToString() + " is not expected in this position\n" + expectedType + " was expected." |> failwith

let value x = (x:>Lexeme<string>).value

type OpPrior =
   | T
   | B

let checker curOp lNextOp rNextOp =
    match curOp, lNextOp, rNextOp with 
    | B,_,T 
    | T,T,T -> true 
    | _,_,_ -> false

let s0 expr = 
    let inner  = 
        match expr with
        | RESeq [x0] -> 
            let (res:int,nextOp:OpPrior) =
                let yardElemAction expr = 
                    match expr with
                    | RELeaf e -> (e :?> _ ) 
                    | x -> getUnmatched x "RELeaf"

                yardElemAction(x0)
            (res)
        | x -> getUnmatched x "RESeq"
    box (inner)
let e1 expr = 
    let inner  = 
        match expr with
        | REAlt(Some(x), None) -> 
            let yardLAltAction expr = 
                match expr with
                | RESeq [x0] -> 
                    let (n) =
                        let yardElemAction expr = 
                            match expr with
                            | RELeaf tNUMBER -> tNUMBER :?> 'a
                            | x -> getUnmatched x "RELeaf"

                        yardElemAction(x0)
                    (value n |> int, T)
                | x -> getUnmatched x "RESeq"

            yardLAltAction x 
        | REAlt(None, Some(x)) -> 
            let yardRAltAction expr = 
                match expr with
                | RESeq [x0; x1; x2] -> 
                    let (l,lNextOp) =
                        let yardElemAction expr = 
                            match expr with
                            | RELeaf e -> (e :?> _ ) 
                            | x -> getUnmatched x "RELeaf"

                        yardElemAction(x0)
                    let (op,curOp) =
                        let yardElemAction expr = 
                            match expr with
                            | REAlt(Some(x), None) -> 
                                let yardLAltAction expr = 
                                    match expr with
                                    | RESeq [_] -> 

                                        ( (+),B )
                                    | x -> getUnmatched x "RESeq"

                                yardLAltAction x 
                            | REAlt(None, Some(x)) -> 
                                let yardRAltAction expr = 
                                    match expr with
                                    | REAlt(Some(x), None) -> 
                                        let yardLAltAction expr = 
                                            match expr with
                                            | RESeq [_] -> 

                                                ( ( * ),T )
                                            | x -> getUnmatched x "RESeq"

                                        yardLAltAction x 
                                    | REAlt(None, Some(x)) -> 
                                        let yardRAltAction expr = 
                                            match expr with
                                            | RESeq [_] -> 

                                                ( (-),B )
                                            | x -> getUnmatched x "RESeq"

                                        yardRAltAction x 
                                    | x -> getUnmatched x "REAlt"

                                yardRAltAction x 
                            | x -> getUnmatched x "REAlt"

                        yardElemAction(x1)
                    let (r,rNextOp) =
                        let yardElemAction expr = 
                            match expr with
                            | RELeaf e -> (e :?> _ ) 
                            | x -> getUnmatched x "RELeaf"

                        yardElemAction(x2)
                    if not (checker curOp lNextOp rNextOp) then raise Constants.CheckerFalse

                    ((op l r),curOp)
                | x -> getUnmatched x "RESeq"

            yardRAltAction x 
        | x -> getUnmatched x "REAlt"
    box (inner)

let ruleToAction = dict [|("e",e1); ("s",s0)|]

