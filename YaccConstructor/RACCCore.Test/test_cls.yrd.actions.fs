//this file was generated by RACC
//source grammar:..\Tests\RACC\test_cls\\test_cls.yrd
//date:2/12/2011 10:56:51 AM

module RACC.Actions_Cls

open Yard.Generators.RACCGenerator

let getUnmatched x expectedType =
    "Unexpected type of node\nType " + x.ToString() + " is not expected in this position\n" + expectedType + " was expected." |> failwith

let value x = x.value

let s0 expr = 
    let inner  = 
        match expr with
        | RESeq [x0] -> 
            let (lst) =
                let yardElemAction expr = 
                    match expr with
                    | REClosure(lst) -> 
                        let yardClsAction expr = 
                            match expr with
                            | RESeq [x0] -> 
                                let (m) =
                                    let yardElemAction expr = 
                                        match expr with
                                        | RELeaf tMULT -> tMULT :?> 'a
                                        | x -> getUnmatched x "RELeaf"

                                    yardElemAction(x0)
                                (m)
                            | x -> getUnmatched x "RESeq"

                        List.map yardClsAction lst 
                    | x -> getUnmatched x "REClosure"

                yardElemAction(x0)
            (List.map value lst |> String.concat ";")
        | x -> getUnmatched x "RESeq"
    box (inner)

let ruleToAction = dict [|("s",s0)|]

