//this tables was generated by RACC
//source grammar:RaccParser.yrd
//date:4/1/2011 17:10:53

#light "off"
module Yard.Generators.RACCGenerator.Tables

open Yard.Generators.RACCGenerator

type symbol =
    | T_INT
    | NT_list_elem
    | NT_list
    | NT_start
    | NT_raccStart
let getTag smb =
    match smb with
    | T_INT -> 4
    | NT_list_elem -> 3
    | NT_list -> 2
    | NT_start -> 1
    | NT_raccStart -> 0
let getName tag =
    match tag with
    | 4 -> T_INT
    | 3 -> NT_list_elem
    | 2 -> NT_list
    | 1 -> NT_start
    | 0 -> NT_raccStart
    | _ -> failwith "getName: bad tag."
let private autumataDict = 
dict [|(0,{ 
   DIDToStateMap = dict [|(0,(State 0));(1,(State 1));(2,DummyState)|];
   DStartState   = 0;
   DFinaleStates = Set.ofArray [|1|];
   DRules        = Set.ofArray [|{ 
   FromStateID = 0;
   Symbol      = (DSymbol 1);
   Label       = Set.ofArray [|List.ofArray [|(FATrace (TSmbS 0))|]
|];
   ToStateID   = 1;
}
;{ 
   FromStateID = 1;
   Symbol      = Dummy;
   Label       = Set.ofArray [|List.ofArray [|(FATrace (TSmbE 0))|]
|];
   ToStateID   = 2;
}
|];
}
);(1,{ 
   DIDToStateMap = dict [|(0,(State 0));(1,(State 1));(2,DummyState)|];
   DStartState   = 0;
   DFinaleStates = Set.ofArray [|1|];
   DRules        = Set.ofArray [|{ 
   FromStateID = 0;
   Symbol      = (DSymbol 2);
   Label       = Set.ofArray [|List.ofArray [|(FATrace (TSeqS 2));(FATrace (TSmbS 1))|]
|];
   ToStateID   = 1;
}
;{ 
   FromStateID = 1;
   Symbol      = Dummy;
   Label       = Set.ofArray [|List.ofArray [|(FATrace (TSmbE 1));(FATrace (TSeqE 2))|]
|];
   ToStateID   = 2;
}
|];
}
);(2,{ 
   DIDToStateMap = dict [|(0,(State 0));(1,(State 1));(2,(State 2));(3,DummyState);(4,DummyState)|];
   DStartState   = 0;
   DFinaleStates = Set.ofArray [|1;2|];
   DRules        = Set.ofArray [|{ 
   FromStateID = 0;
   Symbol      = (DSymbol 3);
   Label       = Set.ofArray [|List.ofArray [|(FATrace (TSeqS 8));(FATrace (TClsS 3));(FATrace (TSeqS 5));(FATrace (TSmbS 4))|]
|];
   ToStateID   = 1;
}
;{ 
   FromStateID = 1;
   Symbol      = (DSymbol 3);
   Label       = Set.ofArray [|List.ofArray [|(FATrace (TSmbE 4));(FATrace (TSeqE 5));(FATrace (TClsE 3));(FATrace (TSeqE 8))|]
;List.ofArray [|(FATrace (TSmbE 4));(FATrace (TSeqE 5));(FATrace (TSeqS 7));(FATrace (TSmbS 6))|]
|];
   ToStateID   = 2;
}
;{ 
   FromStateID = 1;
   Symbol      = Dummy;
   Label       = Set.ofArray [|List.ofArray [|(FATrace (TSmbE 4));(FATrace (TSeqE 5));(FATrace (TClsE 3));(FATrace (TSeqE 8))|]
;List.ofArray [|(FATrace (TSmbE 4));(FATrace (TSeqE 5));(FATrace (TSeqS 7));(FATrace (TSmbS 6))|]
|];
   ToStateID   = 3;
}
;{ 
   FromStateID = 2;
   Symbol      = (DSymbol 3);
   Label       = Set.ofArray [|List.ofArray [|(FATrace (TSmbE 6));(FATrace (TSeqE 7));(FATrace (TClsE 3));(FATrace (TSeqE 8))|]
;List.ofArray [|(FATrace (TSmbE 6));(FATrace (TSeqE 7));(FATrace (TSeqS 7));(FATrace (TSmbS 6))|]
|];
   ToStateID   = 2;
}
;{ 
   FromStateID = 2;
   Symbol      = Dummy;
   Label       = Set.ofArray [|List.ofArray [|(FATrace (TSmbE 6));(FATrace (TSeqE 7));(FATrace (TClsE 3));(FATrace (TSeqE 8))|]
;List.ofArray [|(FATrace (TSmbE 6));(FATrace (TSeqE 7));(FATrace (TSeqS 7));(FATrace (TSmbS 6))|]
|];
   ToStateID   = 4;
}
|];
}
);(3,{ 
   DIDToStateMap = dict [|(0,(State 0));(1,(State 1));(2,DummyState)|];
   DStartState   = 0;
   DFinaleStates = Set.ofArray [|1|];
   DRules        = Set.ofArray [|{ 
   FromStateID = 0;
   Symbol      = (DSymbol 4);
   Label       = Set.ofArray [|List.ofArray [|(FATrace (TSeqS 10));(FATrace (TSmbS 9))|]
|];
   ToStateID   = 1;
}
;{ 
   FromStateID = 1;
   Symbol      = Dummy;
   Label       = Set.ofArray [|List.ofArray [|(FATrace (TSmbE 9));(FATrace (TSeqE 10))|]
|];
   ToStateID   = 2;
}
|];
}
)|]

let private gotoSet = 
    Set.ofArray [|((0, 0, 1), set [(0, 1)]);((0, 0, 2), set [(1, 1)]);((0, 0, 3), set [(2, 1)]);((0, 0, 4), set [(3, 1)]);((1, 0, 2), set [(1, 1)]);((1, 0, 3), set [(2, 1)]);((1, 0, 4), set [(3, 1)]);((2, 0, 3), set [(2, 1)]);((2, 0, 4), set [(3, 1)]);((2, 1, 3), set [(2, 2)]);((2, 1, 4), set [(3, 1)]);((2, 2, 3), set [(2, 2)]);((2, 2, 4), set [(3, 1)]);((3, 0, 4), set [(3, 1)])|]
    |> dict
let tables = { gotoSet = gotoSet; automataDict = autumataDict }

