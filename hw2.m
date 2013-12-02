(* ::Package:: *)

(* 
   Implementation of BOS state. 
   The state is represented by the list of pairs { varName, value }.
*)
ClearAll[oneStep];
initState[] :=
    {}; 

put[state_, varName_String, value_] :=
    Sow[
    Append[DeleteCases[state, {varName, _}], {varName, value}]
    ]; 

get[state_, varName_String] :=
    If[ MemberQ[state, {varName, _}],
        Last[First[Cases[state, {varName, _}]]],
        Null
    ];
intQ[i_]:=IntegerQ[i]
doubleQ[d_]:=NumberQ[d] && Not[IntegerQ[d]]


(*Big-Step Operational Semantics*)
(*2*)oneStep[\[Sigma]_,CBlock[{s___}]]:=oneStep[\[Sigma],{s}];

(*3*)oneStep[\[Sigma]_,{}]:={\[Sigma],Null};

(*4*)oneStep[\[Sigma]_,{c_,p___}]:=oneStep[\[Sigma],c];

(*5*)oneStep[\[Sigma]_,CDeclare[type_,varName_String]]:={put[initState[],varName,Undefined],Null};

(*8*)oneStep[\[Sigma]_,k_Number]:={\[Sigma],k};

(*9*)oneStep[\[Sigma]_,varName_String]:={\[Sigma],get[initState[],varName]};

(*10*)oneStep[\[Sigma]_,CAssign[varName_String,e_Number]]:={put[initState[],varName,e],e};

(*11*)oneStep[\[Sigma]_,COperator[Minus,e_Number]]:={\[Sigma],-e};

(*12-13*)oneStep[\[Sigma]_,COperator[Not,e_Number]]:=If[e===0,{\[Sigma],1},{\[Sigma],0}];

(*14*)oneStep[\[Sigma]_,COperator[Plus,{e1_Number,e2_Number}]]:={\[Sigma],e1+e2}

(*15*)oneStep[\[Sigma]_,COperator[Subtract,{e1_Number,e2_Number}]]:={\[Sigma],e1-e2}




(*Typing System*)

(*39*)typeOf[\[CapitalGamma]_,CBlock[{stm___}]]:={(* YOUR CODE HERE *)};

(*40*)typeOf[\[CapitalGamma]_,{}]:={(* YOUR CODE HERE *)};

(*41*)typeOf[\[CapitalGamma]_,{n_,stm___}]:={(* YOUR CODE HERE *)};

(*42*)typeOf[\[CapitalGamma]_,CAssign[var_,e_]]:={(* YOUR CODE HERE *)};

(*44*)typeOf[\[CapitalGamma]_,{a:CAssign[var_,e_],stm___}]:={(* YOUR CODE HERE *)};


program={
CDeclare["int","i"],
CAssign["i",0]
};
oneStep[initState[],program]
Reap[oneStep[initState[],program]]





