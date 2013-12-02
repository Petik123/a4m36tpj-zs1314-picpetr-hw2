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

(*8*)oneStep[\[Sigma]_,k_]:={\[Sigma],k};

(*9*)oneStep[\[Sigma]_,varName_String]:={\[Sigma],{varName,get[initState[],varName]}};

(*10*)oneStep[\[Sigma]_,CAssign[varName_String,e_]]:={put[initState[],varName,e],e};


(*Typing System*)

(*39*)typeOf[\[CapitalGamma]_,CBlock[{stm___}]]:={(* YOUR CODE HERE *)};

(*40*)typeOf[\[CapitalGamma]_,{}]:={(* YOUR CODE HERE *)};

(*41*)typeOf[\[CapitalGamma]_,{n_,stm___}]:={(* YOUR CODE HERE *)};

(*42*)typeOf[\[CapitalGamma]_,CAssign[var_,e_]]:={(* YOUR CODE HERE *)};

(*44*)typeOf[\[CapitalGamma]_,{a:CAssign[var_,e_],stm___}]:={(* YOUR CODE HERE *)};


(*program="x";
oneStep[initState[],program]
Reap[oneStep[initState[],program]]
*)




