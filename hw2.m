(* ::Package:: *)

(* 
   Implementation of BOS state. 
   The state is represented by the list of pairs { varName, value }.
*)
ClearAll[oneStep,CAssign];
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

(*9*)oneStep[\[Sigma]_,varName_String]:={\[Sigma],get[initState[],varName]};

(*10*)oneStep[\[Sigma]_,CAssign[varName_String,e_]]:={put[initState[],varName,e],e};
(*oneStep[\[Sigma]_,CAssign[varName_String,e_]]:={put[initState[],varName,e],CAssign[varName,e]};
CAssign[varName_String,e_]:=e;
*)
(*oneStep[\[Sigma]_,CAssign[varName_String,e_]]:={CAssign[varName,e],e};
CAssign[varName_String,e_]:=put[initState[],varName,e];*)
(*11*)oneStep[\[Sigma]_,COperator[Minus,e_]]:={\[Sigma],-e};

(*12-13*)oneStep[\[Sigma]_,COperator[Not,e_]]:=If[e===0,{\[Sigma],1},{\[Sigma],0}];

(*14*)oneStep[\[Sigma]_,COperator[Plus,{e1_,e2_}]]:={\[Sigma],e1+e2};
	(*oneStep[\[Sigma]_,COperator[Plus,{e1_,CAssign[varName_String,e_]}]]:={\[Sigma],e1+{"x",1}};*)

(*15*)oneStep[\[Sigma]_,COperator[Subtract,{e1_,e2_}]]:={\[Sigma],e1-e2};

(*16*)oneStep[\[Sigma]_,COperator[Times,{e1_,e2_}]]:={\[Sigma],e1*e2};

(*17-18*)oneStep[\[Sigma]_,COperator[Divide,{e1_Integer,e2_Integer}]]:=If[Equal[e2,0],{\[Sigma],$Failed},{\[Sigma],e1/e2}];
		oneStep[\[Sigma]_,COperator[Divide,{e1_Real,e2_Real}]]:=If[Equal[e2,0],{\[Sigma],$Failed},{\[Sigma],e1/e2}];

(*19-20*)oneStep[\[Sigma]_,COperator[Greater,{e1_,e2_}]]:=If[Greater[e1,e2],{\[Sigma],1},{\[Sigma],0}];

(*21-22*)oneStep[\[Sigma]_,COperator[GreaterEqual,{e1_,e2_}]]:=If[GreaterEqual[e1,e2],{\[Sigma],1},{\[Sigma],0}];

(*23-24*)oneStep[\[Sigma]_,COperator[Less,{e1_,e2_}]]:=If[Less[e1,e2],{\[Sigma],1},{\[Sigma],0}];

(*25-26*)oneStep[\[Sigma]_,COperator[LessEqual,{e1_,e2_}]]:=If[LessEqual[e1,e2],{\[Sigma],1},{\[Sigma],0}];

(*27-28*)oneStep[\[Sigma]_,COperator[Equal,{e1_,e2_}]]:=If[Equal[e1,e2],{\[Sigma],1},{\[Sigma],0}];

(*29-30*)oneStep[\[Sigma]_,COperator[Unequal,{e1_,e2_}]]:=If[Unequal[e1,e2],{\[Sigma],1},{\[Sigma],0}];

(*31-32-33*)oneStep[\[Sigma]_,COperator[And,{e1_,e2_}]]:=If[(e1===0||(e1=!=0&&e2===0)),{\[Sigma],0},{\[Sigma],1}];

(*34-35-36*)oneStep[\[Sigma]_,COperator[Or,{e1_,e2_}]]:=If[(e1=!=0||(e1===0&&e2=!=0)),{\[Sigma],1},{\[Sigma],0}];






(*Typing System*)

(*39*)typeOf[\[CapitalGamma]_,CBlock[{stm___}]]:={(* YOUR CODE HERE *)};

(*40*)typeOf[\[CapitalGamma]_,{}]:={(* YOUR CODE HERE *)};

(*41*)typeOf[\[CapitalGamma]_,{n_,stm___}]:={(* YOUR CODE HERE *)};

(*42*)typeOf[\[CapitalGamma]_,CAssign[var_,e_]]:={(* YOUR CODE HERE *)};

(*44*)typeOf[\[CapitalGamma]_,{a:CAssign[var_,e_],stm___}]:={(* YOUR CODE HERE *)};


(*program=COperator[Plus[0,CAssign["x",1]]];
(*program=CAssign["x",10];*)
oneStep[initState[],program]
Reap[oneStep[initState[],program]]
CAssign["b",15]
oneStep[initState[],CAssign["h",30]]
*)




