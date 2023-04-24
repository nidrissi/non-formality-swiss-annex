(* ::Package:: *)

(* ::Title:: *)
(*Annex to: "Non-formality of Voronov's Swiss-Cheese operad"*)


(* ::Subtitle:: *)
(*Najib Idrissi (Universit\[EAcute] Paris Cit\[EAcute] and Sorbonne Universit\[EAcute], CNRS, IMJ-PRG, F-75013 Paris, France)*)
(*Renato Vasconcellos Vieira (Universidade de S\[ATilde]o Paulo, ICMC, S\[ATilde]o Carlos, Brasil)*)


(* ::Text:: *)
(*See https://arxiv.org/abs/2303.16979 for the preprint of the main article.*)


BeginPackage["SwissCheese`"]


(* ::Section:: *)
(*Public*)


(* ::Text:: *)
(*We "declare" all the variables and functions that should be made publicly accessible.*)


\[GothicO]
\[GothicC]
sign
format
formatTable


check
checkPairEqual


centroid
colors
showSquare
display2d
grid2d
showCube
display
grid
export


compose
composeAt
\[Gamma]


cat
tile


\[CapitalPhi]
\[CapitalPsi]


\[Iota]
swapClosed
\[Sigma]
\[Tau]


id
\[Mu]
\[Alpha]


\[Beta]
\[Eta]


\[Mu]S
\[Alpha]S
\[Beta]S


\[Kappa]
\[Eta]1\[LetterSpace]plus\[LetterSpace]components
\[Eta]1
\[Eta]1\[LetterSpace]components
l


equal\[Beta]1
equal\[Beta]2
equal\[Eta]2
equal\[Eta]\[Beta]


hemisphereComponents


(* ::Section:: *)
(*Private*)


Begin["`Private`"]


(* ::Subsection:: *)
(*Notation*)


\[GothicO]::usage = "\[GothicO][\[PlusMinus]1, \[PlusMinus]1, ...] the open color indexed by a string of \[PlusMinus].\n\[GothicO][] The empty sequence (open color)."; 
\[GothicC]::usage = "\[GothicC][k] the closed color indexed by the integer k."; 


sign[1] = "+"; 
sign[-1] = "-"; 
SetAttributes[sign, Listable]


formatRules = {swapClosed -> Identity, HoldPattern[id[_:0]] -> "\[DoubleStruckOne]", HoldPattern[\[Alpha][_:0]] -> "\[Alpha]", 
    HoldPattern[\[Gamma][p_, q_]] :> Row[Flatten[{p, KeyValueMap[{Subscript["\[SmallCircle]", #1], #2} & , Association[q]]}], " "], HoldPattern[\[Beta][\[Epsilon]_][_]] :> Subscript["\[Beta]", \[Epsilon]], 
    HoldPattern[\[Beta][n_, S_, \[Epsilon]_][_]] :> Subsuperscript["\[Beta]", \[Epsilon], S], HoldPattern[\[Eta][\[Epsilon]_, S_, T_][_]] :> Subscript["\[Eta]", \[Epsilon], S, T], HoldPattern[\[GothicO][]] -> "\[GothicO]", 
    HoldPattern[\[GothicO][s__]] :> Row[sign[{s}]], HoldPattern[\[GothicC][k_]] :> Subscript["\[GothicC]", k], HoldPattern[Cuboid[x1_, x2_]] :> 
     DisplayForm[RowBox[Riffle[MapThread[RowBox[{"[", #1, ",", #2, "]"}] & , {x1, x2}], "\[Times]"]]]}; 


format::usage = "format[expr] formats expr to be displayed in a plot legend."; 
SyntaxInformation[format] ^= {"ArgumentsPattern" -> {_}}; 


format[as_Association] := KeyMap[format, format /@ as]
format[expr_] := expr //. formatRules


formatTable::usage = "formatTable[assoc_] Shows a nested association as a table with headers (avoids weird behavior of Dataset for non-string keys)."; 


formatTable[tab_] := With[{sorted = Map[KeySort, tab, {0, 1}]}, TableForm[Map[format @* Values, sorted, {0, 1}], TableHeadings -> {format[Keys[sorted]], format[Keys[First[sorted]]]}, 
    TableSpacing -> {1, 1}]]


(* ::Subsection:: *)
(*Helpers*)


check[vars_][statement_] := Reduce[ForAll[vars, Implies[And @@ Thread[-1 <= vars <= 1], statement]], Reals]


checkPairEqual[vars_][pair_] := With[{conv = Activate[pair] /. Cuboid -> List}, AllTrue[MapThread[Equal, KeySort /@ conv], check[vars]]]


colors = With[{open = Catenate[Table[\[GothicO] @@@ Tuples[{1, -1}, i], {i, 0, 2}]], closed = \[GothicC] /@ Range[3]}, 
   With[{all = Join[open, closed]}, AssociationThread[all, Join[{LightGray}, ColorData[3, "ColorList"][[{2, 3, 4, 5, 6, 7, 9, 10}]], {Brown}]]]]


showSquare[k_, square_] := {colors[k], square, If[MatchQ[k, _\[GothicC]], Nothing, {GrayLevel[0, 1/3], HatchFilling[Pi/4, 0.1], square}]}


display2d::usage = "display2d[l] displays a 2d configuration of rectangles."; 
SyntaxInformation[display2d] ^= {"ArgumentsPattern" -> {_, OptionsPattern[Graphics]}}; 


display2d[a_Association, options___] := Legended[Show[Graphics[GeometricTransformation[{EdgeForm[Black], KeyValueMap[showSquare, a]}, AffineTransform[{{0, 1}, {1, 0}}]], Axes -> True, 
     Ticks -> None, PlotRange -> {{-1, 1}, {0, 1}}], options], SwatchLegend[colors /@ Keys[a], format[Keys[a]]]]


grid2d::usage = "grid2d[\[Sigma], n] displays a row of n+1 pictures of the 2d configuration \[Sigma] spread regularly in [0, 1]."; 
SyntaxInformation[grid2d] ^= {"ArgumentsPattern" -> {_, _, OptionsPattern[Graphics]}}; 


grid2d[\[Sigma]_, n_, options___] := TableForm[{Table[display2d[\[Sigma][s], options] /. Legended[g_, ___] :> g, {s, -1, 1, 1/n}]}, TableSpacing -> {1, 1}, TableAlignments -> Center, 
   TableHeadings -> {None, (TraditionalForm[HoldForm["t" == #1]] & ) /@ Range[-1, 1, 1/n]}]


showCube[k_, cube_] := {colors[k], If[MatchQ[k, _\[GothicC]], Nothing, HalftoneShading[0.4, GrayLevel[0], "Line"]], cube}


display::usage = "display[l] displays a 3d configuration of cubes."; 
SyntaxInformation[display] ^= {"ArgumentsPattern" -> {_, OptionsPattern[Graphics3D]}}; 


display[a_Association, options___] := Legended[Show[Graphics3D[GeometricTransformation[{Opacity[0.7], EdgeForm[Black], KeyValueMap[showCube, a]}, 
      AffineTransform[{{0, 1, 0}, {0, 0, 1}, {1, 0, 0}}]], Lighting -> "Accent", Boxed -> False, Axes -> True, AxesLabel -> {"y", "z", "x"}, Ticks -> None, 
     PlotRange -> {{-1, 1}, {-1, 1}, {0, 1}}], options], SwatchLegend[colors /@ Keys[a], format /@ Keys[a]]]


grid::usage = "grid[\[Sigma], n] displays a row of (n+1)*(n+1) pictures of the 3d configuration \[Sigma] spread regularly in [0, 1] \[Times] [0, 1]."; 
SyntaxInformation[grid] ^= {"ArgumentsPattern" -> {_, _, OptionsPattern[Graphics3D]}}; 


grid[\[Sigma]_, n_, options___] := TableForm[Table[display[\[Sigma][s, t], ImageSize -> UpTo[Tiny], Axes -> False] /. Legended[g_, ___] :> g, {t, 1, -1, -(1/n)}, {s, -1, 1, 1/n}], 
   TableHeadings -> {(TraditionalForm[HoldForm["t" == #1]] & ) /@ Range[1, -1, -(1/n)], (TraditionalForm[HoldForm["s" == #1]] & ) /@ Range[-1, 1, 1/n]}, TableAlignments -> Center, 
   TableSpacing -> {1, 1}]


export::usage = "export[path, obj] exports obj as a picture in the file at the location path."; 
SyntaxInformation[export] ^= {"ArgumentsPattern" -> {_, _}}; 


export[name_, obj_] := (Export[FileNameJoin[{NotebookDirectory[], "fig", name}], obj, ImageResolution -> 400]; obj)


(* ::Subsection:: *)
(*Structural operations*)


(* ::Text:: *)
(*We use Join[Cuboid[a,b],Cuboid[c,d],2]==Cuboid[Join[a,c],Join[b,d]] to represent the Cartesian product of two cuboids (where a,b,c,d are lists of coordinates).*)


(* ::Input::Initialization:: *)
(c1_Cuboid) \[CircleTimes] (c2_Cuboid) := Join[c1, c2, 2]


(* ::Subsubsection:: *)
(*Operad structure*)


\[Gamma]::usage = "\[Gamma][outer, innerList] composes the elements of innerList into the input of the element outer."; 
SyntaxInformation[\[Gamma]] ^= {"ArgumentsPattern" -> {_, {___}}}; 


compose[Cuboid[x1_, x2_], Cuboid[y1_, y2_]] := Module[{\[Delta], v}, \[Delta] = (x2 - x1)/2; v = (x1 + x2)/2; \[Delta][[1]] *= 2; v[[1]] = x1[[1]]; Cuboid[\[Delta]*y1 + v, \[Delta]*y2 + v]]


fixColors[cols_] := With[{positions = Position[cols, _\[GothicC], {1}, Heads -> False]}, With[{newNames = \[GothicC] /@ Range[Length[positions]]}, 
    ReplacePart[cols, AssociationThread[positions, newNames]]]]


(* ::Text:: *)
(*This is wrong in the general case : \[Epsilon] and \[Phi] should be riffled together (according to how the operations were extended, cf . paper), not always concatenated .*)


joinColors[\[GothicO][\[Epsilon]___], \[GothicO][\[Phi]___]] := \[GothicO][\[Epsilon], \[Phi]]
joinColors[(\[GothicO] | \[GothicC])[a___], (\[GothicO] | \[GothicC])[b___]] := \[GothicC][{a, b}]; 


composeAt::nokey = "Key `1` is not present in `2`"; 
composeAt[\[Sigma]_, \[Tau]_, k_] := With[{new\[Tau] = KeyMap[joinColors[k, #1] & , (compose[\[Sigma][k], #1] & ) /@ \[Tau]]}, With[{new\[Sigma] = Join[Delete[\[Sigma], Key[k]], new\[Tau]]}, 
     With[{newColors = AssociationThread[Keys[new\[Sigma]] -> fixColors[Keys[new\[Sigma]]]]}, KeyMap[newColors, new\[Sigma]]]]] /; KeyExistsQ[\[Sigma], k] || Message[composeAt::nokey, k, \[Sigma]]


\[Gamma][outer_,inner_]/;Length[inner]==0:=outer


\[Gamma][outer_, inner_Association] := KeySort[Fold[composeAt[#1, inner[#2], #2] & , outer, Keys[inner]]]


\[Gamma][outer_, inner_List] := \[Gamma][outer, Association[inner]]


(* ::Subsubsection:: *)
(*Concatenation*)


cat::usage = "cat[{\[Sigma]1, ...}] concatenates several paths into one single path."; 
SyntaxInformation[cat] ^= {"ArgumentsPattern" -> {{__}}}; 


splitInterval[m_] := Partition[Subdivide[-1, 1, m], 2, 1]


cat[\[Sigma]_][s_] := With[{m = Length[\[Sigma]]}, Cuboid @@@ MapThread[PiecewiseExpand[Piecewise[{##1}], Assumptions -> -1 <= s <= 1] & , 
     MapThread[Function[{\[Tau], int}, Map[{#1, s <= int[[2]]} & , \[Tau][m*(s - int[[1]]) - 1], {3}]], {\[Sigma], splitInterval[m]}], 3]]


splitSquare[m_, n_] := Outer[List, splitInterval[m], splitInterval[n], 1]


tile::usage = "tile[{{\[Sigma]11,...}, ...} tiles together several square 2-chains into a single one."; 
SyntaxInformation[tile] ^= {"ArgumentsPattern" -> {{__}..}}; 


tile[\[Sigma]_][s_, t_] := With[{dim = Dimensions[\[Sigma]]}, With[{m = First[dim], n = Last[dim]}, 
    Cuboid @@@ MapThread[PiecewiseExpand[Piecewise[{##1}], Assumptions -> -1 <= s <= 1 && -1 <= t <= 1] & , 
      Catenate[MapThread[Function[{\[Tau], sq}, Map[{#1, s <= sq[[1,2]] && t <= sq[[2,2]]} & , \[Tau][m*(s - sq[[1,1]]) - 1, n*(t - sq[[2,1]]) - 1], {3}]], {\[Sigma], splitSquare[m, n]}, 2]], 3]]]


(* ::Subsubsection:: *)
(*Working with configurations*)


(* ::Text:: *)
(*The variable correctS is necessary because if S={s1, s2,\[Ellipsis]} then Insert[x, -1, S] will insert at s1, then at s2 with s1 already inserted, i.e., s1+1, etc. We must insert at {s1,s2-1,s3-2,...} instead. Another thing to keep in mind is that in the paper, the first index is at 0, then 1, then 2, etc, so we have to add 1 at the end.*)


\[Iota]::usage = "\[Iota][S, Cuboid] grows the cuboid into a higher-dimensional cuboid by adding \[PlusMinus] into the coordinates indexed by S."; 


\[Iota][S_, Cuboid[x_, y_]] := With[{correctS = List /@ (Sort[S] - Range[-1, Length[S] - 2])}, Cuboid[Insert[x, -1, correctS], Insert[y, 1, correctS]]]


\[Iota][S_, as_Association] := (\[Iota][S, #1] & ) /@ as


swapClosed[\[Sigma]_] := KeySort[KeyMap[Replace[{\[GothicC][1] -> \[GothicC][2], \[GothicC][2] -> \[GothicC][1]}], \[Sigma]]]


\[Sigma]::usage = "\[Sigma][\[PlusMinus]1, t] The value of the helper function \[Sigma]."; 
\[Tau]::usage = "\[Tau][{\[PlusMinus]1, \[PlusMinus]1, ...}, {t1, t2, ...}] The value of the helper function \[Tau]."; 


\[Sigma][1, t_] := Min[0, t]
\[Sigma][-1, t_] = -1; 


\[Tau][\[Epsilon]_List, t_List] := Max[1/2, Pick[(1 + t)/2, \[Epsilon], -1]]


(* ::Subsection:: *)
(*Basic elements*)


id::usage = "id[n] The identity in (n+1)d."; 
SyntaxInformation[id] ^= {"ArgumentsPattern" -> {_}}; 


id[n_Integer] := id[n] = Association[\[GothicO][] -> Cuboid[{0}, {1}] \[CircleTimes] Cuboid[ConstantArray[-1, n], ConstantArray[1, n]]]


\[Alpha]::usage = "\[Alpha][1] The action in (n+1)d."; 
SyntaxInformation[\[Alpha]] ^= {"ArgumentsPattern" -> {_,_.}}; 


\[Alpha][n_Integer] := \[Alpha][n] = With[{box = Cuboid[ConstantArray[-1, n], ConstantArray[1, n]]}, Association[\[GothicO][] -> Cuboid[{0}, {1/2}] \[CircleTimes] box, \[GothicC][1] -> Cuboid[{1/2}, {1}] \[CircleTimes] box]]


\[Alpha][n_Integer, S_] := \[Iota][Complement[Range[n], S], \[Alpha][Length[S]]]


\[Mu]::usage="\[Mu][n] The 2^n-fold product in the Swiss-Cheese operad in (n+1) dimensions.";
SyntaxInformation[\[Mu]]^={"ArgumentsPattern"->{_,_.}};


\[Mu][n_Integer] := \[Mu][n] = AssociationMap[Cuboid[{0}, {1}] \[CircleTimes] Cuboid[(1/2)*(List @@ #1 - 1), (1/2)*(List @@ #1 + 1)] & , Tuples[\[GothicO][-1, 1], n]]


\[Mu][n_Integer, S_] := \[Iota][Complement[Range[n], S], \[Mu][Length[S]]]


(* ::Subsubsection:: *)
(*In dimension 2, larger operad*)


\[Kappa]::usage = "\[Kappa][t] The 1-chain used in Livernet's paper"; 
SyntaxInformation[\[Kappa]] ^= {"ArgumentsPattern" -> {_}}; 


\[Kappa]\[LetterSpace]plus\[LetterSpace]components = {Association[\[GothicO][] -> Cuboid[{0, 0}, {(3 - #1)/4, 1}], \[GothicC][1] -> Cuboid[{(1 + #1)/4, -1}, {1, 0}]] & , 
    Association[\[GothicO][] -> Cuboid[{0, (-(1/2))*(1 + #1)}, {1/2, 1}], \[GothicC][1] -> Cuboid[{1/2, -1}, {1, (1 + #1)/2}]] & }; 


\[Kappa][t_] := cat[Join[\[Kappa]\[LetterSpace]plus\[LetterSpace]components, (MapAt[Minus, {All, All, 2}] @* #1 @* Minus & ) /@ Reverse[\[Kappa]\[LetterSpace]plus\[LetterSpace]components]]][t]


(* ::Subsubsection:: *)
(*The chains \[Beta]*)


\[Beta]::usage = "\[Beta][{\[Epsilon]1, ...}][{t1, ...}] The chain \[Beta] from Definition 4.5.

\[Beta][n, S, {\[Epsilon]1, ...}][{t1, ...}] The chain \[Beta] indexed by S, included in the (n+1)d Swiss-Cheese operad.\nConvention: \[Epsilon] has length #S."; 


\[Beta]\[GothicC][\[Epsilon]_\[GothicO]][t_] := Cuboid[{1/2}, {1}] \[CircleTimes] Cuboid[MapThread[\[Sigma], {List @@ \[Epsilon], t}], -MapThread[\[Sigma], {-List @@ \[Epsilon], t}]]


\[Beta]\[GothicO][\[Epsilon]_\[GothicO], \[Phi]_][t_] := Cuboid[{0}, {\[Tau][List @@ \[Epsilon]*\[Phi], t]}] \[CircleTimes] Cuboid[(\[Phi] - 1)/2, (\[Phi] + 1)/2]


\[Beta][\[Epsilon]_\[GothicO]][t_] := Append[AssociationMap[\[Beta]\[GothicO][\[Epsilon], List @@ #1][t] & , Tuples[\[GothicO][-1, 1], Length[\[Epsilon]]]], \[GothicC][1] -> \[Beta]\[GothicC][\[Epsilon]][t]] /; Length[\[Epsilon]] == Length[t]


(* ::Text:: *)
(*We have to pass the dimension n as an argument, as we cannot deduce it from the others.*)


\[Beta][n_Integer, S_List, \[Epsilon]_\[GothicO]][t_] /; Length[S] == Length[\[Epsilon]] := \[Iota][Complement[Range[n], S], \[Beta][\[Epsilon]][t]]


(* ::Subsubsection:: *)
(*The helpers*)


\[CapitalPhi]::usage = "\[CapitalPhi][S, T, t] The map \[CapitalPhi] from Section 1.1.1."; 
\[CapitalPhi]::subset = "Sets `1` and `2` should be subsets of {1, ..., `3`}."; 
\[CapitalPhi]::disjoint = "Sets `1` and `2` should be disjoint."; 


\[CapitalPhi][S_, T_][t_] := With[{n = Length[t]}, Table[Which[MemberQ[T, j], Nothing & , MemberQ[S, j], Min, True, Max][t[[j]], -Max[t[[T]]]], {j, 1, n}] /; 
    ((SubsetQ[Range[n], S] && SubsetQ[Range[n], T]) || Message[\[CapitalPhi]::subset, S, T, n]) && (DisjointQ[S, T] || Message[\[CapitalPhi]::disjoint, S, T])]


\[CapitalPsi]::usage = "\[Psi][{t1, ..., tn}] The map \[CapitalPsi] from Section 1.1.1."


\[CapitalPsi][t_] := With[{n = Length[t] - 1}, Append[t[[Range[n - 1]]], Min[t[[{n, n + 1}]]]]]


(* ::Subsubsection:: *)
(*The chains \[Eta]*)


\[Eta]::usage = "\[Eta][{\[Epsilon]1, ...}, S, T][{t1, ...}] The chain \[Eta] from Definition 4.12.\nConvention: \[Epsilon] has length n."
\[Eta]::disjoint = "`1` and `2` should be disjoint."


\[Eta][\[Epsilon]_\[GothicO], S_List, {}][t_] := With[{n = Length[\[Epsilon]]}, With[{Sc = Complement[Range[n], S]}, \[Gamma][\[Beta][n, Sc, Minus /@ \[Epsilon][[Sc]]][t[[Sc]]], 
     Join[AssociationMap[\[Mu][n, S] & , Tuples[\[GothicO][1, -1], Length[Sc]]], Association[\[Epsilon][[Sc]] -> \[Beta][n, S, \[Epsilon][[S]]][t[[S]]]]]]]]


\[Eta][\[Epsilon]_\[GothicO], S_List, T_List][t_] := With[{n = Length[\[Epsilon]]}, With[{Tc = Complement[Range[n], T], STc = Complement[Range[n], Join[S, T]]}, 
     \[Gamma][\[Mu][n, T], Join[AssociationMap[\[Mu][n, Tc] & , Complement[Tuples[\[GothicO][1, -1], Length[T]], {\[Epsilon][[T]], Minus /@ \[Epsilon][[T]]}]], Association[\[Epsilon][[T]] -> \[Beta][n, Tc, \[Epsilon][[Tc]]][\[CapitalPhi][S, T][t]], 
        Minus /@ \[Epsilon][[T]] -> \[Gamma][\[Beta][n, STc, \[Epsilon][[STc]]][t[[STc]]], AssociationMap[\[Mu][n, S] & , Tuples[\[GothicO][1, -1], Length[STc]]]]]]]]] /; 
   Length[T] > 0 && (DisjointQ[S, T] || Message[\[Eta]::disjoint, S, T])


(* ::Subsubsection:: *)
(*The loops*)


l::usage = "l[n, t] The standard n-dimensional loop in the little (n+1)-cubes operad.";  


SubPlus[l][n_Integer][t_] := Association[\[GothicC][1] -> Cuboid[{(1 - Max[2*Abs[t]])/4}, {(3 - Max[2*Abs[t]])/4}] \[CircleTimes] Cuboid[(-1 - 2*t)/4, (1 - 2*t)/4], 
   \[GothicC][2] -> Cuboid[{(-3 + Max[2*Abs[t]])/4}, {(-1 + Max[2*Abs[t]])/4}] \[CircleTimes] Cuboid[(-1 + 2*t)/4, (1 + 2*t)/4]]


l[n_Integer][t_] := If[First[t] < 0, SubPlus[l][n][MapAt[1 + 2*#1 & , t, 1]], swapClosed[SubPlus[l][n][MapAt[-1 - 2*#1 & , -t, 1]]]]


(* ::Subsection:: *)
(*Proofs*)


(* ::Subsubsection:: *)
(*Non-formality in 2d*)


\[Eta]1::usage = "\[Eta]1[t] The 1-chain representing the circle in the 2d Swiss-Cheese operad."; 
\[Eta]1\[LetterSpace]components::usage = "The components of \[Eta]1."; 


\[Eta]1\[LetterSpace]plus\[LetterSpace]components = {\[Gamma][\[Beta][\[GothicO][1]][{-#1}], {\[GothicO][-1] -> \[Alpha][1]}] & , \[Gamma][\[Alpha][1], {\[GothicO][] -> \[Beta][\[GothicO][-1]][{-#1}]}] & , \[Gamma][\[Alpha][1], {\[GothicO][] -> \[Beta][\[GothicO][1]][{#1}]}] & , 
    \[Gamma][\[Beta][\[GothicO][-1]][{#1}], {\[GothicO][1] -> \[Alpha][1]}] & }; 


SubPlus[\[Eta]1][t_] := cat[\[Eta]1\[LetterSpace]plus\[LetterSpace]components][t]


\[Eta]1\[LetterSpace]components = Join[\[Eta]1\[LetterSpace]plus\[LetterSpace]components, (swapClosed @* #1 & ) /@ \[Eta]1\[LetterSpace]plus\[LetterSpace]components]; 


\[Eta]1[t_] := cat[\[Eta]1\[LetterSpace]components][t]


(* ::Subsubsection:: *)
(*Non-formality in 3d*)


hemisphereHelper[\[Epsilon]_,S_,T_]/;DisjointQ[S,T]:=\[Eta][\[Epsilon],S,T]
hemisphereHelper[_,_,_]:=Nothing


hemisphereComponents=Flatten[Table[hemisphereHelper[\[Epsilon],S,T],{\[Epsilon],Tuples[\[GothicO][-1,1],2]},{S,Subsets[{1,2}]},{T,Subsets[{1,2}]}]]


(* ::Section:: *)
(*Fin*)


End[]


EndPackage[]
