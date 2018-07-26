(* ::Package:: *)

(* :Title: ResistorBuilder *)
(* :Context: Global` *)
(* :Author: David K. Zhang *)
(* :Date: 2018-07-26 *)

(* :Package Version: 2.0 *)
(* :Wolfram Language Version: 11.3 *)

(* :Summary: ResistorBuilder is a graphical application that finds
	efficient ways to approximately construct a resistor with a given value
	by combining standard E6 series resistors in series and parallel. *)

(* :History:
	2.0 - Reformatted code, renamed functions, and added usage strings.
		Renamed from ResistorCalculator to ResistorBuilder.
		2018-07-26.
	1.5 - Revised into package and refactored circuit drawing code.
		2017-01-07.
	1.0 - Initial notebook version.
		Spring 2015. *)

(* :Copyright: (c) 2015-2018 David K. Zhang

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE. *)

OrderedIntegerPartitions::usage = "OrderedIntegerPartitions[n, len, min] \
gives a list of all possible ways to express the integer n as a sum of len \
integers, each greater than or equal to min. Partitions that differ only by \
a permutation are considered distinct.";

OrderedIntegerPartitions[n_Integer, 0, min_Integer] :=
	If[n === 0, {{}}, {}];

OrderedIntegerPartitions[n_Integer, len_Integer, min_Integer] :=
	OrderedIntegerPartitions[n, len, min] =
		Join @@ Table[
			Prepend[k] /@ OrderedIntegerPartitions[n - k, len - 1, min],
			{k, min, n - min * (len - 1)}];

FullKaryTrees::usage = "FullKaryTrees[n, k] gives a list of all full k-ary \
trees with n leaves, represented as expressions with nodes \[FormalV] and \
leaves \[FormalL].";

FullKaryTrees[n_Integer, 0] := If[n === 0, {\[FormalV][]}, {}];

FullKaryTrees[1, k_Integer] /; k > 1 := {\[FormalL]};

FullKaryTrees[n_Integer, k_Integer] /; k > 1 := FullKaryTrees[n, k] =
	Join @@ Map[
		Map[Apply[\[FormalV]]] @* Tuples @* Map[Curry[FullKaryTrees][k]],
		OrderedIntegerPartitions[n, k, 1]];

EnumerateTree::usage = "EnumerateTree[t] takes a tree t in the format \
returned by FullKaryTrees and gives an indexed copy of t with its nodes and \
leaves enumerated according to an in-order traversal.";

EnumerateTree[t_] := Block[{i = 0, j = 0},
	t /. {
		\[FormalV] :> Indexed[\[FormalV], ++i],
		\[FormalL] :> Indexed[\[FormalL], ++j]}];

CircuitGraphics[circuit_] := With[{w = CircuitWidth[circuit]},
	Graphics[{
		Line[{{0, 0}, {1, 0}}],
		CircuitRender[circuit, {1, 0}],
		Line[{{w + 1, 0}, {w + 2, 0}}]}]];

Resistor::usage = "Resistor[r] is an inert symbol representing a resistor \
with a resistance of r Ohms.";

CircuitWidth[Resistor[_]] = 1;
CircuitHeight[Resistor[_]] = 1;

CircuitRender[Resistor[r_], {x_, y_}] := {
	Line[{x + #, y + Sin[6 * Pi * #] / 6}& /@ Range[0, 1, 1 / 12]],
	Text[Style[StringForm["``\[CapitalOmega]", r], Bold, FontSize -> 12],
		{x + 0.5, y + 0.4}]};

SeriesCircuit::usage = "SeriesCircuit[c1, c2, ...] is an inert symbol \
representing the circuit composed of components (c1, c2, ...) connected \
in series.";

SetAttributes[SeriesCircuit, {Flat, OneIdentity, Orderless}];

CircuitWidth[SeriesCircuit[components__]] :=
	Total[CircuitWidth /@ {components}] + Length[{components}] - 1;
CircuitHeight[SeriesCircuit[components__]] :=
	Max[CircuitHeight /@ {components}];

CircuitRender[SeriesCircuit[components__], {x_, y_}] :=
	With[{
		compList = {components}},
	With[{
		widthSums = Prepend[Accumulate[CircuitWidth /@ compList], 0],
		n = Length[compList]},
	Append[
		Table[Line[{
				{x + (i - 1) + widthSums[[i + 1]], y},
				{x + i + widthSums[[i + 1]], y}
			}], {i, n - 1}],
		Join @@ Table[
			CircuitRender[compList[[i]], {x + (i - 1) + widthSums[[i]], y}],
			{i, n}]]]];

ParallelCircuit::usage = "ParallelCircuit[c1, c2, ...] is an inert symbol \
representing the circuit composed of components (c1, c2, ...) connected \
in parallel.";

SetAttributes[ParallelCircuit, {Flat, OneIdentity, Orderless}];

CircuitWidth[ParallelCircuit[components__]] :=
	Max[CircuitWidth /@ {components}] + 2;
CircuitHeight[ParallelCircuit[components__]] :=
	Total[CircuitHeight /@ {components}];

CircuitRender[ParallelCircuit[components__], {x_, y_}] :=
	With[{
		compList = {components}},
	With[{
		n = Length[compList],
		h = CircuitHeight /@ compList,
		w = CircuitWidth /@ compList},
	With[{
		xright = x + 2 + Max[w],
		ytop = y + Total[h] / 2,
		ybot = y - Total[h] / 2},
	With[{
		xmid = (x + xright) / 2,
		ypos = ytop - Most@Prepend[Accumulate[h], 0] - h / 2},
	Join[
		Join @@ Table[
			CircuitRender[compList[[i]], {xmid - w[[i]] / 2, ypos[[i]]}],
			{i, n}],
		Table[
			Line[{{x, ypos[[i]]}, {xmid - w[[i]] / 2, ypos[[i]]}}],
			{i, n}],
		Table[
			Line[{{xright, ypos[[i]]}, {xmid + w[[i]] / 2, ypos[[i]]}}],
			{i, n}],
		Map[
			Line[{{#, ytop - First[h] / 2}, {#, ybot + Last[h] / 2}}]&,
			{x, xright}]]]]]];

e6ResistorValues = {100, 150, 220, 330, 470, 680};
calcResistance = ReplaceAll[{
	Resistor -> Identity,
	SeriesCircuit -> Plus,
	ParallelCircuit -> (1 / Total[1 / {##}]&)}];
resistorCircuits = KeySort@GroupBy[
	Union@Flatten@Table[tree,
		{n, 4},
		{tree, EnumerateTree /@ FullKaryTrees[n, 2]},
		{\[FormalV], Tuples[{SeriesCircuit, ParallelCircuit}, n - 1]},
		{\[FormalL], Tuples[Resistor /@ e6ResistorValues, n]}],
	calcResistance];
Manipulate[Grid@Map[
		{Show[CircuitGraphics[#], ImageSize -> 500], N@calcResistance[#]}&,
		Flatten@Values@KeySelect[resistorCircuits,
			Between[{0.997, 1.003} * r]]],
	{r, 100, 1000, 1}]
