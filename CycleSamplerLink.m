(* ::Package:: *)

BeginPackage["CycleSamplerLink`", {"CCompilerDriver`"}];


Begin["`Private`"];

sphereRadiiUsage = "\"SphereRadii\" -> \[Rho]. If the edgelengths \!\(\*FormBox[TemplateBox[<|\"boxes\" -> FormBox[RowBox[{SubscriptBox[StyleBox[\"r\", \"TI\"], \"1\"], \",\", \"\[Ellipsis]\", \",\", SubscriptBox[StyleBox[\"r\", \"TI\"], StyleBox[\"n\", \"TI\"]]}], TraditionalForm], \"errors\" -> {}, \"input\" -> \"r_1, \\\\dotsc, r_n\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"],
TraditionalForm]\) are different from one another, there are different plausible choices of metric for the space of polygons. If \!\(\*FormBox[TemplateBox[<|\"boxes\" -> FormBox[RowBox[{SuperscriptBox[StyleBox[\"S\", \"TI\"], RowBox[{StyleBox[\"d\", \"TI\"], \"-\", \"1\"}]], \"(\", StyleBox[\"r\", \"TI\"], \")\"}], TraditionalForm], \"errors\" -> {}, \"input\" -> \"S^{d-1}(r)\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"],
TraditionalForm]\) is the sphere of radius \!\(\*FormBox[TemplateBox[<|\"boxes\" -> FormBox[StyleBox[\"r\", \"TI\"], TraditionalForm], \"errors\" -> {}, \"input\" -> \"r\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"],
TraditionalForm]\), we may define

	\!\(\*FormBox[TemplateBox[<|\"boxes\" -> FormBox[RowBox[{\"Pol\", RowBox[{\"(\", RowBox[{StyleBox[\"n\", \"TI\"], \",\", StyleBox[\"r\", \"TI\"]}], \")\"}], \"\[LongEqual]\", RowBox[{\"{\", RowBox[{StyleBox[\"x\", \"TI\"], \"\[Element]\", SuperscriptBox[StyleBox[\"S\", \"TI\"], RowBox[{StyleBox[\"d\", \"TI\"], \"-\", \"1\"}]], RowBox[{\"(\", \"1\", \")\"}], \"\[Cross]\", \"\[CenterEllipsis]\", \"\[Cross]\", SuperscriptBox[StyleBox[\"S\", \"TI\"], RowBox[{StyleBox[\"d\", \"TI\"], \"-\", \"1\"}]], RowBox[{\"(\", \"1\", \")\"}], \"|\", \"\[Sum]\", SubscriptBox[StyleBox[\"r\", \"TI\"], StyleBox[\"i\", \"TI\"]], SubscriptBox[StyleBox[\"x\", \"TI\"], StyleBox[\"i\", \"TI\"]], \"\[LongEqual]\", \"0\"}], \"}\"}]}], TraditionalForm], \"errors\" -> {}, \"input\" -> \"\\\\text{Pol}(n,r) = \\\\{ x \\\\in S^{d-1}(1) \\\\times \\\\cdots \\\\times S^{d-1}(1) \\\\mid \\\\sum r_i x_i = 0 \\\\}\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"],
TraditionalForm]\)

or we may define

	\!\(\*FormBox[TemplateBox[<|\"boxes\" -> FormBox[RowBox[{\"Pol\", RowBox[{\"(\", RowBox[{StyleBox[\"n\", \"TI\"], \",\", StyleBox[\"r\", \"TI\"]}], \")\"}], \"\[LongEqual]\", RowBox[{\"{\", RowBox[{StyleBox[\"x\", \"TI\"], \"\[Element]\", SuperscriptBox[StyleBox[\"S\", \"TI\"], RowBox[{StyleBox[\"d\", \"TI\"], \"-\", \"1\"}]], RowBox[{\"(\", SubscriptBox[StyleBox[\"r\", \"TI\"], \"1\"], \")\"}], \"\[Cross]\", \"\[CenterEllipsis]\", \"\[Cross]\", SuperscriptBox[StyleBox[\"S\", \"TI\"], RowBox[{StyleBox[\"d\", \"TI\"], \"-\", \"1\"}]], RowBox[{\"(\", SubscriptBox[StyleBox[\"r\", \"TI\"], StyleBox[\"n\", \"TI\"]], \")\"}], \"|\", \"\[Sum]\", SubscriptBox[StyleBox[\"x\", \"TI\"], StyleBox[\"i\", \"TI\"]], \"\[LongEqual]\", \"0\"}], \"}\"}]}], TraditionalForm], \"errors\" -> {}, \"input\" -> \"\\\\text{Pol}(n,r) = \\\\{ x \\\\in S^{d-1}(r_1) \\\\times \\\\cdots \\\\times S^{d-1}(r_n) \\\\mid \\\\sum x_i = 0 \\\\}\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"],
TraditionalForm]\)
	
In the special case of Millson and Kapovich's symplectic structure on polygons in \!\(\*FormBox[TemplateBox[<|\"boxes\" -> FormBox[SuperscriptBox[StyleBox[\"R\", FontSlant -> \"Bold\"], \"3\"], TraditionalForm], \"errors\" -> {}, \"input\" -> \"\\\\mathbf{R}^3\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"],
TraditionalForm]\), they define

	\!\(\*FormBox[TemplateBox[<|\"boxes\" -> FormBox[RowBox[{\"Pol\", RowBox[{\"(\", RowBox[{StyleBox[\"n\", \"TI\"], \",\", StyleBox[\"r\", \"TI\"]}], \")\"}], \"\[LongEqual]\", RowBox[{\"{\", RowBox[{StyleBox[\"x\", \"TI\"], \"\[Element]\", SuperscriptBox[StyleBox[\"S\", \"TI\"], \"2\"], RowBox[{\"(\", SqrtBox[SubscriptBox[StyleBox[\"r\", \"TI\"], \"1\"]], \")\"}], \"\[Cross]\", \"\[CenterEllipsis]\", \"\[Cross]\", SuperscriptBox[StyleBox[\"S\", \"TI\"], \"2\"], RowBox[{\"(\", SqrtBox[SubscriptBox[StyleBox[\"r\", \"TI\"], StyleBox[\"n\", \"TI\"]]], \")\"}], \"|\", \"\[Sum]\", SqrtBox[SubscriptBox[StyleBox[\"r\", \"TI\"], StyleBox[\"i\", \"TI\"]]], SubscriptBox[StyleBox[\"x\", \"TI\"], StyleBox[\"i\", \"TI\"]], \"\[LongEqual]\", \"0\"}], \"}\"}]}], TraditionalForm], \"errors\" -> {}, \"input\" -> \"\\\\text{Pol}(n,r) = \\\\{ x \\\\in S^2(\\\\sqrt{r_1}) \\\\times \\\\cdots \\\\times S^2(\\\\sqrt{r_n}) \\\\mid \\\\sum \\\\sqrt{r_i} x_i = 0 \\\\}\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"],
TraditionalForm]\)
	
These choices yield different metrics on the space of polygons and hence different measures for sampling.
The default choice is the second option (sphere radii equal to edgelengths), but the user may set the 
radii of the spheres \!\(\*FormBox[TemplateBox[<|\"boxes\" -> FormBox[SubscriptBox[\"\[Rho]\", StyleBox[\"i\", \"TI\"]], TraditionalForm], \"errors\" -> {}, \"input\" -> \"\\\\rho_i\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"],
TraditionalForm]\) separately.
";

End[];


CycleSample::usage = "CycleSample[d_Integer?Positive, r_?(VectorQ[#,NumericQ]&), samplecount_Integer] draws samples of closed polygons in d-dimensional Euclidean space and evaluates a list of random variables on them. The drawn polygons are discarded afterwards.
The vector r contains the length of each edge of the polygon. As option one can set:

"<>CycleSamplerLink`Private`sphereRadiiUsage;


CycleSampleChordLength::usage = "CycleSampleChordLength[d_Integer?Positive, r_?(VectorQ[#,NumericQ]&), \[Rho]_?(VectorQ[#,NumericQ]&), {i_Integer, j_Integer}, samplecount_Integer] draws samplecount samples of closed polygons of edge lengths r in d-dimensional Euclidean space. Then it evaluates the chord length between vertices i and j. As option one can set:

"<>CycleSamplerLink`Private`sphereRadiiUsage;


RandomOpenPolygons::usage="RandomOpenPolygons[d_Integer?Positive, edgecount_Integer?Positive, samplecount_Integer?Positive] generates samplecount open Length[r]-gons in d dimensional Euclidean space. The result is equivalent to - but significantly faster to obtain than - RandomPoint[Sphere[ConstantArray[0.,d]],{samplecount,edgecount}].";


RandomClosedPolygons::usage="RandomClosedPolygons[d_Integer?Positive, r_?(VectorQ[#,NumericQ]&), samplecount_Integer?Positive] generates samplecount open Length[r]-gons in d dimensional Euclidean space, closes them via the conformal barycenter method. Then it returns: 
	(i)   the open polygons' unit edge vectors;
	(ii)  the conformal shift vectors; 
	(iii) the closed polygons' unit edge vectors;
	(iv)  the sampling weights for the edge space; and
	(v)   the sampling weights for the quotient space of the edge space by the action of SO(d).

	 As option one can set:

"<>CycleSamplerLink`Private`sphereRadiiUsage;


ConformalClosures::usage="ConformalClosures[r_?(VectorQ[#,NumericQ]&), x_?((ArrayQ[#]&&(ArrayDepth[#]==3))&)] closes the Length[r]-gons stored in the 3-tensor x via the conformal barycenter method. Then it returns: 
	(i)   the open polygons' unit edge vectors;
	(ii)  the conformal shift vectors; 
	(iii) the closed polygons' unit edge vectors;
	(iv)  the sampling weights for the edge space; and
	(v)   the sampling weights for the quotient space of the edge space by the action of SO(d).

	 As option one can set:

"<>CycleSamplerLink`Private`sphereRadiiUsage;


ActionAngleSample::usage="ActionAngleSample[edgecount_Integer?Positive, samplecount_Integer?Positive] samples samplecount closed, equilateral polygons with edgecount edges in 3-dimensional Euclidean space.";


(*Some error and warning messages.*)

CycleSamplerLink::badedgelengths = "One edge has more than half the total length of the polygon. No closed polygons with these edgelengths exist.";

CycleSamplerLink::badsphereradii = "The Length of the vector given as value for option \"SphereRadii\" does not coincide with the Length of the second argument.";

CycleSamplerLink::badsphereradii2 = "The value for the option \"SphereRadii\" has to be a numerical vector or a String.";

CycleSamplerLink::badsphereradii3 = "Bad value `1` for the option \"SphereRadii\".";


Begin["`Private`"];


$packageFile  = $InputFileName;
$packageDirectory  = DirectoryName[$InputFileName];

$libraryDirectory  = FileNameJoin[{$packageDirectory, "LibraryResources", $SystemID}];
If[!FileExistsQ[$libraryDirectory],CreateDirectory[$libraryDirectory]];
$sourceDirectory   = FileNameJoin[{$packageDirectory, "LibraryResources", "Source"}];
If[!FileExistsQ[$sourceDirectory],CreateDirectory[$sourceDirectory]];

$logFile = FileNameJoin[{$packageDirectory, "LibraryResources", $SystemID,"Log.txt"}];

If[FileExistsQ[$logFile],DeleteFile[$logFile]];

LogFile[] := Import[$logFile,"Text"];

(* Add $libraryDirectory to $LibraryPath in case the package is not installed in $UserBaseDirectory/Applications. *)
If[Not@MemberQ[$LibraryPath, $libraryDirectory],AppendTo[$LibraryPath, $libraryDirectory]];

$compilationOptions := $compilationOptions = Get[FileNameJoin[{$sourceDirectory,"BuildSettings.m"}]];


Get[FileNameJoin[{$sourceDirectory, "cSampleRandomVariable.m"}]];

Options[CycleSample] = {
	"SphereRadii" -> "EdgeLengths",
	"QuotientSpace" -> True,
	"ThreadCount" :> ("ParallelThreadNumber"/.("ParallelOptions"/.SystemOptions["ParallelOptions"]))
};

(* This is the Mathematica wrapper for the compiled library. It allocates the accumulation buffers, 
sends them to the dynamic library, and postprocesses the outputs.*)
CycleSample[fun_String, d_Integer?Positive, r_?(VectorQ[#,NumericQ]&), samplecount_Integer?Positive, OptionsPattern[]]:=Module[{\[Rho], err, values, weights}, 
	
	(* Allocation. *)
	values  = ConstantArray[0., {samplecount}]; 
	weights = ConstantArray[0., {samplecount}];
	
	\[Rho] = processSphereRadii[r,OptionValue["SphereRadii"]];
	If[\[Rho]===$Failed,Return[$Failed]];
	
	If[2 Max[r] > Total[r], Message[CycleSamplerLink::badedgelengths]; Return[$Failed]];
	
	(* Here the dynamic library is looked up and then called on the allocated buffers. *)
	err = cSampleRandomVariable[d][
		fun, r, \[Rho], values, weights, If[OptionValue["QuotientSpace"]=!=False,1,0], samplecount, OptionValue["ThreadCount"]
	];
	
	If[err===0,
		WeightedData[values,weights]
	,
		$Failed
	]
];


Get[FileNameJoin[{$sourceDirectory, "cSampleChordLength.m"}]];

Options[CycleSampleChordLength] = {
	"SphereRadii" -> "EdgeLengths",
	"QuotientSpace" -> True,
	"ThreadCount" :> ("ParallelThreadNumber"/.("ParallelOptions"/.SystemOptions["ParallelOptions"]))
};

CycleSampleChordLength[d_Integer?Positive, r_?(VectorQ[#,NumericQ]&), {i_Integer, j_Integer}, samplecount_Integer?Positive, OptionsPattern[]]:=Module[{\[Rho], err, values, weights}, 

	If[2 Max[r] > Total[r], Message[CycleSamplerLink::badedgelengths]; Return[$Failed]];
		
	\[Rho] = processSphereRadii[r, OptionValue["SphereRadii"]];
	If[\[Rho]===$Failed, Return[$Failed]];		
	
	values  = ConstantArray[0., {samplecount}]; 
	weights = ConstantArray[0., {samplecount}]; 
	
	err = cSampleChordLength[d][i, j, r, \[Rho], values, weights, If[OptionValue["QuotientSpace"]=!=False,1,0], samplecount, OptionValue["ThreadCount"]];
	
	If[err===0,
		WeightedData[values,weights]
	,
		$Failed
	]
];


Get[FileNameJoin[{$sourceDirectory, "cRandomOpenPolygons.m"}]];

Options[RandomOpenPolygons ]= {
	"ThreadCount" :> ("ParallelThreadNumber"/.("ParallelOptions"/.SystemOptions["ParallelOptions"]))
};

RandomOpenPolygons[d_Integer?Positive, edgecount_Integer?Positive, samplecount_Integer?Positive, OptionsPattern[]]:=Module[{x},

	x = ConstantArray[0.,{samplecount,edgecount,d}];

	cRandomOpenPolygons[d][x,Min[samplecount,OptionValue["ThreadCount"]]];

	x
];


Get[FileNameJoin[{$sourceDirectory, "cRandomClosedPolygons.m"}]];

RandomClosedPolygons::len="Lengths of the input vectors in the second and third argument are expected to coincide.";

Options[RandomClosedPolygons ]= {
	"ThreadCount" :> ("ParallelThreadNumber"/.("ParallelOptions"/.SystemOptions["ParallelOptions"])),
	"SphereRadii" -> "EdgeLengths"
};

RandomClosedPolygons[d_Integer?Positive, r_?(VectorQ[#,NumericQ]&), samplecount_Integer?Positive, OptionsPattern[]]:=Module[{W,edgecount,x,w,y,Klist,Kquotlist,\[Rho]},

	If[2 Max[r]>Total[r], Message[CycleSamplerLink::badedgelengths]; Return[$Failed]];
		
	\[Rho] = processSphereRadii[r, OptionValue["SphereRadii"]];
	If[\[Rho]===$Failed, Return[$Failed]];

	edgecount=Length[r];
		
	x = ConstantArray[0.,{samplecount,edgecount,d}];
	w = ConstantArray[0.,{samplecount,          d}];
	y = ConstantArray[0.,{samplecount,edgecount,d}];

	Klist     = ConstantArray[0.,{samplecount}];
	Kquotlist = ConstantArray[0.,{samplecount}];

	cRandomClosedPolygons[d][r,\[Rho],x,w,y,Klist,Kquotlist,Min[samplecount,OptionValue["ThreadCount"]]];

	Association[
		"OpenPolygonUnitEdgeVectors"->x,
		"ShiftVectors"->w,
		"ClosedPolygonUnitEdgeVectors"->y,
		"EdgeSpaceSamplingWeights"->Klist,
		"EdgeQuotientSpaceSamplingWeights"->Kquotlist,
		"EdgeLengths"->r,
		"SphereRadii"->\[Rho]
	]
];


Get[FileNameJoin[{$sourceDirectory, "cConformalClosures.m"}]];

ConformalClosures::len="Length of the first input does coincide with second dimension of second input.";

Options[ConformalClosures ]= {
	"ThreadCount" :> ("ParallelThreadNumber"/.("ParallelOptions"/.SystemOptions["ParallelOptions"])),
	"SphereRadii" -> "EdgeLengths"
};

ConformalClosures[r_?(VectorQ[#,NumericQ]&), x_?((ArrayQ[#]&&(ArrayDepth[#]==3))&), OptionsPattern[]]:=Module[{W,edgecount,samplecount,d,w,y,Klist,Kquotlist,\[Rho]},

	If[2 Max[r]>Total[r], Message[CycleSamplerLink::badedgelengths]; Return[$Failed]];
		
	\[Rho] = processSphereRadii[r, OptionValue["SphereRadii"]];
	If[\[Rho]===$Failed, Return[$Failed]];

	{samplecount, edgecount, d}= Dimensions[x];
	
	If[ Length[r] != edgecount,
		Message[ConformalClosures::len];
		Return[$Failed];
	];
	
	w = ConstantArray[0.,{samplecount,          d}];
	y = ConstantArray[0.,{samplecount,edgecount,d}];

	Klist     = ConstantArray[0.,{samplecount}];
	Kquotlist = ConstantArray[0.,{samplecount}];

	cConformalClosures[d][r,\[Rho],x,w,y,Klist,Kquotlist,Min[samplecount,OptionValue["ThreadCount"]]];

	Association[
		"OpenPolygonUnitEdgeVectors"->x,
		"ShiftVectors"->w,
		"ClosedPolygonUnitEdgeVectors"->y,
		"EdgeSpaceSamplingWeights"->Klist,
		"EdgeQuotientSpaceSamplingWeights"->Kquotlist,
		"EdgeLengths"->r,
		"SphereRadii"->\[Rho]
	]
];


Get[FileNameJoin[{$sourceDirectory, "cActionAngleSampler.m"}]];

Options[ActionAngleSample] = {
	"ThreadCount" :> ("ParallelThreadNumber"/.("ParallelOptions"/.SystemOptions["ParallelOptions"]))
};

ActionAngleSample[edgecount_Integer?Positive, samplecount_Integer?Positive, OptionsPattern[]]:=Module[{p,trials},
	p = ConstantArray[0.,{samplecount,edgecount,3}];
	trials = cActionAngleSampler[p,OptionValue["ThreadCount"]];
	Association[
		"ClosedPolygons"->p,
		"Trials"->trials
	]
]


processSphereRadii[r_?(VectorQ[#,NumericQ]&), \[Rho]_]:=If[
	VectorQ[\[Rho],NumericQ]
,
	If[
		!TrueQ[Length[\[Rho]]==Length[r]]
	,
		Message[CycleSamplerLink::badsphereradii];
		Return[$Failed];
	,   
		Return[\[Rho]];
	];
,
	If[
		!StringQ[\[Rho]]
	,
		Message[CycleSamplerLink::badsphereradii2];
		Return[$Failed];
	,
		Switch[
			\[Rho]
			,
			"EdgeLengths", Return[r];
			,
			"One", Return[ConstantArray[1.,Length[r]]];
			,
			"SymplecticQuotient",Return[Sqrt[r]];
			,
			_, (Message[CycleSamplerLink::badsphereradii3,\[Rho]];Return[$Failed];)
		];
	];
];


clearLibraries[]:=(
	Scan[DeleteFile,FileNames["*"<>CCompilerDriver`CCompilerDriverBase`$PlatformDLLExtension,$libraryDirectory]];
	Get[$packageFile];
);


End[];


EndPackage[];
