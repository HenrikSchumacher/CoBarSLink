(* ::Package:: *)

BeginPackage["CoBarSLink`", {"CCompilerDriver`"}];


Begin["`Private`"];

sphereRadiiUsage = "
\"SphereRadii\" -> \[Rho]. If the edgelengths \!\(\*FormBox[TemplateBox[<|\"boxes\" -> FormBox[RowBox[{SubscriptBox[StyleBox[\"r\", \"TI\"], \"1\"], \",\", \"\[Ellipsis]\", \",\", SubscriptBox[StyleBox[\"r\", \"TI\"], StyleBox[\"n\", \"TI\"]]}], TraditionalForm], \"errors\" -> {}, \"input\" -> \"r_1, \\\\dotsc, r_n\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"],
TraditionalForm]\) are different from one another, there are different plausible choices of metric for the space of polygons

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

\"QuotientSpace\" - If set to True, then the Riemannian quotient space probability measure with respect to the SO(d) action is used for sampling. This is the default. Otherwise, the push-forward measure along the quotient map is used.

\"ThreadCount\" - The number of threads to be used for sampling. The default is \"ParallelOptions\"/.\[VeryThinSpace]SystemOptions[\"ParallelOptions\"].";

confidenceUsage = "

\"ConfidenceLevel\" - The confidence level. The default value is 95%.

\"ChunkSize\" - The number of samples to be sampled at once by the backend library. Greater values reduce overhead; but they may also cause unneccessary high sample counts if chosen too large.

\"MaxSamples\" - Sampling will stop after this many samples, no matter if the desired confidence has been reached or not.

\"RelativeErrorMode\" - If set to True, the entries of the input vector confidenceradii are treated as relative to the corresponding sample mean. This feature is turned off by default.

\"Verbose\" - If set to True, the backend library will printed intermediate sampling results to the notebook. Turn this off for reducing message overhead. This feature is turned on by default.
";

End[];


CoBarSample::usage = "CoBarSample[randomvariable_String,d_Integer?Positive, r_?(VectorQ[#,NumericQ]&), samplecount_Integer] draws samplecount samples of closed polygons in d-dimensional Euclidean space and evaluates a list of random variables on them. The drawn polygons are discarded afterwards.
The vector r contains the length of each edge of the polygon. 

The following options can be set:
"<>CoBarSLink`Private`sphereRadiiUsage;


CoBarConfidenceSample::usage = "CoBarConfidenceSample[funs:{__}, d_Integer?Positive, r_?(VectorQ[#,NumericQ]&), confidenceradii:{___?NumericQ}, opts___] draws samples of closed polygons in d-dimensional Euclidean space and evaluates the list of random variables specified by funs on them. It stops if each random variable's confidence radius is below the prescribed value in the condidenceradii. Returned are sample mean, sample variance, and some further statistics. The drawn polygons are discarded afterwards. The vector r contains the length of each edge of the polygon. 

The following options can be set:
"<>CoBarSLink`Private`sphereRadiiUsage<>CoBarSLink`Private`confidenceUsage;


RandomOpenPolygons::usage = "RandomOpenPolygons[d_Integer?Positive, edgecount_Integer?Positive, samplecount_Integer?Positive] generates samplecount open Length[r]-gons in d dimensional Euclidean space. The result is equivalent to - but significantly faster to obtain than - RandomPoint[Sphere[ConstantArray[0.,d]],{samplecount,edgecount}].";


RandomClosedPolygons::usage = "RandomClosedPolygons[d_Integer?Positive, r_?(VectorQ[#,NumericQ]&), samplecount_Integer?Positive] generates samplecount open Length[r]-gons in d dimensional Euclidean space, closes them via the conformal barycenter method. Then it returns: 
	(i)   the open polygons' unit edge vectors;
	(ii)  the conformal shift vectors; 
	(iii) the closed polygons' unit edge vectors;
	(iv)  the sampling weights for the edge space; and
	(v)   the sampling weights for the quotient space of the edge space by the action of SO(d).

The following options can be set:
"<>CoBarSLink`Private`sphereRadiiUsage;


ConformalClosures::usage = "ConformalClosures[r_?(VectorQ[#,NumericQ]&), x_?((ArrayQ[#]&&(ArrayDepth[#]==3))&)] closes the Length[r]-gons stored in the 3-tensor x via the conformal barycenter method. Then it returns: 
	(i)   the open polygons' unit edge vectors;
	(ii)  the conformal shift vectors; 
	(iii) the closed polygons' unit edge vectors;
	(iv)  the sampling weights for the edge space; and
	(v)   the sampling weights for the quotient space of the edge space by the action of SO(d).

The following options can be set:
"<>CoBarSLink`Private`sphereRadiiUsage;


ActionAngleSample::usage = "ActionAngleSample[edgecount_Integer?Positive, samplecount_Integer?Positive] samples samplecount closed, equilateral polygons with edgecount edges in 3-dimensional Euclidean space.";


(*Some error and warning messages.*)

CoBarSLink::badedgelengths = "One edge has more than half the total length of the polygon. No closed polygons with these edgelengths exist.";

CoBarSLink::badsphereradii = "The Length of the vector given as value for option \"SphereRadii\" does not coincide with the Length of the second argument.";

CoBarSLink::badsphereradii2 = "The value for the option \"SphereRadii\" has to be a numerical vector or a String.";

CoBarSLink::badsphereradii3 = "Bad value `1` for the option \"SphereRadii\".";

CoBarSLink::badvertex = "Vertex index `1` is out of bounds {1,...,`2`}.";


Begin["`Private`"];


$packageFile  = $InputFileName;
$packageDirectory  = DirectoryName[$InputFileName];

$libraryDirectory  = FileNameJoin[{$packageDirectory, "LibraryResources", $SystemID}];
If[!FileExistsQ[$libraryDirectory],CreateDirectory[$libraryDirectory]];
$sourceDirectory   = FileNameJoin[{$packageDirectory, "LibraryResources", "Source"}];
If[!FileExistsQ[$sourceDirectory],CreateDirectory[$sourceDirectory]];

$logFile = FileNameJoin[{$packageDirectory, "LibraryResources", $SystemID,"Tools_Log.txt"}];

If[FileExistsQ[$logFile],DeleteFile[$logFile]];

LogFile[] := Import[$logFile,"Text"];

(* Add $libraryDirectory to $LibraryPath in case the package is not installed in $UserBaseDirectory/Applications. *)
If[Not@MemberQ[$LibraryPath, $libraryDirectory],AppendTo[$LibraryPath, $libraryDirectory]];

$compilationOptions := $compilationOptions = Get[FileNameJoin[{$sourceDirectory,"BuildSettings.m"}]];


Get[FileNameJoin[{$sourceDirectory, "cSampleRandomVariables.m"}]];

Options[CoBarSample] = {
	"SphereRadii" -> "EdgeLengths",
	"QuotientSpace" -> True,
	"ThreadCount" :> ("ParallelThreadNumber"/.("ParallelOptions"/.SystemOptions["ParallelOptions"]))
};

(* This is the Mathematica wrapper for the compiled library. It allocates the accumulation buffers, 
sends them to the dynamic library, and postprocesses the outputs.*)
CoBarSample[fun_, d_Integer?Positive, r_?(VectorQ[#,NumericQ]&), samplecount_Integer?Positive, opts___] := With[{result=CoBarSample[{fun}, d, r, samplecount, opts]},

	If[result===$Failed,
		$Failed,
		result[[Key[fun]]]
	]
];

CoBarSample[funs:{__}, d_Integer?Positive, r_?(VectorQ[#,NumericQ]&), samplecount_Integer?Positive, OptionsPattern[]]:=Module[{ordinaryfuns, chords, funcount, \[Rho], err, values, weights}, 
	
	If[2 Max[r] > Total[r], 
		Message[CoBarSLink::badedgelengths]; 
		Return[$Failed]
	];
	
	ordinaryfuns = Cases[funs,_String];
	chords = Cases[funs,"ChordLength"[i_Integer,j_Integer]:>{i,j}];
	
	funcount = Length[ordinaryfuns]+Length[chords];
	
	(* Allocation. *)
	values  = ConstantArray[0., {samplecount,funcount}]; 
	weights = ConstantArray[0., {samplecount}];
	
	\[Rho] = processSphereRadii[r,OptionValue["SphereRadii"]];
	If[\[Rho]===$Failed,Return[$Failed]];
	
	(* Here the dynamic library is looked up and then called on the allocated buffers. *)
	err = cSampleRandomVariables[d][
		StringRiffle[ordinaryfuns," "], chords, r, \[Rho], values, weights, TrueQ[OptionValue["QuotientSpace"]], samplecount, OptionValue["ThreadCount"]
	];
	
	values = Transpose[values];
	
	If[err===0,	
		Association[
			Join[
				Table[funs[[i]]->WeightedData[values[[i]],weights],{i,1,Length[ordinaryfuns]}],
				Table["ChordLength"[chords[[i,1]],chords[[i,2]]]->WeightedData[values[[Length[ordinaryfuns]+i]],weights],{i,1,Length[chords]}]
			]
		]
	,
		$Failed
	]
];


Get[FileNameJoin[{$sourceDirectory, "cConfidenceSampleRandomVariables.m"}]];

Options[CoBarConfidenceSample] = {
	"SphereRadii" -> "EdgeLengths",
	"QuotientSpace" -> True,
	"ThreadCount" :> ("ParallelThreadNumber"/.("ParallelOptions"/.SystemOptions["ParallelOptions"])),
	"ChunkSize"->1000000,
	"MaxSamples"->10000000,
	"ConfidenceLevel"->0.95,
	"RelativeErrorMode"->False,
	"Verbose"->True
};

(* This is the Mathematica wrapper for the compiled library. It allocates the accumulation buffers, 
sends them to the dynamic library, and postprocesses the outputs.*)

CoBarConfidenceSample[fun_, d_Integer?Positive, r_?(VectorQ[#,NumericQ]&), confidenceradius_?NumericQ, opts___] := Module[{data},
	data = CoBarConfidenceSample[{fun}, d, r, {confidenceradius}, opts];
	
	Merge[{data[[Key[fun]]],data[[-12;;]]},First]
];

CoBarConfidenceSample[funs:{__}, d_Integer?Positive, r_?(VectorQ[#,NumericQ]&), confidenceradii:{___?NumericQ}, OptionsPattern[]]:=Module[{funcount, ordinaryfuns, chords, \[Rho], means, variances, errors, samplecount, time, relativeQ}, 
	
	If[2 Max[r] > Total[r], 
		Message[CoBarSLink::badedgelengths]; 
		Return[$Failed]
	];
	
	ordinaryfuns = Cases[funs,_String];
	chords = Cases[funs,"ChordLength"[i_Integer,j_Integer]:>{i,j}];
	
	funcount = Min[Length[ordinaryfuns]+Length[chords],Length[confidenceradii]];
	
	(* Allocation. *)
	means     = ConstantArray[0., {funcount}]; 
	variances = ConstantArray[0., {funcount}]; 
	errors    = ConstantArray[0., {funcount}];
	
	relativeQ = TrueQ[OptionValue["RelativeErrorMode"]];
	
	\[Rho] = processSphereRadii[r,OptionValue["SphereRadii"]];
	If[\[Rho]===$Failed,Return[$Failed]];
	
	(* Here the dynamic library is looked up and then called on the allocated buffers. *)
	
	time = AbsoluteTiming[
		samplecount = cConfidenceSampleRandomVariables[d][
			StringRiffle[ordinaryfuns," "], chords, r, \[Rho], means, variances, errors, confidenceradii, 
			OptionValue["MaxSamples"], 
			TrueQ[OptionValue["QuotientSpace"]], 
			OptionValue["ThreadCount"],
			OptionValue["ConfidenceLevel"],
			OptionValue["ChunkSize"],
			relativeQ,
			TrueQ[OptionValue["Verbose"]]
		];
	][[1]];
	
	Association[
		Sequence@@Join[
			Table[
				ordinaryfuns[[i]] -> Association[
					"SampleMean" -> means[[i]],
					"SampleVariance" -> variances[[i]],
					"ConfidenceRadius" -> errors[[i]],
					"PrescribedError" -> confidenceradii[[i]]
				]
			,{i,1,Length[ordinaryfuns]}],
			Table[
				"ChordLength"[chords[[i,1]],chords[[i,2]]] -> Association[
					"SampleMean" -> means[[Length[ordinaryfuns]+i]],
					"SampleVariance" -> variances[[Length[ordinaryfuns]+i]],
					"ConfidenceRadius" -> errors[[Length[ordinaryfuns]+i]],
					"PrescribedError" -> confidenceradii[[Length[ordinaryfuns]+i]]
				]
			,{i,1,Length[chords]}]
		],
		"ConfidenceLevel" -> OptionValue["ConfidenceLevel"],
		"AmbientDimension" -> d,
		"EdgeCount" -> Length[r],
		"r" -> r,
		"\[Rho]" -> \[Rho],
		"SampleCount" -> samplecount,
		"MaxSamples" -> OptionValue["MaxSamples"],
		"QuotientSpace" -> TrueQ[OptionValue["QuotientSpace"]],
		"ThreadCount" -> OptionValue["ThreadCount"],
		"ChunkSize" -> OptionValue["ChunkSize"],
		"Chunks" -> samplecount/OptionValue["ChunkSize"],
		"Timing" -> time
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

	If[2 Max[r] > Total[r], 
		Message[CoBarSLink::badedgelengths]; 
		Return[$Failed]
	];
		
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

	If[2 Max[r] > Total[r], 
		Message[CoBarSLink::badedgelengths]; 
		Return[$Failed]
	];
		
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


Get[FileNameJoin[{$sourceDirectory, "cActionAngleSample.m"}]];

Options[ActionAngleSample] = {
	"ThreadCount" :> ("ParallelThreadNumber"/.("ParallelOptions"/.SystemOptions["ParallelOptions"])),
	"Progressive"->True
};

ActionAngleSample[edgecount_Integer?Positive, samplecount_Integer?Positive, OptionsPattern[]]:=Module[{p,trials},
	p = ConstantArray[0.,{samplecount,edgecount,3}];
	trials = cActionAngleSample[TrueQ[OptionValue["Progressive"]]][p,OptionValue["ThreadCount"]];
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
		Message[CoBarSLink::badsphereradii];
		Return[$Failed];
	,   
		Return[\[Rho]];
	];
,
	If[
		!StringQ[\[Rho]]
	,
		Message[CoBarSLink::badsphereradii2];
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
			_, (Message[CoBarSLink::badsphereradii3,\[Rho]];Return[$Failed];)
		];
	];
];


clearLibraries[]:=( Scan[DeleteFile,listLibraries[]]; Get[$packageFile]; );

listLibraries[]:=FileNames["*"<>CCompilerDriver`CCompilerDriverBase`$PlatformDLLExtension,$libraryDirectory];


End[];


EndPackage[];
