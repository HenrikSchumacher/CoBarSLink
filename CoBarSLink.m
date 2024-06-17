(* ::Package:: *)

BeginPackage["CoBarSLink`", {"CCompilerDriver`"}];


Begin["`Private`"];

sphereRadiiUsage = "
\"SphereRadii\" -> \[Rho], \"One\", \"SymplecticQuotient\", \"EdgeLengths\". The space \!\(\*SubscriptBox[\(Pol\), \(d\)]\)(n;r) of closed n-edge polygons 
in \!\(\*SuperscriptBox[\(\[DoubleStruckCapitalR]\), \(d\)]\) (up to translation) with edgelengths r=(\!\(\*SubscriptBox[\(r\), \(1\)]\),...,\!\(\*SubscriptBox[\(r\), \(n\)]\)) is parametrized by lists of unit vectors x=(\!\(\*SubscriptBox[\(x\), \(1\)]\),...,\!\(\*SubscriptBox[\(x\), \(n\)]\)) 
in \!\(\*SuperscriptBox[\(S\), \(d - 1\)]\)\[Cross]...\[Cross]\!\(\*SuperscriptBox[\(S\), \(d - 1\)]\) with \[Sum] \!\(\*SubscriptBox[\(r\), \(i\)]\)\!\(\*SubscriptBox[\(x\), \(i\)]\)=0. If we choose a metric on each \!\(\*SuperscriptBox[\(S\), \(d - 1\)]\), the probability measure on this space is determined 
by its volume form as a Riemannian submanifold of the product of spheres. CoBarS always assumes that the metric on 
each sphere is round and allows the user to choose the (metric) radius of the sphere. 

There are four options: 

	a list of positive reals \!\(\*SubscriptBox[\(\[CurlyRho]\), \(1\)]\),...,\!\(\*SubscriptBox[\(\[CurlyRho]\), \(n\)]\) giving sphere radii explicitly, 

	the string \"One\" setting all of the sphere metrics to unit spheres, 

	the string \"SymplecticQuotient\", which sets \!\(\*SubscriptBox[\(\[CurlyRho]\), \(i\)]\)=\!\(\*SqrtBox[SubscriptBox[\(r\), \(i\)]]\) to make the 
	Riemannian volume of \!\(\*SubscriptBox[OverscriptBox[\(Pol\), \(^\)], \(3\)]\)(n;r) = \!\(\*SubscriptBox[\(Pol\), \(3\)]\)(n;r)/SO(3) equal to the Liouville (symplectic) volume,

	the string \"EdgeLengths\", which sets \!\(\*SubscriptBox[\(\[CurlyRho]\), \(i\)]\)=\!\(\*SubscriptBox[\(r\), \(i\)]\).

\"QuotientSpace\" - If set to True, then the Riemannian quotient space probability measure with respect to 
					the SO(d) action is used for sampling. This is the default. 
                    Otherwise, the push-forward measure along the quotient map is used. 

\"ThreadCount\" - The number of threads to be used for sampling. The default is \"ParallelOptions\"/.\[VeryThinSpace]SystemOptions[\"ParallelOptions\"].

To sample from the symplectic volume on \!\(\*SubscriptBox[OverscriptBox[\(Pol\), \(^\)], \(3\)]\)(n;r) as in Kapovich and Millson (The Symplectic Geometry of Polygons in Euclidean Space, 1996)
or Cantarella, Schumacher, Shonkwiler (A Faster Direct Sampling Algorithm for Equilateral Closed Polygons, 2023), 
use \"SphereRadii\"->\"SymplecticQuotient\" and \"QuotientSpace\"->True.
";

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


CoBarConfidenceSample::usage = "CoBarConfidenceSample[funs:{__}, d_Integer?Positive, r_?(VectorQ[#,NumericQ]&), confidenceradii:{___?NumericQ}, opts___] draws samples of closed polygons in d-dimensional Euclidean space and evaluates the list of random variables specified by funs on them. It stops if each random variable's confidence radius is below the prescribed value in the condidence radii. Returned are sample mean, sample variance, and some further statistics. The drawn polygons are discarded afterwards. The vector r contains the length of each edge of the polygon. 

The following options can be set:
"<>CoBarSLink`Private`sphereRadiiUsage<>CoBarSLink`Private`confidenceUsage;


RandomOpenPolygons::usage = "RandomOpenPolygons[d_Integer?Positive, edgecount_Integer?Positive, samplecount_Integer?Positive] generates samplecount open Length[r]-gons in d dimensional Euclidean space. The result is equivalent to Accumulate[Append[#,ConstantArray[0.,d]]&/@RandomPoint[Sphere[ConstantArray[0.,d]],{samplecount,edgecount}]] -- just faster.";


RandomClosedPolygons::usage = "RandomClosedPolygons[d_Integer?Positive, r_?(VectorQ[#,NumericQ]&), samplecount_Integer?Positive] generates samplecount open Length[r]-gons in d dimensional Euclidean space, closes them via the conformal barycenter method. Then it returns the following data in an Association: 
	(i)   \"VertexPositions\" - the closed polygons' vertex positions (the first of each polygon vertex is duplicated and appended);
	(ii)  the sampling weights (the type of sampling weights is determined by the value of the option \"QuotientSpace\" (see below)).

The following options can be set:
"<>CoBarSLink`Private`sphereRadiiUsage;


ConformalClosures::usage = "ConformalClosures[r_?(VectorQ[#,NumericQ]&), p_?((ArrayQ[#]&&(ArrayDepth[#]==3))&)] closes the Length[r]-gons stored in the 3-tensor p via the conformal barycenter method. Then it returns
	(i)   the open polygons' vertex positions.
	(ii)  the conformal shift vectors; 
	(iii) the closed polygons' vertex positions (each polygon's first vertex is duplicated and appended at the end;
	(iv)  the sampling weights for the edge space; and
	(v)   the sampling weights for the quotient space of the edge space by the action of SO(d).

The following options can be set:
"<>CoBarSLink`Private`sphereRadiiUsage;


ActionAngleSample::usage = "ActionAngleSample[edgecount_Integer?Positive, samplecount_Integer?Positive] samples samplecount closed, equilateral polygons with edgecount edges in 3-dimensional Euclidean space and returns their vertex coordinates. The first of each polygon vertex is duplicated and appended.";


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

RandomOpenPolygons[d_Integer?Positive, edgecount_Integer?Positive, samplecount_Integer?Positive, OptionsPattern[]]:=Module[{p},

	p = ConstantArray[0.,{samplecount,edgecount+1,d}];

	cRandomOpenPolygons[d][p,Min[samplecount,OptionValue["ThreadCount"]]];

	p
];


Get[FileNameJoin[{$sourceDirectory, "cRandomClosedPolygons.m"}]];

RandomClosedPolygons::len="Lengths of the input vectors in the second and third argument are expected to coincide.";

Options[RandomClosedPolygons ]= {
	"ThreadCount" :> ("ParallelThreadNumber"/.("ParallelOptions"/.SystemOptions["ParallelOptions"])),
	"SphereRadii" -> "EdgeLengths",
	"QuotientSpace" -> True
};

RandomClosedPolygons[d_Integer?Positive, r_?(VectorQ[#,NumericQ]&), samplecount_Integer?Positive, OptionsPattern[]]:=Module[{W,edgecount,q,Klist,\[Rho]},

	If[2 Max[r] > Total[r], 
		Message[CoBarSLink::badedgelengths]; 
		Return[$Failed]
	];
		
	\[Rho] = processSphereRadii[r, OptionValue["SphereRadii"]];
	If[\[Rho]===$Failed, Return[$Failed]];

	edgecount=Length[r];
	
	q = ConstantArray[0.,{samplecount,edgecount+1,d}];

	Klist     = ConstantArray[0.,{samplecount}];

	cRandomClosedPolygons[d][r,\[Rho],q,Klist,OptionValue["QuotientSpace"],Min[samplecount,OptionValue["ThreadCount"]]];

	Association[
		"VertexPositions"->q,
		"SamplingWeights"->Klist,
		"EdgeLengths"->r,
		"SphereRadii"->\[Rho],
		"QuotientSpace"->OptionValue["QuotientSpace"]
	]
];


Get[FileNameJoin[{$sourceDirectory, "cConformalClosures.m"}]];

ConformalClosures::len="Length of the first input does coincide with second dimension of second input.";

Options[ConformalClosures ]= {
	"ThreadCount" :> ("ParallelThreadNumber"/.("ParallelOptions"/.SystemOptions["ParallelOptions"])),
	"SphereRadii" -> "EdgeLengths"
};

ConformalClosures[r_?(VectorQ[#,NumericQ]&), p_?((ArrayQ[#]&&(ArrayDepth[#]==3))&), OptionsPattern[]]:=Module[{W,edgecount,samplecount,d,w,q,Klist,Kquotlist,\[Rho]},

	If[2 Max[r] > Total[r], 
		Message[CoBarSLink::badedgelengths]; 
		Return[$Failed]
	];
		
	\[Rho] = processSphereRadii[r, OptionValue["SphereRadii"]];
	If[\[Rho]===$Failed, Return[$Failed]];

	samplecount = Dimensions[p][[1]];
	edgecount   = Dimensions[p][[2]]-1;
	d           = Dimensions[p][[3]];
	
	If[ Length[r] != edgecount,
		Message[ConformalClosures::len];
		Return[$Failed];
	];
	
	w = ConstantArray[0.,{samplecount,            d}];
	q = ConstantArray[0.,{samplecount,edgecount+1,d}];

	Klist     = ConstantArray[0.,{samplecount}];
	Kquotlist = ConstantArray[0.,{samplecount}];

	cConformalClosures[d][r,\[Rho],p,w,q,Klist,Kquotlist,Min[samplecount,OptionValue["ThreadCount"]]];

	Association[
		"OpenPolygonVertexPositions"->p,
		"ShiftVectors"->w,
		"ClosedPolygonVertexPositions"->q,
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
	
	p = ConstantArray[0.,{samplecount,edgecount+1,3}];
	
	trials = cActionAngleSample[TrueQ[OptionValue["Progressive"]]][p,OptionValue["ThreadCount"]];
	Association[
		"VertexPositions"->p,
		"Trials"->trials
	]
]


Get[FileNameJoin[{$sourceDirectory, "cDoudayEarleExtension.m"}]];


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
