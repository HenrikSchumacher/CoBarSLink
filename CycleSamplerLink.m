(* ::Package:: *)

BeginPackage["CycleSamplerLink`", {"CCompilerDriver`"}];


CycleSample::usage = "CycleSample[d_Integer, r_?VectorQ, \[Rho]_?VectorQ, samplecount_Integer] draws samplecount samples of closed polygons of edege lengths r in d-dimensional Euclidean space. Then it evaluates a couple of random variables on the samples and returns binned informations as well as their moments. The vector \[Rho] specifies the Riemannian metric on the edge space";

CycleSampleChordLengths::usage = "CycleSampleChordLengths[d_Integer, r_?VectorQ, \[Rho]_?VectorQ, vertexranges_?MatrixQ, samplecount_Integer] draws samplecount samples of closed polygons of edege lengths r in d-dimensional Euclidean space. Then it evaluates the chord length functions specified by vertexranges on the samples and returns binned informations as well as their moments. The vector \[Rho] specifies the Riemannian metric on the edge space";

RandomClosedPolygons::usage="RandomClosedPolygons[d_Integer, r_?VectorQ, \[Rho]_?VectorQ, samplecount_Integer] generates samplecount open Length[r]-gons in d dimensional Euclidean space, closes them via the conformal barycenter method. Then it returns: 
	(i)   the open polygons' unit edge vectors;
	(ii)  the conformal shift vectors; 
	(iii) the closed polygons' unit edge vectors;
	(iv)  the sampling weights for the edge space; and
	(v)   the sampling weights for the quotient space of the edge space by the action of SO(d).";


MomentPolytopeSample::usage="MomentPolytopeSample[edgecount_Integer?Positive, samplecount_Integer?Positive] samples samplecount closed, equilateral polygons with edgecount edges in 3-dimensional Euclidean space.";


Begin["`Private`"];


$packageDirectory  = DirectoryName[$InputFileName];

$libraryDirectory  = FileNameJoin[{$packageDirectory, "LibraryResources", $SystemID}];
If[!FileExists[$libraryDirectory],CreateDirectory[$libraryDirectory]];
$sourceDirectory   = FileNameJoin[{$packageDirectory, "LibraryResources", "Source"}];
If[!FileExists[$sourceDirectory],CreateDirectory[$sourceDirectory]];

(* Add $libraryDirectory to $LibraryPath in case the package is not installed in $UserBaseDirectory/Applications. *)
If[Not@MemberQ[$LibraryPath, $libraryDirectory],AppendTo[$LibraryPath, $libraryDirectory]];


ClearAll[cCycleSample];
cCycleSample[d_Integer?Positive]:=Module[{lib,file,ds,class,name},

	name = "CycleSample";

	ds = IntegerString[d];
	
	class[s_]:="std::make_unique<CycleSampler::"<>s<>"<"<>ds<>",mreal, mint>>";
	
	lib=FileNameJoin[{$libraryDirectory, name<>"_"<>ds<>"D"<>CCompilerDriver`CCompilerDriverBase`$PlatformDLLExtension}];

	If[Not[FileExistsQ[lib]],

		Print["Compiling c"<>name<>"["<>ds<>"]..."];

		file=Export[FileNameJoin[{$sourceDirectory,name<>"_"<>ds<>"D.cpp"}],
"

#include \"WolframLibrary.h\"
#include \"MMA.h\"

#include \"CycleSampler.hpp\"

EXTERN_C DLLEXPORT int "<>name<>"(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res)
{
	MTensor r       = MArgument_getMTensor(Args[0]);
	MTensor rho     = MArgument_getMTensor(Args[1]);

	MTensor bins    = MArgument_getMTensor(Args[2]);
	MTensor moments = MArgument_getMTensor(Args[3]);
	MTensor ranges  = MArgument_getMTensor(Args[4]);

	const mint sample_count = MArgument_getInteger(Args[5]);
	const mint thread_count = MArgument_getInteger(Args[6]);

	const mint clear_buffers = MArgument_getInteger(Args[7]);
	const mint set_ranges    = MArgument_getInteger(Args[8]);
	const mint normalize     = MArgument_getInteger(Args[9]);

	const mint bin_count    = libData->MTensor_getDimensions(bins)[2];
	const mint moment_count = libData->MTensor_getDimensions(moments)[2];

	const mint edge_count = std::min(
			libData->MTensor_getDimensions(r)[0],
			libData->MTensor_getDimensions(rho)[0]
	);

	std::vector< std::unique_ptr<CycleSampler::RandomVariable<"<>ds<>",mreal,mint>> > F_list;

	F_list.push_back( "<>class["ChordLength"]<>"( static_cast<mint>(0), static_cast<mint>(2) ) );
	F_list.push_back( "<>class["ChordLength"]<>"( static_cast<mint>(0), static_cast<mint>(3) ) );
	F_list.push_back( "<>class["ChordLength"]<>"( static_cast<mint>(0), static_cast<mint>(4) ) );
	F_list.push_back( "<>class["DiagonalLength"]<>"( ) );
	F_list.push_back( "<>class["Gyradius"]<>"( ) );
	F_list.push_back( "<>class["ShiftNorm"]<>"( ) );
	F_list.push_back( "<>class["TotalCurvature"]<>"( ) );
	F_list.push_back( "<>class["BendingEnergy"]<>"(2) );
	F_list.push_back( "<>class["MaxAngle"]<>"( ) );
	F_list.push_back( "<>class["EdgeSpaceSamplingWeight"]<>"( ) );
	F_list.push_back( "<>class["EdgeQuotientSpaceSamplingWeight"]<>"( ) );
	F_list.push_back( "<>class["IterationCount"]<>"( ) );

	const mint fun_count    = static_cast<mint>( F_list.size() );

	CycleSampler::Sampler<"<>ds<>",mreal,mint> C (
		libData->MTensor_getRealData(r),
		libData->MTensor_getRealData(rho),
		edge_count
	);

	if( clear_buffers != 0 )
	{
		std::fill( libData->MTensor_getRealData(bins), libData->MTensor_getRealData(bins) + 3*fun_count*bin_count, static_cast<mreal>(0));
		std::fill( libData->MTensor_getRealData(moments), libData->MTensor_getRealData(moments) + 3*fun_count*moment_count, static_cast<mreal>(0));
	}

	if( set_ranges != 0 )
	{
		mreal * restrict const ran = libData->MTensor_getRealData(ranges);
		for( mint j = 0; j < fun_count; ++j )
		{
			ran[2*j+0] = F_list[j]->MinValue( C );
			ran[2*j+1] = F_list[j]->MaxValue( C );
		}
	}

	C.Sample_Binned(
		libData->MTensor_getRealData(bins),
		bin_count,
		libData->MTensor_getRealData(moments),
		moment_count,
		libData->MTensor_getRealData(ranges),
		F_list,
		sample_count,
		thread_count
	);

	if( normalize != 0 )
	{
		C.NormalizeBinnedSamples(
			libData->MTensor_getRealData(bins),
			bin_count,
			libData->MTensor_getRealData(moments),
			moment_count,
			fun_count
		);
	}

	libData->MTensor_disown(bins);
	libData->MTensor_disown(moments);
	libData->MTensor_disown(ranges);

	return LIBRARY_NO_ERROR;
}",
"Text"
		];

		lib=CreateLibrary[
			{file},
			name<>"_"<>ds<>"D",
			"TargetDirectory"-> $libraryDirectory,
			(*"ShellCommandFunction"\[Rule]Print,*)
			(*"ShellOutputFunction"\[Rule]Print,*)
			Get[FileNameJoin[{$sourceDirectory,"BuildSettings.m"}]]
		];
		Print["Compilation done."];
		DeleteFile[file];
	];

	cCycleSample[d] = LibraryFunctionLoad[
		lib, 
		name,
		{
			{Real,1,"Constant"}, {Real,1,"Constant"},
			{Real,3,"Shared"}, {Real,3,"Shared"}, {Real,2,"Shared"},
			Integer, Integer, Integer, Integer, Integer
		},
		"Void"
	]
];


Options[CycleSample] = {
	(*"MaxIterations" -> 1000, 
	"ArmijoSlopeFactor" -> 0.01, 
	"ArmijoShrinkFactor" -> 0.5, 
	"Regularization" -> 1., 
	"Tolerance" -> 1./10^12,*)
	"BinCount" -> 1000, 
	"MomentCount" -> 3,
	"ThreadCount" :> ("ParallelThreadNumber"/.("ParallelOptions"/.SystemOptions["ParallelOptions"])),
	"Normalize" -> True,
	"Ranges" -> Automatic
};

CycleSample[d_Integer, r_?VectorQ, \[Rho]_?VectorQ, samplecount_, OptionsPattern[]]:=Module[{names, momentcount, bincount, funcount, bins, moments, ranges, setranges}, 
	bincount = OptionValue["BinCount"]; 
	momentcount = OptionValue["MomentCount"]; 
	names = {
		"ChordLength(0,2)", 
		"ChordLength(0,3)", 
		"ChordLength(0,4)", 
		"DiagonalLength", 
		"Gyradius", 
		"ShiftNorm", 
		"TotalCurvature", 
		"BendingEnergy(2)",
		"MaxAngle",
		"EdgeSpaceSamplingWeight",
		"EdgeQuotientSpaceSamplingWeight",
		"IterationCount"
	}; 
	funcount  = Length[names]; 
	bins      = ConstantArray[0., {3, funcount, bincount}]; 
	moments   = ConstantArray[0., {3, funcount, momentcount}]; 
	ranges    = OptionValue["Ranges"];
	setranges = False;
	If[ !(MatrixQ[ranges]&&Dimensions[ranges]=={funcount, 2}),
		ranges = ConstantArray[0., {funcount, 2}]; 
		setranges = True;
	];
	
	
	cCycleSample[d][
		r, \[Rho], bins, moments, ranges, samplecount, OptionValue["ThreadCount"],
		Boole[False], Boole[setranges], Boole[OptionValue["Normalize"]]
	];
	
	Association[
		"Naive" -> Association[
			Table[
				names[[i]] -> Association[
					"BinnedData" -> bins[[1,i]], 
					"Moments" -> moments[[1,i]], 
					"DataRange" -> ranges[[i]]
				]
				, 
				{i, 1, Length[names]}
			]
		], 
		"TotalSpace" -> Association[
			Table[
				names[[i]] -> Association[
					"BinnedData" -> bins[[2,i]], 
					"Moments" -> moments[[2,i]], 
					"DataRange" -> ranges[[i]]
				]
				, 
				{i, 1, Length[names]}
			]
		], 
		"QuotientSpace" -> Association[
			Table[
				names[[i]] -> Association[
					"BinnedData" -> bins[[3,i]], 
					"Moments" -> moments[[3,i]], 
					"DataRange" -> ranges[[i]]
				]
				, 
				{i, 1, Length[names]}
			]
		]
	]
];


ClearAll[cCycleSampleChordLengths];
cCycleSampleChordLengths[d_Integer?Positive]:=Module[{lib,file,ds,class,name},

	name = "CycleSampleChordLengths";

	ds=IntegerString[d];
	
	class[s_]:="std::make_unique<CycleSampler::"<>s<>"<"<>ds<>",mreal, mint>>";
	
	lib=FileNameJoin[{$libraryDirectory,name<>"_"<>ds<>"D"<>CCompilerDriver`CCompilerDriverBase`$PlatformDLLExtension}];

	If[Not[FileExistsQ[lib]],

		Print["Compiling c"<>name<>"["<>ds<>"]..."];

		file=Export[FileNameJoin[{$sourceDirectory,name<>"_"<>ds<>"D.cpp"}],
"

#include \"WolframLibrary.h\"
#include \"MMA.h\"

#include \"CycleSampler.hpp\"

EXTERN_C DLLEXPORT int "<>name<>"(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res)
{
	MTensor r       = MArgument_getMTensor(Args[0]);
	MTensor rho     = MArgument_getMTensor(Args[1]);

	MTensor bins    = MArgument_getMTensor(Args[2]);
	MTensor moments = MArgument_getMTensor(Args[3]);
	MTensor ranges  = MArgument_getMTensor(Args[4]);

	MTensor v_ranges = MArgument_getMTensor(Args[5]);

	const mint sample_count = MArgument_getInteger(Args[6]);
	const mint thread_count = MArgument_getInteger(Args[7]);

	const mint fun_count    = libData->MTensor_getDimensions(v_ranges)[0];
	const mint bin_count    = libData->MTensor_getDimensions(bins)[2];
	const mint moment_count = libData->MTensor_getDimensions(moments)[2];

	const mint edge_count = std::min(
			libData->MTensor_getDimensions(r)[0],
			libData->MTensor_getDimensions(rho)[0]
	);

	std::vector< std::unique_ptr<CycleSampler::RandomVariable<"<>ds<>",mreal,mint>> > F_list;

	for( mint i = 0; i < fun_count; ++i )
	{
		F_list.push_back( "<>class["ChordLength"]<>"( 
			libData->MTensor_getIntegerData(v_ranges)[2 * i    ], 
			libData->MTensor_getIntegerData(v_ranges)[2 * i + 1] 
		) );
	}


	CycleSampler::Sampler<"<>ds<>",mreal,mint> C (
		libData->MTensor_getRealData(r),
		libData->MTensor_getRealData(rho),
		edge_count
	);

	C.Sample_Binned(
		libData->MTensor_getRealData(bins),
		bin_count,
		libData->MTensor_getRealData(moments),
		moment_count,
		libData->MTensor_getRealData(ranges),
		F_list,
		sample_count,
		thread_count
	);

	libData->MTensor_disown(bins);
	libData->MTensor_disown(moments);
	libData->MTensor_disown(ranges);

	return LIBRARY_NO_ERROR;
}",
"Text"
		];

		lib=CreateLibrary[
			{file},
			name<>"_"<>ds<>"D",
			"TargetDirectory"-> $libraryDirectory,
			(*"ShellCommandFunction"\[Rule]Print,*)
			(*"ShellOutputFunction"\[Rule]Print,*)
			Get[FileNameJoin[{$sourceDirectory,"BuildSettings.m"}]]
		];
		Print["Compilation done."];
		DeleteFile[file];
	];

	cCycleSampleChordLengths[d] = LibraryFunctionLoad[
		lib, 
		name,
		{
			{Real,1,"Constant"}, {Real,1,"Constant"},
			{Real,3,"Shared"}, {Real,3,"Shared"}, {Real,2,"Shared"},
			{Integer,2,"Constant"},
			Integer, Integer
		},
		"Void"
	]
];


Options[CycleSampleChordLengths] = {
	(*"MaxIterations" -> 1000, 
	"ArmijoSlopeFactor" -> 0.01, 
	"ArmijoShrinkFactor" -> 0.5, 
	"Regularization" -> 1., 
	"Tolerance" -> 1./10^12,*)
	"BinCount" -> 1000, 
	"MomentCount" -> 3,  
	"ThreadCount" :> ("ParallelThreadNumber"/.("ParallelOptions"/.SystemOptions["ParallelOptions"]))
};

CycleSampleChordLengths[d_Integer, r_?VectorQ, \[Rho]_?VectorQ, vertexranges_?MatrixQ, samplecount_, OptionsPattern[]]:=Module[{names, momentcount, bincount, funcount, bins, moments, ranges}, 
	bincount = OptionValue["BinCount"]; 
	momentcount = OptionValue["MomentCount"]; 
	names = Table["ChordLength("<>IntegerString[v[[1]]]<>","<>IntegerString[v[[2]]]<>")",{v,vertexranges}];

	funcount = Length[names]; 
	bins     = ConstantArray[0., {3, funcount, bincount}]; 
	moments  = ConstantArray[0., {3, funcount, momentcount}]; 
	ranges   = ConstantArray[0., {   funcount, 2}]; 
	
	cCycleSampleChordLengths[d][r, \[Rho], bins, moments, ranges, vertexranges, samplecount, OptionValue["ThreadCount"]];
	
	Association[
		"Naive" -> Association[
			Table[
				names[[i]] -> Association[
					"BinnedData" -> bins[[1,i]], 
					"Moments" -> moments[[1,i]], 
					"DataRange" -> ranges[[i]]
				]
				, 
				{i, 1, Length[names]}
			]
		], 
		"TotalSpace" -> Association[
			Table[
				names[[i]] -> Association[
					"BinnedData" -> bins[[2,i]], 
					"Moments" -> moments[[2,i]], 
					"DataRange" -> ranges[[i]]
				]
				, 
				{i, 1, Length[names]}
			]
		], 
		"QuotientSpace" -> Association[
			Table[
				names[[i]] -> Association[
					"BinnedData" -> bins[[3,i]], 
					"Moments" -> moments[[3,i]], 
					"DataRange" -> ranges[[i]]
				]
				, 
				{i, 1, Length[names]}
			]
		]
	]
];


ClearAll[cRandomClosedPolygons];
cRandomClosedPolygons[d_Integer?Positive]:=Module[{lib,file,ds,name},

	name = "RandomClosedPolygons";
	
	ds = IntegerString[d];
	
	lib = FileNameJoin[{$libraryDirectory,name<>"_"<>ds<>"D"<>CCompilerDriver`CCompilerDriverBase`$PlatformDLLExtension}];

	If[Not[FileExistsQ[lib]],

		Print["Compiling c"<>name<>"["<>ds<>"]..."];

		file=Export[FileNameJoin[{$sourceDirectory,name<>"_"<>ds<>"D.cpp"}],
"

#include \"WolframLibrary.h\"
#include \"MMA.h\"

#include \"CycleSampler.hpp\"

EXTERN_C DLLEXPORT int "<>name<>"(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res)
{

	MTensor r      = MArgument_getMTensor(Args[0]);
	MTensor rho    = MArgument_getMTensor(Args[1]);

	MTensor x      = MArgument_getMTensor(Args[2]);
	MTensor w      = MArgument_getMTensor(Args[3]);
	MTensor y      = MArgument_getMTensor(Args[4]);

	MTensor K      = MArgument_getMTensor(Args[5]);
	MTensor K_quot = MArgument_getMTensor(Args[6]);

	const mint thread_count = MArgument_getInteger(Args[7]);

	const mint edge_count = std::min(
		std::min(
			libData->MTensor_getDimensions(r)[0],
			libData->MTensor_getDimensions(rho)[0]
		)
		,
		std::min(
			libData->MTensor_getDimensions(x)[1],
			libData->MTensor_getDimensions(y)[1]
		)
	);

	const mint sample_count = std::min(
		std::min(
			std::min( 
				libData->MTensor_getDimensions(x)[0], 
				libData->MTensor_getDimensions(y)[0]
			),
			std::min(
				libData->MTensor_getDimensions(K)[0],
				libData->MTensor_getDimensions(K_quot)[0]
			)
		),
		libData->MTensor_getDimensions(w)[0]
	);

	CycleSampler::Sampler<"<>ds<>",mreal,mint> C (
		libData->MTensor_getRealData(r),
		libData->MTensor_getRealData(rho),
		edge_count
	);

	C.RandomClosedPolygons( 
		libData->MTensor_getRealData(x), 
		libData->MTensor_getRealData(w),
		libData->MTensor_getRealData(y),
		libData->MTensor_getRealData(K),
		libData->MTensor_getRealData(K_quot),
		sample_count, 
		thread_count 
	);

	libData->MTensor_disown(x);
	libData->MTensor_disown(w);
	libData->MTensor_disown(y);
	libData->MTensor_getRealData(K);
	libData->MTensor_getRealData(K_quot);

	return LIBRARY_NO_ERROR;
}",
"Text"
		];

		lib=CreateLibrary[
			{file},
			name<>"_"<>ds<>"D",
			"TargetDirectory"-> $libraryDirectory,
			(*"ShellCommandFunction"\[Rule]Print,*)
			(*"ShellOutputFunction"\[Rule]Print,*)
			Get[FileNameJoin[{$sourceDirectory,"BuildSettings.m"}]]
		];
		Print["Compilation done."];
		DeleteFile[file];
	];

	cRandomClosedPolygons[d]=LibraryFunctionLoad[lib,name,
		{
			{Real,1,"Constant"},{Real,1,"Constant"},
			{Real,3,"Shared"},{Real,2,"Shared"},{Real,3,"Shared"},
			{Real,1,"Shared"},{Real,1,"Shared"},
			Integer
		},
		"Void"
	]
];


RandomClosedPolygons::len="Lengths of the input vectors in the second and third argument are expected to coincide.";

Options[RandomClosedPolygons ]= {
	"ThreadCount" :> ("ParallelThreadNumber"/.("ParallelOptions"/.SystemOptions["ParallelOptions"]))
}

RandomClosedPolygons[d_Integer, r_?VectorQ, \[Rho]_?VectorQ, samplecount_, OptionsPattern[]]:=If[
	Length[r]==Length[\[Rho]]
	,
	Module[{W,edgecount,x,w,y,Klist,Kquotlist},
		edgecount=Length[r];
		
		x = ConstantArray[0.,{samplecount,edgecount,d}];
		w = ConstantArray[0.,{samplecount,          d}];
		y = ConstantArray[0.,{samplecount,edgecount,d}];
		
		Klist     = ConstantArray[0.,{samplecount}];
		Kquotlist = ConstantArray[0.,{samplecount}];
		
		cRandomClosedPolygons[d][r,\[Rho],x,w,y,Klist,Kquotlist,OptionValue["ThreadCount"]];
		
		Association[
			"OpenPolygonUnitEdgeVectors"->x,
			"ShiftVectors"->w,
			"ClosedPolygonUnitEdgeVectors"->y,
			"EdgeSpaceSamplingWeights"->Klist,
			"EdgeQuotientSpaceSamplingWeights"->Kquotlist,
			"EdgeLengths"->r,
			"RiemannianWeights"->\[Rho]
		]
	]
	,
	(
		Message[RandomClosedPolygons::len];
		$Failed
	)
];


ClearAll[cMomentPolytopeSampler];
cMomentPolytopeSampler := cMomentPolytopeSampler = Module[{lib,file,name},

	name = "MomentPolytopeSampler";
	
	lib=FileNameJoin[{$libraryDirectory,name<>CCompilerDriver`CCompilerDriverBase`$PlatformDLLExtension}];

	If[Not[FileExistsQ[lib]],

		Print["Compiling c"<>name<>"..."];

		file=Export[FileNameJoin[{$sourceDirectory,name<>".cpp"}],
"

#include \"WolframLibrary.h\"
#include \"MMA.h\"

#include \"CycleSampler.hpp\"

EXTERN_C DLLEXPORT int "<>name<>"(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res)
{
	MTensor p = MArgument_getMTensor(Args[0]);
	const mint thread_count = MArgument_getInteger(Args[1]);

	const mint sample_count = libData->MTensor_getDimensions(p)[0];
	const mint edge_count   = libData->MTensor_getDimensions(p)[1];

	CycleSampler::MomentPolytopeSampler<mreal,mint> M ( edge_count );

	const mint trials = M.RandomClosedPolygons(
		libData->MTensor_getRealData(p),
		sample_count,
		thread_count
	);

	MArgument_setInteger(Res, trials);

	libData->MTensor_disown(p);

	return LIBRARY_NO_ERROR;
}",
"Text"
		];

		lib=CreateLibrary[
			{file},
			name,
			"TargetDirectory"-> $libraryDirectory,
			(*"ShellCommandFunction"\[Rule]Print,*)
			(*"ShellOutputFunction"\[Rule]Print,*)
			Get[FileNameJoin[{$sourceDirectory,"BuildSettings.m"}]]
		];
		Print["Compilation done."];
		DeleteFile[file];
	];

	LibraryFunctionLoad[
		lib, 
		name,
		{
			{Real,3,"Shared"}, Integer
		},
		Integer
	]
];


Options[MomentPolytopeSample] = {
	"ThreadCount" :> ("ParallelThreadNumber"/.("ParallelOptions"/.SystemOptions["ParallelOptions"]))
};

MomentPolytopeSample[edgecount_Integer?Positive, samplecount_Integer?Positive, OptionsPattern[]]:=Module[{p,trials},
	p = ConstantArray[0.,{samplecount,edgecount,3}];
	trials = cMomentPolytopeSampler[p,OptionValue["ThreadCount"]];
	Association[
		"ClosedPolygons"->p,
		"Trials"->trials
	]
]


End[];


EndPackage[];
