(* ::Package:: *)

ClearAll[cRandomClosedPolygons];
cRandomClosedPolygons[d_Integer?Positive]:=Module[{lib, libname, file, ds, name, t},

	name = "RandomClosedPolygons";

	ds = IntegerString[d];
	
	libname = name<>"_"<>ds<>"D";
	
	lib = FileNameJoin[{$libraryDirectory, libname<>CCompilerDriver`CCompilerDriverBase`$PlatformDLLExtension}];
	
	If[Not[FileExistsQ[lib]],

		Print["Compiling c"<>name<>"["<>ds<>"]..."];

		file=Export[FileNameJoin[{$sourceDirectory,name<>"_"<>ds<>"D.cpp"}],
"

#define NDEBUG

#define TOOLS_ENABLE_PROFILER

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

		(* Invoke CreateLibrary to compile the C++ code. *)
		t = AbsoluteTiming[
			lib=CreateLibrary[
				{file},
				libname,
				"TargetDirectory"-> $libraryDirectory,
				(*"ShellCommandFunction"\[Rule]Print,*)
				(*"ShellOutputFunction"\[Rule]Print,*)
				Get[FileNameJoin[{$sourceDirectory,"BuildSettings.m"}]]
			]
		][[1]];
		Print["Compilation done. Time elapsed = ", t, " s.\n"];
		DeleteFile[file];
	];

	cRandomClosedPolygons[d] = LibraryFunctionLoad[lib,name,
		{
			{Real,1,"Constant"},{Real,1,"Constant"},
			{Real,3,"Shared"},{Real,2,"Shared"},{Real,3,"Shared"},
			{Real,1,"Shared"},{Real,1,"Shared"},
			Integer
		},
		"Void"
	]
];
