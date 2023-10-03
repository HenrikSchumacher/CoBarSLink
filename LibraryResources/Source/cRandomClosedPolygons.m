(* ::Package:: *)

ClearAll[cRandomClosedPolygons];
cRandomClosedPolygons[d_Integer?Positive] := cRandomClosedPolygons[d] = Module[{lib, libname, file, code,ds, name, t},

	name = "RandomClosedPolygons";

	ds = IntegerString[d];
	
	libname = name<>"_"<>ds<>"D";
	
	lib = FileNameJoin[{$libraryDirectory, libname<>CCompilerDriver`CCompilerDriverBase`$PlatformDLLExtension}];
	
	If[Not[FileExistsQ[lib]],

		Print["Compiling c"<>name<>"["<>ds<>"]..."];

		code = StringJoin["

#define NDEBUG

#include \"WolframLibrary.h\"
#include \"MMA.h\"

#include \"CycleSampler.hpp\"

using namespace CycleSampler;

using Real = mreal;
using Int  = int_fast32_t;

EXTERN_C DLLEXPORT int "<>name<>"(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res)
{
	MTensor r      = MArgument_getMTensor(Args[0]);
	MTensor rho    = MArgument_getMTensor(Args[1]);

	MTensor x      = MArgument_getMTensor(Args[2]);
	MTensor w      = MArgument_getMTensor(Args[3]);
	MTensor y      = MArgument_getMTensor(Args[4]);

	MTensor K      = MArgument_getMTensor(Args[5]);
	MTensor K_quot = MArgument_getMTensor(Args[6]);

	const Int thread_count = int_cast<Int>(MArgument_getInteger(Args[7]));

	const Int edge_count = int_cast<Int>(std::min(
		std::min(
			libData->MTensor_getDimensions(r)[0],
			libData->MTensor_getDimensions(rho)[0]
		)
		,
		std::min(
			libData->MTensor_getDimensions(x)[1],
			libData->MTensor_getDimensions(y)[1]
		)
	));

	const Int sample_count = int_cast<Int>(std::min(
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
	));

	Sampler<"<>ds<>",Real,Int> C (
		libData->MTensor_getRealData(r),
		libData->MTensor_getRealData(rho),
		edge_count
	);

	Tools::Time start_time = Tools::Clock::now();
	
	C.RandomClosedPolygons( 
		libData->MTensor_getRealData(x), 
		libData->MTensor_getRealData(w),
		libData->MTensor_getRealData(y),
		libData->MTensor_getRealData(K),
		libData->MTensor_getRealData(K_quot),
		sample_count, 
		thread_count 
	);

	Tools::Time stop_time = Tools::Clock::now();

	std::ofstream file ( \""<>$logFile<>"\" , std::ofstream::app );

	file << C.ClassName() << \" sampled \" << sample_count << \" polygons with \" <<  edge_count << \" edges in "<>ds<>" D within \" << Tools::Duration(start_time,stop_time) << \" s.\" << std::endl;

	libData->MTensor_disown(x);
	libData->MTensor_disown(w);
	libData->MTensor_disown(y);
	libData->MTensor_disown(K);
	libData->MTensor_disown(K_quot);

	return LIBRARY_NO_ERROR;
}"];

		(* Invoke CreateLibrary to compile the C++ code. *)
		t = AbsoluteTiming[
			lib=CreateLibrary[
				code,
				libname,
				"Language"->"C++",
				"TargetDirectory"-> $libraryDirectory,
				(*"ShellCommandFunction"\[Rule]Print,*)
				"ShellOutputFunction"->Print,
				$compilationOptions
			]
		][[1]];
		Print["Compilation done. Time elapsed = ", t, " s.\n"];
	];

	LibraryFunctionLoad[lib,name,
		{
			{Real,1,"Constant"},
			{Real,1,"Constant"},
			{Real,3,"Shared"},
			{Real,2,"Shared"},
			{Real,3,"Shared"},
			{Real,1,"Shared"},
			{Real,1,"Shared"},
			Integer
		},
		"Void"
	]
];
