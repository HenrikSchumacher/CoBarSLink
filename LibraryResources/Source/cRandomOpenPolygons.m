(* ::Package:: *)

ClearAll[cRandomOpenPolygons];
cRandomOpenPolygons[d_Integer?Positive] := cRandomOpenPolygons[d] =  Module[{lib, libname, file, code,ds, name, t},

	name = "RandomOpenPolygons";

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
	MTensor x      = MArgument_getMTensor(Args[0]);

	const Int thread_count = int_cast<Int>(MArgument_getInteger(Args[1]));

	const Int sample_count = libData->MTensor_getDimensions(x)[0];
	const Int edge_count   = libData->MTensor_getDimensions(x)[1];

	Sampler<"<>ds<>",Real,Int> C (
		edge_count
	);

	Tools::Time start_time = Tools::Clock::now();
	
	C.RandomOpenPolygons( 
		libData->MTensor_getRealData(x),
		sample_count, 
		thread_count 
	);

	Tools::Time stop_time = Tools::Clock::now();

	std::ofstream file ( \""<>$logFile<>"\" , std::ofstream::app );

	file << C.ClassName() << \" sampled \" << sample_count << \" polygons with \" <<  edge_count << \" edges in "<>ds<>" D within \" << Tools::Duration(start_time,stop_time) << \" s.\" << std::endl;

	libData->MTensor_disown(x);

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
			{Real,3,"Shared"},
			Integer
		},
		"Void"
	]
];
