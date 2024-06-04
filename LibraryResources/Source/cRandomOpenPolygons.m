(* ::Package:: *)

Quiet[Scan[LibraryFunctionUnload,Cases[DownValues[cRandomOpenPolygons],_LibraryFunction,All]]];
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
#include \"submodules/Tensors/MMA.hpp\"

#include \"CoBarS.hpp\"

using namespace Tools;
using namespace mma;

EXTERN_C DLLEXPORT int "<>name<>"(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res)
{
	MTensor x = get<MTensor>(Args[0]);

	const Int thread_count = get<Int>(Args[1]);

	const Int sample_count = dimensions(x)[0];
	const Int edge_count   = dimensions(x)[1];

	CoBarS::Sampler<"<>ds<>",Real,Int> S ( edge_count );

	Tools::Time start_time = Tools::Clock::now();
	
	S.CreateRandomOpenPolygons( data<Real>(x), sample_count, thread_count );

	Tools::Time stop_time = Tools::Clock::now();

	std::ofstream file ( \""<>$logFile<>"\" , std::ofstream::app );

	file << S.ClassName() << \" sampled \" << sample_count << \" polygons with \" <<  edge_count << \" edges in "<>ds<>" D within \" << Tools::Duration(start_time,stop_time) << \" s.\" << std::endl;

	disown(x);

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
