(* ::Package:: *)

(* The backend routine is a dynamic library that is compiled on the fly when it is called for the first time. Afterwards it is memoized. *)

Quiet[Scan[LibraryFunctionUnload,Cases[DownValues[cActionAngleSample],_LibraryFunction,All]]];
ClearAll[cActionAngleSample];
cActionAngleSample[progressiveQ:(True|False)] := cActionAngleSample[progressiveQ] = Module[{lib, code, name, t},

	name = "cActionAngleSample_"<>If[progressiveQ,"Progressive",""];
	
	lib = FileNameJoin[{$libraryDirectory, name<>CCompilerDriver`CCompilerDriverBase`$PlatformDLLExtension}];

	If[Not[FileExistsQ[lib]],

		Print["Compiling "<>name<>"..."];

		code=StringJoin[
"
#define NDEBUG

#include \"WolframLibrary.h\"
#include \"submodules/Tensors/MMA.hpp\"
#include \"CoBarS.hpp\"

using namespace Tools;
using namespace mma;

using Sampler_T = AAM::Sampler<Real,Int,CoBarS::Xoshiro256Plus,"<>If[TrueQ[progressiveQ],"true","false"]<>">;

EXTERN_C DLLEXPORT int "<>name<>"(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res)
{
	MTensor p = get<MTensor>(Args[0]);

	const Int  thread_count  = get<Int>(Args[1]);

	const Int  sample_count  = dimensions(p)[0];
	const Int  edge_count    = dimensions(p)[1];

	Tools::Time start_time = Tools::Clock::now();

	Sampler_T  S ( edge_count );
		
	Int  trials = S.CreateRandomClosedPolygons( data<Real>(p), sample_count, thread_count );

	Tools::Time stop_time = Tools::Clock::now();

	std::ofstream file ( \""<>$logFile<>"\" , std::ofstream::app );

	file << S.ClassName() << \" sampled \" << sample_count << \" polygons with \" <<  edge_count << \" edges in 3 D within \" << Tools::Duration(start_time,stop_time) << \" s.\" << std::endl;

	get<Int>(Res) = trials;
	disown(p);
	return LIBRARY_NO_ERROR;
}"];

		(* Invoke CreateLibrary to compile the C++ code. *)
		t = AbsoluteTiming[
			lib=CreateLibrary[
				code,
				name,
				"Language"->"C++",
				"TargetDirectory"-> $libraryDirectory,
				(*"ShellCommandFunction"\[Rule]Print,*)
				(*"ShellOutputFunction"\[Rule]Print,*)
				$compilationOptions
			]
		][[1]];
		Print["Compilation done. Time elapsed = ", t, " s.\n"];
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
