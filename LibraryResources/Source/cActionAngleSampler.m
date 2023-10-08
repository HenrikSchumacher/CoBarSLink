(* ::Package:: *)

(* The backend routine is a dynamic library that is compiled on the fly when it is called for the first time. Afterwards it is memoized. *)

ClearAll[cActionAngleSampler];
cActionAngleSampler := cActionAngleSampler = Module[{lib, code, name, t},

	name = "ActionAngleSampler";
	
	lib = FileNameJoin[{$libraryDirectory, name<>CCompilerDriver`CCompilerDriverBase`$PlatformDLLExtension}];

	If[Not[FileExistsQ[lib]],

		Print["Compiling c"<>name<>"..."];

		code=StringJoin[
"
#define NDEBUG

#include \"WolframLibrary.h\"
#include \"MMA.h\"

#include \"CycleSampler.hpp\

using namespace CycleSampler;
using namespace mma;

using Int  = mint;
using Real = mreal;

EXTERN_C DLLEXPORT int "<>name<>"(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res)
{
	MTensor p = get<MTensor>(Args[0]);

	const Int  thread_count  = get<Int>(Args[1]);
	const bool progressive_Q = get<Int>(Args[2])>0;

	const Int  sample_count  = dimensions(p)[0];
	const Int  edge_count    = dimensions(p)[1];
	
	Int trials;

	std::string class_name;

	Tools::Time start_time = Tools::Clock::now();

	
	if ( progressive_Q ) 
	{
		ActionAngleSampler<Real,Int,CycleSampler::Xoshiro256Plus,true> PAAM ( edge_count );
		
		trials = PAAM.RandomClosedPolygons( data<Real>(p), sample_count, thread_count );

		class_name = PAAM.ClassName();
	}
	else
	{
		ActionAngleSampler<Real,Int,CycleSampler::Xoshiro256Plus,false> AAM ( edge_count );
		
		trials = AAM.RandomClosedPolygons( data<Real>(p), sample_count, thread_count );

		class_name = AMM.ClassName();
	}

	Tools::Time stop_time = Tools::Clock::now();

	std::ofstream file ( \""<>$logFile<>"\" , std::ofstream::app );

	file << class_name << \" sampled \" << sample_count << \" polygons with \" <<  edge_count << \" edges in 3 D within \" << Tools::Duration(start_time,stop_time) << \" s.\" << std::endl;

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
			{Real,3,"Shared"}, Integer, Integer
		},
		Integer
	]
];
