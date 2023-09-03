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

#include \"CycleSampler.hpp\"

EXTERN_C DLLEXPORT int "<>name<>"(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res)
{
	MTensor p = MArgument_getMTensor(Args[0]);
	const mint thread_count = MArgument_getInteger(Args[1]);
	const bool iterative_Q  = MArgument_getInteger(Args[2])>0;

	const mint sample_count = libData->MTensor_getDimensions(p)[0];
	const mint edge_count   = libData->MTensor_getDimensions(p)[1];
	
	mint trials;

	std::string class_name;

	Tools::Time start_time = Tools::Clock::now();

	
	if ( iterative_Q ) 
	{
		CycleSampler::ActionAngleSampler<mreal,mint,CycleSampler::Xoshiro256Plus,true> M ( edge_count );
		
		trials = M.RandomClosedPolygons(
			libData->MTensor_getRealData(p),
			sample_count,
			thread_count
		);

		class_name = M.ClassName();
	}
	else
	{
		CycleSampler::ActionAngleSampler<mreal,mint,CycleSampler::Xoshiro256Plus,false> M ( edge_count );
		
		trials = M.RandomClosedPolygons(
			libData->MTensor_getRealData(p),
			sample_count,
			thread_count
		);

		class_name = M.ClassName();
	}

	Tools::Time stop_time = Tools::Clock::now();

	std::ofstream file ( \""<>$logFile<>"\" , std::ofstream::app );

	file << class_name << \" sampled \" << sample_count << \" polygons with \" <<  edge_count << \" edges in 3 D within \" << Tools::Duration(start_time,stop_time) << \" s.\" << std::endl;

	MArgument_setInteger(Res, trials);

	libData->MTensor_disown(p);

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
