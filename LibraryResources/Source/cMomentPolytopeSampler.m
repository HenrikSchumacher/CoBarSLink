(* ::Package:: *)

(* The backend routine is a dynamic library that is compiled on the fly when it is called for the first time. Afterwards it is memoized. *)

ClearAll[cMomentPolytopeSampler];
cMomentPolytopeSampler := cMomentPolytopeSampler = Module[{lib, file, name, t},

	name = "MomentPolytopeSampler";
	
	lib = FileNameJoin[{$libraryDirectory, name<>CCompilerDriver`CCompilerDriverBase`$PlatformDLLExtension}];

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

		(* Invoke CreateLibrary to compile the C++ code. *)
		t = AbsoluteTiming[
			lib=CreateLibrary[
				{file},
				name,
				"TargetDirectory"-> $libraryDirectory,
				(*"ShellCommandFunction"\[Rule]Print,*)
				(*"ShellOutputFunction"\[Rule]Print,*)
				Get[FileNameJoin[{$sourceDirectory,"BuildSettings.m"}]]
			]
		][[1]];
		Print["Compilation done. Time elapsed = ", t, " s.\n"];
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
