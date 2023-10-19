(* ::Package:: *)

ClearAll[cConformalClosures];
cConformalClosures[d_Integer?Positive] := cConformalClosures[d] = Module[{lib, libname, file, code,ds, name, t},

	name = "ConformalClosures";

	ds = IntegerString[d];
	
	libname = name<>"_"<>ds<>"D";
	
	lib = FileNameJoin[{$libraryDirectory, libname<>CCompilerDriver`CCompilerDriverBase`$PlatformDLLExtension}];
	
	If[Not[FileExistsQ[lib]],

		Print["Compiling c"<>name<>"["<>ds<>"]..."];

		code = StringJoin["

#define NDEBUG

#include \"WolframLibrary.h\"

#include \"MMA.hpp\"
#include \"CoBarS.hpp\"

using Real = mreal;
using Int  = mint;

EXTERN_C DLLEXPORT int "<>name<>"(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res)
{
	MTensor r      = get<MTensor>(Args[0]);
	MTensor rho    = get<MTensor>(Args[1]);

	MTensor x      = get<MTensor>(Args[2]);
	MTensor w      = get<MTensor>(Args[3]);
	MTensor y      = get<MTensor>(Args[4]);

	MTensor K      = get<MTensor>(Args[5]);
	MTensor K_quot = get<MTensor>(Args[6]);

	const Int thread_count = get<Int>(Args[7]);

	const Int edge_count = std::min(
		std::min( dimension(r)[0], dimension(rho)[0] ),
		std::min( dimension(x)[1], dimension(y)[1] )
	);

	const Int sample_count = std::min(
		std::min(
			std::min( dimension(x)[0], dimension(y)[0] ),
			std::min( dimension(K)[0], dimension(K_quot)[0] )
		),
		dimension(w)[0]
	);

	CoBarS::Sampler<"<>ds<>",Real,Int> S ( data<Real>(r), data<Real>(rho), edge_count );

	Tools::Time start_time = Tools::Clock::now();
	
	S.ConformalClosures( 
		data<Real>(x), data<Real>(w), data<Real>(y), data<Real>(K), data<Real>(K_quot),
		sample_count, thread_count 
	);

	Tools::Time stop_time = Tools::Clock::now();

	std::ofstream file ( \""<>$logFile<>"\" , std::ofstream::app );

	file << S.ClassName() << \" sampled \" << sample_count << \" polygons with \" <<  edge_count << \" edges in "<>ds<>" D within \" << Tools::Duration(start_time,stop_time) << \" s.\" << std::endl;

	disown(w);
	disown(y);
	disown(K);
	disown(K_quot);
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
			{Real,3,"Constant"},
			{Real,2,"Shared"},
			{Real,3,"Shared"},
			{Real,1,"Shared"},
			{Real,1,"Shared"},
			Integer
		},
		"Void"
	]
];
