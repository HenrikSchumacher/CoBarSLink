(* ::Package:: *)

Quiet[Scan[LibraryFunctionUnload,Cases[DownValues[cRandomClosedPolygons],_LibraryFunction,All]]];
ClearAll[cRandomClosedPolygons];
cRandomClosedPolygons[
	d_Integer?Positive,
	rng:"MersenneTwister"|"PCG"|"Xoshiro"|"WY":"Xoshiro",
	vectorizedQ:True|False:True,
	zerofyfirstQ:True|False:False	
] := cRandomClosedPolygons[d,rng,vectorizedQ,zerofyfirstQ] = Module[{lib, libname, file, code,ds, name, t},

	name = "RandomClosedPolygons";

	ds = IntegerString[d];
	
	libname = StringJoin[name,"_",ds,"D","_",rng,"_",ToString[Boole[vectorizedQ]],"_",ToString[Boole[zerofyfirstQ]]];
	
	lib = FileNameJoin[{$libraryDirectory, libname<>CCompilerDriver`CCompilerDriverBase`$PlatformDLLExtension}];
	
	If[Not[FileExistsQ[lib]],

		Print["Compiling c"<>libname<>"..."];

		code = StringJoin["

#define NDEBUG

#include \"WolframLibrary.h\"

#include \"submodules/Tensors/MMA.hpp\"
#include \"CoBarS.hpp\"

using namespace Tools;
using namespace mma;

using RNG_T = ",
	Switch[rng,
		"MersenneTwister" ,"CoBarS::MT64",
		"PCG"             ,"CoBarS::PCG64",
		"Xoshiro"         ,"CoBarS::Xoshiro256Plus",
		"WY"              ,"CoBarS::WyRand"
	],
";

constexpr bool vectorizedQ  = ",If[vectorizedQ,"true","false"],";
constexpr bool zerofyfirstQ = ",If[zerofyfirstQ,"true","false"],";

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

	const Int edge_count = static_cast<Int>( std::min(
		std::min( dimensions(r)[0], dimensions(rho)[0] ) ,
		std::min( dimensions(x)[1], dimensions(y)[1] )
	) );

	const Int sample_count = static_cast<Int>( std::min(
		std::min(
			std::min( dimensions(x)[0], dimensions(y)[0] ),
			std::min( dimensions(K)[0], dimensions(K_quot)[0] )
		),
		dimensions(w)[0]
	) );

	CoBarS::Sampler<"<>ds<>",Real,Int,RNG_T,vectorizedQ,zerofyfirstQ> S ( data<Real>(r), data<Real>(rho), edge_count );

	Tools::Time start_time = Tools::Clock::now();
	
	S.CreateRandomClosedPolygons( 
		data<Real>(x), data<Real>(w), data<Real>(y), data<Real>(K), data<Real>(K_quot),
		sample_count, thread_count 
	);

	Tools::Time stop_time = Tools::Clock::now();

	std::ofstream file ( \""<>$logFile<>"\" , std::ofstream::app );

	file << S.ClassName() << \" sampled \" << sample_count << \" polygons with \" <<  edge_count << \" edges in "<>ds<>" D within \" << Tools::Duration(start_time,stop_time) << \" s.\" << std::endl;

	disown(x);
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
