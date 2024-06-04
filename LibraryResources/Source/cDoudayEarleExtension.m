(* ::Package:: *)

ClearAll[cDouadyEarleExtension];
cDouadyEarleExtension[d_Integer?Positive] := cDouadyEarleExtension[d] = Module[{lib, libname, file, code,ds, name, t},

	name = "DouadyEarleExtension";

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

using namespace mma;
using namespace Tools;

static constexpr Int AmbDim = "<>ds<>";

using DouadyEarle_T = CoBarS::DouadyEarleExtension<AmbDim,Real,Int>;

EXTERN_C DLLEXPORT int "<>name<>"(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res)
{
	MTensor curve  = get<MTensor>(Args[0]);
	MTensor disk_points = get<MTensor>(Args[1]);

	const Int edge_count   = get<Int>(Args[2]);
	const Int thread_count = get<Int>(Args[3]);

	if( dimensions(curve)[1] != AmbDim )
	{
		eprint( std::string( \""<>name<>": Second dimension of first input does not match ambient dimension "<>ds<>".\") );
		return !LIBRARY_NO_ERROR;
	}

	if( dimensions(disk_points)[1] != 2 )
	{
		eprint( std::string( \""<>name<>": Second dimension of second input does not equal to 2.\") );
		return !LIBRARY_NO_ERROR;
	}

	MTensor ball_points = make_MTensor<Real>( { dimensions(disk_points)[0], AmbDim } );

	DouadyEarle_T E( edge_count );

	E.LoadCurve( data<Real>(curve), dimensions(curve)[0] );

	E( data<Real>(disk_points), dimensions(disk_points)[0], data<Real>(ball_points), thread_count );

	get<MTensor>(Res) = ball_points;

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
			{Real,2,"Constant"},(*the curve to extend*)
			{Real,2,"Constant"},(*the 2D points to evaluate on*)
			Integer,
			Integer
		},
		{Real,2}
	]
];
