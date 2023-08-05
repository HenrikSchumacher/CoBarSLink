(* ::Package:: *)

Quiet[LibraryFunctionUnload[cppRectangleConvolutionPower]];
ClearAll[cppRectangleConvolutionPower];
cppRectangleConvolutionPower = Module[{name,lib,libname,code,t},
	name = "cppRectangleConvolutionPower";
		
	libname = name;
	
	lib = FileNameJoin[{$libraryDirectory, libname<>CCompilerDriver`CCompilerDriverBase`$PlatformDLLExtension}];
	
	If[Not[FileExistsQ[lib]],
	
		Print["Compiling "<>name<>"..."];

		code=StringJoin["
#include \"WolframLibrary.h\"
#include \"MMA.h\"

#include \"CycleSampler.hpp\"
#include \"src/RectangleConvolutionPower.hpp\"

using namespace Tools;
using namespace Tensors;
using namespace CycleSampler;

EXTERN_C DLLEXPORT int "<>name<>"(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res)
{
	mreal x = MArgument_getReal   (Args[0]);
	mint  n = MArgument_getInteger(Args[1]);

	RectangleConvolutionPower<mreal,mint> f ( n );

	MArgument_setReal(Res, f.Value(x) );

	return LIBRARY_NO_ERROR;
}"
		];
	
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
	
	LibraryFunctionLoad[lib, name, {Real, Integer}, Real]
];


Quiet[LibraryFunctionUnload[cppRectangleConvolutionPowerMany]];
ClearAll[cppRectangleConvolutionPowerMany];
cppRectangleConvolutionPowerMany = Module[{name,lib,libname,code,t},
	name = "cppRectangleConvolutionPowerMany";

	libname = name;
	
	lib = FileNameJoin[{$libraryDirectory, libname<>CCompilerDriver`CCompilerDriverBase`$PlatformDLLExtension}];
	
	If[Not[FileExistsQ[lib]],
	
		Print["Compiling "<>name<>"..."];
	
		code=StringJoin["
#include \"WolframLibrary.h\"
#include \"MMA.h\"

#include \"CycleSampler.hpp\"
#include \"src/RectangleConvolutionPower.hpp\"

using namespace Tools;
using namespace Tensors;
using namespace CycleSampler;

EXTERN_C DLLEXPORT int "<>name<>"(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res)
{
	MTensor x_              = MArgument_getMTensor(Args[0]);
	const mint n            = MArgument_getInteger(Args[1]);
	const mint thread_count = MArgument_getInteger(Args[2]);

	const mint m            = libData->MTensor_getDimensions(x_)[0];

	cptr<mreal> x = libData->MTensor_getRealData(x_);

	MTensor y_;

	(void)libData->MTensor_new(MType_Real, 1, &m, &y_);

	mptr<mreal> y = libData->MTensor_getRealData(y_);

	ParallelDo(
		[x,y,m,n,thread_count]( const mint thread )
		{
			RectangleConvolutionPower<mreal,mint> f ( n );

			const mint i_begin = JobPointer( m, thread_count, thread     );
			const mint i_end   = JobPointer( m, thread_count, thread + 1 );

			for( mint i = i_begin; i < i_end; ++i )
			{
				y[i] = f.Value(x[i]);
			}
		},
		thread_count
	);

	MArgument_setMTensor(Res, y_ );

	return LIBRARY_NO_ERROR;
}"
		];
	
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
	
	LibraryFunctionLoad[lib, name, {{Real, 1, "Constant"}, Integer, Integer}, {Real, 1}]
];


Quiet[LibraryFunctionUnload[cppDRectangleConvolutionPower]];
ClearAll[cppDRectangleConvolutionPower];
cppDRectangleConvolutionPower=Module[{name,lib,libname,code,t},
	name = "cppDRectangleConvolutionPower";

	libname = name;
	
	lib = FileNameJoin[{$libraryDirectory, libname<>CCompilerDriver`CCompilerDriverBase`$PlatformDLLExtension}];
	
	If[Not[FileExistsQ[lib]],
	
		Print["Compiling "<>name<>"..."];
	
		code=StringJoin["
#include \"WolframLibrary.h\"
#include \"MMA.h\"

#include \"CycleSampler.hpp\"
#include \"src/RectangleConvolutionPower.hpp\"

using namespace Tools;
using namespace Tensors;
using namespace CycleSampler;

EXTERN_C DLLEXPORT int "<>name<>"(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res)
{
	mreal x = MArgument_getReal   (Args[0]);
	mint  n = MArgument_getInteger(Args[1]);

	RectangleConvolutionPower<mreal,mint> f ( n );

	MArgument_setReal(Res, f.Derivative(x) );

	return LIBRARY_NO_ERROR;
}"
		];
	
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

	LibraryFunctionLoad[lib, name, {Real, Integer}, Real]
];


Quiet[LibraryFunctionUnload[cppDRectangleConvolutionPowerMany]];
ClearAll[cppDRectangleConvolutionPowerMany];
cppDRectangleConvolutionPowerMany = Module[{name,lib,libname,code,t},
	name = "cppDRectangleConvolutionPowerMany";

	libname = name;
	
	lib = FileNameJoin[{$libraryDirectory, libname<>CCompilerDriver`CCompilerDriverBase`$PlatformDLLExtension}];
	
	If[Not[FileExistsQ[lib]],
	
		Print["Compiling "<>name<>"..."];
	
		code=StringJoin["
#include \"WolframLibrary.h\"
#include \"MMA.h\"

#include \"CycleSampler.hpp\"
#include \"src/RectangleConvolutionPower.hpp\"

using namespace Tools;
using namespace Tensors;
using namespace CycleSampler;

EXTERN_C DLLEXPORT int "<>name<>"(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res)
{
	MTensor x_              = MArgument_getMTensor(Args[0]);
	const mint n            = MArgument_getInteger(Args[1]);
	const mint thread_count = MArgument_getInteger(Args[2]);

	const mint m            = libData->MTensor_getDimensions(x_)[0];

	cptr<mreal> x = libData->MTensor_getRealData(x_);

	MTensor y_;

	(void)libData->MTensor_new(MType_Real, 1, &m, &y_);

	mptr<mreal> y = libData->MTensor_getRealData(y_);

	ParallelDo(
		[x,y,m,n,thread_count]( const mint thread )
		{
			RectangleConvolutionPower<mreal,mint> f ( n );

			const mint i_begin = JobPointer( m, thread_count, thread     );
			const mint i_end   = JobPointer( m, thread_count, thread + 1 );

			for( mint i = i_begin; i < i_end; ++i )
			{
				y[i] = f.Derivative(x[i]);
			}
		},
		thread_count
	);

	MArgument_setMTensor(Res, y_ );

	return LIBRARY_NO_ERROR;
}"
		];
	
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
	
	LibraryFunctionLoad[lib, name, {{Real, 1, "Constant"}, Integer, Integer}, {Real, 1}]
];
