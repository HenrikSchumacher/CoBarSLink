(* ::Package:: *)

Begin["CoBarSLink`Private`"];

Quiet[Scan[LibraryFunctionUnload,Cases[DownValues[cLapackEigenvalues],_LibraryFunction,All]]];
ClearAll[cLapackEigenvalues];
cLapackEigenvalues[d_Integer] := cLapackEigenvalues[d] = Module[{lib, code, name, ds,t},

	ds = IntegerString[d];
	name = "cLapackEigenvalues_"<>ds;

	
	lib = FileNameJoin[{$libraryDirectory, name<>CCompilerDriver`CCompilerDriverBase`$PlatformDLLExtension}];

	If[Not[FileExistsQ[lib]],

		Print["Compiling "<>name<>"..."];

		code=StringJoin[
"
#define NDEBUG

#include \"WolframLibrary.h\"
//#include \"submodules/Tensors/submodules/Tools/Tools.hpp\"
#include \"submodules/Tensors/Accelerate.hpp\"
#include \"submodules/Tensors/MMA.hpp\"
#include \"CoBarS.hpp\"
#include \"submodules/Tensors/BLAS_Wrappers.hpp\"
#include \"submodules/Tensors/LAPACK_Wrappers.hpp\"

using namespace Tools;
using namespace Tensors;
using namespace mma;

constexpr Int d = ",ds,";
constexpr Int dd = d * d;

EXTERN_C DLLEXPORT int "<>name<>"(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res)
{

	MTensor A_ = get<MTensor>(Args[0]);
	const Int thread_count = get<Int>(Args[1]);

	const Int n = libData->MTensor_getDimensions(A_)[0];
	
	const Int dims [2] = { n , d };

	MTensor eigs_;
	(void)libData->MTensor_new(MType_Real, 2, &dims[0], &eigs_);

	mptr<Real> A_ptr    = libData->MTensor_getRealData(A_);
	mptr<Real> eigs_ptr = libData->MTensor_getRealData(eigs_);

	ParallelDo(
		[=]( const Int thread )
		{		
			Tensors::LAPACK::SelfAdjointEigensolver<Layout::RowMajor,UpLo::Upper,Real> S;

			const Int i_begin = JobPointer( n, thread_count, thread     );
			const Int i_end   = JobPointer( n, thread_count, thread + 1 );

			for( Int i = i_begin; i < i_end; ++i )
			{			
				S.Eigenvalues( d, &A_ptr[dd * i], d, &eigs_ptr[d * i] );
			}
		},
		thread_count
	);

	get<MTensor>(Res) = eigs_;

	return LIBRARY_NO_ERROR;
}"];
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

	LibraryFunctionLoad[ lib, name, {{Real,3,"Constant"},Integer},{Real,2}]
];

End[];
