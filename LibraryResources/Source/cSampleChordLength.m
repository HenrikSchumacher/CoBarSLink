(* ::Package:: *)

(* The backend routine is a dynamic library that is compiled on the fly when it is called for the first time. Afterwards it is memoized. *)

ClearAll[cSampleChordLength];
cSampleChordLength[d_Integer?Positive] := cSampleChordLength[d] = Module[{lib, libname, code, ds, class, name, t},

	name = "SampleChordLength";

	ds = IntegerString[d];
	
	libname = name<>"_"<>ds<>"D";
	
	lib = FileNameJoin[{$libraryDirectory, libname<>CCompilerDriver`CCompilerDriverBase`$PlatformDLLExtension}];
	
	class[s_]:="std::make_shared<"<>s<>"<SamplerBase_T>>";

	If[Not[FileExistsQ[lib]],

		Print["Compiling c"<>name<>"["<>ds<>"]..."];

		code=StringJoin[
"
// This is the actual C++ code.

#define NDEBUG

#include \"WolframLibrary.h\"
#include \"MMA.h\"
#include <unordered_map>
#include \"CycleSampler.hpp\"

using namespace CycleSampler;
using namespace mma;

using Int  = mint;
using Real = mreal;

using Sampler_T     = Sampler<"<>ds<>",Real,Int>;
using SamplerBase_T = SamplerBase<"<>ds<>",Real,Int>;

using RandomVariable_Ptr = std::shared_ptr<RandomVariable<SamplerBase_T>>;


EXTERN_C DLLEXPORT int "<>name<>"(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res )
{
	const Int i     = get<Int>(Args[0])-1;
	const Int j     = get<Int>(Args[1])-1;

	MTensor r       = get<MTensor>(Args[2]);
	MTensor rho     = get<MTensor>(Args[3]);

	MTensor values  = get<MTensor>(Args[4]);
	MTensor weights = get<MTensor>(Args[5]);

	const Int space_flag   = get<Int>(Args[6]);
	const Int sample_count = get<Int>(Args[7]);
	const Int thread_count = get<Int>(Args[8]);

	const Int edge_count = std::min( dimensions(r)[0], dimensions(rho)[0] );

	Sampler_T S ( data<Real>(r), data<Real>(rho), edge_count );

	RandomVariable_Ptr F = "<>class["ChordLength"]<>"(i,j);

	// Start the sampling process.
	if( space_flag == 0 )
	{
		S.Sample( data<Real>(values), data<Real>(weights), nullptr, F, sample_count, thread_count );
	}
	else
	{
		S.Sample( data<Real>(values), nullptr, data<Real>(weights), F, sample_count, thread_count );
	}

	get<Int>(Res) = 0;
	disown(values);
	disown(weights);
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
				(*"ShellOutputFunction"\[Rule]Print,*)
				$compilationOptions
			]
		][[1]];
		Print["Compilation done. Time elapsed = ", t, " s.\n"];
	];
	
	(* Load the resulting dynamic libary into the Mathematica session; use memoization to quickly look up already loaded libraries.*)
	LibraryFunctionLoad[
		lib, 
		name,
		{
			Integer,             (* first  vertex *)
			Integer,             (* second  vertex *)
			
			{Real,1,"Constant"}, (* r *)
			{Real,1,"Constant"}, (* \[Rho] *)
			{Real,1,"Shared"},   (* sampled values *)
			{Real,1,"Shared"},   (* weights *)
			Integer,             (* flag for specifying the space: values 0 means total space metric, all other values mean quotient space metric *)
			Integer,             (* number of samples to take *)
			Integer              (* number of threads *)
		},
		(*return*) Integer  (* error flag; ==0 if succeeded. *)
	]
];
