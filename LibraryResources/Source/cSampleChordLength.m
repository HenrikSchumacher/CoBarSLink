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

using Sampler_T     = Sampler<"<>ds<>",mreal,mint>;
using SamplerBase_T = SamplerBase<"<>ds<>",mreal,mint>;

using RandomVariable_Ptr = std::shared_ptr<RandomVariable<SamplerBase_T>>;


EXTERN_C DLLEXPORT int "<>name<>"(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res )
{
	const mint i    = MArgument_getInteger(Args[0])-1;
	const mint j    = MArgument_getInteger(Args[1])-1;

	MTensor r       = MArgument_getMTensor(Args[2]);
	MTensor rho     = MArgument_getMTensor(Args[3]);

	MTensor values  = MArgument_getMTensor(Args[4]);
	MTensor weights = MArgument_getMTensor(Args[5]);

	const mint space_flag   = MArgument_getInteger(Args[6]);
	const mint sample_count = MArgument_getInteger(Args[7]);
	const mint thread_count = MArgument_getInteger(Args[8]);

	const mint edge_count = std::min(
			libData->MTensor_getDimensions(r)[0],
			libData->MTensor_getDimensions(rho)[0]
	);

	Sampler_T S (
			libData->MTensor_getRealData(r),
			libData->MTensor_getRealData(rho),
			edge_count
	);

	RandomVariable_Ptr F = "<>class["ChordLength"]<>"(i,j);

	// Start the sampling process.
	if( space_flag == 0 )
	{
		S.Sample(
			libData->MTensor_getRealData(values),
			libData->MTensor_getRealData(weights),
			nullptr,
			F,
			sample_count,
			thread_count
		);
	}
	else
	{
		S.Sample(
			libData->MTensor_getRealData(values),
			nullptr,
			libData->MTensor_getRealData(weights),
			F,
			sample_count,
			thread_count
		);
	}

	MArgument_setInteger(Res, 0);

	libData->MTensor_disown(values);
	libData->MTensor_disown(weights);

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
