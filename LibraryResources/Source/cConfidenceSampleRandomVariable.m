(* ::Package:: *)

(* The backend routine is a dynamic library that is compiled on the fly when it is called for the first time. Afterwards it is memoized. *)

ClearAll[cConfidenceSampleRandomVariable];
cConfidenceSampleRandomVariable[d_Integer?Positive] := cConfidenceSampleRandomVariable[d] = Module[{lib, libname, code, ds, class, name, t},

	name = "ConfidenceSampleRandomVariable";

	ds = IntegerString[d];
	
	libname = name<>"_"<>ds<>"D";
	
	lib = FileNameJoin[{$libraryDirectory, libname<>CCompilerDriver`CCompilerDriverBase`$PlatformDLLExtension}];
	
	class[s_]:="std::make_shared<"<>s<>"<SamplerBase_T>>";

	If[Not[FileExistsQ[lib]],

		Print["Compiling c"<>name<>"["<>ds<>"]..."];

		code = StringJoin[
"
// This is the actual C++ code.

#define NDEBUG

//#define TOOLS_ENABLE_PROFILER

#include \"WolframLibrary.h\"
#include \"MMA.h\"
#include <unordered_map>
#include \"CycleSampler.hpp\"

using namespace Tools;
using namespace Tensors;
using namespace CycleSampler;

using Sampler_T     = Sampler<"<>ds<>",mreal,mint>;
using SamplerBase_T = SamplerBase<"<>ds<>",mreal,mint>;

using RandomVariable_Ptr = std::shared_ptr<RandomVariable<SamplerBase_T>>;

EXTERN_C DLLEXPORT int "<>name<>"(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res )
{
	//Profiler::Clear(\""<>$HomeDirectory<>"\");

	std::string key ( MArgument_getUTF8String(Args[0]) );

	MTensor r       = MArgument_getMTensor(Args[1]);
	MTensor rho     = MArgument_getMTensor(Args[2]);

	MTensor means   = MArgument_getMTensor(Args[3]);
	MTensor errors  = MArgument_getMTensor(Args[4]);

	MTensor radii   = MArgument_getMTensor(Args[5]);

	const mint  max_sample_count = MArgument_getInteger(Args[6]);
	const mint  space_flag       = MArgument_getInteger(Args[7]);
	const mint  thread_count     = MArgument_getInteger(Args[8]);
	const mreal confidence       = MArgument_getReal   (Args[9]);
	const mint  chunk_size       = MArgument_getInteger(Args[10]);

	std::unordered_map<std::string,RandomVariable_Ptr> function_lookup;
	function_lookup.insert( {\"DiagonalLength\",                  "<>class["DiagonalLength"]<>"()                  } );
	function_lookup.insert( {\"Gyradius\",                        "<>class["Gyradius"]<>"()                        } );
	function_lookup.insert( {\"SquaredGyradius\",                 "<>class["SquaredGyradius"]<>"()                 } );
	function_lookup.insert( {\"ShiftNorm\",                       "<>class["ShiftNorm"]<>"()                       } );
	function_lookup.insert( {\"TotalCurvature\",                  "<>class["TotalCurvature"]<>"()                  } );
	function_lookup.insert( {\"BendingEnergy\",                   "<>class["BendingEnergy"]<>"(2)                  } );
	function_lookup.insert( {\"MaxAngle\",                        "<>class["MaxAngle"]<>"()                        } );
	function_lookup.insert( {\"EdgeSpaceSamplingWeight\",         "<>class["EdgeSpaceSamplingWeight"]<>"()         } );
	function_lookup.insert( {\"EdgeQuotientSpaceSamplingWeight\", "<>class["EdgeQuotientSpaceSamplingWeight"]<>"() } );
	function_lookup.insert( {\"IterationCount\",                  "<>class["IterationCount"]<>"()                  } );
	function_lookup.insert( {\"BarycenterNorm\",                  "<>class["BarycenterNorm"]<>"()                  } );

	auto iter = function_lookup.find(key);

    if ( iter != function_lookup.end() )
    {
		const mint edge_count = std::min(
				libData->MTensor_getDimensions(r)[0],
				libData->MTensor_getDimensions(rho)[0]
		);
	
		// This creates an instance S of the Sampler class.
		Sampler_T S (
			libData->MTensor_getRealData(r),
			libData->MTensor_getRealData(rho),
			edge_count
		);
	
		std::vector<RandomVariable_Ptr> F_list;
		F_list.push_back( iter->second->Clone() );

		mint N = S.ConfidenceSample(
	        F_list,
	        libData->MTensor_getRealData(means),
	        libData->MTensor_getRealData(errors),
	        libData->MTensor_getRealData(radii),
	        max_sample_count,
	        static_cast<bool>(space_flag),
	        thread_count,
	        confidence,
	        chunk_size
	    );


		MArgument_setInteger(Res, N);
	}
    else
	{
		eprint(\"cSampleRandomVariable: Random variable with tag \\\"\"+key+\"\\\" not found. Aborting.\");
		MArgument_setInteger(Res, -1);
	}

	libData->MTensor_disown(means);
	libData->MTensor_disown(errors);

	return LIBRARY_NO_ERROR;
}"];
		
		(* Invoke CreateLibrary to compile the C++ code. *)
		t = AbsoluteTiming[
			lib = CreateLibrary[
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
			"UTF8String",        (* tag of random variable *)
			{Real,1,"Constant"}, (* r *)
			{Real,1,"Constant"}, (* \[Rho] *)
			{Real,1,"Shared"},   (* means *)
			{Real,1,"Shared"},   (* errors *)
			{Real,1,"Constant"}, (* confidence radii *)
			Integer,             (* max_sample_count *)
			Integer,             (* flag for specifying the space: values 0 means total space metric, all other values mean quotient space metric *)
			Integer,             (* number of threads *)
			Real,                (* confidence level *)
			Integer              (* chunk_size *)
		},
		(*return*) Integer  (* If positive: succeed and number of samples taken is returned. If negative: Error occured. *)
	]
];
