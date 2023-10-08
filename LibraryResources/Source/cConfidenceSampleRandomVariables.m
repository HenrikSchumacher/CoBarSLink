(* ::Package:: *)

(* The backend routine is a dynamic library that is compiled on the fly when it is called for the first time. Afterwards it is memoized. *)

ClearAll[cConfidenceSampleRandomVariables];
cConfidenceSampleRandomVariables[d_Integer?Positive] := cConfidenceSampleRandomVariable[d] = Module[{lib, libname, code, ds, class, name, t},

	name = "cConfidenceSampleRandomVariables";

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
using namespace mma;

using Int  = mint;
using Real = mreal;

using Sampler_T     = Sampler<"<>ds<>",Real,Int>;
using SamplerBase_T = SamplerBase<"<>ds<>",Real,Int>;

using RandomVariable_Ptr = std::shared_ptr<RandomVariable<SamplerBase_T>>;

EXTERN_C DLLEXPORT int "<>name<>"(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res )
{
	//Profiler::Clear(\""<>$HomeDirectory<>"\");

	std::string key_string ( MArgument_getUTF8String(Args[0]) );

	MTensor r       = get<MTensor>(Args[1]);
	MTensor rho     = get<MTensor>(Args[2]);

	MTensor means   = get<MTensor>(Args[3]);
	MTensor errors  = get<MTensor>(Args[4]);

	MTensor radii   = get<MTensor>(Args[5]);

	const Int  max_sample_count = get<Int> (Args[6]);
	const Int  space_flag       = get<Int> (Args[7]);
	const Int  thread_count     = get<Int> (Args[8]);
	const Real confidence       = get<Real>(Args[9]);
	const Int  chunk_size       = get<Int> (Args[10]);

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

	std::vector<RandomVariable_Ptr> F_list;

	const Size_T key_string_length = key_string.length();

	std::string key;
	for( Size_T i = 0; i < key_string_length; ++i ) 
	{
		// Check if the current iteration is equal to ' ' or
        // it's the last character
        if( (key_string[i] == ' ') or (i == key_string_length-1) ) 
		{
			if( i == key_string_length-1 )
			{
				key += key_string[i];
			}	

            auto iter = function_lookup.find(key);

			if ( iter != function_lookup.end() )
			{
				F_list.push_back( iter->second->Clone() );
			}
			else
			{
				eprint(\""<>name<>": Random variable with tag \\\"\"+key+\"\\\" not found. Aborting.\");

				get<Int>(Res) = -1;

				disown(means);
				disown(errors);

				return LIBRARY_NO_ERROR;
			}

            key = \"\";
        }
        else 
		{
            key += key_string[i];
        }
    }

	const Int edge_count = std::min( dimensions(r)[0], dimensions(rho)[0] );
	
	// This creates an instance S of the Sampler class.
	Sampler_T S ( data<Real>(r), data<Real>(rho), edge_count );

	Int N = S.ConfidenceSample(
        F_list, data<Real>(means), data<Real>(errors), data<Real>(radii),
        max_sample_count,
        static_cast<bool>(space_flag),
        thread_count,
        confidence,
        chunk_size
	);

	get<Int>(Res) = N;

	disown(means);
	disown(errors);

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
