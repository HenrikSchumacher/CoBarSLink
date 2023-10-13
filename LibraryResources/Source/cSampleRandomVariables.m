(* ::Package:: *)

(* The backend routine is a dynamic library that is compiled on the fly when it is called for the first time. Afterwards it is memoized. *)

ClearAll[cSampleRandomVariables];
cSampleRandomVariables[d_Integer?Positive] := cSampleRandomVariables[d] = Module[{lib, libname, code, ds, class, name, t},

	name = "SampleRandomVariables";

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
	std::string key_string ( get<char *>(Args[0]) );

	MTensor r       = get<MTensor>(Args[1]);
	MTensor rho     = get<MTensor>(Args[2]);
	MTensor values  = get<MTensor>(Args[3]);
	MTensor weights = get<MTensor>(Args[4]);

	const bool quotientQ    = get<mbool>(Args[5]);
	const Int  sample_count = get<Int  >(Args[6]);
	const Int  thread_count = get<Int  >(Args[7]);

	std::unordered_map<std::string,RandomVariable_Ptr> function_lookup;
	function_lookup.insert( {\"DiagonalLength\",                  "<>class["DiagonalLength"]<>"()                  } );
	function_lookup.insert( {\"SquaredGyradius\",                 "<>class["SquaredGyradius"]<>"()                 } );
	function_lookup.insert( {\"Gyradius\",                        "<>class["Gyradius"]<>"()                        } );
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
				eprint(\"cSampleRandomVariables: Random variable with tag \\\"\"+key+\"\\\" not found. Aborting.\");

				get<Int>(Res) = -1;
				disown(values);
				disown(weights);
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

	// Start the sampling process.
	if( quotientQ )
	{
		S.Sample( data<Real>(values), nullptr, data<Real>(weights), F_list, sample_count, thread_count );
	}
	else
	{
		S.Sample( data<Real>(values), data<Real>(weights), nullptr, F_list, sample_count, thread_count );
	}

	get<Int>(Res) = 0;
	disown(values);
	disown(weights);
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
			{Real,2,"Shared"},   (* sampled values *)
			{Real,1,"Shared"},   (* weights *)
			"Boolean",           (* flag for specifying the space: "False" means total space metric, "True" means quotient space metric *)
			Integer,             (* number of samples to take *)
			Integer              (* number of threads *)
		},
		(*return*) Integer  (* error flag; ==0 if succeeded. *)
	]
];
