(* ::Package:: *)

(* The backend routine is a dynamic library that is compiled on the fly when it is called for the first time. Afterwards it is memoized. *)

ClearAll[cSampleRandomVariable];
cSampleRandomVariable[d_Integer?Positive]:=Module[{lib, libname, file, ds, class, name, t},

	name = "SampleRandomVariable";

	ds = IntegerString[d];
	
	libname = name<>"_"<>ds<>"D";
	
	lib = FileNameJoin[{$libraryDirectory, libname<>CCompilerDriver`CCompilerDriverBase`$PlatformDLLExtension}];
	
	class[s_]:="std::make_unique<CycleSampler::"<>s<>"<"<>ds<>",mreal, mint>>";

	If[Not[FileExistsQ[lib]],

		Print["Compiling c"<>name<>"["<>ds<>"]..."];

		file = Export[FileNameJoin[{$sourceDirectory,libname<>".cpp"}],
"
// This is the actual C++ code.

#include \"WolframLibrary.h\"
#include \"MMA.h\"
#include <unordered_map>
#include \"Tensors/Tensors.hpp\"
#include \"CycleSampler.hpp\"

using RandomVariable_Ptr = std::unique_ptr<CycleSampler::RandomVariable<"<>ds<>",mreal,mint>>;


EXTERN_C DLLEXPORT int "<>name<>"(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res )
{

	std::string key ( MArgument_getUTF8String(Args[0]) );

	MTensor r       = MArgument_getMTensor(Args[1]);
	MTensor rho     = MArgument_getMTensor(Args[2]);

	MTensor values  = MArgument_getMTensor(Args[3]);
	MTensor weights = MArgument_getMTensor(Args[4]);

	const mint space_flag   = MArgument_getInteger(Args[5]);
	const mint sample_count = MArgument_getInteger(Args[6]);
	const mint thread_count = MArgument_getInteger(Args[7]);


	std::unordered_map<std::string,RandomVariable_Ptr> function_lookup;
	function_lookup.insert( {\"DiagonalLength\",                  "<>class["DiagonalLength"]<>"()                  } );
	function_lookup.insert( {\"Gyradius\",                        "<>class["Gyradius"]<>"()                        } );
	function_lookup.insert( {\"ShiftNorm\",                       "<>class["ShiftNorm"]<>"()                       } );
	function_lookup.insert( {\"TotalCurvature\",                  "<>class["TotalCurvature"]<>"()                  } );
	function_lookup.insert( {\"BendingEnergy\",                   "<>class["BendingEnergy"]<>"(2)                  } );
	function_lookup.insert( {\"MaxAngle\",                        "<>class["MaxAngle"]<>"()                        } );
	function_lookup.insert( {\"EdgeSpaceSamplingWeight\",         "<>class["EdgeSpaceSamplingWeight"]<>"()         } );
	function_lookup.insert( {\"EdgeQuotientSpaceSamplingWeight\", "<>class["EdgeQuotientSpaceSamplingWeight"]<>"() } );
	function_lookup.insert( {\"IterationCount\",                  "<>class["IterationCount"]<>"()                  } );

	auto iter = function_lookup.find(key);

    if ( iter != function_lookup.end() )
    {
		const mint edge_count = std::min(
				libData->MTensor_getDimensions(r)[0],
				libData->MTensor_getDimensions(rho)[0]
		);
	
		
		// This creates an instance S of the Sampler class.
		CycleSampler::Sampler<"<>ds<>",mreal,mint> S (
			libData->MTensor_getRealData(r),
			libData->MTensor_getRealData(rho),
			edge_count
		);
	
		RandomVariable_Ptr F = iter->second->Clone();
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
	}
    else
	{
		CycleSampler::eprint(\"cSampleRandomVariable: Random variable with tag \\\"\"+key+\"\\\" not found. Aborting.\");
		MArgument_setInteger(Res, 1);
	}

	libData->MTensor_disown(values);
	libData->MTensor_disown(weights);

	return LIBRARY_NO_ERROR;
}",
"Text"
		];
		
		(* Invoke CreateLibrary to compile the C++ code. *)
		t = AbsoluteTiming[
			lib = CreateLibrary[
				{file},
				libname,
				"TargetDirectory"-> $libraryDirectory,
				(*"ShellCommandFunction"\[Rule]Print,*)
				(*"ShellOutputFunction"\[Rule]Print,*)
				Get[FileNameJoin[{$sourceDirectory,"BuildSettings.m"}]]
			]
		][[1]];
		Print["Compilation done. Time elapsed = ", t, " s.\n"];
		DeleteFile[file];
	];
	
	(* Load the resulting dynamic libary into the Mathematica session; use memoization to quickly look up already loaded libraries.*)
	cSampleRandomVariable[d] = LibraryFunctionLoad[
		lib, 
		name,
		{
			"UTF8String",        (* tag of random variable *)
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
