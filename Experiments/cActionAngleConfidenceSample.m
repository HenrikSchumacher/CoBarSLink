(* ::Package:: *)

ClearAll[cActionAngleConfidenceSample];
cActionAngleConfidenceSample[progressiveQ:(True|False)] := cActionAngleConfidenceSample[progressiveQ] = Module[{lib, code, name, t},

	name = "cActionAngleConfidenceSample_"<>If[progressiveQ,"Progressive",""];
	
	lib = FileNameJoin[{$libraryDirectory, name<>CCompilerDriver`CCompilerDriverBase`$PlatformDLLExtension}];

	If[True,

		Print["Compiling "<>name<>"..."];

		code=StringJoin[
"
#define NDEBUG
//#define TOOL_ENABLE_PROFILER

#include \"WolframLibrary.h\"
#include \"MMA.h\"

#include \"CycleSampler.hpp\"

using namespace CycleSampler;
using namespace Tensors;
using namespace mma;

using Int  = mint;
using Real = mreal;

static constexpr bool progressiveQ = "<>If[TrueQ[progressiveQ],"true","false"]<>";

using Sampler_T = ActionAngleSampler<Real,Int,CycleSampler::Xoshiro256Plus,progressiveQ>;

EXTERN_C DLLEXPORT int "<>name<>"(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res)
{
	//Profiler::Clear(\""<>$libraryDirectory<>"\");

	const Int  edge_count       = get<Int  >( Args[0] );
	const Real radius           = get<Real >( Args[1] );
	const Int  max_sample_count = get<Int  >( Args[2] );
	const Int  thread_count     = get<Int  >( Args[3] );
	const Real confidence       = get<Real >( Args[4] );
	const Int  chunk_size       = get<Int  >( Args[5] );
	const bool relativeQ        = get<mbool>( Args[6] );
	const bool verboseQ         = get<mbool>( Args[7] );

	
	Sampler_T S ( edge_count );        
            
	if ( verboseQ )
	{
		valprint( \"dimension       \", 3                 );
		valprint( \"edge_count      \", edge_count        );
		valprint( \"thread_count    \", thread_count      );
		valprint( \"confidence level\", confidence        );
	}

	Int N = 0;
	Real total_time = 0;

	Tiny::Vector<2,Real,Int> moments;
	moments.SetZero();

	std::mutex moment_mutex;

	bool completed = false;

	while( !completed )
	{
		Tools::Time start_time = Tools::Clock::now();

		ParallelDo(
			[&]( const Int thread )
			{
				//Time start = Clock::now();

				const Int k_begin = JobPointer( chunk_size, thread_count, thread     );
				const Int k_end   = JobPointer( chunk_size, thread_count, thread + 1 );
	
				const Int repetitions = k_end - k_begin;

				const Real edge_count_inv = Inv<Real>(edge_count);

				Tensor2<Real,Int> p ( edge_count, 3 );

				Sampler_T S ( edge_count );

				Tiny::Vector<2,Real,Int> local_moments;
				local_moments.SetZero();

				for( Int k = 0; k < repetitions; ++k )
				{
					S.RandomClosedPolygon( p.data() );

					Tiny::Vector<3,Real,Int> barycenter;
					Tiny::Vector<3,Real,Int> v;
					
					barycenter.SetZero();

					for( Int i = 0; i < edge_count; ++i )
					{
						v.Read( p.data(i) );
						barycenter += v;
					}

					barycenter *= edge_count_inv;

					Real F = 0;

					for( Int i = 0; i < edge_count; ++i )
					{
						v.Read( p.data(i) );
						v -= barycenter;

						F += v.SquaredNorm();
					}	

					F *= edge_count_inv;

					local_moments[0] += F;
					local_moments[1] += F * F;
				}

				{
					const std::lock_guard<std::mutex> lock ( moment_mutex );

					moments += local_moments;
				}

				//Time stop = Clock::now();

				//logprint(\"Thread \" + ToString(thread) + \" done. Time elapsed = \" + ToString( Duration(start, stop) ) + \".\" );

			},
			thread_count
		);

		Tools::Time stop_time = Tools::Clock::now();

		Real time = Tools::Duration(start_time,stop_time);

		total_time += time;

		N += chunk_size;



		if( N > max_sample_count )
		{
			wprint(std::string(\""<>name<>"\") + \": Maximal number of samples reached. Sampling aborted after \" + ToString(N) + \" samples.\");
			break;
		}


		const Real Bessel_corr = Frac<Real>( N, N-1 );
            
		completed = true;

		const Real mean_F     = Frac<Real>( moments[0], N );
		const Real var_F      = Bessel_corr * ( Frac<Real>( moments[1], N ) - mean_F * mean_F );
		const Real mean_X     = mean_F;
		const Real var_X      = Frac( var_F, N);
		const Real sigma_inv  = Inv( std::sqrt(var_X) );

		const Real absolute_radius = relativeQ ? radius * mean_F : radius;
		
		// Check for sufficient confidence.
	
		const Real current_confidence = Scalar::One<Real> - Scalar::Two<Real> * N_CDF( - absolute_radius * sigma_inv );

		//const Real current_confidence = N_CDF( absolute_radius * sigma_inv ) - N_CDF( - absolute_radius * sigma_inv );

		if( verboseQ )
		{
			dump( N );
		
			valprint(\"  total_time\", total_time );

			Tools::print( std::string(\"  Current estimate of SquaredGyradius = \") +  ToString(mean_X) + \" +/- \" + ToString(radius) + \" with confidence = \" + ToString(current_confidence) + \".\" );
        }                     
		completed = completed && ( current_confidence > confidence );
	}

	const Real Bessel_corr = Frac<Real>( N, N-1 );
	const Real mean_F      = Frac<Real>( moments[0], N );
	const Real var_F       = Bessel_corr * ( Frac<Real>( moments[1], N ) - mean_F * mean_F );
	//const Real mean_X    = mean_F;
	const Real var_X       = Frac( var_F, N);
	const Real sigma_inv   = Inv( std::sqrt(var_X) );

	auto P = [sigma_inv]( const Real c )
	{
		return Scalar::One<Real> - Scalar::Two<Real> * N_CDF( - c * sigma_inv );

		//return N_CDF( c * sigma_inv ) - N_CDF( - c * sigma_inv );
	};

	const Real absolute_radius = relativeQ ? radius * mean_F : radius;

	Real a = 0;
	Real b = absolute_radius;

	// Extend the search interval to make sure that the actual confidence radius lies within [a,b)
	while( P(b) < confidence )
	{
		a = b;
		b *= Scalar::Two<Real>;
	}

	b = BisectionSearch<1>( std::move(P), a, b, confidence, 0.001 );

	MTensor result_ = make_MTensor<Real>( { 5 } );

	Real * result = data<Real>(result_);
	get<MTensor>(Res) = result_;

	result[0] = mean_F;
	result[1] = var_F;
	result[2] = b;
	result[3] = static_cast<Real>(N);
	result[4] = total_time;

	return LIBRARY_NO_ERROR;
}"];

		(* Invoke CreateLibrary to compile the C++ code. *)
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

	LibraryFunctionLoad[lib, name,
		{
			Integer,  (* edge_count *)
			Real,     (* confidence radius *)
			Integer,  (* max_sample_count *)
			Integer,  (* number of threads *)
			Real,     (* confidence level *)
			Integer,  (* chunk_size *)
			"Boolean",(* relativeQ *)
			"Boolean" (* verboseQ *)
		},
		{Real,1}      (*sample mean, sample variance, error, number of samples, time taken*)
	]
];

