(* ::Package:: *)

Switch[ $OperatingSystem
	
	,"MacOSX", 
	(* Compilation settings for OS X. Assuming Apple Clang compiler is used and that libomp is installed homebrew. (MacPorts might also work) *)
	{
		"CompileOptions" -> {
			" -Wall"
			,"-Wextra"
			,"-Wno-unused-parameter"
			,"-mmacosx-version-min=12.0"
			,"-std=c++17"
			,"-Xpreprocessor -fopenmp -Xpreprocessor -fopenmp-simd"
			,"-fno-math-errno"
			,"-ffast-math"
			,"-Ofast"
			,"-flto"
			,"-gline-tables-only"
			,"-gcolumn-info"
			(*,"-framework Accelerate"*)
			,Switch[$SystemID
			
				,"MacOSX-ARM64"
				,"-mcpu=apple-m1"
		
				,"MacOSX-x86-64"
				,"-march=native"
		
				,_,$Failed
			]
			,"-mtune=native"
		}
		,"LinkerOptions"->Switch[
			$SystemID
			,"MacOSX-ARM64"
			,{"-lm","-ldl","-lomp"}
			
			,"MacOSX-x86-64"
			,{"-lm","-ldl","-liomp5"(*,"-lmkl_intel_ilp64","-lmkl_core","-lmkl_intel_thread","-lpthread"*)}
			
			,_,
			$Failed
		]
		,"IncludeDirectories" -> Flatten[{
			("IncludeDirectories"/.Compiler`$CCompilerOptions)/."IncludeDirectories"->Nothing
			,DirectoryName[$InputFileName]
			,FileNameJoin[{DirectoryName[$InputFileName],"CycleSampler"}]
			,Map[
			  If[FileExistsQ[#],#,Nothing]&,
			  (*The problem is to find a omp.h header that is compatible with the libomp.dylib/libiomp5.dylib shipped with Mathematica.*)
			  (*Oddly enough, the header files are not provided.*)
			  (*Using gcc's omp'h won't work! (Intel's and clang's versions seem to be compatible.)*)
			  {
			  "/usr/local/include"(* system libraries *)
			    ,"/opt/local/include"(* used by macports and by homebrew (on x86 architectures only) *)
			    ,"/opt/local/opt/libomp/include"(* maybe used by homebrew when libomp collides with gcc's libgomp (on x86 architectures only) *)
			    ,"/opt/homebrew/include"(* used by homebrew on Apple Silicon *)
			    ,"/opt/homebrew/opt/libomp/include"(* used by homebrew when libomp collides with gcc's libgomp (on Apple Silicon) *)
			    ,"/opt/local/include/libomp"(* used by macports *)
			 }]
		}]
		,"LibraryDirectories" -> Flatten[{
			("LibraryDirectories"/.Compiler`$CCompilerOptions)/."LibraryDirectories"->Nothing
			,Map[
			  If[FileExistsQ[#],#,Nothing]&,
			  (*CreateLibrary will always link Mathematica's version of OpenMP (libomp.dylib or libiomp5.dylib).*)
			  (*Anyways, we provide search paths for sane installations of homebrew and macports.*)
			  {
			  "/usr/local/lib"(* system libraries *)
			    ,"/opt/local/lib"(* used by macports and by homebrew (on x86 architectures only) *)
			    ,"/opt/local/opt/libomp/lib"(* maybe used by homebrew when libomp collides with gcc's libgomp (on x86 architectures only) *)
			    ,"/opt/homebrew/lib"(* used by homebrew on Apple Silicon *)
			    ,"/opt/homebrew/opt/libomp/lib"(* used by homebrew when libomp collides with gcc's libgomp (on Apple Silicon) *)
			    ,"/opt/local/include/lib"(* used by macports *)
			 }]
			 }]
		(*,"ShellCommandFunction" -> Print*)
		,"ShellOutputFunction" -> Print
	},
	"Unix", (* Compilation settings for Linux on x86 architecture. Untested so far. *)
	{
		"CompileOptions" -> {
			" -Wall"
			,"-Wextra"
			,"-std=c++17"
			,"-Wno-unused-parameter"
			,"-fno-math-errno"
			,"-ffast-math"
			,"-Ofast"
			,"-flto"
			,"-m64"
			,"-fopenmp -fopenmp-simd"
			,"-march=native"
			,"-mtune=native"
		}
		,"LinkerOptions"->{"-lm","-ldl","-liomp5"}
		,"IncludeDirectories" -> {
			("IncludeDirectories"/.Compiler`$CCompilerOptions)/."IncludeDirectories"->Nothing
			,FileNameJoin[{DirectoryName[$InputFileName]}]
			,FileNameJoin[{DirectoryName[$InputFileName],"CycleSampler"}]
			,{(*Add you own include directories here*)}
		}
		,"LibraryDirectories" -> Flatten[{
			("LibraryDirectories"/.Compiler`$CCompilerOptions)/."LibraryDirectories"->Nothing
			,{(*Add you own library directories here*)}
		}]
		(*,"ShellCommandFunction" -> Print*)
		,"ShellOutputFunction" -> Print
	},
	
	"Windows", (* Compilation settings for Windows and Microsoft Visual Studio. Untested so far. *)
	{
		"CompileOptions" -> {"/EHsc", "/wd4244", "/DNOMINMAX", "/arch:AVX"}
		,"LinkerOptions"->{"libiomp5md.lib"}
		,"IncludeDirectories" -> Flatten[{
			("IncludeDirectories"/.Compiler`$CCompilerOptions)/."IncludeDirectories"->Nothing
			,FileNameJoin[{DirectoryName[$InputFileName]}]
			,FileNameJoin[{DirectoryName[$InputFileName],"CycleSampler"}]
			,{(*Add you own include directories here*)}
		}]
		,"LibraryDirectories" -> Flatten[{
			"LibraryDirectories"/.Compiler`$CCompilerOptions
			,{(*Add you own library directories here*)}
		}]
		(*,"ShellCommandFunction" -> Print*)
		,"ShellOutputFunction" -> Print
	}
]
