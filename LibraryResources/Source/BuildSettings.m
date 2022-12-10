(* ::Package:: *)

Switch[
	$OperatingSystem
	
	,"MacOSX", (* Compilation settings for OS X. Assuming Apple Clang compiler is used. *)
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
			,"-framework Accelerate"
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
		,"IncludeDirectories" -> Join[{
			DirectoryName[$InputFileName]
			,FileNameJoin[{DirectoryName[$InputFileName],"CycleSampler"}]
			},
			Map[
			  If[FileExistsQ[#],#,Nothing]&,
			  {
			    "/opt/local/include"
			    ,"/usr/local/include"
			    ,"/opt/homebrew/include"
			 }]
			 ]
		,"LibraryDirectories" -> Join[{
			FileNameJoin[{$InstallationDirectory,"SystemFiles","Libraries",$SystemID}]
			},
			Map[
			  If[FileExistsQ[#],#,Nothing]&,
			  {
				"/opt/local/lib"
				,"/usr/local/lib"
				,"/opt/homebrew/lib"
				(*,"/opt/homebrew/opt/openblas/lib"*)
			 }]
			 ]
		(*,"ShellCommandFunction" -> Print*)
		,"ShellOutputFunction" -> Print
	},
	"Unix", (* Compilation settings for Linux. Untested so far. *)
	{
		"CompileOptions" -> {
			" -Wall"
			,"-Wextra"
			,"-std=c++17"
			,"-Wno-unused-parameter"
			,"-fno-math-errno"
			,"-ffast-math"
			,"-Ofast"
			,"-m64"
			,"-fopenmp -fopenmp-simd"
			,"-march=native"
		}
		,"LinkerOptions"->{
			"-lm"
			,"-ldl"
			,"-liomp5"
		}
		,"IncludeDirectories" -> {
			FileNameJoin[{DirectoryName[$InputFileName]}]
			,FileNameJoin[{DirectoryName[$InputFileName],"libomp"}]
			,FileNameJoin[{DirectoryName[$InputFileName],"CycleSampler"}]
		}
		,"LibraryDirectories" -> {
			FileNameJoin[{$InstallationDirectory,"SystemFiles","Libraries",$SystemID}]
		}
		(*,"ShellCommandFunction" -> Print*)
		,"ShellOutputFunction" -> Print
	},
	
	"Windows", (* Compilation settings for Windows. Untested so far. *)
	{
		"CompileOptions" -> {"/EHsc", "/wd4244", "/DNOMINMAX", "/arch:AVX"}
		,"LinkerOptions"->{
			"libiomp5md.lib"
		}
		,"IncludeDirectories" -> {
			FileNameJoin[{DirectoryName[$InputFileName]}]
			,FileNameJoin[{DirectoryName[$InputFileName],"libomp"}]
			,FileNameJoin[{DirectoryName[$InputFileName],"CycleSampler"}]
		}
		,"LibraryDirectories" -> {
			FileNameJoin[{$InstallationDirectory,"SystemFiles","Libraries",$SystemID}]
		}
		(*,"ShellCommandFunction" -> Print*)
		,"ShellOutputFunction" -> Print
	}
]
