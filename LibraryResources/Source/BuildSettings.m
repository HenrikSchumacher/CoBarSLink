(* ::Package:: *)

(* The result of executing this script will be handed over to CreateLibrary as options. *)
(* No guarantees are given for correctness of the settings for Linux and Windows architecture. *)
(* Please edit this file as needed to support your system. *)

Switch[ $OperatingSystem
	
	,"MacOSX", 
	(* Compilation settings for OS X. Assuming Apple Clang compiler is used.*)
	{
		"CompileOptions" -> {
			" -Wall"
			,"-Wextra"
			,"-Wno-unused-parameter"
			(*,"-mmacosx-version-min=12.0"*)
			,"-std=c++20"
			,"-fno-math-errno"
			,"-ffast-math"
			,"-Ofast"
			,"-flto"
			,"-gline-tables-only"
			,"-gcolumn-info"
			(*,"-framework Accelerate"*)
			,Switch[$SystemID
				,"MacOSX-ARM64","-mcpu=apple-m1"
				,"MacOSX-x86-64","-march=native"
				,_,$Failed
			]
			,"-mtune=native"
		}
		,"LinkerOptions"->Switch[
			$SystemID
			,"MacOSX-ARM64",{"-lm","-ldl"}
			,"MacOSX-x86-64",{"-lm","-ldl"}
			,_,
			$Failed
		]
		,"IncludeDirectories" -> Flatten[{
			("IncludeDirectories"/.Compiler`$CCompilerOptions)/."IncludeDirectories"->Nothing
			,DirectoryName[$InputFileName]
			,FileNameJoin[{DirectoryName[$InputFileName],"CycleSampler"}]
		}]
		,"LibraryDirectories" -> Flatten[{
			("LibraryDirectories"/.Compiler`$CCompilerOptions)/."LibraryDirectories"->Nothing
			 }]
		(*,"ShellCommandFunction" -> Print*)
		,"ShellOutputFunction" -> (If[#=!="",Print[#]]&)
	},
	"Unix", (* Compilation settings for Linux on x86 architecture. Untested so far. *)
	{
		"CompileOptions" -> {
			" -Wall"
			,"-Wextra"
			,"-std=c++20"
			,"-Wno-unused-parameter"
			,"-fno-math-errno"
			,"-ffast-math"
			,"-Ofast"
			,"-flto"
			,"-m64"
			,"-march=native","-mtune=native"
		}
		,"LinkerOptions"->{"-lm","-ldl"}
		,"IncludeDirectories" -> {
			("IncludeDirectories"/.Compiler`$CCompilerOptions)/."IncludeDirectories"->Nothing
			,FileNameJoin[{DirectoryName[$InputFileName]}]
			,FileNameJoin[{DirectoryName[$InputFileName],"CycleSampler"}]
			,{(*Put your own include directories here.*)}
		}
		,"LibraryDirectories" -> Flatten[{
			("LibraryDirectories"/.Compiler`$CCompilerOptions)/."LibraryDirectories"->Nothing
			,{(*Put your own library directories here.*)}
		}]
		(*,"ShellCommandFunction" -> Print*)
		,"ShellOutputFunction" -> Print
	},
	
	"Windows", (* Compilation settings for Windows and Microsoft Visual Studio. Untested so far. *)
	{
		"CompileOptions" -> {"/EHsc", "/wd4244", "/DNOMINMAX", "/arch:AVX"}
		,"LinkerOptions"->{}
		,"IncludeDirectories" -> Flatten[{
			("IncludeDirectories"/.Compiler`$CCompilerOptions)/."IncludeDirectories"->Nothing
			,FileNameJoin[{DirectoryName[$InputFileName]}]
			,FileNameJoin[{DirectoryName[$InputFileName],"CycleSampler"}]
			,{(*Add you own include directories here*)}
		}]
		,"LibraryDirectories" -> Flatten[{
			"LibraryDirectories"/.Compiler`$CCompilerOptions/."LibraryDirectories"->Nothing
			,{(*Put your own library directories here.*)}
		}]
		(*,"ShellCommandFunction" -> Print*)
		,"ShellOutputFunction" -> Print
	}
]
