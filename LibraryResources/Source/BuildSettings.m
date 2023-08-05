(* ::Package:: *)

Print["Reading build settings from ",FileNameJoin[{DirectoryName[$InputFileName],"BuildSettings.m"}]];
Switch[ $OperatingSystem
	
	,"MacOSX", 
	(* Compilation settings for OS X. Assuming Apple Clang compiler is used.*)
	{
		"CompileOptions" -> {
			" -Wall"
			,"-Wextra"
			,"-Wno-unused-parameter"
			,"-gline-tables-only"
			,"-gcolumn-info"
			,"-mmacosx-version-min="<>StringSplit[Import["!sw_vers &2>1","Text"]][[4]]
			,"-std=c++20"
			,"-Ofast"
			,"-fno-math-errno"
			,"-flto"
			,"-pthread"
			,"-framework Accelerate"
			,Switch[$SystemID
				,"MacOSX-ARM64","-mcpu=apple-m1","-mtune=native"
				,"MacOSX-x86-64","-march=native","-mtune=native"
				,_,$Failed
			]
		}
		,"LinkerOptions"->Switch[
			$SystemID
			,"MacOSX-ARM64",{"-lm","-ldl"}
			,"MacOSX-x86-64",{"-lm","-ldl"}
			,_,
			$Failed
		]
		,"IncludeDirectories" -> Flatten[{
			DirectoryName[$InputFileName]
			,FileNameJoin[{DirectoryName[$InputFileName],"CycleSampler"}]
		}]
		,"LibraryDirectories" -> {}
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
			,"-Ofast"
			,"-flto"
			,"-pthread"
			,"-m64"
			,"-march=native","-mtune=native"
		}
		,"LinkerOptions"->{"-lm","-ldl"}
		,"IncludeDirectories" -> {
			FileNameJoin[{DirectoryName[$InputFileName]}]
			,FileNameJoin[{DirectoryName[$InputFileName],"CycleSampler"}]
			,{(*Put your own include directories here.*)}
		}
		,"LibraryDirectories" -> {}
		(*,"ShellCommandFunction" -> Print*)
		,"ShellOutputFunction" -> Print
	},
	
	"Windows", (* Compilation settings for Windows and Microsoft Visual Studio. Untested so far. *)
	{
		"CompileOptions" -> {"/EHsc", "/wd4244", "/DNOMINMAX", "/arch:AVX","/O2"}
		,"LinkerOptions"->{}
		,"IncludeDirectories" -> Flatten[{
			FileNameJoin[{DirectoryName[$InputFileName]}]
			,FileNameJoin[{DirectoryName[$InputFileName],"CycleSampler"}]
			,{(*Add you own include directories here*)}
		}]
		,"LibraryDirectories" -> {}
		(*,"ShellCommandFunction" -> Print*)
		,"ShellOutputFunction" -> Print
	}
]
