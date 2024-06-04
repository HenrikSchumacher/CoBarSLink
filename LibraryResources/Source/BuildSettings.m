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
			,"-flto"
			,"-fno-math-errno"
			,"-pthread"
			,"-fenable-matrix"
			,"-framework Accelerate"
			,"-march=native"
			,"-mtune=native"
		}
		,"LinkerOptions"->{}
		,"IncludeDirectories" -> Flatten[{
			DirectoryName[$InputFileName]
			,FileNameJoin[{DirectoryName[$InputFileName],"CoBarS"}]
		}]
		,"LibraryDirectories" -> {}
		(*,"ShellCommandFunction" -> Print*)
		,"ShellOutputFunction" -> (If[#=!="",Print[#]]&)
	},
	"Unix", (* Compilation settings for Linux on x86 architecture. Assuming gcc. Untested so far. *)
	{
		"CompileOptions" -> {
			" -Wall"
			,"-Wextra"
			,"-std=c++20"
			,"-Wno-unused-parameter"
			,"-fno-math-errno"
			,"-Ofast"
			,"-pthread"
			,"-march=native","-mtune=native"
		}
		,"LinkerOptions"->{}
		,"IncludeDirectories" -> {
			FileNameJoin[{DirectoryName[$InputFileName]}]
			,FileNameJoin[{DirectoryName[$InputFileName],"CoBarS"}]
			,{(*Put your own include directories here.*)}
		}
		,"LibraryDirectories" -> {}
		(*,"ShellCommandFunction" -> Print*)
		,"ShellOutputFunction" -> Print
	},
	
	"Windows", (* Compilation settings for Windows and Microsoft Visual Studio. Untested so far. *)
	{
		"CompileOptions" -> {"/EHsc", "/wd4244", "/DNOMINMAX","/O2"}
		,"LinkerOptions"->{}
		,"IncludeDirectories" -> Flatten[{
			FileNameJoin[{DirectoryName[$InputFileName]}]
			,FileNameJoin[{DirectoryName[$InputFileName],"CoBarS"}]
			,{(*Add you own include directories here*)}
		}]
		,"LibraryDirectories" -> {}
		(*,"ShellCommandFunction" -> Print*)
		,"ShellOutputFunction" -> Print
	}
]
