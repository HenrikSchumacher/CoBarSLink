(* ::Package:: *)

BeginPackage["ActionAngleConfidenceSampler`", {"CCompilerDriver`"}];


ActionAngleConfidenceSample::usage = "";


Begin["`Private`"];


$packageFile  = $InputFileName;
$packageDirectory  = ParentDirectory[DirectoryName[$InputFileName]];

$libraryDirectory  = FileNameJoin[{$packageDirectory, "LibraryResources", $SystemID}];
If[!FileExistsQ[$libraryDirectory],CreateDirectory[$libraryDirectory]];
$sourceDirectory   = FileNameJoin[{$packageDirectory, "LibraryResources", "Source"}];
If[!FileExistsQ[$sourceDirectory],CreateDirectory[$sourceDirectory]];

$logFile = FileNameJoin[{$packageDirectory, "LibraryResources", $SystemID,"Tools_Log.txt"}];

If[FileExistsQ[$logFile],DeleteFile[$logFile]];

LogFile[] := Import[$logFile,"Text"];

(* Add $libraryDirectory to $LibraryPath in case the package is not installed in $UserBaseDirectory/Applications. *)
If[Not@MemberQ[$LibraryPath, $libraryDirectory],AppendTo[$LibraryPath, $libraryDirectory]];

$compilationOptions := $compilationOptions = Get[FileNameJoin[{$sourceDirectory,"BuildSettings.m"}]];


Get[FileNameJoin[{DirectoryName[$InputFileName], "cActionAngleConfidenceSample.m"}]];

Options[ActionAngleConfidenceSample] = {
	"ThreadCount" :> ("ParallelThreadNumber"/.("ParallelOptions"/.SystemOptions["ParallelOptions"])),
	"ChunkSize"->1000000,
	"MaxSamples"->10000000,
	"ConfidenceLevel"->0.95,
	"Progressive"->True,
	"RelativeErrorMode"->False,
	"Verbose"->True
};

ActionAngleConfidenceSample[edgecount_Integer?Positive, radius_?NumericQ, OptionsPattern[]]:=Module[{data},
	data = cActionAngleConfidenceSample[ 
		TrueQ[OptionValue["Progressive"]] 
	][
		edgecount,radius,
		OptionValue["MaxSamples"],
		OptionValue["ThreadCount"],
		OptionValue["ConfidenceLevel"],
		OptionValue["ChunkSize"],
		TrueQ[OptionValue["RelativeErrorMode"]],
		TrueQ[OptionValue["Verbose"]]
	];
	
	Association[
		"SampleMean"->data[[1]],
		"SampleVariance"->data[[2]],
		"ConfidenceRadius"->data[[3]],
		"PrescribedRadius"->radius,
		"ConfidenceLevel" -> OptionValue["ConfidenceLevel"],
		"AmbientDimension" -> 3,
		"EdgeCount" -> edgecount,
		"r" -> ConstantArray[1.,edgecount],
		"\[Rho]" -> ConstantArray[1.,edgecount],
		"SampleCount"->Round[data[[4]]],
		"MaxSamples" -> OptionValue["MaxSamples"],
		"QuotientSpace" -> True,
		"ThreadCount" -> OptionValue["ThreadCount"],
		"ChunkSize" -> OptionValue["ChunkSize"],
		"Chunks" -> Round[data[[4]]]/OptionValue["ChunkSize"],
		"Timing"->data[[5]]
	]
]


clearLibraries[]:=(
	Scan[DeleteFile,FileNames["*"<>CCompilerDriver`CCompilerDriverBase`$PlatformDLLExtension,$libraryDirectory]];
	Get[$packageFile];
);


End[];


EndPackage[];
