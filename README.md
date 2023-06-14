# CycleSamplerLink
by Jason Cantarella and Henrik Schumacher


A Mathematica interface for CycleSampler, a C++ library for Monte-Carlo sampling of cylic polygons with prescribed edge lengths.

# Installation

Please clone with

    git clone --recurse-submodules git@github.com:HenrikSchumacher/CycleSamplerLink.git

to load also all submodules. If you forgot to do that, you can also run the following afterwards:

    git submodule update --init --recursive

Currently the package is configured and tested only for macos (both Apple Silicon and Intel) and with Apple Clang as compiler. The library should compile also with other platforms, provided the correct compiler flags are given. These can be edited in the file LibraryResources/Source/BuildSettings.m. It's some time since I ran this build system under Linux or Windows. So please contact me if you are interested and need support.

# Usage

From within Mathematica, just run 

    Get[FileNameJoin[{<<path to cloned repo>>,"CycleSamplerLink.m"}];
    
to load the package.

You may also consider to clone directly to the path where _Mathematica_ looks for packages. You can find this path by executing the following the line in _Mathematica_:

    FileNameJoin[{$BaseDirectory, "Applications"}]
    
Then you can load the package just by executing

    Needs["CycleSamplerLink`"];
    
See also the notebook files *.nb in the "Examples" subdirection for usage examples.
