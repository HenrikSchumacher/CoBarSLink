# CycleSamplerLink
by Jason Cantarella and Henrik Schumacher


A Mathematica interface for CycleSampler, which is a program for Monte-Carlo sampling of cylic polygons with prescribed edge lengths.

# Installation

Please clone with

    git clone --recurse-submodules git@github.com:HenrikSchumacher/CycleSamplerLink.git

to load also all submodules. If you forgot to do that, you can also run the following afterwards:

    git submodule update --init --recursive

Currently, the package depends on eigen (see https://eigen.tuxfamily.org) and OpenMP. So please make sure that that they are installed and found by the compiler.

Currently the package is configured and tested only for macos (both Apple Silicon and Intel) and with Apple Clang as compiler. It should also compile on other platforms, provided the correct compiler flags are given. These can be edited in the file Source/BuildSettings.m.

# Usage

From within Mathematica, just run 

    Get[FileNameJoin[{<<path to cloned repo>>,"CycleSamplerLink.m"}];
    
to load the package.
    
See also the notebook files *.nb in the "Examples" subdirection for usage examples.
