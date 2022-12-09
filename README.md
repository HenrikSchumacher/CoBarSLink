# CycleSamplerLink
by Jason Cantarella and Henrik Schumacher


A Mathematica interface for CycleSampler, which is a program for Monte-Carlo sampling of cylic polygons with prescribed edge lengths.

# Installation

Please clone with

    git clone --recurse-submodules git@github.com:HenrikSchumacher/CycleSamplerLink.git

to load also all submodules. If you forgot to do that, you can also run the following afterwards:

    git submodule update --init --recursive

From within Mathematica, just run 

    Get[FileNameJoin[{<<path to cloned repo>>,"CycleSamplerLink.m"}];
    
See also the notebook files *.nb in the "Examples" subdirection.
