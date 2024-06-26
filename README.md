# CoBarSLink
by Jason Cantarella and Henrik Schumacher


A _Mathematica_ interface for [CoBarS](https://github.com/HenrikSchumacher/CoBarS), a C++ library for Monte-Carlo sampling of cylic polygons with prescribed edge lengths in any dimension.

# Installation

To make sure that all submodules are cloned, too, please clone by running the following in the command line:

    git clone --depth 1 --recurse-submodules --shallow-submodules git@github.com:HenrikSchumacher/CoBarSLink.git

Currently the package is configured and tested only for macos (both Apple Silicon and Intel) and with Apple Clang as compiler. The library should compile also with other platforms, provided the correct compiler flags are given. These can be edited in the file `LibraryResources/Source/BuildSettings.m`. It's some time since we ran this build system under Linux or Windows. So please contact us if you are interested and need support. Also, please contact us if you make this work on other sytems; we would gladly add the configurations into this package.

# Usage

From within _Mathematica_, just run 

    Get[FileNameJoin[{<<path to cloned repo>>,"CoBarSLink.m"}];
    
to load the package.

You may also consider to clone directly to the path where _Mathematica_ looks for packages. You can find this path by executing the following line in _Mathematica_:

    FileNameJoin[{$BaseDirectory, "Applications"}]
    
Then you can load the package just by executing

    Needs["CoBarSLink`"];
    
After loading the package, you get an overview of the most important symbols in the package with

    ?CoBarSLink`*
    
To generate `samplecount` closed random `n`-gons in dimension `d`, simply run

    d   = 3;
    n   = 64;
    r   = ConstantArray[1., n];
    rho = ConstantArray[1., n];
    samplecount = 1000000;
    dataCoBarS = RandomClosedPolygons[d, r, samplecount,
        "QuotientSpace" -> True,
        "SphereRadii" -> rho
    ];
    
See also the notebook file `Example.nb` in the `Examples` subdirection for further usage examples.
