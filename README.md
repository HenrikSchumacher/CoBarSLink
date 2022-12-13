# CycleSamplerLink
by Jason Cantarella and Henrik Schumacher


A Mathematica interface for CycleSampler, a C++ library for Monte-Carlo sampling of cylic polygons with prescribed edge lengths.

# Installation

Please clone with

    git clone --recurse-submodules git@github.com:HenrikSchumacher/CycleSamplerLink.git

to load also all submodules. If you forgot to do that, you can also run the following afterwards:

    git submodule update --init --recursive

Currently the package is configured and tested only for macos (both Apple Silicon and Intel) and with Apple Clang as compiler. 
The package depends on OpenMP for parallelization. So please make sure that it is installed and found by the compiler. Unfortunaly, this can be somewhat fiddly at times. _Mathematica_'s `CreateLibrary` command always links against the OpenMP version shipped with _Mathematica_, but it does not provide any compatible header file 'omp.h'. Beware, GNU's `omp.h` (the one shipped with _gcc_ and _libgomp_) is _not_ compatible!! So a possible way to solve this is as follows:

First install _libomp_ via homebrew or MacPorts. The build script (which is run by the package in the background) searches several of the default installation directories for both package managers, but incompatiblity with GNU's _libgomp_ may lead to _libomp_ being installed in some inconventional places. So keep an eye open for warning messages when installing OpenMP via homebrew or MacPorts! They will typically tell you the correct paths to both the header and the library file.

Then try to run the package in _Mathematica_. If you get an error message of the form

    fatal error: 'omp.h' file not found #include <omp.h>
    
then you have to hunt down the file `omp.h` on your system. Here are two methods to communicate the correct paths to _Mathematica_.


Method 1.: The build script always sources the _zsh_ configure file ~/zshrc. So what you can do then is to add the lines

    export CPLUS_INCLUDE _PATH="<<directory that contains omp.h>>:$CPLUS_INCLUDE_PATH"
    export LIBRARY_PATH="<<directory that contains the correct OpenMP library>>:$LIBRARY_PATH"
    export LD_LIBRARY _PATH="<<directory that contains the correct OpenMP library>>:$LD_LIBRARY_PATH"
    
and then relaunch the _Mathematica_ kernel and run the package again.

Method 2.: Execute the following _Mathematica_ statement before one of the package's routines is run before the first time:
    Compile`$CCompilerOptions = {
      "IncludeDirectories" -> <<directory that contains omp.h>>,
      "LibraryDirectories" -> <<directory that contains the correct OpenMP library>>
      }

Then relaunch the _Mathematica_ kernel and run the package again. 


In both cases the build script will search these directories first (but after the system headers and libraries).

If that does not work, then please contact me via email. I am happy to help.


The library might compile also with other platforms, provided the correct compiler flags are given. These can be edited in the file Source/BuildSettings.m. It's some time since I ran this build system under Linux or Windows. So please contact me if you are interested and need support.

# Usage

From within Mathematica, just run 

    Get[FileNameJoin[{<<path to cloned repo>>,"CycleSamplerLink.m"}];
    
to load the package.
    
See also the notebook files *.nb in the "Examples" subdirection for usage examples.
