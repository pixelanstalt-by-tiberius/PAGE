# Pixelanstalt Game Engine (PAGE)
A game engine for retro 2D games written in Freepascal and Lazarus. 

## Building the project

### Prerequisites 
* The Lazarus IDE (https://www.lazarus-ide.org)
* The freepascal compiler (https://www.freepascal.org)
* SDL2 header files for pascal (https://github.com/PascalGameDevelopment/SDL2-for-Pascal)
* *Optional:* fptest for running the internal unit-tests (https://github.com/graemeg/fptest)
* *Optional:* pasdoc for generating the source code documentation (https://github.com/pasdoc/pasdoc/wiki)

### Using GNU make
The easiest way to compile the entire project is using the GNU make utility. The makefile is designed to recognize the platform (windows or unix) on which it is running and to choose the names of the executables according to it. 

`make` (without options) will build the `release` target of the lazarus package (the very core of the engine), the needed resources, a dynamic link library and a starter application (with integrated debug functionalities).

`make debug` builds the debug versions of all components (debug symbols included in binaries and only some code optimizations are done by the compiler).

~~`make doc` builds the source code documentation using pasdoc.~~ 

## Runtime Dependencies
* SDL2 (https://www.libsdl.org/download-2.0.php)
* SDL2 Image (https://www.libsdl.org/projects/SDL_image/) 

*Please note (for Windows systems only):* As of SDL2 Image version 2.0.5, at least the Win32-x64 binary contains a "faulty" libpng16-16.dll, which will not work together with the other dlls included. You may have to get a working version of the libpng16-16.dll out of the 2.0.1 release (https://www.libsdl.org/projects/SDL_image/release/SDL2_image-2.0.1-win32-x86.zip or https://www.libsdl.org/projects/SDL_image/release/SDL2_image-2.0.1-win32-x64.zip). This issue is already documented [here](https://bugzilla.libsdl.org/show_bug.cgi?id=4198) and [here](https://discourse.libsdl.org/t/sdl2-image-fails-loading-libpng-on-latest-versions-when-cross-compiling/24494).
