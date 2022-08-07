# Marsh Critter
My contribution to the 1k intro compo in Assembly 2022. Repository was made public after the compo was shown on 6 August, 2022.

To build, you need to get stuff:
  - glext.h from https://registry.khronos.org/OpenGL/api/GL/glext.h or wherever
  - KHR/khrplatform from https://registry.khronos.org/EGL/api/KHR/khrplatform.h similarly
  - MS Visual C++
  - NASM assembler
  - Shader minifier
  - Crinkler

Set things up so that Visual Studio thinks Crinkler.exe is linker.exe, and it can do
custom build steps using commands "nasm ARGLIST" and "shader_minifier ARGLIST".

