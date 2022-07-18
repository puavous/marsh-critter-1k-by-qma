// My first demoscene production on the Windows platform.
// This is based on iq's example code https://github.com/in4k/isystem1k4k

#define WIN32_LEAN_AND_MEAN
#define WIN32_EXTRA_LEAN
#include <windows.h>
#include <mmreg.h>
#include <mmsystem.h>
#include <math.h>
#include <GL/gl.h>
#include "glext.h"

#include "shader.h"

#define XPOS    (0)
#define YPOS    (0)

#define XRES    (10*192 - 2 * XPOS)
#define YRES    (10*108 - 2 * XPOS)

// 0x800000 holds at maximum 174.76266666666666 seconds of audio.
#define AUDIO_BUF_SIZE_IN_SAMPLES (0x800000)

extern "C" { const char * __stdcall get_our_RIFF(); }

static const PIXELFORMATDESCRIPTOR pfd = {
    0,0,PFD_SUPPORT_OPENGL|PFD_DOUBLEBUFFER,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};

// For forcibly mode-switched fullscreen:
static DEVMODE screenSettings = { 
    {0},0,0,sizeof(DEVMODE),0,
    DM_BITSPERPEL | DM_PELSWIDTH | DM_PELSHEIGHT,
    {0},0,0,0,0,0,{0},0,32,XRES,YRES,{0}, 0,
    #if(WINVER >= 0x0400)
    0,0,0,0,0,0,
    #if (WINVER >= 0x0500) || (_WIN32_WINNT >= 0x0400)
    0,0
    #endif
    #endif
    };

//--------------------------------------------------------------------------//

float myMuzik[AUDIO_BUF_SIZE_IN_SAMPLES * 2 + 22];

void* myglfunc[4];

#define oglCreateShaderProgramv ((PFNGLCREATESHADERPROGRAMVPROC)myglfunc[0])
#define oglUseProgram ((PFNGLUSEPROGRAMPROC)myglfunc[1])
#define oglGetUniformLocation ((PFNGLGETUNIFORMLOCATIONPROC)myglfunc[2])
#define oglUniform4i ((PFNGLUNIFORM4IPROC)myglfunc[3])

// The entrypoint for Crinkler shall be naked - it's not called nor returning.
__declspec(naked) void entrypoint( void )
{
    // Inner scope needed to enter the naked function body.
    {
        // Windows 10 API to ignore DPI scaling.. Hmm.. so much depends on
        // the settings of the system that will run the exe.. Some assumptions
        // or wishes should be made and documented to 1k intro compocrew about
        // DPI settings? Like "Please run in 1080p and set DPI Scaling to 100%"?
        // Save those precious few bytes in the 1k?
        
        //SetProcessDpiAwarenessContext(DPI_AWARENESS_CONTEXT_UNAWARE);
        SetProcessDpiAwarenessContext(DPI_AWARENESS_CONTEXT_SYSTEM_AWARE);
        //SetProcessDpiAwarenessContext(DPI_AWARENESS_CONTEXT_PER_MONITOR_AWARE);
        
        // Forced mode change and full screen.. not a very nice thing to do, IMHO
        // if( ChangeDisplaySettings(&screenSettings,CDS_FULLSCREEN|CDS_RESET)!=DISP_CHANGE_SUCCESSFUL) ExitProcess(1);

        ShowCursor( 0 );

        //HDC hDC = GetDC(CreateWindow("edit", 0, WS_POPUP | WS_VISIBLE | WS_MAXIMIZE, 0, 0, 0, 0, 0, 0, 0, 0));
        HDC hDC = GetDC(CreateWindow("edit", 0,
            WS_BORDER | WS_POPUP | WS_VISIBLE ,
            XPOS, YPOS, XRES, YRES, 0, 0, 0, 0));

        // init opengl
        SetPixelFormat(hDC, ChoosePixelFormat(hDC, &pfd), &pfd);
        wglMakeCurrent(hDC, wglCreateContext(hDC));

        // Some GL functions must be imported by their true names
        myglfunc[0] = wglGetProcAddress("glCreateShaderProgramv");
        myglfunc[1] = wglGetProcAddress("glUseProgram");
        myglfunc[2] = wglGetProcAddress("glGetUniformLocation");
        myglfunc[3] = wglGetProcAddress("glUniform4i");


        // create shader
        GLuint program = oglCreateShaderProgramv(GL_FRAGMENT_SHADER, 1, &shader_frag);
        oglUseProgram(program);
        GLint uloc = oglGetUniformLocation(program, "u");

#ifdef _DEBUG
        char    info[2048];
        ((PFNGLGETPROGRAMINFOLOGPROC)wglGetProcAddress("glGetProgramInfoLog"))(program, 1024, NULL, info);
        MessageBox(0, info, "Shader compile info", MB_OK | MB_ICONEXCLAMATION);
#endif

        // Instead of IQ's example, I make the whole RIFF file over there:
        get_our_RIFF();
        sndPlaySound((const char*)&myMuzik, SND_ASYNC | SND_MEMORY);

        // Hmm.. There appears to be a long delay before audio.. maybe varies between installations?
        // Makes it quite difficult / impossible to synchronize audio and video truly?
        // Sleep(300);
        DWORD tbeg = timeGetTime();
        do
        {
            DWORD t = timeGetTime() - tbeg;
            //oglUniform4f(uloc, (float)t, XRES, YRES, 1.);
            oglUniform4i(uloc, t, XRES, YRES, 1);
            glRects(-1, -1, 1, 1);
            SwapBuffers(hDC);
        } while (!GetAsyncKeyState(VK_ESCAPE));

        ExitProcess(0);
    }
}


#ifdef _DEBUG
int WINAPI WinMain(_In_ HINSTANCE hInstance,
	_In_opt_ HINSTANCE hPrevInstance,
	_In_ LPSTR lpCmdLine,
	_In_ int nShowCmd)
{
	entrypoint();
	return 0;
}
#endif
