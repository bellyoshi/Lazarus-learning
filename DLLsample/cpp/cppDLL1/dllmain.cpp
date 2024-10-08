// dllmain.cpp : DLL アプリケーションのエントリ ポイントを定義します。
#include "pch.h"
#define MYDLL_API __declspec(dllexport)

extern "C" {
    MYDLL_API int add(int a, int b);
    MYDLL_API int getValueof1();
    MYDLL_API const char* getMessage();
}

BOOL APIENTRY DllMain( HMODULE hModule,
                       DWORD  ul_reason_for_call,
                       LPVOID lpReserved
                     )
{
    switch (ul_reason_for_call)
    {
    case DLL_PROCESS_ATTACH:
    case DLL_THREAD_ATTACH:
    case DLL_THREAD_DETACH:
    case DLL_PROCESS_DETACH:
        break;
    }
    return TRUE;
}

MYDLL_API int add(int a, int b) {
    return a + b;
}

MYDLL_API int getValueof1() {
    return 1;
}

MYDLL_API const char* getMessage() {
    return "Hello from the DLL!";
}