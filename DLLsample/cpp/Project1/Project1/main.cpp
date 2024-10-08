#include <iostream>
#include <windows.h>  // For LoadLibrary, GetProcAddress
#include <string>

typedef int (*AddFunc)(int, int);
typedef int (*GetValueof1Func)();
typedef const char* (*GetMessageFunc)();

std::wstring GetDLLPath()
{
    wchar_t exePath[MAX_PATH];
    GetModuleFileNameW(NULL, exePath, MAX_PATH);

    // 2. exe�̃f�B���N�g���p�X���擾����
    std::wstring exeDirectory = exePath;
    size_t pos = exeDirectory.find_last_of(L"\\/");
    if (pos != std::wstring::npos) {
        exeDirectory = exeDirectory.substr(0, pos);
    }

    // 3. DLL�̃t���p�X���쐬����
    std::wstring dllPath = exeDirectory + L"\\cppDLL1.dll";
    return dllPath;
}

int main(int argc, char *argv[])
{
    HMODULE hDll = LoadLibraryW(GetDLLPath().c_str());
    if (!hDll) {
        std::cerr << "DLL could not be loaded!" << std::endl;
        return 1;
    }

    // 2. �֐��̃A�h���X���擾����
    AddFunc add = (AddFunc)GetProcAddress(hDll, "add");
    GetValueof1Func getValueof1 = (GetValueof1Func)GetProcAddress(hDll, "getValueof1");
    GetMessageFunc getMessage = (GetMessageFunc)GetProcAddress(hDll, "getMessage");

    // 3. �֐����������擾�ł������m�F����
    if (!add || !getValueof1 || !getMessage) {
        std::cerr << "Could not locate functions in the DLL!" << std::endl;
        FreeLibrary(hDll);
        return 1;
    }

    // 4. �֐����Ăяo��
    int sum = add(10, 20);
    int value = getValueof1();
    const char* message = getMessage();

    std::cout << "Sum: " << sum << std::endl;
    std::cout << "Value: " << value << std::endl;
    std::cout << "Message: " << message << std::endl;

    // 5. DLL���������
    FreeLibrary(hDll);
    return 0;
}