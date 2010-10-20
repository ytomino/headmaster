#include <windows.h>
#include <tchar.h>

LRESULT WINAPI WndProc(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
	switch(msg){
	case WM_DESTROY:
		PostQuitMessage(0);
		return 0;
	default:
		return DefWindowProc(hWnd, msg, wParam, lParam);
	}
}

int WINAPI _tWinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, TCHAR *lpCmdLine, int nCmdShow)
{
	static TCHAR const wc_name[] = _T("my_window");
	WNDCLASSEX wc = {
		.cbSize = sizeof(WNDCLASSEX),
		.style = CS_HREDRAW | CS_VREDRAW,
		.lpfnWndProc = WndProc,
		.cbClsExtra = 0L,
		.cbWndExtra = 0L,
		.hInstance = hInstance,
		.hIcon = NULL,
		.hCursor = LoadCursor(NULL, IDC_ARROW),
		.hbrBackground = GetStockObject(WHITE_BRUSH),
		.lpszMenuName = NULL,
		.lpszClassName = wc_name,
		.hIconSm = NULL};
	RegisterClassEx(&wc);
	HWND window = CreateWindowEx(0, wc_name, _T(""), WS_OVERLAPPEDWINDOW,
		CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT,
		NULL, NULL, hInstance, NULL);
	ShowWindow(window, nCmdShow);
	UpdateWindow(window);
	MSG msg;
	BOOL r;
	while(r = GetMessage(&msg, NULL, 0U, 0U), r != 0 && r != -1){
		TranslateMessage(&msg);
		DispatchMessage(&msg);
	}
	return 0;
}
