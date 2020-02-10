#include <iostream>
#include <fstream>
#include <string>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

//#include <atomic>
//#include <fstream>

#include <verilated.h>
#include "Vjaguar.h"


#include "imgui.h"
#include "imgui_impl_win32.h"
#include "imgui_impl_dx11.h"
#include <d3d11.h>
#define DIRECTINPUT_VERSION 0x0800
#include <dinput.h>
#include <tchar.h>

#include "imgui_memory_editor.h"

uint32_t t_size;
uint32_t t_addr;

uint32_t exe_load_start;
uint32_t exe_load_end;

uint32_t ram_load_start;
uint32_t ram_load_end;

uint32_t seg_addr;
uint32_t s_size;
uint32_t pc0;
uint32_t gp0;

bool exe_loaded = 0;

// DirectX data
static ID3D11Device*            g_pd3dDevice = NULL;
static ID3D11DeviceContext*     g_pd3dDeviceContext = NULL;
static IDXGIFactory*            g_pFactory = NULL;
static ID3D11Buffer*            g_pVB = NULL;
static ID3D11Buffer*            g_pIB = NULL;
static ID3D10Blob*              g_pVertexShaderBlob = NULL;
static ID3D11VertexShader*      g_pVertexShader = NULL;
static ID3D11InputLayout*       g_pInputLayout = NULL;
static ID3D11Buffer*            g_pVertexConstantBuffer = NULL;
static ID3D10Blob*              g_pPixelShaderBlob = NULL;
static ID3D11PixelShader*       g_pPixelShader = NULL;
static ID3D11SamplerState*      g_pFontSampler = NULL;
static ID3D11ShaderResourceView*g_pFontTextureView = NULL;
static ID3D11RasterizerState*   g_pRasterizerState = NULL;
static ID3D11BlendState*        g_pBlendState = NULL;
static ID3D11DepthStencilState* g_pDepthStencilState = NULL;
static int                      g_VertexBufferSize = 5000, g_IndexBufferSize = 10000;


// Instantiation of module.
Vjaguar* top = new Vjaguar;

char my_string[1024];

char serial_string[1024];
int serial_index = 0;

int str_i = 0;

unsigned int row;
unsigned int col;
unsigned int bank;
unsigned int dram_address;

int pix_count = 0;

unsigned char rgb[3];
bool prev_vsync = 0;
int frame_count = 0;

bool vga_file_select = 0;

bool prev_hsync = 0;
int line_count = 0;

bool prev_sram_we_n = 0;

uint32_t inst_data_temp;

uint32_t prev_pc = 0xDEADBEEF;

unsigned int avm_byte_addr;
unsigned int avm_word_addr;

unsigned int burstcount;
unsigned int byteenable;
unsigned int writedata;

unsigned int datamux;	// What the aoR3000 core is actually reading from the bus! Only valid when avm_readdata_valid is High!
unsigned int datatemp;

unsigned int old_pc;
unsigned int inst_count = 0;

unsigned int old_hw_addr;
unsigned int hw_count = 0;

bool trigger1 = 0;
bool trigger2 = 0;

int trig_count = 0;

uint16_t byteena_bits;

bool ram_read_flag = 0;
bool ram_write_flag = 0;

FILE *vgap;

int last_sdram_writedata = 0;
int last_sdram_byteaddr = 0;
int last_sdram_ben = 0;

bool run_enable = 0;
bool single_step = 0;
bool multi_step = 0;
int multi_step_amount = 1024;

static float arr[20] ={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};


// Data
static IDXGISwapChain*          g_pSwapChain = NULL;
static ID3D11RenderTargetView*  g_mainRenderTargetView = NULL;

void CreateRenderTarget()
{
	ID3D11Texture2D* pBackBuffer;
	g_pSwapChain->GetBuffer(0, __uuidof(ID3D11Texture2D), (LPVOID*)&pBackBuffer);
	g_pd3dDevice->CreateRenderTargetView(pBackBuffer, NULL, &g_mainRenderTargetView);
	pBackBuffer->Release();
}

void CleanupRenderTarget()
{
	if (g_mainRenderTargetView) { g_mainRenderTargetView->Release(); g_mainRenderTargetView = NULL; }
}

HRESULT CreateDeviceD3D(HWND hWnd)
{
	// Setup swap chain
	DXGI_SWAP_CHAIN_DESC sd;
	ZeroMemory(&sd, sizeof(sd));
	sd.BufferCount = 2;
	sd.BufferDesc.Width = 0;
	sd.BufferDesc.Height = 0;
	sd.BufferDesc.Format = DXGI_FORMAT_R8G8B8A8_UNORM;
	sd.BufferDesc.RefreshRate.Numerator = 60;
	sd.BufferDesc.RefreshRate.Denominator = 1;
	sd.Flags = DXGI_SWAP_CHAIN_FLAG_ALLOW_MODE_SWITCH;
	sd.BufferUsage = DXGI_USAGE_RENDER_TARGET_OUTPUT;
	sd.OutputWindow = hWnd;
	sd.SampleDesc.Count = 1;
	sd.SampleDesc.Quality = 0;
	sd.Windowed = TRUE;
	sd.SwapEffect = DXGI_SWAP_EFFECT_DISCARD;

	UINT createDeviceFlags = 0;
	//createDeviceFlags |= D3D11_CREATE_DEVICE_DEBUG;
	D3D_FEATURE_LEVEL featureLevel;
	const D3D_FEATURE_LEVEL featureLevelArray[2] = { D3D_FEATURE_LEVEL_11_0, D3D_FEATURE_LEVEL_10_0, };
	if (D3D11CreateDeviceAndSwapChain(NULL, D3D_DRIVER_TYPE_HARDWARE, NULL, createDeviceFlags, featureLevelArray, 2, D3D11_SDK_VERSION, &sd, &g_pSwapChain, &g_pd3dDevice, &featureLevel, &g_pd3dDeviceContext) != S_OK)
		return E_FAIL;

	CreateRenderTarget();

	return S_OK;
}

void CleanupDeviceD3D()
{
	CleanupRenderTarget();
	if (g_pSwapChain) { g_pSwapChain->Release(); g_pSwapChain = NULL; }
	if (g_pd3dDeviceContext) { g_pd3dDeviceContext->Release(); g_pd3dDeviceContext = NULL; }
	if (g_pd3dDevice) { g_pd3dDevice->Release(); g_pd3dDevice = NULL; }
}

extern LRESULT ImGui_ImplWin32_WndProcHandler(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam);
LRESULT WINAPI WndProc(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
	if (ImGui_ImplWin32_WndProcHandler(hWnd, msg, wParam, lParam))
		return true;

	switch (msg)
	{
	case WM_SIZE:
		if (g_pd3dDevice != NULL && wParam != SIZE_MINIMIZED)
		{
			CleanupRenderTarget();
			g_pSwapChain->ResizeBuffers(0, (UINT)LOWORD(lParam), (UINT)HIWORD(lParam), DXGI_FORMAT_UNKNOWN, 0);
			CreateRenderTarget();
		}
		return 0;
	case WM_SYSCOMMAND:
		if ((wParam & 0xfff0) == SC_KEYMENU) // Disable ALT application menu
			return 0;
		break;
	case WM_DESTROY:
		PostQuitMessage(0);
		return 0;
	}
	return DefWindowProc(hWnd, msg, wParam, lParam);
}

static float values[90] = { 0 };
static int values_offset = 0;





vluint64_t main_time = 0;	// Current simulation time.
vluint64_t old_main_time = 0;	// Current simulation time.

unsigned int file_size;

unsigned char buffer[16];

unsigned int rom_size = 1024 * 128;			// 128KB. (8-bit wide).
uint8_t *rom_ptr = (uint8_t *) malloc(rom_size);

unsigned int exp_size = 1024 * 512 * 4;		// 2MB. (32-bit wide).
uint32_t *exp_ptr = (uint32_t *)malloc(exp_size);

unsigned int sp_ram_size = 256 * 8 * 2;		// 4KB. (16-bit wide).
uint16_t *sp_ram_ptr = (uint16_t *)malloc(sp_ram_size);

unsigned int fb0_size = 1024 * 128 * 2;		// 256KB. (16-bit wide).
uint16_t *fb0_ptr = (uint16_t *)malloc(fb0_size);

unsigned int fb1_size = 1024 * 128 * 2;		// 256KB. (16-bit wide).
uint16_t *fb1_ptr = (uint16_t *)malloc(fb1_size);

unsigned int ram_size = 1024 * 512 * 4;		// 2MB. (32-bit wide).
uint32_t *ram_ptr = (uint32_t *) malloc(ram_size);

unsigned int vram_size = 1024 * 1024 * 4;	// 4MB. (32-bit wide).
uint32_t *vram_ptr = (uint32_t *) malloc(vram_size);

unsigned int disp_size = 1024 * 1024 * 4;	// 4MB. (32-bit wide).
uint32_t *disp_ptr = (uint32_t *)malloc(disp_size);

uint32_t vga_size  = 1024 * 1024 * 4;		// 4MB. (32-bit wide).
uint32_t *vga_ptr  = (uint32_t *) malloc(vga_size);

uint32_t exe_size = 1024 * 512 * 4;			// 2MB. (32-bit wide).
uint32_t *exe_ptr = (uint32_t *) malloc(exe_size);

uint32_t first_cmd_word = 0;

uint8_t clk_cnt = 0;


double sc_time_stamp () {	// Called by $time in Verilog.
	return main_time;
}


ImVector<char*>       Items;
static char* Strdup(const char *str) { size_t len = strlen(str) + 1; void* buf = malloc(len); IM_ASSERT(buf); return (char*)memcpy(buf, (const void*)str, len); }

void    MyAddLog(const char* fmt, ...) IM_FMTARGS(2)
{
	// FIXME-OPT
	char buf[1024];
	va_list args;
	va_start(args, fmt);
	vsnprintf(buf, IM_ARRAYSIZE(buf), fmt, args);
	buf[IM_ARRAYSIZE(buf) - 1] = 0;
	va_end(args);
	Items.push_back(Strdup(buf));
}

// Demonstrate creating a simple console window, with scrolling, filtering, completion and history.
// For the console example, here we are using a more C++ like approach of declaring a class to hold the data and the functions.
struct MyExampleAppConsole
{
	char                  InputBuf[256];
	ImVector<const char*> Commands;
	ImVector<char*>       History;
	int                   HistoryPos;    // -1: new line, 0..History.Size-1 browsing history.
	ImGuiTextFilter       Filter;
	bool                  AutoScroll;
	bool                  ScrollToBottom;

	MyExampleAppConsole()
	{
		ClearLog();
		memset(InputBuf, 0, sizeof(InputBuf));
		HistoryPos = -1;
		Commands.push_back("HELP");
		Commands.push_back("HISTORY");
		Commands.push_back("CLEAR");
		Commands.push_back("CLASSIFY");  // "classify" is only here to provide an example of "C"+[tab] completing to "CL" and displaying matches.
		AutoScroll = true;
		ScrollToBottom = false;
		MyAddLog("Jaguar core - Sim start");
		MyAddLog("");
	}
	~MyExampleAppConsole()
	{
		ClearLog();
		for (int i = 0; i < History.Size; i++)
			free(History[i]);
	}

	// Portable helpers
	static int   Stricmp(const char* str1, const char* str2) { int d; while ((d = toupper(*str2) - toupper(*str1)) == 0 && *str1) { str1++; str2++; } return d; }
	static int   Strnicmp(const char* str1, const char* str2, int n) { int d = 0; while (n > 0 && (d = toupper(*str2) - toupper(*str1)) == 0 && *str1) { str1++; str2++; n--; } return d; }
	//	static char* Strdup(const char *str) { size_t len = strlen(str) + 1; void* buf = malloc(len); IM_ASSERT(buf); return (char*)memcpy(buf, (const void*)str, len); }
	static void  Strtrim(char* str) { char* str_end = str + strlen(str); while (str_end > str && str_end[-1] == ' ') str_end--; *str_end = 0; }

	void    ClearLog()
	{
		for (int i = 0; i < Items.Size; i++)
			free(Items[i]);
		Items.clear();
	}

	/*
	void    MyAddLog(const char* fmt, ...) IM_FMTARGS(2)
	{
	// FIXME-OPT
	char buf[1024];
	va_list args;
	va_start(args, fmt);
	vsnprintf(buf, IM_ARRAYSIZE(buf), fmt, args);
	buf[IM_ARRAYSIZE(buf) - 1] = 0;
	va_end(args);
	Items.push_back(Strdup(buf));
	}
	*/

	void    Draw(const char* title, bool* p_open)
	{
		ImGui::SetNextWindowSize(ImVec2(520, 600), ImGuiCond_FirstUseEver);
		if (!ImGui::Begin(title, p_open))
		{
			ImGui::End();
			return;
		}

		// As a specific feature guaranteed by the library, after calling Begin() the last Item represent the title bar. So e.g. IsItemHovered() will return true when hovering the title bar.
		// Here we create a context menu only available from the title bar.
		if (ImGui::BeginPopupContextItem())
		{
			if (ImGui::MenuItem("Close Console"))
				*p_open = false;
			ImGui::EndPopup();
		}

		//ImGui::TextWrapped("This example implements a console with basic coloring, completion and history. A more elaborate implementation may want to store entries along with extra data such as timestamp, emitter, etc.");
		//ImGui::TextWrapped("Enter 'HELP' for help, press TAB to use text completion.");

		// TODO: display items starting from the bottom

		//if (ImGui::SmallButton("Add Dummy Text")) { MyAddLog("%d some text", Items.Size); MyAddLog("some more text"); MyAddLog("display very important message here!"); } ImGui::SameLine();
		//if (ImGui::SmallButton("Add Dummy Error")) { MyAddLog("[error] something went wrong"); } ImGui::SameLine();
		if (ImGui::SmallButton("Clear")) { ClearLog(); } ImGui::SameLine();
		bool copy_to_clipboard = ImGui::SmallButton("Copy");
		//static float t = 0.0f; if (ImGui::GetTime() - t > 0.02f) { t = ImGui::GetTime(); MyAddLog("Spam %f", t); }

		ImGui::Separator();

		// Options menu
		if (ImGui::BeginPopup("Options"))
		{
			ImGui::Checkbox("Auto-scroll", &AutoScroll);
			ImGui::EndPopup();
		}

		// Options, Filter
		if (ImGui::Button("Options"))
			ImGui::OpenPopup("Options");
		ImGui::SameLine();
		Filter.Draw("Filter (\"incl,-excl\") (\"error\")", 180);
		ImGui::Separator();

		const float footer_height_to_reserve = ImGui::GetStyle().ItemSpacing.y + ImGui::GetFrameHeightWithSpacing(); // 1 separator, 1 input text
		ImGui::BeginChild("ScrollingRegion", ImVec2(0, -footer_height_to_reserve), false, ImGuiWindowFlags_HorizontalScrollbar); // Leave room for 1 separator + 1 InputText
		if (ImGui::BeginPopupContextWindow())
		{
			if (ImGui::Selectable("Clear")) ClearLog();
			ImGui::EndPopup();
		}

		// Display every line as a separate entry so we can change their color or add custom widgets. If you only want raw text you can use ImGui::TextUnformatted(log.begin(), log.end());
		// NB- if you have thousands of entries this approach may be too inefficient and may require user-side clipping to only process visible items.
		// You can seek and display only the lines that are visible using the ImGuiListClipper helper, if your elements are evenly spaced and you have cheap random access to the elements.
		// To use the clipper we could replace the 'for (int i = 0; i < Items.Size; i++)' loop with:
		//     ImGuiListClipper clipper(Items.Size);
		//     while (clipper.Step())
		//         for (int i = clipper.DisplayStart; i < clipper.DisplayEnd; i++)
		// However, note that you can not use this code as is if a filter is active because it breaks the 'cheap random-access' property. We would need random-access on the post-filtered list.
		// A typical application wanting coarse clipping and filtering may want to pre-compute an array of indices that passed the filtering test, recomputing this array when user changes the filter,
		// and appending newly elements as they are inserted. This is left as a task to the user until we can manage to improve this example code!
		// If your items are of variable size you may want to implement code similar to what ImGuiListClipper does. Or split your data into fixed height items to allow random-seeking into your list.
		ImGui::PushStyleVar(ImGuiStyleVar_ItemSpacing, ImVec2(4, 1)); // Tighten spacing
		if (copy_to_clipboard)
			ImGui::LogToClipboard();
		for (int i = 0; i < Items.Size; i++)
		{
			const char* item = Items[i];
			if (!Filter.PassFilter(item))
				continue;

			// Normally you would store more information in your item (e.g. make Items[] an array of structure, store color/type etc.)
			bool pop_color = false;
			if (strstr(item, "[error]")) { ImGui::PushStyleColor(ImGuiCol_Text, ImVec4(1.0f, 0.4f, 0.4f, 1.0f)); pop_color = true; }
			else if (strncmp(item, "# ", 2) == 0) { ImGui::PushStyleColor(ImGuiCol_Text, ImVec4(1.0f, 0.8f, 0.6f, 1.0f)); pop_color = true; }
			ImGui::TextUnformatted(item);
			if (pop_color)
				ImGui::PopStyleColor();
		}
		if (copy_to_clipboard)
			ImGui::LogFinish();

		if (ScrollToBottom || (AutoScroll && ImGui::GetScrollY() >= ImGui::GetScrollMaxY()))
			ImGui::SetScrollHereY(1.0f);
		ScrollToBottom = false;

		ImGui::PopStyleVar();
		ImGui::EndChild();
		ImGui::Separator();

		// Command-line
		bool reclaim_focus = false;
		if (ImGui::InputText("Input", InputBuf, IM_ARRAYSIZE(InputBuf), ImGuiInputTextFlags_EnterReturnsTrue | ImGuiInputTextFlags_CallbackCompletion | ImGuiInputTextFlags_CallbackHistory, &TextEditCallbackStub, (void*)this))
		{
			char* s = InputBuf;
			Strtrim(s);
			if (s[0])
				ExecCommand(s);
			strcpy(s, "");
			reclaim_focus = true;
		}

		// Auto-focus on window apparition
		ImGui::SetItemDefaultFocus();
		if (reclaim_focus)
			ImGui::SetKeyboardFocusHere(-1); // Auto focus previous widget

		ImGui::End();
	}

	void    ExecCommand(const char* command_line)
	{
		MyAddLog("# %s\n", command_line);

		// Insert into history. First find match and delete it so it can be pushed to the back. This isn't trying to be smart or optimal.
		HistoryPos = -1;
		for (int i = History.Size - 1; i >= 0; i--)
			if (Stricmp(History[i], command_line) == 0)
			{
				free(History[i]);
				History.erase(History.begin() + i);
				break;
			}
		History.push_back(Strdup(command_line));

		// Process command
		if (Stricmp(command_line, "CLEAR") == 0)
		{
			ClearLog();
		}
		else if (Stricmp(command_line, "HELP") == 0)
		{
			MyAddLog("Commands:");
			for (int i = 0; i < Commands.Size; i++)
				MyAddLog("- %s", Commands[i]);
		}
		else if (Stricmp(command_line, "HISTORY") == 0)
		{
			int first = History.Size - 10;
			for (int i = first > 0 ? first : 0; i < History.Size; i++)
				MyAddLog("%3d: %s\n", i, History[i]);
		}
		else
		{
			MyAddLog("Unknown command: '%s'\n", command_line);
		}

		// On commad input, we scroll to bottom even if AutoScroll==false
		ScrollToBottom = true;
	}

	static int TextEditCallbackStub(ImGuiInputTextCallbackData* data) // In C++11 you are better off using lambdas for this sort of forwarding callbacks
	{
		MyExampleAppConsole* console = (MyExampleAppConsole*)data->UserData;
		return console->TextEditCallback(data);
	}

	int     TextEditCallback(ImGuiInputTextCallbackData* data)
	{
		//MyAddLog("cursor: %d, selection: %d-%d", data->CursorPos, data->SelectionStart, data->SelectionEnd);
		switch (data->EventFlag)
		{
		case ImGuiInputTextFlags_CallbackCompletion:
		{
			// Example of TEXT COMPLETION

			// Locate beginning of current word
			const char* word_end = data->Buf + data->CursorPos;
			const char* word_start = word_end;
			while (word_start > data->Buf)
			{
				const char c = word_start[-1];
				if (c == ' ' || c == '\t' || c == ',' || c == ';')
					break;
				word_start--;
			}

			// Build a list of candidates
			ImVector<const char*> candidates;
			for (int i = 0; i < Commands.Size; i++)
				if (Strnicmp(Commands[i], word_start, (int)(word_end - word_start)) == 0)
					candidates.push_back(Commands[i]);

			if (candidates.Size == 0)
			{
				// No match
				MyAddLog("No match for \"%.*s\"!\n", (int)(word_end - word_start), word_start);
			}
			else if (candidates.Size == 1)
			{
				// Single match. Delete the beginning of the word and replace it entirely so we've got nice casing
				data->DeleteChars((int)(word_start - data->Buf), (int)(word_end - word_start));
				data->InsertChars(data->CursorPos, candidates[0]);
				data->InsertChars(data->CursorPos, " ");
			}
			else
			{
				// Multiple matches. Complete as much as we can, so inputing "C" will complete to "CL" and display "CLEAR" and "CLASSIFY"
				int match_len = (int)(word_end - word_start);
				for (;;)
				{
					int c = 0;
					bool all_candidates_matches = true;
					for (int i = 0; i < candidates.Size && all_candidates_matches; i++)
						if (i == 0)
							c = toupper(candidates[i][match_len]);
						else if (c == 0 || c != toupper(candidates[i][match_len]))
							all_candidates_matches = false;
					if (!all_candidates_matches)
						break;
					match_len++;
				}

				if (match_len > 0)
				{
					data->DeleteChars((int)(word_start - data->Buf), (int)(word_end - word_start));
					data->InsertChars(data->CursorPos, candidates[0], candidates[0] + match_len);
				}

				// List matches
				MyAddLog("Possible matches:\n");
				for (int i = 0; i < candidates.Size; i++)
					MyAddLog("- %s\n", candidates[i]);
			}

			break;
		}
		case ImGuiInputTextFlags_CallbackHistory:
		{
			// Example of HISTORY
			const int prev_history_pos = HistoryPos;
			if (data->EventKey == ImGuiKey_UpArrow)
			{
				if (HistoryPos == -1)
					HistoryPos = History.Size - 1;
				else if (HistoryPos > 0)
					HistoryPos--;
			}
			else if (data->EventKey == ImGuiKey_DownArrow)
			{
				if (HistoryPos != -1)
					if (++HistoryPos >= History.Size)
						HistoryPos = -1;
			}

			// A better implementation would preserve the data on the current input line along with cursor position.
			if (prev_history_pos != HistoryPos)
			{
				const char* history_str = (HistoryPos >= 0) ? History[HistoryPos] : "";
				data->DeleteChars(0, data->BufTextLen);
				data->InsertChars(0, history_str);
			}
		}
		}
		return 0;
	}
};

static void ShowMyExampleAppConsole(bool* p_open)
{
	static MyExampleAppConsole console;
	console.Draw("Debug Log", p_open);
}

/*
int verilate() {
	if (!Verilated::gotFinish()) {
		//while ( top->FL_ADDR < 0x0100 ) {		// Only run for a short time.
		if (main_time < 4) {
			top->xresetl = 0;   	// Assert reset (active LOW)
		}
		if (main_time == 4) {	// Do == here, so we can still reset it in the main loop.
			top->xresetl = 1;   // Deassert reset./
		}
		if ((main_time & 1) == 1) {
			top->sys_clk = 1;       // Toggle clock
		}
		if ((main_time & 1) == 0) {
			top->sys_clk = 0;

			pix_count++;


			// Write VGA output to a file. RAW RGB!
			//rgb[0] = top->VGA_R;
			//rgb[1] = top->VGA_G;
			//rgb[2] = top->VGA_B;
			////fwrite(rgb, 1, 3, vgap);		// Write 24-bit values to the file.
			//uint32_t vga_addr = (line_count * 1024) + pix_count;
			//if (vga_addr <= vga_size) vga_ptr[vga_addr] = (rgb[0] << 24) | (rgb[1] << 16) | (rgb[2] << 8) | 0xCC;

			//for (int i = 0; i < 100; i++) disp_ptr[1000 + i] = 0xFF00FF00;

			//if (top->jaguar__DOT__fb0_we) {
				//rgb[0] = (top->jaguar__DOT__fb0_dout & 0x0F00) >> 4;	// [4:0] Red.
				//rgb[1] = (top->jaguar__DOT__fb0_dout & 0x00F0) >> 0;	// [9:5] Green.
				//rgb[2] = (top->jaguar__DOT__fb0_dout & 0x000F) << 4;	// [14:10] Blue.
				rgb[0] = (top->jaguar__DOT__fb0_dout & 0x0008) << 4;	// [4:0] Red.
				rgb[1] = (top->jaguar__DOT__fb0_dout & 0x0001) << 7;	// [9:5] Green.
				rgb[2] = (top->jaguar__DOT__fb0_dout & 0x0006) << 5;	// [14:10] Blue.
				disp_ptr[top->jaguar__DOT__fb0_addr] = 0xff<<24 | rgb[2] << 16 | rgb[1] << 8 | rgb[0];	// Our debugger framebuffer is in the 32-bit RGBA format.
				//disp_ptr[top->jaguar__DOT__fb0_addr] = 0xFF00FF00;

				if ((frame_count & 1) == 0) {
					fb0_ptr[top->jaguar__DOT__fb0_addr] = top->jaguar__DOT__fb0_dout;
				}
				//else fb0_ptr[top->system_top__DOT__GPU_addr] = 0xFFFF0000;	// Force a colour, because it's broken atm, and I can't see anything. ElectronAsh.
			//}

//			if (prev_sram_we_n == 0 && top->SRAM_WE_N == 1) {
//				fb0_ptr[top->SRAM_ADDR] = top->system_top__DOT__sram_dq_out;
//				//printf("SRAM_WE_N: %d  SRAM_ADDR: %06x  SRAM_DQ: %04x\n", top->SRAM_WE_N, top->SRAM_ADDR, top->SRAM_DQ);
//			}
//			prev_sram_we_n = top->SRAM_WE_N;

			//if (top->SRAM_OE_N == 0) {
//				rgb[0] = (fb0_ptr[top->SRAM_ADDR] & 0xFF000000) >> 24;
//				rgb[1] = (fb0_ptr[top->SRAM_ADDR] & 0x00FF0000) >> 16;
//				rgb[2] = (fb0_ptr[top->SRAM_ADDR] & 0x0000FF00) >> 8;
//				top->SRAM_DQ = (rgb[0]<<7) | (rgb[1] << 2) | (rgb[0] >> 3);

				top->jaguar__DOT__fb0_din = fb0_ptr[top->jaguar__DOT__fb0_addr];

				//printf("SRAM_OE_N: %d  SRAM_ADDR: %06x  SRAM_DQ: %04x\n", top->SRAM_OE_N, top->SRAM_ADDR, fb0_ptr[top->SRAM_ADDR]);
			//}

				top->jaguar__DOT__sp_dout = ((sp_ram_ptr[top->jaguar__DOT__sp_addr]&0xFF00) >> 8) | ((sp_ram_ptr[top->jaguar__DOT__sp_addr]&0x00FF)<<8);

				//if (top->jaguar__DOT__sp_we) sp_ram_ptr[top->jaguar__DOT__sp_addr] = top->jaguar__DOT__sp_din;

				// NOTE: Might need to byte-swap each 4-byte chunk!
				if (top->jaguar__DOT__gaddr < rom_size)  top->jaguar__DOT__gdata = rom_ptr[top->jaguar__DOT__gaddr];

//			if (prev_hsync && !top->VGA_HS) {
//				//printf("Line Count: %d\n", line_count);
//				//printf("Pix count: %d\n", pix_count);
//				line_count++;
//				pix_count = 0;
//			}
			prev_hsync = top->VGA_HS;
			
//			if (prev_vsync && !top->VGA_VS) {
//				frame_count++;
//				line_count = 0;
				printf("Frame: %06d  VSync! \n", frame_count);
				
//				if (frame_count > 46) {
//					printf("Dumping framebuffer to vga_out.raw!\n");
//					char vga_filename[40];
//					sprintf(vga_filename, "vga_frame_%d.raw", frame_count);
//					vgap = fopen(vga_filename, "w");
//					if (vgap != NULL) {
//						printf("\nOpened %s for writing OK.\n", vga_filename);
//					}
//					else {
//						printf("\nCould not open %s for writing!\n\n", vga_filename);
//						return 0;
//					};
//					fseek(vgap, 0L, SEEK_SET);
//				}
				
//				for (int i = 0; i < (1600 * 521); i++) {	// Pixels per line * total lines.
//					rgb[0] = (fb0_ptr[i] & 0x001F) << 3;	// [4:0] Red.
//					rgb[1] = (fb0_ptr[i] & 0x03E0) >> 2;	// [9:5] Green.
//					rgb[2] = (fb0_ptr[i] & 0x7C00) >> 7;	// [14:10] Blue.

//					//rgb[0] = (vga_ptr[i] & 0xFF0000) >> 24;
//					//rgb[1] = (vga_ptr[i] & 0x00FF00) >> 16;
//					//rgb[2] = (vga_ptr[i] & 0x0000FF) >> 8;

//					//if (frame_count > =75) fwrite(rgb, 1, 3, vgap);	// Write pixels to the file.
//					if (frame_count >= 75) fwrite(rgb, 3, 1, vgap);	// Write pixels to the file.
//				}
//				if (frame_count > 46) fclose(vgap);

//				//printf("pc: %08X  addr: %08X  inst: %08X\n", top->pc << 2, top->interp_addr, top->inst);
//			}
//			prev_vsync = top->VGA_VS;

//			//if (top->VGA_we==1) printf("VGA_we is High!\n");

//			//if (top->SRAM_DQ > 0) printf("SRAM_DQ is High!!!\n");
//			//if (top->VGA_R > 0 || top->VGA_G > 0 || top->VGA_B > 0) printf("VGA is High!!!\n");
//
//		}

		top->eval();            // Evaluate model!
		main_time++;            // Time passes...

		return 1;
	}
	// Stop Verilating...
	top->final();
	delete top;
	exit(0);
	return 0;
}
*/

//int verilate(int argc, char **argv, char **env)
int verilate()
{
	if (!Verilated::gotFinish()) {
#if VM_TRACE
		if ((ms % LOG_EVERY_MS) == 0)
			tfp->openNext();
#endif
		//fprintf(stderr, "@%d ms\n", ms);
		//fprintf(stdout, "@%d ms\n", ms);

		bool cart_load_done = 0;

		//top->DDRAM_DOUT_READY = 0;
		char ram_ready_count = 0;

		while (!Verilated::gotFinish()) {
			top->dram_oe = !((top->dram_oe_n>>3)&1) || !((top->dram_oe_n>>2)&1) || !((top->dram_oe_n>>1)&1) || !((top->dram_oe_n>>0)&1);
			//top->cart_oe = ~top->cart_oe_n;

			top->xwaitl = 1;
			top->turbo = 0;
			top->ntsc = 1;
			top->ram_rdy = 1;

			//top->os_rom_oe = (!top->os_rom_oe_n) && (!top->os_rom_ce_n);
			top->os_rom_q = rom_ptr[top->abus_out];

			if (main_time < 100) {
				top->xresetl = 0;		// Assert reset
			}
			if (main_time == 100) {
				top->xresetl = 1;   	// Deassert reset
			}

			/*
			if ((main_time & 1) == 1) {
				top->sys_clk = 1;
			}
			if ((main_time & 1) == 0) {
				top->sys_clk = 0;
			*/
				// Dump VGA output
				// vga_clk   = top->OSC_CLK0;
				/*if (top->OSC_CLK0)
				vga_clk = vga_clk ^ 1;*/
				//vga_vs = top->VGA_VS;
				//vga_hs = top->VGA_HS;
				//vga_r = top->VGA_R;
				//vga_g = top->VGA_G;
				//vga_b = top->VGA_B;
				//vga->eval(hcycle / 2,
					//vga_clk, vga_vs, vga_hs,
					//vga_r, vga_g, vga_b);

				// OS ROM
				//bios_clk = sys_clk;
				//bios_ce_n = top->os_rom_ce_n;
				//bios_oe_n = top->os_rom_oe_n;
				//bios_a = top->os_rom_a;

				//bios->eval(hcycle / 2, bios_clk,
					//bios_ce_n, bios_oe_n, bios_a,
					//bios_q, bios_oe);

				//top->os_rom_q = bios_q;
				//top->os_rom_oe = bios_oe;

				//top->os_rom_oe = (!top->os_rom_ce_n && !top->os_rom_oe_n) ? 1 : 0;
				//top->os_rom_q = rom_ptr[ top->os_rom_a ];

				//uint64_t ramdata;

				//top->DDRAM_BUSY = 0;

				//if ( (top->DDRAM_ADDR>>3) > ram_size) printf("DDRAM_ADDR outside of ram_ptr range!\n");		
				/*
				if (top->DDRAM_WE) {
					ramdata = ram_ptr[top->DDRAM_ADDR];	// Read the existing data from our RAM.

														// Check the BE bits, to mask which bytes get written to our RAM value (using the existing data as well).
					bool BE7 = top->DDRAM_BE & 0x80;
					bool BE6 = top->DDRAM_BE & 0x40;
					bool BE5 = top->DDRAM_BE & 0x20;
					bool BE4 = top->DDRAM_BE & 0x10;
					bool BE3 = top->DDRAM_BE & 0x08;
					bool BE2 = top->DDRAM_BE & 0x04;
					bool BE1 = top->DDRAM_BE & 0x02;
					bool BE0 = top->DDRAM_BE & 0x01;
					if (BE7) ramdata = ramdata & 0x00FFFFFFFFFFFFFF | top->DDRAM_DIN & 0xFF00000000000000;
					if (BE6) ramdata = ramdata & 0xFF00FFFFFFFFFFFF | top->DDRAM_DIN & 0x00FF000000000000;
					if (BE5) ramdata = ramdata & 0xFFFF00FFFFFFFFFF | top->DDRAM_DIN & 0x0000FF0000000000;
					if (BE4) ramdata = ramdata & 0xFFFFFF00FFFFFFFF | top->DDRAM_DIN & 0x000000FF00000000;
					if (BE3) ramdata = ramdata & 0xFFFFFFFF00FFFFFF | top->DDRAM_DIN & 0x00000000FF000000;
					if (BE2) ramdata = ramdata & 0xFFFFFFFFFF00FFFF | top->DDRAM_DIN & 0x0000000000FF0000;
					if (BE1) ramdata = ramdata & 0xFFFFFFFFFFFF00FF | top->DDRAM_DIN & 0x000000000000FF00;
					if (BE0) ramdata = ramdata & 0xFFFFFFFFFFFFFF00 | top->DDRAM_DIN & 0x00000000000000FF;

					ram_ptr[top->DDRAM_ADDR] = ramdata;	// Write the modifed value back to our RAM!

					if (cart_load_done == 1) printf("DDR WRITE  BCNT=%d  DDR_ADDR=%08x  BYTE_ADDR=%08x  DOUT=%016lx  BE=%d%d%d%d%d%d%d%d  NEWDATA=%016lx\n", top->DDRAM_BURSTCNT, top->DDRAM_ADDR, top->DDRAM_ADDR << 3, top->DDRAM_DIN, BE7, BE6, BE5, BE4, BE3, BE2, BE1, BE0, ramdata);
				}

				if (top->DDRAM_RD) {
					printf("DDR READ   BCNT=%d  DDR_ADDR=%08x  BYTE_ADDR=%08x  DATA=%016lx\n", top->DDRAM_BURSTCNT, top->DDRAM_ADDR, top->DDRAM_ADDR << 3, ramdata);
					ram_ready_count = 6;	// Simulate the DDR latency. Sort of.
				}

				//printf("cart_a: 0x%06X\n", top->cart_a);

				if (ram_ready_count>0) {
					top->DDRAM_DOUT_READY = 0;
					ram_ready_count--;
				}
				else {
					ramdata = ram_ptr[top->DDRAM_ADDR];
					top->DDRAM_DOUT = ramdata;
					top->DDRAM_DOUT_READY = 1;
				}
				*/

#if VM_TRACE
				// Dump signals into VCD file
				if (tfp) {
					if (ms >= LOG_START_MS) {
						tfp->dump(hcycle * HALF_PER_PS);
					}
				}
#endif
			//} // if ((main_time & 1) == 0)


			top->sys_clk = 1;
			top->eval();            // Evaluate model!
			top->sys_clk = 0;
			top->eval();            // Evaluate model!
			main_time++;            // Time passes...

			return 1;
		}
	}

	// Stop Verilating...
	top->final();
	delete top;
	exit(0);
	return 0;

#if VM_TRACE
	if (tfp) tfp->close();
#endif
}


int my_count = 0;

static MemoryEditor mem_edit_1;

int main(int argc, char** argv, char** env) {

	// Create application window
	WNDCLASSEX wc = { sizeof(WNDCLASSEX), CS_CLASSDC, WndProc, 0L, 0L, GetModuleHandle(NULL), NULL, NULL, NULL, NULL, _T("ImGui Example"), NULL };
	RegisterClassEx(&wc);
	HWND hwnd = CreateWindow(wc.lpszClassName, _T("Dear ImGui DirectX11 Example"), WS_OVERLAPPEDWINDOW, 100, 100, 1280, 800, NULL, NULL, wc.hInstance, NULL);

	// Initialize Direct3D
	if (CreateDeviceD3D(hwnd) < 0)
	{
		CleanupDeviceD3D();
		UnregisterClass(wc.lpszClassName, wc.hInstance);
		return 1;
	}

	// Show the window
	ShowWindow(hwnd, SW_SHOWDEFAULT);
	UpdateWindow(hwnd);

	// Setup Dear ImGui context
	IMGUI_CHECKVERSION();
	ImGui::CreateContext();
	ImGuiIO& io = ImGui::GetIO(); (void)io;
	//io.ConfigFlags |= ImGuiConfigFlags_NavEnableKeyboard;  // Enable Keyboard Controls

	// Setup Dear ImGui style
	ImGui::StyleColorsDark();
	//ImGui::StyleColorsClassic();

	// Setup Platform/Renderer bindings
	ImGui_ImplWin32_Init(hwnd);
	ImGui_ImplDX11_Init(g_pd3dDevice, g_pd3dDeviceContext);

	// Load Fonts
	// - If no fonts are loaded, dear imgui will use the default font. You can also load multiple fonts and use ImGui::PushFont()/PopFont() to select them.
	// - AddFontFromFileTTF() will return the ImFont* so you can store it if you need to select the font among multiple.
	// - If the file cannot be loaded, the function will return NULL. Please handle those errors in your application (e.g. use an assertion, or display an error and quit).
	// - The fonts will be rasterized at a given size (w/ oversampling) and stored into a texture when calling ImFontAtlas::Build()/GetTexDataAsXXXX(), which ImGui_ImplXXXX_NewFrame below will call.
	// - Read 'misc/fonts/README.txt' for more instructions and details.
	// - Remember that in C/C++ if you want to include a backslash \ in a string literal you need to write a double backslash \\ !
	//io.Fonts->AddFontDefault();
	//io.Fonts->AddFontFromFileTTF("../../misc/fonts/Roboto-Medium.ttf", 16.0f);
	//io.Fonts->AddFontFromFileTTF("../../misc/fonts/Cousine-Regular.ttf", 15.0f);
	//io.Fonts->AddFontFromFileTTF("../../misc/fonts/DroidSans.ttf", 16.0f);
	//io.Fonts->AddFontFromFileTTF("../../misc/fonts/ProggyTiny.ttf", 10.0f);
	//ImFont* font = io.Fonts->AddFontFromFileTTF("c:\\Windows\\Fonts\\ArialUni.ttf", 18.0f, NULL, io.Fonts->GetGlyphRangesJapanese());
	//IM_ASSERT(font != NULL);


	Verilated::commandArgs(argc, argv);
	
	memset(fb0_ptr, 0x00, fb0_size);

	//uint32_t value = 0xffaaaaaa;
	uint32_t value = 0x00000000;
	for (uint32_t i = 0; i < disp_size/2; i+=4 ) {
		memcpy(((char*)disp_ptr) + i, &value, 4);
	}

	
	memset(vga_ptr,  0xAA, vga_size);

	memset(ram_ptr, 0x00, ram_size);
	
	/*
	// Can't get this to work without it segfaulting atm? OzOnE.
	FILE *fontp;
	fontp = fopen("fontRomData.bin","r");
	if (fontp!=NULL) {
		printf("\nLoaded Font ROM file OK.\n\n");
	} else {
		printf("\nFont ROM file not found!\n\n");
		return 0;
	};	
	for (uint32_t i=0;i<2048;i++) {
		fread(buffer, 1, 1, fontp);
		uint32_t font_data = buffer[0];
		osd->set_mem(i, font_data);
	};
	*/

	/*
	FILE *spritefile;
	//spritefile = fopen("aburn_sp_ram", "r");
	spritefile = fopen("aburn_sp_2", "r");
	if (spritefile != NULL) { sprintf(my_string, "\nSprite RAM file loaded OK.\n"); MyAddLog(my_string); }
	else { sprintf(my_string, "\nSprite RAM file not found!\n\n"); MyAddLog(my_string); return 0; }
	unsigned int file_size;
	fseek(spritefile, 0L, SEEK_END);
	file_size = ftell(spritefile);
	fseek(spritefile, 0L, SEEK_SET);
	fread(sp_ram_ptr, 1, sp_ram_size, spritefile);	// Read the whole ROM file into RAM.
	*/

	FILE *romfile;
	romfile = fopen("jagboot.rom", "r");
	if (romfile != NULL) { sprintf(my_string, "\nBIOS ROM file loaded OK.\n");  MyAddLog(my_string); }
	else { sprintf(my_string, "\nBIOS ROM file not found!\n\n"); MyAddLog(my_string); return 0; }

	if (romfile != NULL) printf("\nBIOS ROM file loaded OK.\n");
	else printf("\nBIOS ROM file not found!\n\n");

	//unsigned int file_size;
	fseek(romfile, 0L, SEEK_END);
	file_size = ftell(romfile);
	fseek(romfile, 0L, SEEK_SET);
	fread(rom_ptr, 1, rom_size, romfile);	// Read the whole ROM file into RAM.
	
	/*
	vgap = fopen("vga_out.raw","w");
	if (vgap!=NULL) {
		printf("\nOpened vga_out.raw for writing OK.\n");
	}
	else {	
		printf("\nCould not open vga_out.raw for writing!\n\n");
		return 0;
	};
	*/

	// Our state
	bool show_demo_window = true;
	bool show_another_window = false;
	ImVec4 clear_color = ImVec4(0.45f, 0.55f, 0.60f, 1.00f);


	// Build texture atlas
	int width  = 512;
	int height = 256;

	// Upload texture to graphics system
	D3D11_TEXTURE2D_DESC desc;
	ZeroMemory(&desc, sizeof(desc));
	desc.Width = width;
	desc.Height = height;
	desc.MipLevels = 1;
	desc.ArraySize = 1;
	desc.Format = DXGI_FORMAT_R8G8B8A8_UNORM;
	//desc.Format = DXGI_FORMAT_B8G8R8A8_UNORM;
	//desc.Format = DXGI_FORMAT_B5G5R5A1_UNORM;
	desc.SampleDesc.Count = 1;
	desc.Usage = D3D11_USAGE_DEFAULT;
	desc.BindFlags = D3D11_BIND_SHADER_RESOURCE;
	desc.CPUAccessFlags = D3D11_CPU_ACCESS_WRITE;

	ID3D11Texture2D *pTexture = NULL;
	D3D11_SUBRESOURCE_DATA subResource;
	subResource.pSysMem = disp_ptr;
	//subResource.pSysMem = vga_ptr;
	subResource.SysMemPitch = desc.Width * 4;
	subResource.SysMemSlicePitch = 0;
	g_pd3dDevice->CreateTexture2D(&desc, &subResource, &pTexture);

	// Create texture view
	D3D11_SHADER_RESOURCE_VIEW_DESC srvDesc;
	ZeroMemory(&srvDesc, sizeof(srvDesc));
	srvDesc.Format = DXGI_FORMAT_R8G8B8A8_UNORM;
	srvDesc.ViewDimension = D3D11_SRV_DIMENSION_TEXTURE2D;
	srvDesc.Texture2D.MipLevels = desc.MipLevels;
	srvDesc.Texture2D.MostDetailedMip = 0;
	g_pd3dDevice->CreateShaderResourceView(pTexture, &srvDesc, &g_pFontTextureView);
	pTexture->Release();

	// Store our identifier
	ImTextureID my_tex_id = (ImTextureID)g_pFontTextureView;

	
	// Create texture sampler
	{
		D3D11_SAMPLER_DESC desc;
		ZeroMemory(&desc, sizeof(desc));
		desc.Filter = D3D11_FILTER_MIN_MAG_MIP_LINEAR;
		desc.AddressU = D3D11_TEXTURE_ADDRESS_WRAP;
		desc.AddressV = D3D11_TEXTURE_ADDRESS_WRAP;
		desc.AddressW = D3D11_TEXTURE_ADDRESS_WRAP;
		desc.MipLODBias = 0.f;
		desc.ComparisonFunc = D3D11_COMPARISON_ALWAYS;
		desc.MinLOD = 0.f;
		desc.MaxLOD = 0.f;
		g_pd3dDevice->CreateSamplerState(&desc, &g_pFontSampler);
	}


	bool follow_writes = 0;
	int write_address = 0;

	static bool show_app_console = true;
	
	// imgui Main loop stuff...
	MSG msg;
	ZeroMemory(&msg, sizeof(msg));
	while (msg.message != WM_QUIT)
	{
		// Poll and handle messages (inputs, window resize, etc.)
		// You can read the io.WantCaptureMouse, io.WantCaptureKeyboard flags to tell if dear imgui wants to use your inputs.
		// - When io.WantCaptureMouse is true, do not dispatch mouse input data to your main application.
		// - When io.WantCaptureKeyboard is true, do not dispatch keyboard input data to your main application.
		// Generally you may always pass all inputs to dear imgui, and hide them from your application based on those two flags.
		if (PeekMessage(&msg, NULL, 0U, 0U, PM_REMOVE))
		{
			TranslateMessage(&msg);
			DispatchMessage(&msg);
			continue;
		}

		// Start the Dear ImGui frame
		ImGui_ImplDX11_NewFrame();
		ImGui_ImplWin32_NewFrame();
		ImGui::NewFrame();

		// 1. Show the big demo window (Most of the sample code is in ImGui::ShowDemoWindow()! You can browse its code to learn more about Dear ImGui!).
		//if (show_demo_window)
		//	ImGui::ShowDemoWindow(&show_demo_window);

		// 2. Show a simple window that we create ourselves. We use a Begin/End pair to created a named window.
		static float f = 0.1f;
		static int counter = 0;

		ImGui::Begin("Virtual Dev Board v1.0");		// Create a window called "Virtual Dev Board v1.0" and append into it.

		ShowMyExampleAppConsole(&show_app_console);

		//ImGui::Text("Verilator sim running... ROM_ADDR: 0x%05x", top->ROM_ADDR);               // Display some text (you can use a format strings too)
																							   //ImGui::Checkbox("Demo Window", &show_demo_window);      // Edit bools storing our window open/close state
																							   //ImGui::Checkbox("Another Window", &show_another_window);

		//ImGui::SliderFloat("float", &f, 0.0f, 1.0f);            // Edit 1 float using a slider from 0.0f to 1.0f
		//ImGui::ColorEdit3("clear color", (float*)&clear_color); // Edit 3 floats representing a color

																//if (ImGui::Button("Button"))                            // Buttons return true when clicked (most widgets return true when edited/activated)
																//counter++;

																//ImGui::SameLine();
																//ImGui::Text("counter = %d", counter);
		//ImGui::Text("samp_index = %d", samp_index);
		//ImGui::Text("Application average %.3f ms/frame (%.1f FPS)", 1000.0f / ImGui::GetIO().Framerate, ImGui::GetIO().Framerate);
		//ImGui::PlotLines("Lines", values, IM_ARRAYSIZE(values), values_offset, "sample", -1.0f, 1.0f, ImVec2(0, 80));
		if (ImGui::Button("RESET")) {
			main_time = 0;
			memset(fb0_ptr, 0x00, fb0_size);	// Clear the framebuffer.
			// Clear the DISPLAY buffer...
			uint32_t value = 0x00000000;
			for (uint32_t i = 0; i < disp_size / 2; i += 4) {
				memcpy(((char*)disp_ptr) + i, &value, 4);
			}
		}
		ImGui::Text("main_time %d", main_time);
		ImGui::Text("frame_count: %d  line_count: %d", frame_count, line_count);

		/*
		ImGui::Text("Addr:   0x%08X", top->mem_addr << 2);
		
		ImGui::Text("PC:     0x%08X", top->pc << 2);
		if (top->system_top__DOT__core__DOT__PC__DOT__enable) {
			ImGui::SameLine(150); ImGui::Text("<- WRITE 0x%08X", top->system_top__DOT__core__DOT__IF_PCIn);
		}

		if (top->system_top__DOT__core__DOT__PC__DOT__exe_pc_write) {
			ImGui::SameLine(150); ImGui::Text("<- EXE_PC WRITE 0x%08X", top->system_top__DOT__core__DOT__exe_pc);
		}
	
		ImGui::Text("Inst:   0x%08X", top->system_top__DOT__core__DOT__InstMem_In);
		*/

		//if (ImGui::Button("Reset!")) top->KEY = 0;
		//else top->KEY = 1;

		ImGui::Checkbox("RUN", &run_enable);

		if (single_step == 1) single_step = 0;
		if (ImGui::Button("Single Step")) {
			run_enable = 0;
			single_step = 1;
		}

		if (multi_step == 1) multi_step = 0;
		if (ImGui::Button("Multi Step")) {
			run_enable = 0;
			multi_step = 1;
		}
		ImGui::SameLine(); ImGui::SliderInt("Step amount", &multi_step_amount, 8, 1024);
		ImGui::Text("Last SDRAM WRITE. byte_addr: 0x%08X  write_data: 0x%08X  data_ben: 0x%01X\n", last_sdram_byteaddr, last_sdram_writedata, last_sdram_ben);	//  Note sd_data_i is OUT of the sim!

		ImGui::Image(my_tex_id, ImVec2(width*2, height*2), ImVec2(0, 0), ImVec2(1, 1), ImColor(255, 255, 255, 255), ImColor(255, 255, 255, 128));
		ImGui::End();


		ImGui::Begin("BIOS ROM Viewer");
		/*
		ImGui::Checkbox("Follow Writes", &follow_writes);
		if (follow_writes) write_address = top->sd_addr << 2;
		*/
		mem_edit_1.DrawContents(rom_ptr, rom_size, 0);
		ImGui::End();

		ImGui::Begin("Core Registers");
		ImGui::Text("Tom aen:	 %d",top->jaguar__DOT__aen);
		ImGui::Text("Jerry aen:	 %d",top->jaguar__DOT__j_aen);
		ImGui::Text("abus_out:	 0x%06X", top->abus_out);
		ImGui::Text("dbus:       0x%016X",top->jaguar__DOT__dbus);
		ImGui::Separator();
		ImGui::Text("fx68k_addr: 0x%06X",top->jaguar__DOT__fx68k_byte_addr);
		ImGui::Text("fx68k_din:  0x%04X",top->jaguar__DOT__fx68k_din);
		ImGui::Text("fx68k_dout: 0x%04X",top->jaguar__DOT__fx68k_dout);
		ImGui::Text("fx68k_br_n: %d",top->jaguar__DOT__fx68k_br_n);
		ImGui::Text("fx68k_bg_n: %d",top->jaguar__DOT__fx68k_bg_n);
		ImGui::Text("fx68k_bgack_n: %d",top->jaguar__DOT__fx68k_bgack_n);
		ImGui::Text("fx68k_pch: 0x%06X",top->jaguar__DOT__fx68k_inst__DOT__excUnit__DOT__PcH);
		ImGui::Text("fx68k_pcl: 0x%06X",top->jaguar__DOT__fx68k_inst__DOT__excUnit__DOT__PcL);
		ImGui::Separator();
		ImGui::Text("clkdiv:     %d", top->jaguar__DOT__clkdiv);
		ImGui::Text("tlw:        %d",top->jaguar__DOT__tom_inst__DOT__tlw);
		ImGui::Text("xpclk:      %d", top->jaguar__DOT__tom_inst__DOT__xpclk);
		ImGui::Text("xvclk:      %d", top->jaguar__DOT__tom_inst__DOT__xvclk);
		ImGui::Text("enPhi1:     %d", top->jaguar__DOT__fx68k_enPhi1);
		ImGui::Text("enPhi2:     %d", top->jaguar__DOT__fx68k_enPhi2);
		ImGui::Separator();
		ImGui::Text("os_rom_q:   0x%02X", top->os_rom_q);
		//ImGui::Text("os_rom_oe:  %d", top->os_rom_oe);
		ImGui::Separator();
		ImGui::Text("cart_q:     0x%08X", top->cart_q);
		//ImGui::Text("cart_oe:    0x%01X", top->cart_oe);
		ImGui::Separator();
		ImGui::Text("dram_a:     0x%02X", top->dram_a);
		ImGui::Text("startcas:   %d", top->startcas);
		ImGui::Text("dram_cas_n: %d", top->dram_cas_n);
		ImGui::Text("dram_oe_n:  b%d%d", (top->dram_oe_n>>1)&1, top->dram_oe_n&1);
		ImGui::Text("dram_uw_n:  b%d%d%d%d", (top->dram_uw_n>>3)&1, (top->dram_uw_n>>2)&1, (top->dram_uw_n>>1)&1, top->dram_uw_n&1);
		ImGui::Text("dram_lw_n:  b%d%d%d%d", (top->dram_lw_n>>3)&1, (top->dram_lw_n>>2)&1, (top->dram_lw_n>>1)&1, top->dram_lw_n&1);
		ImGui::Text("dram_oe:    b%d%d%d%d", (top->dram_oe>>3)&1, (top->dram_oe>>2)&1, (top->dram_oe>>1)&1, top->dram_oe&1);
		ImGui::Separator();

		if (main_time != old_main_time) {
			for (int i=0; i<19; i++) arr[i] = arr[i+1];
			arr[19] = top->jaguar__DOT__fx68k_as_n;
		}
		old_main_time = main_time;

		ImGui::PlotHistogram("as_n",arr,IM_ARRAYSIZE(arr));

		ImGui::End();
	
		ImGui::End();

		// 3. Show another simple window.
		/*
		if (show_another_window)
		{
		ImGui::Begin("Another Window", &show_another_window);   // Pass a pointer to our bool variable (the window will have a closing button that will clear the bool when clicked)
		ImGui::Text("Hello from another window!");
		if (ImGui::Button("Close Me"))
		show_another_window = false;
		ImGui::End();
		}
		*/

		// Update the texture for disp_ptr!
		// D3D11_USAGE_DEFAULT MUST be set in the texture description (somewhere above) for this to work.
		// (D3D11_USAGE_DYNAMIC is for use with map / unmap.) ElectronAsh.
		g_pd3dDeviceContext->UpdateSubresource(pTexture, 0, NULL, disp_ptr, width * 4, 0);

		// Rendering
		ImGui::Render();
		g_pd3dDeviceContext->OMSetRenderTargets(1, &g_mainRenderTargetView, NULL);
		g_pd3dDeviceContext->ClearRenderTargetView(g_mainRenderTargetView, (float*)&clear_color);
		ImGui_ImplDX11_RenderDrawData(ImGui::GetDrawData());

		//g_pSwapChain->Present(1, 0); // Present with vsync
		g_pSwapChain->Present(0, 0); // Present without vsync


		if (run_enable) /*for (int step = 0; step < 1024; step++)*/ verilate();	// Simulates MUCH faster if it's done in batches.
		else {																									// But, that will affect the values we can grab for the GUI.
			if (single_step) verilate();
			if (multi_step) for (int step = 0; step < multi_step_amount; step++) verilate();
		}
	}
	// Close imgui stuff properly...
	ImGui_ImplDX11_Shutdown();
	ImGui_ImplWin32_Shutdown();
	ImGui::DestroyContext();

	CleanupDeviceD3D();
	DestroyWindow(hwnd);
	UnregisterClass(wc.lpszClassName, wc.hInstance);

	return 0;
}
