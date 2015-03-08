/*
 * stdafx.h : include file for standard system include files,
 * or project specific include files that are used frequently, but
 * are changed infrequently
 */

#pragma once

#include <math.h>
#include <stdio.h>

#include "external.h"

#if _MSC_VER >= 1400
extern "C" __declspec(dllimport) int __stdcall IsDebuggerPresent();

#define fprintf(file, format_string, ...) \
	{ \
	   if (IsDebuggerPresent()) {\
			char output_string[2048];\
			sprintf_s(output_string, 2048, format_string, ##__VA_ARGS__); \
			System::Diagnostics::Trace::Write(gcnew System::String(output_string)); \
			fprintf_s(file, format_string, ##__VA_ARGS__); \
	   } else {\
			fprintf_s(file, format_string, ##__VA_ARGS__); \
	   }\
	}
#endif

