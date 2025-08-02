#define UNW_LOCAL_ONLY
#include <libunwind.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#ifdef __APPLE__
#include <dlfcn.h>
#include <execinfo.h>
#include <mach-o/dyld.h>
#else
// Linux includes
#include <dlfcn.h>
#ifdef HAVE_LIBDW
#include <elfutils/libdw.h>
#include <elfutils/libdwfl.h>
#endif
#endif

typedef struct {
  char filename[256];
  char function[256];
  int line_number;
  int column;
  int found;
} source_location_t;

#ifdef __APPLE__
static void resolve_source_location_macos(unw_word_t ip,
                                          source_location_t *loc) {
  loc->found = 0;
  loc->line_number = 0;
  loc->column = 0;
  strcpy(loc->filename, "<unknown>");
  strcpy(loc->function, "<unknown>");

  Dl_info info;
  if (!dladdr((void *)ip, &info)) {
    return;
  }

  char exe_path[1024];
  uint32_t size = sizeof(exe_path);
  if (_NSGetExecutablePath(exe_path, &size) != 0) {
    return;
  }

  uintptr_t load_addr = (uintptr_t)info.dli_fbase;
  uintptr_t file_addr = ip - load_addr;

  // Create atos command to resolve the address
  char atos_cmd[2048];
  snprintf(atos_cmd, sizeof(atos_cmd),
           "atos -o '%s' -l 0x%lx 0x%lx 2>/dev/null", exe_path, load_addr,
           (unsigned long)ip);

  FILE *fp = popen(atos_cmd, "r");
  if (!fp) {
    return;
  }

  char result[1024];
  if (fgets(result, sizeof(result), fp)) {
    char *newline = strchr(result, '\n');
    if (newline)
      *newline = '\0';

    // printf("DEBUG: atos returned: '%s'\n", result);

    // Parse atos output which might be: "function_name (in executable)
    // (file.k1:line)" or just: "0x... (in executable)"

    // Look for file:line pattern first
    char *colon = strrchr(result, ':');
    if (colon && *(colon + 1) >= '0' && *(colon + 1) <= '9') {
      loc->line_number = atoi(colon + 1);

      // Find the filename by looking backwards for the opening parenthesis
      char *open_paren = strrchr(result, '(');
      if (open_paren && open_paren < colon) {
        char *filename_start = open_paren + 1;
        int filename_len = colon - filename_start;
        if (filename_len > 0 && filename_len < sizeof(loc->filename)) {
          strncpy(loc->filename, filename_start, filename_len);
          loc->filename[filename_len] = '\0';
        }
      }

      // Extract function name (everything before first parenthesis)
      char *first_paren = strchr(result, ' ');
      if (first_paren) {
        int func_len = first_paren - result;
        if (func_len > 0 && func_len < sizeof(loc->function)) {
          strncpy(loc->function, result, func_len);
          loc->function[func_len] = '\0';
        }
      }

      loc->found = 1;
    } else {
      // No line number found, just extract function name if available
      char *first_paren = strchr(result, ' ');
      if (first_paren) {
        int func_len = first_paren - result;
        if (func_len > 0 && func_len < sizeof(loc->function)) {
          strncpy(loc->function, result, func_len);
          loc->function[func_len] = '\0';
        }
      } else {
        strncpy(loc->function, result, sizeof(loc->function) - 1);
      }
    }
  }
  pclose(fp);
}
#endif

// NOTE(backtrace): Linux version for resolving source location
#ifndef __APPLE__
static void resolve_source_location_linux(unw_word_t ip,
                                          source_location_t *loc) {
  loc->found = 0;
  loc->line_number = 0;
  loc->column = 0;
  strcpy(loc->filename, "<unknown>");
  strcpy(loc->function, "<unknown>");

  Dl_info info;
  if (!dladdr((void *)ip, &info)) {
    return;
  }

#ifdef HAVE_LIBDW
  // TODO(backtrace): Implement full libdw-based line number resolution
  // This would involve:
  // 1. Opening the executable with dwfl_begin()
  // 2. Finding the compilation unit for the IP with dwfl_addrmodule()
  // 3. Getting line info with dwfl_getsrc()
  // 4. Extracting filename and line number

  // For now, just use function name from dladdr
  if (info.dli_sname) {
    strncpy(loc->function, info.dli_sname, sizeof(loc->function) - 1);
    loc->function[sizeof(loc->function) - 1] = '\0';
  }

  // Extract executable name for potential use with addr2line
  if (info.dli_fname) {
    char *basename = strrchr(info.dli_fname, '/');
    basename = basename ? basename + 1 : (char *)info.dli_fname;

    // Calculate relative address for tools like addr2line
    uintptr_t relative_addr = (uintptr_t)ip - (uintptr_t)info.dli_fbase;

    // NOTE(backtrace): Users can manually resolve with:
    // addr2line -e executable_name -f -C relative_addr
    snprintf(loc->filename, sizeof(loc->filename), "%s+0x%lx", basename,
             relative_addr);
  }
#else
  // NOTE(backtrace): Fallback version without DWARF support
  if (info.dli_sname) {
    strncpy(loc->function, info.dli_sname, sizeof(loc->function) - 1);
    loc->function[sizeof(loc->function) - 1] = '\0';
  }

  if (info.dli_fname) {
    char *basename = strrchr(info.dli_fname, '/');
    basename = basename ? basename + 1 : (char *)info.dli_fname;
    strncpy(loc->filename, basename, sizeof(loc->filename) - 1);
    loc->filename[sizeof(loc->filename) - 1] = '\0';
  }
#endif
}
#endif

// NOTE(backtrace): Main function to resolve source location
static void resolve_source_location(unw_word_t ip, source_location_t *loc) {
#ifdef __APPLE__
  resolve_source_location_macos(ip, loc);
#else
  resolve_source_location_linux(ip, loc);
#endif
}

void _k1_show_backtrace(void) {
  unw_cursor_t cursor;
  unw_context_t uc;
  unw_word_t ip, sp, off;

  unw_getcontext(&uc);
  unw_init_local(&cursor, &uc);

  printf("Backtrace (most recent call first):\n");

  int frame_num = 0;
  while (unw_step(&cursor) > 0) {
    char symbol[256] = {"<unknown>"};
    unw_get_proc_name(&cursor, symbol, sizeof(symbol), &off);
    unw_get_reg(&cursor, UNW_REG_IP, &ip);
    unw_get_reg(&cursor, UNW_REG_SP, &sp);

    source_location_t loc;
    // nocommit: its megaslow, maybe see if claude can fix
    resolve_source_location(ip, &loc);

    if (loc.found && loc.line_number > 0) {
      // Rust-like format: "   0: function_name at file.k1:line:column"
      printf("  %2d: %s at %s:%d\n", frame_num, loc.function, loc.filename,
             loc.line_number);
    } else {
      // Fallback format when we can't resolve line numbers
      printf("  %2d: %s [0x%lx]\n", frame_num, symbol, (long)ip);
    }

    frame_num++;

    // Limit backtrace depth to avoid overwhelming output
    if (frame_num >= 50) {
      printf("  ... (backtrace truncated)\n");
      break;
    }
  }
}
