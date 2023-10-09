/* Copyright 2001,2006,2008,2016,2018,2021
     Free Software Foundation, Inc.

   This file is part of Guile.

   Guile is free software: you can redistribute it and/or modify it
   under the terms of the GNU Lesser General Public License as published
   by the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   Guile is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
   License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with Guile.  If not, see
   <https://www.gnu.org/licenses/>.  */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <versionhelpers.h>
#include <c-strcase.h>
#include <process.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <signal.h>
#include <io.h>
#include <fcntl.h>
#include <unistr.h>

#include "gc.h"        /* for scm_*alloc, scm_strdup */
#include "threads.h"   /* for scm_i_scm_pthread_mutex_lock */

#include "posix-w32.h"

/*
 * Get name and information about current kernel.
 */
int
uname (struct utsname *uts)
{
  SYSTEM_INFO sysinfo;

  memset (uts, 0, sizeof (*uts));

  GetSystemInfo (&sysinfo);

  /* Getting the version number of Windows is deprecated, because each
   * application can ask to manifest a different supported version of
   * Windows.  The version numbers are approximate and come from
   * https://learn.microsoft.com/en-us/windows/win32/sysinfo/operating-system-version. */
#if defined(__MINGW64__)
  const char *root = "MINGW64_NT";
#elif defined(__MINGW32__)
  const char *root = "MINGW32_NT";
#elif defined(__MSYS__)
  const char *root = "MSYS_NT";
#elif defined(__CYGWIN__)
  const char *root = "CYGWIN_NT";
#else
  const char *root = NULL;
#endif
  if (IsWindows10OrGreater ())
    {
      if (root)
        sprintf (uts->sysname, "%s-10.0", root);
      else
        strcpy (uts->sysname, "Windows 10");
      strcpy (uts->version, "10.0");
    }
  else if (IsWindows8Point1OrGreater ())
    {
      if (root)
        sprintf (uts->sysname, "%s-6.3", root);
      else
        strcpy (uts->sysname, "Windows 8.1");
      strcpy (uts->version, "6.3");
    }
  else if (IsWindows8OrGreater ())
    {
      if (root)
        sprintf (uts->sysname, "%s-6.2", root);
      else
        strcpy (uts->sysname, "Windows 8");
      strcpy (uts->version, "6.2");
    }
  else if (IsWindows7SP1OrGreater ())
    {
      if (root)
        sprintf (uts->sysname, "%s-6.1", root);
      else
        strcpy (uts->sysname, "Windows 7SP1");
      strcpy (uts->version, "6.1");
    }
  else if (IsWindows7OrGreater ())
    {
      if (root)
        sprintf (uts->sysname, "%s-6.1", root);
      else
        strcpy (uts->sysname, "Windows 7");
      strcpy (uts->version, "6.1");
    }
  else if (IsWindowsVistaOrGreater ())
    {
      if (root)
        sprintf (uts->sysname, "%s-6.0", root);
      else
        strcpy (uts->sysname, "Windows Vista");
      strcpy (uts->version, "6.0");
    }
  else
    {
      if (root)
        sprintf (uts->sysname, "%s-5.0", root);
      else
        strcpy (uts->sysname, "Windows");
      strcpy (uts->version, "5.0");
    }

  /* There is no non-deprecated way to get the build number. */
  strcpy (uts->release, "unknown");

  switch (sysinfo.wProcessorArchitecture)
    {
    case PROCESSOR_ARCHITECTURE_ARM:
      strcpy (uts->machine,"ARM");
      break;
    case PROCESSOR_ARCHITECTURE_ARM64:
      strcpy (uts->machine, "ARM64");
      break;
    case PROCESSOR_ARCHITECTURE_IA64:
      strcpy (uts->machine, "ia64");
      break;
    case PROCESSOR_ARCHITECTURE_INTEL:
      strcpy (uts->machine, "x86");
      break;
    case PROCESSOR_ARCHITECTURE_AMD64:
      strcpy (uts->machine, "x86_64");
      break;
    default:
      strcpy (uts->machine, "unknown");
      break;
  }

  unsigned long sLength = sizeof (uts->nodename) - 1;
  GetComputerName (uts->nodename, &sLength);
  return 0;
}


/* Translate abnormal exit status of Windows programs into the signal
   that terminated the program.  This is required to support scm_kill
   and WTERMSIG.  */

struct signal_and_status {
  int sig;
  DWORD status;
};

static const struct signal_and_status sigtbl[] = {
  {SIGSEGV, 0xC0000005},        /* access to invalid address */
  {SIGSEGV, 0xC0000008},        /* invalid handle */
  {SIGILL,  0xC000001D},        /* illegal instruction */
  {SIGILL,  0xC0000025},        /* non-continuable instruction */
  {SIGSEGV, 0xC000008C},        /* array bounds exceeded */
  {SIGFPE,  0xC000008D},        /* float denormal */
  {SIGFPE,  0xC000008E},        /* float divide by zero */
  {SIGFPE,  0xC000008F},        /* float inexact */
  {SIGFPE,  0xC0000090},        /* float invalid operation */
  {SIGFPE,  0xC0000091},        /* float overflow */
  {SIGFPE,  0xC0000092},        /* float stack check */
  {SIGFPE,  0xC0000093},        /* float underflow */
  {SIGFPE,  0xC0000094},        /* integer divide by zero */
  {SIGFPE,  0xC0000095},        /* integer overflow */
  {SIGILL,  0xC0000096},        /* privileged instruction */
  {SIGSEGV, 0xC00000FD},        /* stack overflow */
  {SIGTERM, 0xC000013A},        /* Ctrl-C exit */
  {SIGINT,  0xC000013A}
};

static int
w32_signal_to_status (int sig)
{
  int i;

  for (i = 0; i < sizeof (sigtbl) / sizeof (sigtbl[0]); i++)
    if (sig == sigtbl[i].sig)
      return sigtbl[i].status;

  return (int)0xC000013A;
}

int
waitpid (pid_t pid, int *status, int options)
{
  /* Not supported on MS-Windows.  */
  if (pid <= 0 || options != 0)
    {
      errno = ENOSYS;
      return -1;
    }

  return _cwait (status, pid, WAIT_CHILD);
}

int
w32_status_to_termsig (DWORD status)
{
  int i;

  for (i = 0; i < sizeof (sigtbl) / sizeof (sigtbl[0]); i++)
    if (status == sigtbl[i].status)
      return sigtbl[i].sig;

  return SIGTERM;
}

/* This only implements the absolute minimum features for
   foreign-library.scm. */
void *
dlopen_w32 (const char *name, int flags)
{
  void *ret = NULL;
  if (name == NULL || *name == '\0')
    return (void *) GetModuleHandle (NULL);
  // LoadLibrary expects a wchar_t string that is either an absolute
  // path using backslashes or is just a filename.
  size_t len;
  uint16_t *c_wpath = u8_to_u16 (name, strlen(name) + 1, NULL, &len);
  int relative = 0;
  for (size_t i = 0; i < len; i ++)
    {
      if (c_wpath[i] == L'/')
        {
          c_wpath[i] = L'\\';
          relative = 1;
        }
      // The ':' is because c:foo.dll is also a relative path.
      else if (c_wpath[i] == L'\\' || c_wpath[i] == L':')
        relative = 1;
    }
  if (relative)
    {
      wchar_t buffer[4096];
      DWORD outlen = GetFullPathNameW (c_wpath, 4096, buffer, NULL);
      if (outlen < 4096)
        {
          ret = (void *) LoadLibraryW (buffer);
          if (ret != NULL)
            GetModuleHandleExW (0, buffer, (HMODULE *) & ret);
        }
      else
        ret = NULL;
    }
  else
    {
      ret = (void *) LoadLibraryW (c_wpath);
      wprintf(L"LoadLibrary %ls %p %lu\n", c_wpath, ret, GetLastError());
      if (ret != NULL)
        GetModuleHandleExW (0, c_wpath, (HMODULE *) & ret);
    }
  free (c_wpath);
  return ret;
}

void *
dlsym_w32 (void *handle, const char *name)
{
  return (void *) GetProcAddress ((HMODULE) handle, name);
}

int
dlclose_w32 (void *handle)
{
  FreeLibrary ((HMODULE) handle);
  return 0;
}

#define DLERROR_LEN 80
static char dlerror_str[DLERROR_LEN + 1];

char *
dlerror_w32 ()
{
  char *msg_buf;
  DWORD dw = GetLastError ();
  FormatMessage (FORMAT_MESSAGE_ALLOCATE_BUFFER |
		 FORMAT_MESSAGE_FROM_SYSTEM |
		 FORMAT_MESSAGE_IGNORE_INSERTS,
		 NULL,
		 dw,
		 MAKELANGID (LANG_NEUTRAL, SUBLANG_DEFAULT),
		 (LPTSTR) & msg_buf, 0, NULL);
  if (dw == 0)
    snprintf (dlerror_str, DLERROR_LEN, "No error");
  else
    snprintf (dlerror_str, DLERROR_LEN, "error %ld: %s", (long) dw, msg_buf);
  return dlerror_str;
}

/* Check for the Windows 11 bug where there's a return character in the
 * console input queue despite draining the input. */
int
console_has_return_keyevent_w32 (int fdes)
{
  HANDLE h;
  DWORD mode;

  h = (HANDLE) _get_osfhandle (fdes);
  if (h == -1)
    return 0;
  if (GetConsoleMode (h, &mode) == 0)
    return 0;

  // Rarely need more than 1 INPUT_RECORD for this test, but just in
  // case there is a mouse event in the queue.
#define NBUFFER 8
  INPUT_RECORD irbuffer[NBUFFER];
  BOOL bRet;
  DWORD avail;
  int i;
  int n_chars = 0;
  int n_returns = 0;

  while (1)
    {
      bRet = PeekConsoleInput (h, irbuffer, NBUFFER, &avail);
      if (!bRet || avail == 0)
        break;

      for (i = 0; i < avail; i++)
        if (irbuffer[i].EventType == KEY_EVENT)
          {
            n_chars ++;
            if (irbuffer[i].Event.KeyEvent.uChar.AsciiChar == 13)
              n_returns ++;
          }
      if (avail < NBUFFER)
        break;
    }

  if (n_chars == 1 && n_returns == 1)
    return 1;
  return 0;
#undef NBUFFER
}

int
getpagesize_w32 (void)
{
  return 4 * 1024;
}
