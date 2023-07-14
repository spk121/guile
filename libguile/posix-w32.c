/* Copyright 2001,2006,2008,2016,2018,2021,2025
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
#include <c-strcase.h>
#include <process.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <signal.h>
#include <io.h>
#include <fcntl.h>

#include "gc.h"        /* for scm_*alloc, scm_strdup */
#include "threads.h"   /* for scm_i_scm_pthread_mutex_lock */

#include "posix-w32.h"

/*
 * Get name and information about current kernel.
 */
int
uname (struct utsname *uts)
{
  enum { WinNT, Win95, Win98, WinUnknown };
  OSVERSIONINFO osver;
  SYSTEM_INFO sysinfo;
  DWORD sLength;
  DWORD os = WinUnknown;

  memset (uts, 0, sizeof (*uts));

  osver.dwOSVersionInfoSize = sizeof (osver);
  GetVersionEx (&osver);
  GetSystemInfo (&sysinfo);

  switch (osver.dwPlatformId)
    {
    case VER_PLATFORM_WIN32_NT: /* NT, Windows 2000 or Windows XP */
      if (osver.dwMajorVersion == 4)
        strcpy (uts->sysname, "Windows NT4x"); /* NT4x */
      else if (osver.dwMajorVersion <= 3)
        strcpy (uts->sysname, "Windows NT3x"); /* NT3x */
      else if (osver.dwMajorVersion == 5 && osver.dwMinorVersion < 1)
        strcpy (uts->sysname, "Windows 2000"); /* 2k */
      else if (osver.dwMajorVersion < 6)
        strcpy (uts->sysname, "Windows XP");   /* XP */
      else if (osver.dwMajorVersion == 6)
        {
          if (osver.dwMinorVersion < 1)
            strcpy (uts->sysname, "Windows Vista");   /* Vista */
          else if (osver.dwMinorVersion < 2)
            strcpy (uts->sysname, "Windows 7"); /* Windows 7 */
          else if (osver.dwMinorVersion < 3)
            strcpy (uts->sysname, "Windows 8"); /* Windows 8 */
          else if (osver.dwMinorVersion < 4)
            strcpy (uts->sysname, "Windows 8.1"); /* Windows 8.1 */
        }
      else if (osver.dwMajorVersion >= 10)
        strcpy (uts->sysname, "Windows 10 or later"); /* Windows 10 and later */
      os = WinNT;
      break;

    case VER_PLATFORM_WIN32_WINDOWS: /* Win95, Win98 or WinME */
      if ((osver.dwMajorVersion > 4) ||
          ((osver.dwMajorVersion == 4) && (osver.dwMinorVersion > 0)))
        {
          if (osver.dwMinorVersion >= 90)
            strcpy (uts->sysname, "Windows ME"); /* ME */
          else
            strcpy (uts->sysname, "Windows 98"); /* 98 */
          os = Win98;
        }
      else
        {
          strcpy (uts->sysname, "Windows 95"); /* 95 */
          os = Win95;
        }
      break;

    case VER_PLATFORM_WIN32s: /* Windows 3.x */
      strcpy (uts->sysname, "Windows");
      break;
    }

  sprintf (uts->version, "%ld.%02ld",
           osver.dwMajorVersion, osver.dwMinorVersion);

  if (osver.szCSDVersion[0] != '\0' &&
      (strlen (osver.szCSDVersion) + strlen (uts->version) + 1) <
      sizeof (uts->version))
    {
      strcat (uts->version, " ");
      strcat (uts->version, osver.szCSDVersion);
    }

  sprintf (uts->release, "build %ld", osver.dwBuildNumber & 0xFFFF);

  switch (sysinfo.wProcessorArchitecture)
    {
    case PROCESSOR_ARCHITECTURE_PPC:
      strcpy (uts->machine, "ppc");
      break;
    case PROCESSOR_ARCHITECTURE_ALPHA:
      strcpy (uts->machine, "alpha");
      break;
    case PROCESSOR_ARCHITECTURE_MIPS:
      strcpy (uts->machine, "mips");
      break;
    case PROCESSOR_ARCHITECTURE_IA64:
      strcpy (uts->machine, "ia64");
      break;
    case PROCESSOR_ARCHITECTURE_INTEL:
      /*
       * dwProcessorType is only valid in Win95 and Win98 and WinME
       * wProcessorLevel is only valid in WinNT
       */
      switch (os)
        {
        case Win95:
        case Win98:
          switch (sysinfo.dwProcessorType)
            {
            case PROCESSOR_INTEL_386:
            case PROCESSOR_INTEL_486:
            case PROCESSOR_INTEL_PENTIUM:
              sprintf (uts->machine, "i%ld", sysinfo.dwProcessorType);
              break;
            default:
              strcpy (uts->machine, "i386");
              break;
          }
          break;
        case WinNT:
          sprintf (uts->machine, "i%d86", sysinfo.wProcessorLevel);
          break;
        default:
          strcpy (uts->machine, "unknown");
          break;
        }
      break;
    case PROCESSOR_ARCHITECTURE_AMD64:
      strcpy (uts->machine, "x86_64");
      break;
    default:
      strcpy (uts->machine, "unknown");
      break;
  }

  sLength = sizeof (uts->nodename) - 1;
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
  ret = (void *) LoadLibrary (name);
  GetModuleHandleEx (0, name, (HMODULE *) & ret);
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
