#ifndef SCM_POSIX_W32_H
#define SCM_POSIX_W32_H

/* Copyright 2001,2006,2018,2020,2025
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

#ifdef _WIN32
#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif
#include <windows.h>
#endif
#include <string.h>
#include "libguile/scm.h"

#define _UTSNAME_LENGTH 65
#define _UTSNAME_NODENAME_LENGTH _UTSNAME_LENGTH
#define _UTSNAME_DOMAIN_LENGTH _UTSNAME_LENGTH

/* Structure describing the system and machine.  */
struct utsname
{
  /* Name of the implementation of the operating system.  */
  char sysname[_UTSNAME_LENGTH];

  /* Name of this node on the network.  */
  char nodename[_UTSNAME_NODENAME_LENGTH];

  /* Current release level of this implementation.  */
  char release[_UTSNAME_LENGTH];

  /* Current version level of this release.  */
  char version[_UTSNAME_LENGTH];

  /* Name of the hardware type the system is running on.  */
  char machine[_UTSNAME_LENGTH];

  /* Name of the domain of this node on the network.  */
  char domainname[_UTSNAME_DOMAIN_LENGTH];
};

#define WNOHANG               1

#define WEXITSTATUS(stat_val) ((stat_val) & 255)
/* MS-Windows programs that crash due to a fatal exception exit with
   an exit code whose 2 MSB bits are set.  */
#define WIFEXITED(stat_val)   (((stat_val) & 0xC0000000) == 0)
#define WIFSIGNALED(stat_val) (((stat_val) & 0xC0000000) == 0xC0000000)
#define WTERMSIG(stat_val)    w32_status_to_termsig (stat_val)
/* The funny conditional avoids a compiler warning in status:stop_sig.  */
#define WIFSTOPPED(stat_val)  ((stat_val) == (stat_val) ? 0 : 0)
#define WSTOPSIG(stat_var)    (0)

SCM_INTERNAL int uname (struct utsname * uts);
SCM_INTERNAL int waitpid (intptr_t, int *, int);
SCM_INTERNAL int w32_status_to_termsig (DWORD status);
SCM_INTERNAL void *dlopen_w32 (const char *name, int flags);
SCM_INTERNAL void *dlsym_w32 (void *handle, const char *name);
SCM_INTERNAL int dlclose_w32 (void *handle);
SCM_INTERNAL char *dlerror_w32 (void);
SCM_INTERNAL int console_has_return_keyevent_w32 (int fdes);
SCM_INTERNAL int getpagesize_w32 (void);

#define HAVE_UNAME 1
#define HAVE_WAITPID 1

#define RTLD_NOW 1
#define RTLD_LAZY 2
#define RTLD_GLOBAL 4
#define RTLD_LOCAL 8

#endif /* SCM_POSIX_W32_H */
