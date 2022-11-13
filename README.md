# Relocatable, Packagable Guile 

This fork of Guile intends make it possible put Guile in three forms
relocatable binary packages.

- MSIX for Windows 10/11 on x86-64
- Flatpak on Linux x86-64
- Snap on Linux x86-64

## Freedom to Share

Let's say I wanted to share a game I wrote with my Dad.  My Dad lives in
another country, so I can't just come by with my laptop.  If I tried,
this is what would happen.

"Is it on Steam?" he'd ask.

"Sorry, no," I'd say.

"Is it a web browser game, then?"

"No, it is a regular program."

"Microsoft Store?"

"Nope."

"So, I download an installer, then?" he'd ask. My Dad's no newbie; he
knows what an installer is.

At this point, were the answer 'yes', I might be able to get it to
him. But if I started talking about tarballs, unzipping, right-clicking,
or compiling, it would be game over.  So to get him to try it, it would
have to be a normal install for his PC. Click to install, and then run
the program from the menu.

Unfortunately, if I'd written this little game for a Lisp Game Jam, and
I'd written the game in GNU Guile, this would be difficult.  This
document describes the lessons I learn and the challenges I face
trying to make that happen.  It may have some useful information for
others who walk this path.

If you're going to criticize me for attempting this, flame away. I can
take it. I'll be dealing with non-free software, pre-compiled binaries,
sandboxing; the whole pile of nonsense.  I'll also be saying the word
'Linux' a lot as shorthand for GNU/Linux.

I believe in software freedom, but, sometimes, I just want someone check
out my stuff.  Sometimes, the freedom to share is what matters.

# Chapter 1: Building Guile

## Dependencies

Here I list some basic info about how Guile is built.  This is all
boring background, skip ahead.  But I wanted to get it written down
for reference.

To build Guile, one needs a C compiler and the fundamental C
library. Generally this means...
- [gcc](https://gcc.gnu.org/) usually, or possibly
  [clang](https://clang.llvm.org/)
- [glibc](https://www.gnu.org/software/libc/) usually
- an implementation of
  [pthreads](https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/pthread.h.html)
  which is usually provided by glibc

Guile expects to run on a computer that provides the
[POSIX](https://pubs.opengroup.org/onlinepubs/9699919799/) standard
operating system interface and environment.  Guile provides some
portability to Windows by using [Gnulib - the GNU Portability
Library](https://www.gnu.org/software/gnulib/), which provides stubs and
replacements for some POSIX functions that are not provided by the
operating system. These stubs are in the source tree under the 'lib'
directory. But these stubs are not always complete or correct
implementations of POSIX functionality on system that lack them.

Guile itself requires other libraries
- [GMP](https://gmplib.org/): the GNU Multiprecision Arithmetic Library
- [libffi](https://sourceware.org/libffi/): a portable foreign function
  interface library
- the [Boehm-Demers-Weiser conservative garbage
  collector](https://www.hboehm.info/gc/)
- [GNU gperf](https://www.gnu.org/software/gperf/): a perfect hash
  function generator
- the [GNU Readline
  Library](https://tiswww.cwru.edu/php/chet/readline/rltop.html):  a set
  of functions for use by applications that allow users to edit command
  lines as they are typed in
- [GNU
  libunistring](https://www.gnu.org/software/libunistring/manual/libunistring.html):
  a library that provides functions for manipulating Unicode strings and
  for manipulating C strings according to the Unicode standard
  
Then there are the dependencies necessary to run the Autotools build
system itself that Guile uses.
- a POSIX shell
- GNU make
- [autoconf](https://www.gnu.org/software/autoconf/)
- [automake](https://www.gnu.org/software/automake/)
- [libtool](https://www.gnu.org/software/libtool/)
- grep, sed, awk
- m4
- pkg-config
- probably many others I'm forgetting

In sum, this is big list of dependencies and requirements.  But on most
distros on Linux, all these programs and libraries are available or are
easily installable by the distro's package manager.

## Building on Windows

It is not possible to build Guile on Windows 10/11 with Microsoft's
native tools.  The native Windows build environment is either the Visual
Studio GUI or the Developer Command Prompt.  It does not have a POSIX
shell, which is fundamental to building with autotools.  The native
Windows build environment works well with
[MSBuild](https://learn.microsoft.com/en-us/visualstudio/msbuild/msbuild),
[CMake](https://cmake.org/), and
[NMake](https://learn.microsoft.com/en-us/cpp/build/reference/nmake-reference). [The
Meson Build system](https://mesonbuild.com/) also works.  But with no
POSIX shell, building with autotools is impossible.

On Windows, I instead used [MSys2](https://www.msys2.org/), a software
distribution and building platform for Windows, which has a package
manager that can install the required libraries and tools necessary to
build with autotools.

## Aside: Native, Cross-Build, Cygwin, MSys2 and WSL

Let's take a moment to talk about the many divergent ways that Linux
programs get compiled onto Windows, and why I chose MSys2.

[Cygwin](https://cygwin.com/) is Linux-like environment that runs on
Windows, and which has all the tools necessary to build Guile.  It
actually has Guile as a package. But if I were to build a graphical
program on Cygwin, it would run on X11 graphical protocol instead of
native Windows graphics.  So to make an installer, I'd need to bundle
the whole X11 graphical stack.  Also Cygwin graphics are a little slow.

[WSL](https://learn.microsoft.com/en-us/windows/wsl/), the Windows
Subsystem for Linux, lets one actually run a Linux distribution on
Windows.  But there is no easy way to make a simple installer of a
program using WSL.

MSys2 is a sort of compromise.  It does provide a `sh` shell, a package
manager that can install Guile's dependencies, and an environment that
can run autotools.  However, programs compiled in MSys2 are linked to a
subset of the Windows SDK, instead of to a POSIX emulation layer. So
they are somewhat native Windows apps compiled in a Linux-like
environment.

Lastly, many Linux distros have a set of cross-compilation tools that
can build Windows application on Linux machines.  But these cross
compilation tools can't get one all the way to MSIX package.  You can
get 99% of the way there, right up to the creation of the MSIX package,
but, then the package needs to be signed on Windows.  I have no good
reason why I went with MSys2 over a cross-compilation from
Linux. Compilation is slower on MSys2, but, testing is faster.

## Aside 2: UCRT vs MSVCRT

MSys2 can build programs using the older Windows C library (MSVCRT) or
the current one. The current one is the [Microsoft C Runtime
Library](https://learn.microsoft.com/en-us/cpp/c-runtime-library/c-run-time-library-reference)
sometimes referred to as UCRT.  The older one is poorly documented, a
strange mix of C89 and C99, and has no UTF-8 locales. The newer one is
fully documented, closer to C99 compilant, and is actually a documented,
published API.  There is no reason to use MSVCRT unless you need to run
on Windows 7.  It is not worth it.

## List of Bugs for Guile's x86_64 on MSys2

This is a list of bugs that exist for Guile on Windows that prevent it
from running correctly in MSys2. e.g. running Guile on Windows, but,
also running with MSys2's somewhat Linux-y shell.
Some of these have been patched in the
[`wip-mingw`](https://git.savannah.gnu.org/cgit/guile.git/log/?h=wip-mingw)
tree.  Some have patches elsewhere.

These "fundamental bugs" prevent Guile from running well even with a
bash shell prompt like MSys2 provides and a filesystem hierarchy similar
to Linux. Later, I discuss further patches necessary to make Guile run
well without MSys2 and with a different filesystem hierarchy.

| Name | Description | Bug ID | Patches / PRs |
|------|-------------|--------|---------------|
| Integer Sizes | 64-bit Windows uses the LLP64 integer model where `int` and `long` are 32-bits and pointers are 64-bits. Guile only supports LP64 model where `long` is 64-bits. | [22046](https://debbugs.gnu.org/cgi/bugreport.cgi?bug=22406) | [4](https://git.savannah.gnu.org/cgit/guile.git/commit/?h=wip-mingw&id=76950b4281c7dfff78b9ead6d3d62c070bbc1f13) [3](https://git.savannah.gnu.org/cgit/guile.git/commit/?h=wip-mingw&id=46bb667f203a842e2cc574054487cd05f4ce8977) [2](https://git.savannah.gnu.org/cgit/guile.git/commit/?h=wip-mingw&id=591e10a121bae5851a796dead0b2df97d08b628f) [1](https://git.savannah.gnu.org/cgit/guile.git/commit/?h=wip-mingw&id=9a29293a88bd884a160ba84f06b8323335e0b039)|
| missing sys/select.h | threads.c needs `#include <sys/select.h>` | | [1](https://git.savannah.gnu.org/cgit/guile.git/commit/?h=wip-mingw&id=558fe0113daf3e6a9b327539d7b3481b064bd180) |
| `gethostname` | Gnulib's gethostname module is needed | | [1](https://git.savannah.gnu.org/cgit/guile.git/commit/?h=wip-mingw&id=06391f1cb774647c1336a2d3b4a1e1604fc17768) |
| suspendable `read-bytes` | For socket reads, `read-bytes` always blocks when no data is available | | [1](https://git.savannah.gnu.org/cgit/guile.git/commit/?h=wip-mingw&id=fe4e19a22aa1807473e6e39119ece02df9aa56c7) |
| `pipe` errors | `pipe` can return a nonsense error message with flag arguments | | [1](https://git.savannah.gnu.org/cgit/guile.git/commit/?h=wip-mingw&id=3c9052b2614071c7af2a04dc1c9868e315c14e45) |
| CRLF support for `read-line` | `read-line` doesn't support the '\r\n' line terminator | | [1](https://git.savannah.gnu.org/cgit/guile.git/commit/?h=wip-mingw-guile-2.2&id=77b33170f4113c1d37f62c66a4807996187d2e24) [2](https://git.savannah.gnu.org/cgit/guile.git/commit/?h=wip-mingw-guile-2.2&id=cd2d5a2500d23f2cd4707011a0806f0b02536894) | 
| missing REPL prompt | On Windows 11, the REPL prompt is never printed because `char-ready?` misbehaves | | |
| locale errors | Some locale tests fail because of differences in currency handling | | |
| timezone | Windows uses `_tzset` to modify time zones, instead of time struct members | | |
| alarm and SIGALARM | Missing functionality | | |
| SIGPROF | missing functionality | | |
| suspendable ports' binary mode | errors exist in handling of port encoding | | |
| JIT | The 64-bit JIT architecture is not working on Windows | | |
| UTF-8 | There are many places where the 8-bit locales are used with the Win32 API calls | | |

# Chapter 2: Relocatability

Typically, the files associated with an installed Guile are spread over
several directories on a system in accordance with the Filesystem
Hierarchy Standard, and moving those files will break Guile.  `/usr/bin`
`/usr/lib` `/usr/share/guile` and the like.  When the guile executable
is run, it requires additional information stored in other folders:
scheme files, compiled scheme files, and shared libraries.

With some changes to core Guile, it can be made to use files installed
into a single directory, and then that directory can be moved higgledy
piggledy to hither and yon.  It is much easier to package a program in
MSIX, Flatpak, or Snap if one first puts all the Guile files in a
single, relocatable directory.

## Guile Directories

Let's imagine the we're making a relocatable Guile, and that staging
directory for that Guile is `/app`.  The staging directory is where it
is initially installed, with the understanding that that directory can
be moved anywhere later.  The following table compares a typical install
location with a possible location for a relocatable install.

| Dir Name            | Standard Path             | Absolute Standard Path   | Relocatable |
|---------------------|---------------------------|--------------------------|-------------|
| bindir              | `$(exec_prefix)/bin`      | /usr/bin                 | /app or /app/bin |
| pkgincludedir       | `$(includedir)/guile`     | /usr/include/guile       | /app/include/guile |
| SCM_PKGDATA_DIR     | `$(pkgdatadir)`           | /usr/share/guile         | /app/share/guile |
| SCM_LIBRARY_DIR     | `$(pkgdatadir)/$(VERSION)` | /usr/share/guile/3.0    | /app/share/guile/3.0 |
| SCM_SITE_DIR        | `$(sitedir)`              | /usr/share/guile/site/3.0 | /app/share/guile/site/3.0 |
| SCM_GLOBAL_SITE_DIR | `$(pkgdatadir)/site`      | /usr/share/guile/site    | /app/share/site |
| SCM_LIB_DIR         | `$(libdir)`               | /usr/lib                 | /app or /app/lib |
| SCM_EXTENSIONS_DIR  | `$(pkglibdir)/$(VERSION)/extensions` | /usr/lib/guile/3.0/extensions | /app/lib/guile/3.0/extensions |
| SCM_CCACHE_DIR      | `$(pkglibdir)/$(VERSION)/ccache` | /usr/lib/guile/3.0/ccache | /app/lib/guile/3.0/ccache |
| SCM_SITE_CCACHE_DIR | `$(pkglibdir)/$(VERSION)/site-ccache` | /usr/lib/guile/3.0/site-ccache | /app/lib/guile/3.0/site-ccache |

One thing to note is that typically in an app, bindir and libdir are in
the root of the staging directory `/app` rather than `/app/bin` and
`/app/lib`. That's what we'll be doing.  For a given Guile, the initial
value of these directories are set when the build is first configured
using `./configure`.

When Guile launches, there are different search mechanisms.
1. The operating system helps the Guile executable find the shared
   libraries dependencies and launch: libffi, libgmp, libunistring, etc.
2. Once launched, Guile finds the compiled scheme files using its
   internal logic

### Library search path in Linux

Run-time linking is handled by `ld.so`.  The search strategy, described
[here](https://www.man7.org/linux/man-pages/man8/ld.so.8.html) for
example, is essentially
- directories in the environment variable `LD_LIBRARY_PATH`
- then, directories in the `DT_RUN_PATH` property of the executable
- then, default directories such as `/lib` and `/usr/lib`

Since I'm avoiding the environment variable strategy, what remains is to
set `DT_RUN_PATH` in the executable.  As well as make sure the `NEEDED`
is correctly set to the required shared objects.  Use `readelf -d` to
see how these are set in a given executable.

The DT_RUN_PATH property of an exectuable can be set with

```
patchelf --set-rpath '$ORIGIN' guile
```

The `$ORIGIN` is ELF shorthand to say that you are searching in the
directory where the executable resided when it was launched.

This way, you can put all the shared libraries in the '/app' directory
that aren't already present in '/usr/lib'

### Library search path in Windows

On Windows, the search is different. For UWP, in this article on
[dynamic link library search
order](https://learn.microsoft.com/en-us/windows/win32/dlls/dynamic-link-library-search-order)
describes how the OS searches for libraries.
1. A directory listed in the application package manifest
2. The directory where the executable resides
3. C:\Windows\System32

Of the three options, #2 requires the least effort. Just need to make
sure that libguile.dll and guile.exe are in the same directory as well
as the dependencies.  This is very common for Windows apps.

### Guile's search algorithm for Scheme files

For a given run,
they can be modified by setting environment variables like
`GUILE_LOAD_PATH` and `GUILE_LOAD_COMPILED_PATH` before Guile is
launched.  This is fine strategy for relocatability if Guile can be
launched by a shell script or batch file.  On Windows apps, however,
shell scripts and batch files require elevated security permissions,
which violates my rules of making a Dad-friendly install. So for my
relocatable Guile, these relative directories need to be baked into
Guile itself without requiring any environment variable tweaks.

## Cache Directory

At run-time Guile writes to cache directory where it stores scheme files
that are compiled JIT.  The different packaging standards have differing
ideas about where the cache directory should be.  Flatpak conventions
are described
[here](https://docs.flatpak.org/en/latest/conventions.html).


| Toolkit | Base directory | Usage	 |Default location|
|-|-|-|-|
|Flatpak | XDG_CONFIG_HOME | User-specific configuration files | `~/.var/app/<app-id>/config` |
| | XDG_DATA_HOME | User-specific data | `~/.var/app/<app-id>/data` |
| | XDG_CACHE_HOME | Non-essential user-specific data	| `~/.var/app/<app-id>/cache` |
| | XDG_STATE_HOME |	State data such as undo history	| `~/.var/app/<app-id>/.local/state` |
| Snap | SNAP_USER_DATA |  per-user writable area for a particular revision of the snap | `~/snap/<snap name>/<revision>`  |
| MSIX | StorageFolder | | |
|      | RoamingFolder | | |
|      | TemporaryFolder | | |
  

## Feature/Bug List for Relocatable Guile

| Name | Description | Bug ID | Patches / PRs |
|------|-------------|--------|---------------|
| Install Directories | Modify the build so that it installs all the build products into the `/app` directory | | |
| Shared Object search | For Linux, use patchelf or some other strategy to set the RUNPATH in the Guile executable | | |
| Launch directory | Add code to Guile that figures out the directory from which Guile is launched | | |
| SCM and GO file search | Modify Guile so that it searches for SCM and GO files in directories relative to the executable | | |
| Cache directory | Change the cache directory to the appropriate one for the packaging scheme | | |
| Libtool install bug |  This is because when libtool is used to install a dll, it puts it in `$libdir/../bin` and not `$bindir`.  These will not be the same if `$libdir` and `$bindir` are set to the same directory by configure.  So if we're going to use libtool to install the executables into the same directory in a relocatable directory you can't put the executable in the root directory, like you'd expect for a windows UWP program | | |

At this point, we have a relocatable app.

# Chapter 3: Launching

In my concept, I'm not distributing Guile for Guile's sake, but, rather
distributing a game that uses Guile.  So I want the user to click on a
menu item or icon and have it launch the executable.

On Linux, it would make sense that the launcher would be a shell script
that calls Guile, passing it the correct command-line arguments.  But on
Windows, shells scripts require elevation to run, which is not in my
conops.  That requires two more modifications.

1. Guile needs a functionality to read its startup options from a file.
   A couple of options are `.guile` and `init.scm`.  I'll have to see if
   they work. I think I can just put init.scm at the top of the sitedir,
   and it will launch my game.  I'll have to try that out.
2. If for some reason the game needs `guild` or `guild-config` after
   packaging, these will have to become native programs, not shell scripts.

Need to ensure that one can rename `guile` or `guile.exe` to the name of
the executable and have it still work.

# Chapter 4: Packaging to MSIX

Now here is where it all gets worse.

## Figuring out the required DLLs

Since the idea is to explicitly *not* be running the application under
MSys2, the package will need to copy all of the DLLs into the libdir.
The easiest way to get this list is to use the
[Dependencies](https://github.com/lucasg/Dependencies) tool.

Dependencies provides this list
- libffi-8.dll
- libgc-1.dll
- libgcc_s_seh-1.dll
- libgmp-10.dll
- libiconv-2.dll
- libwinpthread-1.dll
- libunistring-2.dll

As well as a slew of dlls that are part of Windows itself.

## Icons

MSIX needs a logo and a small logo.  Both are one of is a 'jpg' 'jpeg'
or 'png'.  SmallLogo is 30x30. Logo is 150x150. Other sizes are
possible.  See
[this](https://learn.microsoft.com/en-us/uwp/schemas/appxpackage/appxmanifestschema/element-visualelements)

Is that the right reference? Looks like Windows 8.

## Metadata

### Permissions

## Building

## Signing

# Chapter 4: Packaging to Flatpak

## Figuring out the required libraries and runtime

## Icons

Flatpak wants icons in accordance with the [icon theme
spec](https://specifications.freedesktop.org/icon-theme-spec/icon-theme-spec-latest.html)
The icon theme spec says, at a minimum, there should be a 48x48 pixel
PNG and a scalable SVG icon in the default 'hicolor' theme directory.

```
/app/share/icons/hicolor/48x48/apps/com.lonelycactus.Foogame.png
/app/share/icons/hicolor/scalable/apps/com.lonelycactus.Foogame.svg
```
Replace "com.lonelycactus.Foogame" with your application ID.

## Metadata

### Permissions

## Building

## Signing

# Chapter 4: Packaging to Snap

## Figuring out the required libraries and runtime

## Icons

## Metadata

### Permissions

## Building

## Signing
