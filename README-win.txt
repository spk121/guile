README-win.txt : notes on getting this fork to build, run, and package
on Windows 10/11

This fork was motivated somewhat by the Lisp Game Jam.  I wanted a way
to deliver a guile-based game as a simple zip or msix file, so that it
is easy for someone else to just download and run it.

Alternatively, you can see this document as an object lesson why Guix
exists, and what problems it is trying to solve.

-----------------------------------

CHANGES FROM CORE GUILE

My Guile is a fork from core Guile to add features necessary to
distribute a Guile app on Windows.  These are some of the changes.

Almost all instances of the long integer are replaced with intptr_t,
since on 64-bit Windows, sizeof(long) < sizeof(void *), in violation of
Guile's expectations.

Since the presence of a POSIX shell is not guaranteed, the guild script
may not be usable. A binary executable version of guild, called
guile-tools, is added.  Previously, guile-tools was an alias to guild
but now it is a compiled program.

Since unpacking zip or MSIX files may not preserve timestamps, unpacked
scheme files may have more recent file timestamps than their associated
compiled scheme (*.go) files, causing a recompilation.  A feature is
added such that if a file named FINAL is present in ccache-dir or
site-ccache-dir, the files therein are not recompiled when they have
file timestamps older than their associated scheme files.

On Windows, the fallback directory is set to a more canonical Windows
app directory

  <HOME>\AppData\Local\guile

rather than

  <HOME>\AppData\Local\.cache\guile

On Windows, before a binary extension is loaded, the $libdir and
$extensiondir paths are added to the DLL search directories.  Also the
paths in the LTDL_LIBRARY_PATH and SCM_EXTENSIONS_PATH environment
variables are added to the DLL search directories.  This helps improve
the problem with a DLL failing to load because it depends on a DLL in a
directory that has not yet been added to the DLL search directories.

On Windows, there is a new function add-dll-search-directories.

On Windows, UTF-8 locales are used and tested.  When using the UCRT
library instead of the deprecated MSVCRT libary, guile on Windows has
some UTF-8 support.

read-line now handles the alternate line endings CRLF, CR, LS and PS.
To handle CRLF, read-line may return a string line terminator.  I guess
theoretically NEL is also a line terminator, but, I ignored that.

The HTTP read-header-line is simplfied to take advantage of CRLF support
in read-line.

To help deliver a game as a single zip or tar file, a new configure
option --enable-relative-paths is added.  This installs all of guile
into the $(prefix)/app directory, and modifies the loading logic to
search for scheme, compiled scheme, and extension files relative to the
location of the executable.  The applications and all shared object
library files are in the root of that directory.  $(prefix)/app is just
a staging directory, and that directory can be renamed and relocated.

By default, cmd and powershell scripts require security elevation on
Windows.  As a workaround to avoid having to make a batch file to launch
a game, if guile is called without any command line arguments, it now
searches for a file named "cmdargs.txt" in the current working
directory.  When present, it will be used as command line arguments for
executing guile.  The purpose of this is to allow someone to just
double-click on the Guile executable to launch an app without requiring
a shell script.

In "cmdargs.txt", a line that begins with '#' in the first column is
treated as a comment.  If you need a command-line argument that begins
with a '#', begin the line with a space or tab.  The first non-comment
line of "command-args.txt" is stripped of initial and terminal
whitespace and is used as the first command-line argument (argv[1]).
The 2nd non-comment line is the 2nd command-line argument (argv[2]),
etc.  If the file does not end with newline, the last line is ignored.

An appxmanifest.xml file and some icons are added for the benefit of
MSIX packaging.

=====================
BUILDING FOR WINDOWS
=====================

Here I describe how to build a relocatable Guile on Windows.

1. Install GUILE

This requires Msys64's UCRT64 build toolchain.  That is GCC but using
the UCRT C library.

The preferred configure call is

  ./configure 'CFLAGS=-g -O1 -Wall' --enable-mini-gmp
    --enable-jit=no --disable-lto --enable-relative-paths

'make install' will install everything into $(prefix)/app

For Windows, after install, you need to add the following files into the
$(prefix)/app directory before packaging.  These are taken from
C:\msys64\ucrt64\bin or a similar location on your MSys64 distro

libffi-8.dll
libgc-1.dll
libgcc_s_seh-1.dll
libiconv-2.dll
libintl-8.dll
libunistring-2.dll
libwinpthread-1.dll

You should already have the following as a result from the build.
libguile-3.0-1.dll

If you are going to deliver your app as a zip or msix, add a file named
FINAL to $(prefix)/app/lib/guile/3.0/ccache to prevent recompilation.

2. Install libraries

After Guile is installed, you'll need to build and install your
dependencies into that same directory, so if you rebuild SDL bindings or
Chickadee from source, add the environment variable

  PKG_CONFIG_PATH=XXX/app/lib/pkgconfig:$(PKG_CONFIG_PATH)

but replace XXX/app with wherever you've installed your relocatable
Guile.

in your configure, you may also need to set the GUILE and GUILD
variables so the ones in the relocatable directory are used, instead of
ones in the standard install.

3. Install the game code and assets

The game code and assets also should go into the same directory.  The
game code probably goes in share/guile/site/3.0.

When you compile the game code, the *.go files probably go into the site
ccache directory

4. Set up the command line and rename Guile.

After that, add your "cmdargs.txt" file if necessary.

And you can rename guile.exe to the name of your game so a user can just
double-click on the executable to launch

=====================
PACKAGING
=====================

If you want to deliver a compiled Guile as a zip file, it should be
simple.  If everything went according to plan, just zip up the app
directory and you should be good.  You should stop here unless you are a
crazy person.  Tell people to download your zip, unpack, and
double-click your renamed guile.exe.  Success: game jam achieved!

Note that if you include GPL or AGPL code, you have a responsibility to
inform the user of how to get and build from source.

If you want to deliver an MSIX so the program can be installed
canonically on Windows 11, it gets worse.  Much worse.  Really just
TL;DR this and stop now.

First, to make an MSIX that other people can actually install, you need
an OV Code Signing Certificate that is valid according to Microsoft.
This requires a third party security company, an application process
that might require photographs, showing state identification and other
such nonsense.  Also, getting a CS Cert costs between $60 to $250 a
year.

Alternatively, if you can set yourself up as a developer who can put
something on the Windows Store, you can use their CS Cert.  Or if you
have an Azure account, they have a CS Cert you can use, I think.

But, as it turns out, I have a OV CS Cert.

Once you get a cert, you need to merge your public cert and private key
into a pfx file.  Hopefully your provider will get you a pfx, but, I had
to merge my CRT and my private key into one file by appending my private
key to the CRT.  From then I made a PFX by using

certutil -mergepfx mycert.crt mycert.pfx

Next up, edit the appxmanifest.xml file in the build-aux directory and
save it in the 'app' directory.  Specifically you need to change

Identity -> Name
Identity -> Publisher
Properties -> DisplayName
Applications -> Application -> Id
Applications -> Application -> Executable
Applications -> Application -> Extensions -> uap5:Extension -> uap5:Execution Alias

Then go one directory above 'app'.  From there run

  makeappx pack /h sha384 /d .\app\ /p guile.msix

Note that the sha size needs to match the one associated with your OV CS
Cert

This will result in an appx package, which you can install and run
locally, but, which can't be run on another account without some scary
security messages.

Then the appx can be signed and converted into an msix.

  signtool sign /a /v /fd certhash guile.msix

This final guile.msix can be installed, shared, uploaded to the MS
Store, and all that jazz.

