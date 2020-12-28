# 1 "weak-vector.c"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/usr/include/stdc-predef.h" 1 3 4
# 1 "<command-line>" 2
# 1 "weak-vector.c"
# 26 "weak-vector.c"
# 1 "/usr/include/stdio.h" 1 3 4
# 27 "/usr/include/stdio.h" 3 4
# 1 "/usr/include/bits/libc-header-start.h" 1 3 4
# 33 "/usr/include/bits/libc-header-start.h" 3 4
# 1 "/usr/include/features.h" 1 3 4
# 450 "/usr/include/features.h" 3 4
# 1 "/usr/include/sys/cdefs.h" 1 3 4
# 452 "/usr/include/sys/cdefs.h" 3 4
# 1 "/usr/include/bits/wordsize.h" 1 3 4
# 453 "/usr/include/sys/cdefs.h" 2 3 4
# 1 "/usr/include/bits/long-double.h" 1 3 4
# 454 "/usr/include/sys/cdefs.h" 2 3 4
# 451 "/usr/include/features.h" 2 3 4
# 474 "/usr/include/features.h" 3 4
# 1 "/usr/include/gnu/stubs.h" 1 3 4
# 10 "/usr/include/gnu/stubs.h" 3 4
# 1 "/usr/include/gnu/stubs-64.h" 1 3 4
# 11 "/usr/include/gnu/stubs.h" 2 3 4
# 475 "/usr/include/features.h" 2 3 4
# 34 "/usr/include/bits/libc-header-start.h" 2 3 4
# 28 "/usr/include/stdio.h" 2 3 4





# 1 "/usr/lib/gcc/x86_64-redhat-linux/9/include/stddef.h" 1 3 4
# 209 "/usr/lib/gcc/x86_64-redhat-linux/9/include/stddef.h" 3 4

# 209 "/usr/lib/gcc/x86_64-redhat-linux/9/include/stddef.h" 3 4
typedef long unsigned int size_t;
# 34 "/usr/include/stdio.h" 2 3 4


# 1 "/usr/lib/gcc/x86_64-redhat-linux/9/include/stdarg.h" 1 3 4
# 40 "/usr/lib/gcc/x86_64-redhat-linux/9/include/stdarg.h" 3 4
typedef __builtin_va_list __gnuc_va_list;
# 37 "/usr/include/stdio.h" 2 3 4

# 1 "/usr/include/bits/types.h" 1 3 4
# 27 "/usr/include/bits/types.h" 3 4
# 1 "/usr/include/bits/wordsize.h" 1 3 4
# 28 "/usr/include/bits/types.h" 2 3 4
# 1 "/usr/include/bits/timesize.h" 1 3 4
# 29 "/usr/include/bits/types.h" 2 3 4


typedef unsigned char __u_char;
typedef unsigned short int __u_short;
typedef unsigned int __u_int;
typedef unsigned long int __u_long;


typedef signed char __int8_t;
typedef unsigned char __uint8_t;
typedef signed short int __int16_t;
typedef unsigned short int __uint16_t;
typedef signed int __int32_t;
typedef unsigned int __uint32_t;

typedef signed long int __int64_t;
typedef unsigned long int __uint64_t;






typedef __int8_t __int_least8_t;
typedef __uint8_t __uint_least8_t;
typedef __int16_t __int_least16_t;
typedef __uint16_t __uint_least16_t;
typedef __int32_t __int_least32_t;
typedef __uint32_t __uint_least32_t;
typedef __int64_t __int_least64_t;
typedef __uint64_t __uint_least64_t;



typedef long int __quad_t;
typedef unsigned long int __u_quad_t;







typedef long int __intmax_t;
typedef unsigned long int __uintmax_t;
# 141 "/usr/include/bits/types.h" 3 4
# 1 "/usr/include/bits/typesizes.h" 1 3 4
# 142 "/usr/include/bits/types.h" 2 3 4
# 1 "/usr/include/bits/time64.h" 1 3 4
# 143 "/usr/include/bits/types.h" 2 3 4


typedef unsigned long int __dev_t;
typedef unsigned int __uid_t;
typedef unsigned int __gid_t;
typedef unsigned long int __ino_t;
typedef unsigned long int __ino64_t;
typedef unsigned int __mode_t;
typedef unsigned long int __nlink_t;
typedef long int __off_t;
typedef long int __off64_t;
typedef int __pid_t;
typedef struct { int __val[2]; } __fsid_t;
typedef long int __clock_t;
typedef unsigned long int __rlim_t;
typedef unsigned long int __rlim64_t;
typedef unsigned int __id_t;
typedef long int __time_t;
typedef unsigned int __useconds_t;
typedef long int __suseconds_t;

typedef int __daddr_t;
typedef int __key_t;


typedef int __clockid_t;


typedef void * __timer_t;


typedef long int __blksize_t;




typedef long int __blkcnt_t;
typedef long int __blkcnt64_t;


typedef unsigned long int __fsblkcnt_t;
typedef unsigned long int __fsblkcnt64_t;


typedef unsigned long int __fsfilcnt_t;
typedef unsigned long int __fsfilcnt64_t;


typedef long int __fsword_t;

typedef long int __ssize_t;


typedef long int __syscall_slong_t;

typedef unsigned long int __syscall_ulong_t;



typedef __off64_t __loff_t;
typedef char *__caddr_t;


typedef long int __intptr_t;


typedef unsigned int __socklen_t;




typedef int __sig_atomic_t;
# 39 "/usr/include/stdio.h" 2 3 4
# 1 "/usr/include/bits/types/__fpos_t.h" 1 3 4




# 1 "/usr/include/bits/types/__mbstate_t.h" 1 3 4
# 13 "/usr/include/bits/types/__mbstate_t.h" 3 4
typedef struct
{
  int __count;
  union
  {
    unsigned int __wch;
    char __wchb[4];
  } __value;
} __mbstate_t;
# 6 "/usr/include/bits/types/__fpos_t.h" 2 3 4




typedef struct _G_fpos_t
{
  __off_t __pos;
  __mbstate_t __state;
} __fpos_t;
# 40 "/usr/include/stdio.h" 2 3 4
# 1 "/usr/include/bits/types/__fpos64_t.h" 1 3 4
# 10 "/usr/include/bits/types/__fpos64_t.h" 3 4
typedef struct _G_fpos64_t
{
  __off64_t __pos;
  __mbstate_t __state;
} __fpos64_t;
# 41 "/usr/include/stdio.h" 2 3 4
# 1 "/usr/include/bits/types/__FILE.h" 1 3 4



struct _IO_FILE;
typedef struct _IO_FILE __FILE;
# 42 "/usr/include/stdio.h" 2 3 4
# 1 "/usr/include/bits/types/FILE.h" 1 3 4



struct _IO_FILE;


typedef struct _IO_FILE FILE;
# 43 "/usr/include/stdio.h" 2 3 4
# 1 "/usr/include/bits/types/struct_FILE.h" 1 3 4
# 35 "/usr/include/bits/types/struct_FILE.h" 3 4
struct _IO_FILE;
struct _IO_marker;
struct _IO_codecvt;
struct _IO_wide_data;




typedef void _IO_lock_t;





struct _IO_FILE
{
  int _flags;


  char *_IO_read_ptr;
  char *_IO_read_end;
  char *_IO_read_base;
  char *_IO_write_base;
  char *_IO_write_ptr;
  char *_IO_write_end;
  char *_IO_buf_base;
  char *_IO_buf_end;


  char *_IO_save_base;
  char *_IO_backup_base;
  char *_IO_save_end;

  struct _IO_marker *_markers;

  struct _IO_FILE *_chain;

  int _fileno;
  int _flags2;
  __off_t _old_offset;


  unsigned short _cur_column;
  signed char _vtable_offset;
  char _shortbuf[1];

  _IO_lock_t *_lock;







  __off64_t _offset;

  struct _IO_codecvt *_codecvt;
  struct _IO_wide_data *_wide_data;
  struct _IO_FILE *_freeres_list;
  void *_freeres_buf;
  size_t __pad5;
  int _mode;

  char _unused2[15 * sizeof (int) - 4 * sizeof (void *) - sizeof (size_t)];
};
# 44 "/usr/include/stdio.h" 2 3 4
# 52 "/usr/include/stdio.h" 3 4
typedef __gnuc_va_list va_list;
# 63 "/usr/include/stdio.h" 3 4
typedef __off_t off_t;
# 77 "/usr/include/stdio.h" 3 4
typedef __ssize_t ssize_t;






typedef __fpos_t fpos_t;
# 133 "/usr/include/stdio.h" 3 4
# 1 "/usr/include/bits/stdio_lim.h" 1 3 4
# 134 "/usr/include/stdio.h" 2 3 4



extern FILE *stdin;
extern FILE *stdout;
extern FILE *stderr;






extern int remove (const char *__filename) __attribute__ ((__nothrow__ , __leaf__));

extern int rename (const char *__old, const char *__new) __attribute__ ((__nothrow__ , __leaf__));



extern int renameat (int __oldfd, const char *__old, int __newfd,
       const char *__new) __attribute__ ((__nothrow__ , __leaf__));
# 173 "/usr/include/stdio.h" 3 4
extern FILE *tmpfile (void) ;
# 187 "/usr/include/stdio.h" 3 4
extern char *tmpnam (char *__s) __attribute__ ((__nothrow__ , __leaf__)) ;




extern char *tmpnam_r (char *__s) __attribute__ ((__nothrow__ , __leaf__)) ;
# 204 "/usr/include/stdio.h" 3 4
extern char *tempnam (const char *__dir, const char *__pfx)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__malloc__)) ;







extern int fclose (FILE *__stream);




extern int fflush (FILE *__stream);
# 227 "/usr/include/stdio.h" 3 4
extern int fflush_unlocked (FILE *__stream);
# 246 "/usr/include/stdio.h" 3 4
extern FILE *fopen (const char *__restrict __filename,
      const char *__restrict __modes) ;




extern FILE *freopen (const char *__restrict __filename,
        const char *__restrict __modes,
        FILE *__restrict __stream) ;
# 279 "/usr/include/stdio.h" 3 4
extern FILE *fdopen (int __fd, const char *__modes) __attribute__ ((__nothrow__ , __leaf__)) ;
# 292 "/usr/include/stdio.h" 3 4
extern FILE *fmemopen (void *__s, size_t __len, const char *__modes)
  __attribute__ ((__nothrow__ , __leaf__)) ;




extern FILE *open_memstream (char **__bufloc, size_t *__sizeloc) __attribute__ ((__nothrow__ , __leaf__)) ;





extern void setbuf (FILE *__restrict __stream, char *__restrict __buf) __attribute__ ((__nothrow__ , __leaf__));



extern int setvbuf (FILE *__restrict __stream, char *__restrict __buf,
      int __modes, size_t __n) __attribute__ ((__nothrow__ , __leaf__));




extern void setbuffer (FILE *__restrict __stream, char *__restrict __buf,
         size_t __size) __attribute__ ((__nothrow__ , __leaf__));


extern void setlinebuf (FILE *__stream) __attribute__ ((__nothrow__ , __leaf__));







extern int fprintf (FILE *__restrict __stream,
      const char *__restrict __format, ...);




extern int printf (const char *__restrict __format, ...);

extern int sprintf (char *__restrict __s,
      const char *__restrict __format, ...) __attribute__ ((__nothrow__));





extern int vfprintf (FILE *__restrict __s, const char *__restrict __format,
       __gnuc_va_list __arg);




extern int vprintf (const char *__restrict __format, __gnuc_va_list __arg);

extern int vsprintf (char *__restrict __s, const char *__restrict __format,
       __gnuc_va_list __arg) __attribute__ ((__nothrow__));



extern int snprintf (char *__restrict __s, size_t __maxlen,
       const char *__restrict __format, ...)
     __attribute__ ((__nothrow__)) __attribute__ ((__format__ (__printf__, 3, 4)));

extern int vsnprintf (char *__restrict __s, size_t __maxlen,
        const char *__restrict __format, __gnuc_va_list __arg)
     __attribute__ ((__nothrow__)) __attribute__ ((__format__ (__printf__, 3, 0)));
# 379 "/usr/include/stdio.h" 3 4
extern int vdprintf (int __fd, const char *__restrict __fmt,
       __gnuc_va_list __arg)
     __attribute__ ((__format__ (__printf__, 2, 0)));
extern int dprintf (int __fd, const char *__restrict __fmt, ...)
     __attribute__ ((__format__ (__printf__, 2, 3)));







extern int fscanf (FILE *__restrict __stream,
     const char *__restrict __format, ...) ;




extern int scanf (const char *__restrict __format, ...) ;

extern int sscanf (const char *__restrict __s,
     const char *__restrict __format, ...) __attribute__ ((__nothrow__ , __leaf__));






extern int fscanf (FILE *__restrict __stream, const char *__restrict __format, ...) __asm__ ("" "__isoc99_fscanf")

                               ;
extern int scanf (const char *__restrict __format, ...) __asm__ ("" "__isoc99_scanf")
                              ;
extern int sscanf (const char *__restrict __s, const char *__restrict __format, ...) __asm__ ("" "__isoc99_sscanf") __attribute__ ((__nothrow__ , __leaf__))

                      ;
# 432 "/usr/include/stdio.h" 3 4
extern int vfscanf (FILE *__restrict __s, const char *__restrict __format,
      __gnuc_va_list __arg)
     __attribute__ ((__format__ (__scanf__, 2, 0))) ;





extern int vscanf (const char *__restrict __format, __gnuc_va_list __arg)
     __attribute__ ((__format__ (__scanf__, 1, 0))) ;


extern int vsscanf (const char *__restrict __s,
      const char *__restrict __format, __gnuc_va_list __arg)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__format__ (__scanf__, 2, 0)));




extern int vfscanf (FILE *__restrict __s, const char *__restrict __format, __gnuc_va_list __arg) __asm__ ("" "__isoc99_vfscanf")



     __attribute__ ((__format__ (__scanf__, 2, 0))) ;
extern int vscanf (const char *__restrict __format, __gnuc_va_list __arg) __asm__ ("" "__isoc99_vscanf")

     __attribute__ ((__format__ (__scanf__, 1, 0))) ;
extern int vsscanf (const char *__restrict __s, const char *__restrict __format, __gnuc_va_list __arg) __asm__ ("" "__isoc99_vsscanf") __attribute__ ((__nothrow__ , __leaf__))



     __attribute__ ((__format__ (__scanf__, 2, 0)));
# 485 "/usr/include/stdio.h" 3 4
extern int fgetc (FILE *__stream);
extern int getc (FILE *__stream);





extern int getchar (void);






extern int getc_unlocked (FILE *__stream);
extern int getchar_unlocked (void);
# 510 "/usr/include/stdio.h" 3 4
extern int fgetc_unlocked (FILE *__stream);
# 521 "/usr/include/stdio.h" 3 4
extern int fputc (int __c, FILE *__stream);
extern int putc (int __c, FILE *__stream);





extern int putchar (int __c);
# 537 "/usr/include/stdio.h" 3 4
extern int fputc_unlocked (int __c, FILE *__stream);







extern int putc_unlocked (int __c, FILE *__stream);
extern int putchar_unlocked (int __c);






extern int getw (FILE *__stream);


extern int putw (int __w, FILE *__stream);







extern char *fgets (char *__restrict __s, int __n, FILE *__restrict __stream)
     ;
# 603 "/usr/include/stdio.h" 3 4
extern __ssize_t __getdelim (char **__restrict __lineptr,
                             size_t *__restrict __n, int __delimiter,
                             FILE *__restrict __stream) ;
extern __ssize_t getdelim (char **__restrict __lineptr,
                           size_t *__restrict __n, int __delimiter,
                           FILE *__restrict __stream) ;







extern __ssize_t getline (char **__restrict __lineptr,
                          size_t *__restrict __n,
                          FILE *__restrict __stream) ;







extern int fputs (const char *__restrict __s, FILE *__restrict __stream);





extern int puts (const char *__s);






extern int ungetc (int __c, FILE *__stream);






extern size_t fread (void *__restrict __ptr, size_t __size,
       size_t __n, FILE *__restrict __stream) ;




extern size_t fwrite (const void *__restrict __ptr, size_t __size,
        size_t __n, FILE *__restrict __s);
# 673 "/usr/include/stdio.h" 3 4
extern size_t fread_unlocked (void *__restrict __ptr, size_t __size,
         size_t __n, FILE *__restrict __stream) ;
extern size_t fwrite_unlocked (const void *__restrict __ptr, size_t __size,
          size_t __n, FILE *__restrict __stream);







extern int fseek (FILE *__stream, long int __off, int __whence);




extern long int ftell (FILE *__stream) ;




extern void rewind (FILE *__stream);
# 707 "/usr/include/stdio.h" 3 4
extern int fseeko (FILE *__stream, __off_t __off, int __whence);




extern __off_t ftello (FILE *__stream) ;
# 731 "/usr/include/stdio.h" 3 4
extern int fgetpos (FILE *__restrict __stream, fpos_t *__restrict __pos);




extern int fsetpos (FILE *__stream, const fpos_t *__pos);
# 757 "/usr/include/stdio.h" 3 4
extern void clearerr (FILE *__stream) __attribute__ ((__nothrow__ , __leaf__));

extern int feof (FILE *__stream) __attribute__ ((__nothrow__ , __leaf__)) ;

extern int ferror (FILE *__stream) __attribute__ ((__nothrow__ , __leaf__)) ;



extern void clearerr_unlocked (FILE *__stream) __attribute__ ((__nothrow__ , __leaf__));
extern int feof_unlocked (FILE *__stream) __attribute__ ((__nothrow__ , __leaf__)) ;
extern int ferror_unlocked (FILE *__stream) __attribute__ ((__nothrow__ , __leaf__)) ;







extern void perror (const char *__s);





# 1 "/usr/include/bits/sys_errlist.h" 1 3 4
# 26 "/usr/include/bits/sys_errlist.h" 3 4
extern int sys_nerr;
extern const char *const sys_errlist[];
# 782 "/usr/include/stdio.h" 2 3 4




extern int fileno (FILE *__stream) __attribute__ ((__nothrow__ , __leaf__)) ;




extern int fileno_unlocked (FILE *__stream) __attribute__ ((__nothrow__ , __leaf__)) ;
# 800 "/usr/include/stdio.h" 3 4
extern FILE *popen (const char *__command, const char *__modes) ;





extern int pclose (FILE *__stream);





extern char *ctermid (char *__s) __attribute__ ((__nothrow__ , __leaf__));
# 840 "/usr/include/stdio.h" 3 4
extern void flockfile (FILE *__stream) __attribute__ ((__nothrow__ , __leaf__));



extern int ftrylockfile (FILE *__stream) __attribute__ ((__nothrow__ , __leaf__)) ;


extern void funlockfile (FILE *__stream) __attribute__ ((__nothrow__ , __leaf__));
# 858 "/usr/include/stdio.h" 3 4
extern int __uflow (FILE *);
extern int __overflow (FILE *, int);
# 873 "/usr/include/stdio.h" 3 4

# 27 "weak-vector.c" 2
# 1 "/usr/include/string.h" 1 3 4
# 26 "/usr/include/string.h" 3 4
# 1 "/usr/include/bits/libc-header-start.h" 1 3 4
# 27 "/usr/include/string.h" 2 3 4






# 1 "/usr/lib/gcc/x86_64-redhat-linux/9/include/stddef.h" 1 3 4
# 34 "/usr/include/string.h" 2 3 4
# 43 "/usr/include/string.h" 3 4
extern void *memcpy (void *__restrict __dest, const void *__restrict __src,
       size_t __n) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));


extern void *memmove (void *__dest, const void *__src, size_t __n)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));





extern void *memccpy (void *__restrict __dest, const void *__restrict __src,
        int __c, size_t __n)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));




extern void *memset (void *__s, int __c, size_t __n) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));


extern int memcmp (const void *__s1, const void *__s2, size_t __n)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));
# 91 "/usr/include/string.h" 3 4
extern void *memchr (const void *__s, int __c, size_t __n)
      __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1)));
# 122 "/usr/include/string.h" 3 4
extern char *strcpy (char *__restrict __dest, const char *__restrict __src)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));

extern char *strncpy (char *__restrict __dest,
        const char *__restrict __src, size_t __n)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));


extern char *strcat (char *__restrict __dest, const char *__restrict __src)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));

extern char *strncat (char *__restrict __dest, const char *__restrict __src,
        size_t __n) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));


extern int strcmp (const char *__s1, const char *__s2)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));

extern int strncmp (const char *__s1, const char *__s2, size_t __n)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));


extern int strcoll (const char *__s1, const char *__s2)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));

extern size_t strxfrm (char *__restrict __dest,
         const char *__restrict __src, size_t __n)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (2)));



# 1 "/usr/include/bits/types/locale_t.h" 1 3 4
# 22 "/usr/include/bits/types/locale_t.h" 3 4
# 1 "/usr/include/bits/types/__locale_t.h" 1 3 4
# 28 "/usr/include/bits/types/__locale_t.h" 3 4
struct __locale_struct
{

  struct __locale_data *__locales[13];


  const unsigned short int *__ctype_b;
  const int *__ctype_tolower;
  const int *__ctype_toupper;


  const char *__names[13];
};

typedef struct __locale_struct *__locale_t;
# 23 "/usr/include/bits/types/locale_t.h" 2 3 4

typedef __locale_t locale_t;
# 154 "/usr/include/string.h" 2 3 4


extern int strcoll_l (const char *__s1, const char *__s2, locale_t __l)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2, 3)));


extern size_t strxfrm_l (char *__dest, const char *__src, size_t __n,
    locale_t __l) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (2, 4)));





extern char *strdup (const char *__s)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__malloc__)) __attribute__ ((__nonnull__ (1)));






extern char *strndup (const char *__string, size_t __n)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__malloc__)) __attribute__ ((__nonnull__ (1)));
# 226 "/usr/include/string.h" 3 4
extern char *strchr (const char *__s, int __c)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1)));
# 253 "/usr/include/string.h" 3 4
extern char *strrchr (const char *__s, int __c)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1)));
# 273 "/usr/include/string.h" 3 4
extern size_t strcspn (const char *__s, const char *__reject)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));


extern size_t strspn (const char *__s, const char *__accept)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));
# 303 "/usr/include/string.h" 3 4
extern char *strpbrk (const char *__s, const char *__accept)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));
# 330 "/usr/include/string.h" 3 4
extern char *strstr (const char *__haystack, const char *__needle)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));




extern char *strtok (char *__restrict __s, const char *__restrict __delim)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (2)));



extern char *__strtok_r (char *__restrict __s,
    const char *__restrict __delim,
    char **__restrict __save_ptr)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (2, 3)));

extern char *strtok_r (char *__restrict __s, const char *__restrict __delim,
         char **__restrict __save_ptr)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (2, 3)));
# 385 "/usr/include/string.h" 3 4
extern size_t strlen (const char *__s)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1)));




extern size_t strnlen (const char *__string, size_t __maxlen)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1)));




extern char *strerror (int __errnum) __attribute__ ((__nothrow__ , __leaf__));
# 410 "/usr/include/string.h" 3 4
extern int strerror_r (int __errnum, char *__buf, size_t __buflen) __asm__ ("" "__xpg_strerror_r") __attribute__ ((__nothrow__ , __leaf__))

                        __attribute__ ((__nonnull__ (2)));
# 428 "/usr/include/string.h" 3 4
extern char *strerror_l (int __errnum, locale_t __l) __attribute__ ((__nothrow__ , __leaf__));



# 1 "/usr/include/strings.h" 1 3 4
# 23 "/usr/include/strings.h" 3 4
# 1 "/usr/lib/gcc/x86_64-redhat-linux/9/include/stddef.h" 1 3 4
# 24 "/usr/include/strings.h" 2 3 4










extern int bcmp (const void *__s1, const void *__s2, size_t __n)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));


extern void bcopy (const void *__src, void *__dest, size_t __n)
  __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));


extern void bzero (void *__s, size_t __n) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));
# 68 "/usr/include/strings.h" 3 4
extern char *index (const char *__s, int __c)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1)));
# 96 "/usr/include/strings.h" 3 4
extern char *rindex (const char *__s, int __c)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1)));






extern int ffs (int __i) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__));





extern int ffsl (long int __l) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__));
__extension__ extern int ffsll (long long int __ll)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__));



extern int strcasecmp (const char *__s1, const char *__s2)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));


extern int strncasecmp (const char *__s1, const char *__s2, size_t __n)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));






extern int strcasecmp_l (const char *__s1, const char *__s2, locale_t __loc)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2, 3)));



extern int strncasecmp_l (const char *__s1, const char *__s2,
     size_t __n, locale_t __loc)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2, 4)));



# 433 "/usr/include/string.h" 2 3 4



extern void explicit_bzero (void *__s, size_t __n) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));



extern char *strsep (char **__restrict __stringp,
       const char *__restrict __delim)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));




extern char *strsignal (int __sig) __attribute__ ((__nothrow__ , __leaf__));


extern char *__stpcpy (char *__restrict __dest, const char *__restrict __src)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));
extern char *stpcpy (char *__restrict __dest, const char *__restrict __src)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));



extern char *__stpncpy (char *__restrict __dest,
   const char *__restrict __src, size_t __n)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));
extern char *stpncpy (char *__restrict __dest,
        const char *__restrict __src, size_t __n)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));
# 499 "/usr/include/string.h" 3 4

# 28 "weak-vector.c" 2

# 1 "boolean.h" 1
# 25 "boolean.h"
# 1 "../libguile/scm.h" 1
# 28 "../libguile/scm.h"
# 1 "/usr/lib/gcc/x86_64-redhat-linux/9/include/stdint.h" 1 3 4
# 9 "/usr/lib/gcc/x86_64-redhat-linux/9/include/stdint.h" 3 4
# 1 "/usr/include/stdint.h" 1 3 4
# 26 "/usr/include/stdint.h" 3 4
# 1 "/usr/include/bits/libc-header-start.h" 1 3 4
# 27 "/usr/include/stdint.h" 2 3 4

# 1 "/usr/include/bits/wchar.h" 1 3 4
# 29 "/usr/include/stdint.h" 2 3 4
# 1 "/usr/include/bits/wordsize.h" 1 3 4
# 30 "/usr/include/stdint.h" 2 3 4




# 1 "/usr/include/bits/stdint-intn.h" 1 3 4
# 24 "/usr/include/bits/stdint-intn.h" 3 4
typedef __int8_t int8_t;
typedef __int16_t int16_t;
typedef __int32_t int32_t;
typedef __int64_t int64_t;
# 35 "/usr/include/stdint.h" 2 3 4


# 1 "/usr/include/bits/stdint-uintn.h" 1 3 4
# 24 "/usr/include/bits/stdint-uintn.h" 3 4
typedef __uint8_t uint8_t;
typedef __uint16_t uint16_t;
typedef __uint32_t uint32_t;
typedef __uint64_t uint64_t;
# 38 "/usr/include/stdint.h" 2 3 4





typedef __int_least8_t int_least8_t;
typedef __int_least16_t int_least16_t;
typedef __int_least32_t int_least32_t;
typedef __int_least64_t int_least64_t;


typedef __uint_least8_t uint_least8_t;
typedef __uint_least16_t uint_least16_t;
typedef __uint_least32_t uint_least32_t;
typedef __uint_least64_t uint_least64_t;





typedef signed char int_fast8_t;

typedef long int int_fast16_t;
typedef long int int_fast32_t;
typedef long int int_fast64_t;
# 71 "/usr/include/stdint.h" 3 4
typedef unsigned char uint_fast8_t;

typedef unsigned long int uint_fast16_t;
typedef unsigned long int uint_fast32_t;
typedef unsigned long int uint_fast64_t;
# 87 "/usr/include/stdint.h" 3 4
typedef long int intptr_t;


typedef unsigned long int uintptr_t;
# 101 "/usr/include/stdint.h" 3 4
typedef __intmax_t intmax_t;
typedef __uintmax_t uintmax_t;
# 10 "/usr/lib/gcc/x86_64-redhat-linux/9/include/stdint.h" 2 3 4
# 29 "../libguile/scm.h" 2

# 1 "../libguile/scmconfig.h" 1
# 26 "../libguile/scmconfig.h"
# 1 "/usr/lib/gcc/x86_64-redhat-linux/9/include/stddef.h" 1 3 4
# 143 "/usr/lib/gcc/x86_64-redhat-linux/9/include/stddef.h" 3 4
typedef long int ptrdiff_t;
# 321 "/usr/lib/gcc/x86_64-redhat-linux/9/include/stddef.h" 3 4
typedef int wchar_t;
# 415 "/usr/lib/gcc/x86_64-redhat-linux/9/include/stddef.h" 3 4
typedef struct {
  long long __max_align_ll __attribute__((__aligned__(__alignof__(long long))));
  long double __max_align_ld __attribute__((__aligned__(__alignof__(long double))));
# 426 "/usr/lib/gcc/x86_64-redhat-linux/9/include/stddef.h" 3 4
} max_align_t;
# 27 "../libguile/scmconfig.h" 2
# 1 "/usr/lib/gcc/x86_64-redhat-linux/9/include/limits.h" 1 3 4
# 34 "/usr/lib/gcc/x86_64-redhat-linux/9/include/limits.h" 3 4
# 1 "/usr/lib/gcc/x86_64-redhat-linux/9/include/syslimits.h" 1 3 4






# 1 "/usr/lib/gcc/x86_64-redhat-linux/9/include/limits.h" 1 3 4
# 194 "/usr/lib/gcc/x86_64-redhat-linux/9/include/limits.h" 3 4
# 1 "/usr/include/limits.h" 1 3 4
# 26 "/usr/include/limits.h" 3 4
# 1 "/usr/include/bits/libc-header-start.h" 1 3 4
# 27 "/usr/include/limits.h" 2 3 4
# 183 "/usr/include/limits.h" 3 4
# 1 "/usr/include/bits/posix1_lim.h" 1 3 4
# 27 "/usr/include/bits/posix1_lim.h" 3 4
# 1 "/usr/include/bits/wordsize.h" 1 3 4
# 28 "/usr/include/bits/posix1_lim.h" 2 3 4
# 161 "/usr/include/bits/posix1_lim.h" 3 4
# 1 "/usr/include/bits/local_lim.h" 1 3 4
# 38 "/usr/include/bits/local_lim.h" 3 4
# 1 "/usr/include/linux/limits.h" 1 3 4
# 39 "/usr/include/bits/local_lim.h" 2 3 4
# 162 "/usr/include/bits/posix1_lim.h" 2 3 4
# 184 "/usr/include/limits.h" 2 3 4



# 1 "/usr/include/bits/posix2_lim.h" 1 3 4
# 188 "/usr/include/limits.h" 2 3 4
# 195 "/usr/lib/gcc/x86_64-redhat-linux/9/include/limits.h" 2 3 4
# 8 "/usr/lib/gcc/x86_64-redhat-linux/9/include/syslimits.h" 2 3 4
# 35 "/usr/lib/gcc/x86_64-redhat-linux/9/include/limits.h" 2 3 4
# 28 "../libguile/scmconfig.h" 2
# 1 "/usr/include/sys/time.h" 1 3 4
# 24 "/usr/include/sys/time.h" 3 4
# 1 "/usr/include/bits/types/time_t.h" 1 3 4






typedef __time_t time_t;
# 25 "/usr/include/sys/time.h" 2 3 4
# 1 "/usr/include/bits/types/struct_timeval.h" 1 3 4







struct timeval
{
  __time_t tv_sec;
  __suseconds_t tv_usec;
};
# 26 "/usr/include/sys/time.h" 2 3 4


typedef __suseconds_t suseconds_t;



# 1 "/usr/include/sys/select.h" 1 3 4
# 30 "/usr/include/sys/select.h" 3 4
# 1 "/usr/include/bits/select.h" 1 3 4
# 22 "/usr/include/bits/select.h" 3 4
# 1 "/usr/include/bits/wordsize.h" 1 3 4
# 23 "/usr/include/bits/select.h" 2 3 4
# 31 "/usr/include/sys/select.h" 2 3 4


# 1 "/usr/include/bits/types/sigset_t.h" 1 3 4



# 1 "/usr/include/bits/types/__sigset_t.h" 1 3 4




typedef struct
{
  unsigned long int __val[(1024 / (8 * sizeof (unsigned long int)))];
} __sigset_t;
# 5 "/usr/include/bits/types/sigset_t.h" 2 3 4


typedef __sigset_t sigset_t;
# 34 "/usr/include/sys/select.h" 2 3 4





# 1 "/usr/include/bits/types/struct_timespec.h" 1 3 4
# 9 "/usr/include/bits/types/struct_timespec.h" 3 4
struct timespec
{
  __time_t tv_sec;
  __syscall_slong_t tv_nsec;
};
# 40 "/usr/include/sys/select.h" 2 3 4
# 49 "/usr/include/sys/select.h" 3 4
typedef long int __fd_mask;
# 59 "/usr/include/sys/select.h" 3 4
typedef struct
  {






    __fd_mask __fds_bits[1024 / (8 * (int) sizeof (__fd_mask))];


  } fd_set;






typedef __fd_mask fd_mask;
# 91 "/usr/include/sys/select.h" 3 4

# 101 "/usr/include/sys/select.h" 3 4
extern int select (int __nfds, fd_set *__restrict __readfds,
     fd_set *__restrict __writefds,
     fd_set *__restrict __exceptfds,
     struct timeval *__restrict __timeout);
# 113 "/usr/include/sys/select.h" 3 4
extern int pselect (int __nfds, fd_set *__restrict __readfds,
      fd_set *__restrict __writefds,
      fd_set *__restrict __exceptfds,
      const struct timespec *__restrict __timeout,
      const __sigset_t *__restrict __sigmask);
# 126 "/usr/include/sys/select.h" 3 4

# 33 "/usr/include/sys/time.h" 2 3 4


# 52 "/usr/include/sys/time.h" 3 4
struct timezone
  {
    int tz_minuteswest;
    int tz_dsttime;
  };

typedef struct timezone *__restrict __timezone_ptr_t;
# 68 "/usr/include/sys/time.h" 3 4
extern int gettimeofday (struct timeval *__restrict __tv,
    __timezone_ptr_t __tz) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));




extern int settimeofday (const struct timeval *__tv,
    const struct timezone *__tz)
     __attribute__ ((__nothrow__ , __leaf__));





extern int adjtime (const struct timeval *__delta,
      struct timeval *__olddelta) __attribute__ ((__nothrow__ , __leaf__));




enum __itimer_which
  {

    ITIMER_REAL = 0,


    ITIMER_VIRTUAL = 1,



    ITIMER_PROF = 2

  };



struct itimerval
  {

    struct timeval it_interval;

    struct timeval it_value;
  };






typedef int __itimer_which_t;




extern int getitimer (__itimer_which_t __which,
        struct itimerval *__value) __attribute__ ((__nothrow__ , __leaf__));




extern int setitimer (__itimer_which_t __which,
        const struct itimerval *__restrict __new,
        struct itimerval *__restrict __old) __attribute__ ((__nothrow__ , __leaf__));




extern int utimes (const char *__file, const struct timeval __tvp[2])
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));



extern int lutimes (const char *__file, const struct timeval __tvp[2])
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));


extern int futimes (int __fd, const struct timeval __tvp[2]) __attribute__ ((__nothrow__ , __leaf__));
# 186 "/usr/include/sys/time.h" 3 4

# 29 "../libguile/scmconfig.h" 2
# 1 "/usr/include/time.h" 1 3 4
# 29 "/usr/include/time.h" 3 4
# 1 "/usr/lib/gcc/x86_64-redhat-linux/9/include/stddef.h" 1 3 4
# 30 "/usr/include/time.h" 2 3 4



# 1 "/usr/include/bits/time.h" 1 3 4
# 34 "/usr/include/time.h" 2 3 4



# 1 "/usr/include/bits/types/clock_t.h" 1 3 4






typedef __clock_t clock_t;
# 38 "/usr/include/time.h" 2 3 4

# 1 "/usr/include/bits/types/struct_tm.h" 1 3 4






struct tm
{
  int tm_sec;
  int tm_min;
  int tm_hour;
  int tm_mday;
  int tm_mon;
  int tm_year;
  int tm_wday;
  int tm_yday;
  int tm_isdst;


  long int tm_gmtoff;
  const char *tm_zone;




};
# 40 "/usr/include/time.h" 2 3 4






# 1 "/usr/include/bits/types/clockid_t.h" 1 3 4






typedef __clockid_t clockid_t;
# 47 "/usr/include/time.h" 2 3 4
# 1 "/usr/include/bits/types/timer_t.h" 1 3 4






typedef __timer_t timer_t;
# 48 "/usr/include/time.h" 2 3 4
# 1 "/usr/include/bits/types/struct_itimerspec.h" 1 3 4







struct itimerspec
  {
    struct timespec it_interval;
    struct timespec it_value;
  };
# 49 "/usr/include/time.h" 2 3 4
struct sigevent;




typedef __pid_t pid_t;
# 68 "/usr/include/time.h" 3 4




extern clock_t clock (void) __attribute__ ((__nothrow__ , __leaf__));


extern time_t time (time_t *__timer) __attribute__ ((__nothrow__ , __leaf__));


extern double difftime (time_t __time1, time_t __time0)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__));


extern time_t mktime (struct tm *__tp) __attribute__ ((__nothrow__ , __leaf__));





extern size_t strftime (char *__restrict __s, size_t __maxsize,
   const char *__restrict __format,
   const struct tm *__restrict __tp) __attribute__ ((__nothrow__ , __leaf__));
# 104 "/usr/include/time.h" 3 4
extern size_t strftime_l (char *__restrict __s, size_t __maxsize,
     const char *__restrict __format,
     const struct tm *__restrict __tp,
     locale_t __loc) __attribute__ ((__nothrow__ , __leaf__));
# 119 "/usr/include/time.h" 3 4
extern struct tm *gmtime (const time_t *__timer) __attribute__ ((__nothrow__ , __leaf__));



extern struct tm *localtime (const time_t *__timer) __attribute__ ((__nothrow__ , __leaf__));




extern struct tm *gmtime_r (const time_t *__restrict __timer,
       struct tm *__restrict __tp) __attribute__ ((__nothrow__ , __leaf__));



extern struct tm *localtime_r (const time_t *__restrict __timer,
          struct tm *__restrict __tp) __attribute__ ((__nothrow__ , __leaf__));




extern char *asctime (const struct tm *__tp) __attribute__ ((__nothrow__ , __leaf__));


extern char *ctime (const time_t *__timer) __attribute__ ((__nothrow__ , __leaf__));






extern char *asctime_r (const struct tm *__restrict __tp,
   char *__restrict __buf) __attribute__ ((__nothrow__ , __leaf__));


extern char *ctime_r (const time_t *__restrict __timer,
        char *__restrict __buf) __attribute__ ((__nothrow__ , __leaf__));




extern char *__tzname[2];
extern int __daylight;
extern long int __timezone;




extern char *tzname[2];



extern void tzset (void) __attribute__ ((__nothrow__ , __leaf__));



extern int daylight;
extern long int timezone;





extern int stime (const time_t *__when) __attribute__ ((__nothrow__ , __leaf__));
# 196 "/usr/include/time.h" 3 4
extern time_t timegm (struct tm *__tp) __attribute__ ((__nothrow__ , __leaf__));


extern time_t timelocal (struct tm *__tp) __attribute__ ((__nothrow__ , __leaf__));


extern int dysize (int __year) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__));
# 211 "/usr/include/time.h" 3 4
extern int nanosleep (const struct timespec *__requested_time,
        struct timespec *__remaining);



extern int clock_getres (clockid_t __clock_id, struct timespec *__res) __attribute__ ((__nothrow__ , __leaf__));


extern int clock_gettime (clockid_t __clock_id, struct timespec *__tp) __attribute__ ((__nothrow__ , __leaf__));


extern int clock_settime (clockid_t __clock_id, const struct timespec *__tp)
     __attribute__ ((__nothrow__ , __leaf__));






extern int clock_nanosleep (clockid_t __clock_id, int __flags,
       const struct timespec *__req,
       struct timespec *__rem);


extern int clock_getcpuclockid (pid_t __pid, clockid_t *__clock_id) __attribute__ ((__nothrow__ , __leaf__));




extern int timer_create (clockid_t __clock_id,
    struct sigevent *__restrict __evp,
    timer_t *__restrict __timerid) __attribute__ ((__nothrow__ , __leaf__));


extern int timer_delete (timer_t __timerid) __attribute__ ((__nothrow__ , __leaf__));


extern int timer_settime (timer_t __timerid, int __flags,
     const struct itimerspec *__restrict __value,
     struct itimerspec *__restrict __ovalue) __attribute__ ((__nothrow__ , __leaf__));


extern int timer_gettime (timer_t __timerid, struct itimerspec *__value)
     __attribute__ ((__nothrow__ , __leaf__));


extern int timer_getoverrun (timer_t __timerid) __attribute__ ((__nothrow__ , __leaf__));





extern int timespec_get (struct timespec *__ts, int __base)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));
# 307 "/usr/include/time.h" 3 4

# 30 "../libguile/scmconfig.h" 2


# 1 "/usr/include/stdlib.h" 1 3 4
# 25 "/usr/include/stdlib.h" 3 4
# 1 "/usr/include/bits/libc-header-start.h" 1 3 4
# 26 "/usr/include/stdlib.h" 2 3 4





# 1 "/usr/lib/gcc/x86_64-redhat-linux/9/include/stddef.h" 1 3 4
# 32 "/usr/include/stdlib.h" 2 3 4







# 1 "/usr/include/bits/waitflags.h" 1 3 4
# 40 "/usr/include/stdlib.h" 2 3 4
# 1 "/usr/include/bits/waitstatus.h" 1 3 4
# 41 "/usr/include/stdlib.h" 2 3 4
# 55 "/usr/include/stdlib.h" 3 4
# 1 "/usr/include/bits/floatn.h" 1 3 4
# 119 "/usr/include/bits/floatn.h" 3 4
# 1 "/usr/include/bits/floatn-common.h" 1 3 4
# 24 "/usr/include/bits/floatn-common.h" 3 4
# 1 "/usr/include/bits/long-double.h" 1 3 4
# 25 "/usr/include/bits/floatn-common.h" 2 3 4
# 120 "/usr/include/bits/floatn.h" 2 3 4
# 56 "/usr/include/stdlib.h" 2 3 4


typedef struct
  {
    int quot;
    int rem;
  } div_t;



typedef struct
  {
    long int quot;
    long int rem;
  } ldiv_t;





__extension__ typedef struct
  {
    long long int quot;
    long long int rem;
  } lldiv_t;
# 97 "/usr/include/stdlib.h" 3 4
extern size_t __ctype_get_mb_cur_max (void) __attribute__ ((__nothrow__ , __leaf__)) ;



extern double atof (const char *__nptr)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1))) ;

extern int atoi (const char *__nptr)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1))) ;

extern long int atol (const char *__nptr)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1))) ;



__extension__ extern long long int atoll (const char *__nptr)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1))) ;



extern double strtod (const char *__restrict __nptr,
        char **__restrict __endptr)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));



extern float strtof (const char *__restrict __nptr,
       char **__restrict __endptr) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));

extern long double strtold (const char *__restrict __nptr,
       char **__restrict __endptr)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));
# 176 "/usr/include/stdlib.h" 3 4
extern long int strtol (const char *__restrict __nptr,
   char **__restrict __endptr, int __base)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));

extern unsigned long int strtoul (const char *__restrict __nptr,
      char **__restrict __endptr, int __base)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));



__extension__
extern long long int strtoq (const char *__restrict __nptr,
        char **__restrict __endptr, int __base)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));

__extension__
extern unsigned long long int strtouq (const char *__restrict __nptr,
           char **__restrict __endptr, int __base)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));




__extension__
extern long long int strtoll (const char *__restrict __nptr,
         char **__restrict __endptr, int __base)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));

__extension__
extern unsigned long long int strtoull (const char *__restrict __nptr,
     char **__restrict __endptr, int __base)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));
# 385 "/usr/include/stdlib.h" 3 4
extern char *l64a (long int __n) __attribute__ ((__nothrow__ , __leaf__)) ;


extern long int a64l (const char *__s)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1))) ;




# 1 "/usr/include/sys/types.h" 1 3 4
# 27 "/usr/include/sys/types.h" 3 4






typedef __u_char u_char;
typedef __u_short u_short;
typedef __u_int u_int;
typedef __u_long u_long;
typedef __quad_t quad_t;
typedef __u_quad_t u_quad_t;
typedef __fsid_t fsid_t;


typedef __loff_t loff_t;




typedef __ino_t ino_t;
# 59 "/usr/include/sys/types.h" 3 4
typedef __dev_t dev_t;




typedef __gid_t gid_t;




typedef __mode_t mode_t;




typedef __nlink_t nlink_t;




typedef __uid_t uid_t;
# 103 "/usr/include/sys/types.h" 3 4
typedef __id_t id_t;
# 114 "/usr/include/sys/types.h" 3 4
typedef __daddr_t daddr_t;
typedef __caddr_t caddr_t;





typedef __key_t key_t;
# 144 "/usr/include/sys/types.h" 3 4
# 1 "/usr/lib/gcc/x86_64-redhat-linux/9/include/stddef.h" 1 3 4
# 145 "/usr/include/sys/types.h" 2 3 4



typedef unsigned long int ulong;
typedef unsigned short int ushort;
typedef unsigned int uint;







typedef __uint8_t u_int8_t;
typedef __uint16_t u_int16_t;
typedef __uint32_t u_int32_t;
typedef __uint64_t u_int64_t;


typedef int register_t __attribute__ ((__mode__ (__word__)));
# 176 "/usr/include/sys/types.h" 3 4
# 1 "/usr/include/endian.h" 1 3 4
# 36 "/usr/include/endian.h" 3 4
# 1 "/usr/include/bits/endian.h" 1 3 4
# 37 "/usr/include/endian.h" 2 3 4
# 60 "/usr/include/endian.h" 3 4
# 1 "/usr/include/bits/byteswap.h" 1 3 4
# 33 "/usr/include/bits/byteswap.h" 3 4
static __inline __uint16_t
__bswap_16 (__uint16_t __bsx)
{

  return __builtin_bswap16 (__bsx);



}






static __inline __uint32_t
__bswap_32 (__uint32_t __bsx)
{

  return __builtin_bswap32 (__bsx);



}
# 69 "/usr/include/bits/byteswap.h" 3 4
__extension__ static __inline __uint64_t
__bswap_64 (__uint64_t __bsx)
{

  return __builtin_bswap64 (__bsx);



}
# 61 "/usr/include/endian.h" 2 3 4
# 1 "/usr/include/bits/uintn-identity.h" 1 3 4
# 32 "/usr/include/bits/uintn-identity.h" 3 4
static __inline __uint16_t
__uint16_identity (__uint16_t __x)
{
  return __x;
}

static __inline __uint32_t
__uint32_identity (__uint32_t __x)
{
  return __x;
}

static __inline __uint64_t
__uint64_identity (__uint64_t __x)
{
  return __x;
}
# 62 "/usr/include/endian.h" 2 3 4
# 177 "/usr/include/sys/types.h" 2 3 4
# 185 "/usr/include/sys/types.h" 3 4
typedef __blksize_t blksize_t;






typedef __blkcnt_t blkcnt_t;



typedef __fsblkcnt_t fsblkcnt_t;



typedef __fsfilcnt_t fsfilcnt_t;
# 227 "/usr/include/sys/types.h" 3 4
# 1 "/usr/include/bits/pthreadtypes.h" 1 3 4
# 23 "/usr/include/bits/pthreadtypes.h" 3 4
# 1 "/usr/include/bits/thread-shared-types.h" 1 3 4
# 77 "/usr/include/bits/thread-shared-types.h" 3 4
# 1 "/usr/include/bits/pthreadtypes-arch.h" 1 3 4
# 21 "/usr/include/bits/pthreadtypes-arch.h" 3 4
# 1 "/usr/include/bits/wordsize.h" 1 3 4
# 22 "/usr/include/bits/pthreadtypes-arch.h" 2 3 4
# 65 "/usr/include/bits/pthreadtypes-arch.h" 3 4
struct __pthread_rwlock_arch_t
{
  unsigned int __readers;
  unsigned int __writers;
  unsigned int __wrphase_futex;
  unsigned int __writers_futex;
  unsigned int __pad3;
  unsigned int __pad4;

  int __cur_writer;
  int __shared;
  signed char __rwelision;




  unsigned char __pad1[7];


  unsigned long int __pad2;


  unsigned int __flags;
# 99 "/usr/include/bits/pthreadtypes-arch.h" 3 4
};
# 78 "/usr/include/bits/thread-shared-types.h" 2 3 4




typedef struct __pthread_internal_list
{
  struct __pthread_internal_list *__prev;
  struct __pthread_internal_list *__next;
} __pthread_list_t;
# 118 "/usr/include/bits/thread-shared-types.h" 3 4
struct __pthread_mutex_s
{
  int __lock ;
  unsigned int __count;
  int __owner;

  unsigned int __nusers;
# 148 "/usr/include/bits/thread-shared-types.h" 3 4
  int __kind;
 




  short __spins; short __elision;
  __pthread_list_t __list;
# 165 "/usr/include/bits/thread-shared-types.h" 3 4
 
};




struct __pthread_cond_s
{
  __extension__ union
  {
    __extension__ unsigned long long int __wseq;
    struct
    {
      unsigned int __low;
      unsigned int __high;
    } __wseq32;
  };
  __extension__ union
  {
    __extension__ unsigned long long int __g1_start;
    struct
    {
      unsigned int __low;
      unsigned int __high;
    } __g1_start32;
  };
  unsigned int __g_refs[2] ;
  unsigned int __g_size[2];
  unsigned int __g1_orig_size;
  unsigned int __wrefs;
  unsigned int __g_signals[2];
};
# 24 "/usr/include/bits/pthreadtypes.h" 2 3 4



typedef unsigned long int pthread_t;




typedef union
{
  char __size[4];
  int __align;
} pthread_mutexattr_t;




typedef union
{
  char __size[4];
  int __align;
} pthread_condattr_t;



typedef unsigned int pthread_key_t;



typedef int pthread_once_t;


union pthread_attr_t
{
  char __size[56];
  long int __align;
};

typedef union pthread_attr_t pthread_attr_t;




typedef union
{
  struct __pthread_mutex_s __data;
  char __size[40];
  long int __align;
} pthread_mutex_t;


typedef union
{
  struct __pthread_cond_s __data;
  char __size[48];
  __extension__ long long int __align;
} pthread_cond_t;





typedef union
{
  struct __pthread_rwlock_arch_t __data;
  char __size[56];
  long int __align;
} pthread_rwlock_t;

typedef union
{
  char __size[8];
  long int __align;
} pthread_rwlockattr_t;





typedef volatile int pthread_spinlock_t;




typedef union
{
  char __size[32];
  long int __align;
} pthread_barrier_t;

typedef union
{
  char __size[4];
  int __align;
} pthread_barrierattr_t;
# 228 "/usr/include/sys/types.h" 2 3 4



# 395 "/usr/include/stdlib.h" 2 3 4






extern long int random (void) __attribute__ ((__nothrow__ , __leaf__));


extern void srandom (unsigned int __seed) __attribute__ ((__nothrow__ , __leaf__));





extern char *initstate (unsigned int __seed, char *__statebuf,
   size_t __statelen) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (2)));



extern char *setstate (char *__statebuf) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));







struct random_data
  {
    int32_t *fptr;
    int32_t *rptr;
    int32_t *state;
    int rand_type;
    int rand_deg;
    int rand_sep;
    int32_t *end_ptr;
  };

extern int random_r (struct random_data *__restrict __buf,
       int32_t *__restrict __result) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));

extern int srandom_r (unsigned int __seed, struct random_data *__buf)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (2)));

extern int initstate_r (unsigned int __seed, char *__restrict __statebuf,
   size_t __statelen,
   struct random_data *__restrict __buf)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (2, 4)));

extern int setstate_r (char *__restrict __statebuf,
         struct random_data *__restrict __buf)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));





extern int rand (void) __attribute__ ((__nothrow__ , __leaf__));

extern void srand (unsigned int __seed) __attribute__ ((__nothrow__ , __leaf__));



extern int rand_r (unsigned int *__seed) __attribute__ ((__nothrow__ , __leaf__));







extern double drand48 (void) __attribute__ ((__nothrow__ , __leaf__));
extern double erand48 (unsigned short int __xsubi[3]) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));


extern long int lrand48 (void) __attribute__ ((__nothrow__ , __leaf__));
extern long int nrand48 (unsigned short int __xsubi[3])
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));


extern long int mrand48 (void) __attribute__ ((__nothrow__ , __leaf__));
extern long int jrand48 (unsigned short int __xsubi[3])
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));


extern void srand48 (long int __seedval) __attribute__ ((__nothrow__ , __leaf__));
extern unsigned short int *seed48 (unsigned short int __seed16v[3])
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));
extern void lcong48 (unsigned short int __param[7]) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));





struct drand48_data
  {
    unsigned short int __x[3];
    unsigned short int __old_x[3];
    unsigned short int __c;
    unsigned short int __init;
    __extension__ unsigned long long int __a;

  };


extern int drand48_r (struct drand48_data *__restrict __buffer,
        double *__restrict __result) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));
extern int erand48_r (unsigned short int __xsubi[3],
        struct drand48_data *__restrict __buffer,
        double *__restrict __result) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));


extern int lrand48_r (struct drand48_data *__restrict __buffer,
        long int *__restrict __result)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));
extern int nrand48_r (unsigned short int __xsubi[3],
        struct drand48_data *__restrict __buffer,
        long int *__restrict __result)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));


extern int mrand48_r (struct drand48_data *__restrict __buffer,
        long int *__restrict __result)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));
extern int jrand48_r (unsigned short int __xsubi[3],
        struct drand48_data *__restrict __buffer,
        long int *__restrict __result)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));


extern int srand48_r (long int __seedval, struct drand48_data *__buffer)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (2)));

extern int seed48_r (unsigned short int __seed16v[3],
       struct drand48_data *__buffer) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));

extern int lcong48_r (unsigned short int __param[7],
        struct drand48_data *__buffer)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));




extern void *malloc (size_t __size) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__malloc__))
     __attribute__ ((__alloc_size__ (1))) ;

extern void *calloc (size_t __nmemb, size_t __size)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__malloc__)) __attribute__ ((__alloc_size__ (1, 2))) ;






extern void *realloc (void *__ptr, size_t __size)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__warn_unused_result__)) __attribute__ ((__alloc_size__ (2)));







extern void *reallocarray (void *__ptr, size_t __nmemb, size_t __size)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__warn_unused_result__))
     __attribute__ ((__alloc_size__ (2, 3)));



extern void free (void *__ptr) __attribute__ ((__nothrow__ , __leaf__));


# 1 "/usr/include/alloca.h" 1 3 4
# 24 "/usr/include/alloca.h" 3 4
# 1 "/usr/lib/gcc/x86_64-redhat-linux/9/include/stddef.h" 1 3 4
# 25 "/usr/include/alloca.h" 2 3 4







extern void *alloca (size_t __size) __attribute__ ((__nothrow__ , __leaf__));






# 569 "/usr/include/stdlib.h" 2 3 4





extern void *valloc (size_t __size) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__malloc__))
     __attribute__ ((__alloc_size__ (1))) ;




extern int posix_memalign (void **__memptr, size_t __alignment, size_t __size)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1))) ;




extern void *aligned_alloc (size_t __alignment, size_t __size)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__malloc__)) __attribute__ ((__alloc_size__ (2))) ;



extern void abort (void) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__noreturn__));



extern int atexit (void (*__func) (void)) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));







extern int at_quick_exit (void (*__func) (void)) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));






extern int on_exit (void (*__func) (int __status, void *__arg), void *__arg)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));





extern void exit (int __status) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__noreturn__));





extern void quick_exit (int __status) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__noreturn__));





extern void _Exit (int __status) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__noreturn__));




extern char *getenv (const char *__name) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1))) ;
# 647 "/usr/include/stdlib.h" 3 4
extern int putenv (char *__string) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));





extern int setenv (const char *__name, const char *__value, int __replace)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (2)));


extern int unsetenv (const char *__name) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));






extern int clearenv (void) __attribute__ ((__nothrow__ , __leaf__));
# 675 "/usr/include/stdlib.h" 3 4
extern char *mktemp (char *__template) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));
# 688 "/usr/include/stdlib.h" 3 4
extern int mkstemp (char *__template) __attribute__ ((__nonnull__ (1))) ;
# 710 "/usr/include/stdlib.h" 3 4
extern int mkstemps (char *__template, int __suffixlen) __attribute__ ((__nonnull__ (1))) ;
# 731 "/usr/include/stdlib.h" 3 4
extern char *mkdtemp (char *__template) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1))) ;
# 784 "/usr/include/stdlib.h" 3 4
extern int system (const char *__command) ;
# 800 "/usr/include/stdlib.h" 3 4
extern char *realpath (const char *__restrict __name,
         char *__restrict __resolved) __attribute__ ((__nothrow__ , __leaf__)) ;






typedef int (*__compar_fn_t) (const void *, const void *);
# 820 "/usr/include/stdlib.h" 3 4
extern void *bsearch (const void *__key, const void *__base,
        size_t __nmemb, size_t __size, __compar_fn_t __compar)
     __attribute__ ((__nonnull__ (1, 2, 5))) ;







extern void qsort (void *__base, size_t __nmemb, size_t __size,
     __compar_fn_t __compar) __attribute__ ((__nonnull__ (1, 4)));
# 840 "/usr/include/stdlib.h" 3 4
extern int abs (int __x) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__)) ;
extern long int labs (long int __x) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__)) ;


__extension__ extern long long int llabs (long long int __x)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__)) ;






extern div_t div (int __numer, int __denom)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__)) ;
extern ldiv_t ldiv (long int __numer, long int __denom)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__)) ;


__extension__ extern lldiv_t lldiv (long long int __numer,
        long long int __denom)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__)) ;
# 872 "/usr/include/stdlib.h" 3 4
extern char *ecvt (double __value, int __ndigit, int *__restrict __decpt,
     int *__restrict __sign) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (3, 4))) ;




extern char *fcvt (double __value, int __ndigit, int *__restrict __decpt,
     int *__restrict __sign) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (3, 4))) ;




extern char *gcvt (double __value, int __ndigit, char *__buf)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (3))) ;




extern char *qecvt (long double __value, int __ndigit,
      int *__restrict __decpt, int *__restrict __sign)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (3, 4))) ;
extern char *qfcvt (long double __value, int __ndigit,
      int *__restrict __decpt, int *__restrict __sign)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (3, 4))) ;
extern char *qgcvt (long double __value, int __ndigit, char *__buf)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (3))) ;




extern int ecvt_r (double __value, int __ndigit, int *__restrict __decpt,
     int *__restrict __sign, char *__restrict __buf,
     size_t __len) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (3, 4, 5)));
extern int fcvt_r (double __value, int __ndigit, int *__restrict __decpt,
     int *__restrict __sign, char *__restrict __buf,
     size_t __len) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (3, 4, 5)));

extern int qecvt_r (long double __value, int __ndigit,
      int *__restrict __decpt, int *__restrict __sign,
      char *__restrict __buf, size_t __len)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (3, 4, 5)));
extern int qfcvt_r (long double __value, int __ndigit,
      int *__restrict __decpt, int *__restrict __sign,
      char *__restrict __buf, size_t __len)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (3, 4, 5)));





extern int mblen (const char *__s, size_t __n) __attribute__ ((__nothrow__ , __leaf__));


extern int mbtowc (wchar_t *__restrict __pwc,
     const char *__restrict __s, size_t __n) __attribute__ ((__nothrow__ , __leaf__));


extern int wctomb (char *__s, wchar_t __wchar) __attribute__ ((__nothrow__ , __leaf__));



extern size_t mbstowcs (wchar_t *__restrict __pwcs,
   const char *__restrict __s, size_t __n) __attribute__ ((__nothrow__ , __leaf__));

extern size_t wcstombs (char *__restrict __s,
   const wchar_t *__restrict __pwcs, size_t __n)
     __attribute__ ((__nothrow__ , __leaf__));







extern int rpmatch (const char *__response) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1))) ;
# 957 "/usr/include/stdlib.h" 3 4
extern int getsubopt (char **__restrict __optionp,
        char *const *__restrict __tokens,
        char **__restrict __valuep)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2, 3))) ;
# 1003 "/usr/include/stdlib.h" 3 4
extern int getloadavg (double __loadavg[], int __nelem)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));
# 1013 "/usr/include/stdlib.h" 3 4
# 1 "/usr/include/bits/stdlib-float.h" 1 3 4
# 1014 "/usr/include/stdlib.h" 2 3 4
# 1023 "/usr/include/stdlib.h" 3 4

# 33 "../libguile/scmconfig.h" 2

# 1 "/usr/lib/gcc/x86_64-redhat-linux/9/include/stddef.h" 1 3 4
# 35 "../libguile/scmconfig.h" 2
# 71 "../libguile/scmconfig.h"

# 71 "../libguile/scmconfig.h"
typedef struct timespec scm_t_timespec;
# 93 "../libguile/scmconfig.h"
typedef int64_t scm_t_off;
# 31 "../libguile/scm.h" 2
# 141 "../libguile/scm.h"
typedef intptr_t scm_t_signed_bits;
typedef uintptr_t scm_t_bits;
# 158 "../libguile/scm.h"
  typedef struct scm_unused_struct { char scm_unused_field; } *SCM;
# 521 "../libguile/scm.h"
enum scm_tc8_tags
{
  scm_tc8_flag = 4 + 0x00,
  scm_tc8_char = 4 + 0x08,
  scm_tc8_unused_0 = 4 + 0x10,
  scm_tc8_unused_1 = 4 + 0x18
};
# 820 "../libguile/scm.h"
typedef void *scm_t_subr;


typedef struct scm_dynamic_state scm_t_dynamic_state;
typedef struct scm_print_state scm_print_state;
typedef struct scm_dynstack scm_t_dynstack;
typedef int32_t scm_t_wchar;
struct scm_frame;
struct scm_vm;
union scm_vm_stack_element;
typedef struct scm_thread scm_thread;
# 850 "../libguile/scm.h"
typedef long SCM_STACKITEM;
# 26 "boolean.h" 2
# 83 "boolean.h"
extern int scm_is_bool (SCM);




extern int scm_to_bool (SCM x);
# 127 "boolean.h"
extern SCM scm_not (SCM x);
extern SCM scm_boolean_p (SCM obj);
extern SCM scm_nil_p (SCM obj);
# 148 "boolean.h"
extern void scm_init_boolean (void);
# 30 "weak-vector.c" 2
# 1 "extensions.h" 1
# 29 "extensions.h"
typedef void (*scm_t_extension_init_func)(void*);

extern void scm_c_register_extension (const char *lib, const char *init,
           void (*func) (void *), void *data);

extern void scm_c_load_extension (const char *lib, const char *init);
extern SCM scm_load_extension (SCM lib, SCM init);

extern void scm_init_extensions (void);
# 31 "weak-vector.c" 2
# 1 "gsubr.h" 1
# 25 "gsubr.h"
# 1 "../libguile/snarf.h" 1
# 26 "gsubr.h" 2
# 52 "gsubr.h"
extern uint32_t *
scm_i_alloc_primitive_code_with_instrumentation (size_t uint32_count,
                                                 uint32_t **write_ptr);
extern int scm_i_primitive_code_p (const uint32_t *code);
extern uintptr_t scm_i_primitive_call_ip (SCM subr);
extern SCM scm_i_primitive_name (const uint32_t *code);

extern scm_t_subr scm_subr_function (SCM subr);
extern scm_t_subr scm_subr_function_by_index (uint32_t subr_idx);
extern SCM scm_subr_name (SCM subr);

extern SCM scm_apply_subr (union scm_vm_stack_element *sp,
                                 uint32_t subr_idx, ptrdiff_t nargs);

extern SCM scm_c_make_gsubr (const char *name,
         int req, int opt, int rst, scm_t_subr fcn);
extern SCM scm_c_make_gsubr_with_generic (const char *name,
        int req, int opt, int rst,
        scm_t_subr fcn, SCM *gf);
extern SCM scm_c_define_gsubr (const char *name,
    int req, int opt, int rst, scm_t_subr fcn);
extern SCM scm_c_define_gsubr_with_generic (const char *name,
          int req, int opt, int rst,
          scm_t_subr fcn, SCM *gf);
# 158 "gsubr.h"
extern void scm_init_gsubr (void);
# 32 "weak-vector.c" 2
# 1 "list.h" 1
# 25 "list.h"
# 1 "../libguile/error.h" 1
# 28 "../libguile/error.h"
extern SCM scm_system_error_key;
extern SCM scm_num_overflow_key;
extern SCM scm_out_of_range_key;
extern SCM scm_args_number_key;
extern SCM scm_arg_type_key;
extern SCM scm_misc_error_key;
# 50 "../libguile/error.h"
extern void scm_error (SCM key, const char *subr, const char *message,
   SCM args, SCM rest) __attribute__ ((__noreturn__));
extern SCM scm_error_scm (SCM key, SCM subr, SCM message,
      SCM args, SCM rest) __attribute__ ((__noreturn__));
extern SCM scm_strerror (SCM err);
extern void scm_syserror (const char *subr) __attribute__ ((__noreturn__));
extern void scm_syserror_msg (const char *subr, const char *message,
          SCM args, int eno) __attribute__ ((__noreturn__));
extern void scm_num_overflow (const char *subr) __attribute__ ((__noreturn__));
extern void scm_out_of_range (const char *subr, SCM bad_value)
     __attribute__ ((__noreturn__));
extern void scm_out_of_range_pos (const char *subr, SCM bad_value, SCM pos)
     __attribute__ ((__noreturn__));
extern void scm_wrong_num_args (SCM proc) __attribute__ ((__noreturn__));
extern void scm_error_num_args_subr (const char* subr) __attribute__ ((__noreturn__));
extern void scm_wrong_type_arg (const char *subr, int pos,
     SCM bad_value) __attribute__ ((__noreturn__));
extern void scm_i_wrong_type_arg_symbol (SCM symbol, int pos,
            SCM bad_value) __attribute__ ((__noreturn__));
extern void scm_wrong_type_arg_msg (const char *subr, int pos,
         SCM bad_value, const char *sz) __attribute__ ((__noreturn__));
extern void scm_misc_error (const char *subr, const char *message,
        SCM args) __attribute__ ((__noreturn__));
extern void scm_init_error (void);
# 26 "list.h" 2



extern SCM scm_list_1 (SCM e1);
extern SCM scm_list_2 (SCM e1, SCM e2);
extern SCM scm_list_3 (SCM e1, SCM e2, SCM e3);
extern SCM scm_list_4 (SCM e1, SCM e2, SCM e3, SCM e4);
extern SCM scm_list_5 (SCM e1, SCM e2, SCM e3, SCM e4, SCM e5);
extern SCM scm_list_n (SCM elt, ...);
extern SCM scm_list (SCM objs);
extern SCM scm_list_head (SCM lst, SCM k);
extern SCM scm_make_list (SCM n, SCM init);
extern SCM scm_cons_star (SCM arg, SCM objs);
extern SCM scm_null_p (SCM x);
extern SCM scm_list_p (SCM x);
extern long scm_ilength (SCM sx);
extern SCM scm_length (SCM x);
extern SCM scm_append (SCM args);
extern SCM scm_append_x (SCM args);
extern SCM scm_reverse (SCM lst);
extern SCM scm_reverse_x (SCM lst, SCM newtail);
extern SCM scm_list_ref (SCM lst, SCM k);
extern SCM scm_list_set_x (SCM lst, SCM k, SCM val);
extern SCM scm_list_cdr_set_x (SCM lst, SCM k, SCM val);
extern SCM scm_last_pair (SCM sx);
extern SCM scm_list_tail (SCM lst, SCM k);
extern SCM scm_c_memq (SCM x, SCM lst);
extern SCM scm_memq (SCM x, SCM lst);
extern SCM scm_memv (SCM x, SCM lst);
extern SCM scm_member (SCM x, SCM lst);
extern SCM scm_delq_x (SCM item, SCM lst);
extern SCM scm_delv_x (SCM item, SCM lst);
extern SCM scm_delete_x (SCM item, SCM lst);
extern SCM scm_list_copy (SCM lst);
extern SCM scm_delq (SCM item, SCM lst);
extern SCM scm_delv (SCM item, SCM lst);
extern SCM scm_delete (SCM item, SCM lst);
extern SCM scm_delq1_x (SCM item, SCM lst);
extern SCM scm_delv1_x (SCM item, SCM lst);
extern SCM scm_delete1_x (SCM item, SCM lst);
extern SCM scm_filter (SCM pred, SCM list);
extern SCM scm_filter_x (SCM pred, SCM list);
extern SCM scm_copy_tree (SCM obj);
# 112 "list.h"
extern SCM scm_i_finite_list_copy (SCM );
extern void scm_init_list (void);
# 33 "weak-vector.c" 2
# 1 "pairs.h" 1
# 26 "pairs.h"
# 1 "../libguile/gc.h" 1
# 25 "../libguile/gc.h"
# 1 "../libguile/inline.h" 1
# 26 "../libguile/gc.h" 2
# 1 "../libguile/chooks.h" 1
# 36 "../libguile/chooks.h"
typedef enum scm_t_c_hook_type {
  SCM_C_HOOK_NORMAL,
  SCM_C_HOOK_OR,
  SCM_C_HOOK_AND
} scm_t_c_hook_type;

typedef void *(*scm_t_c_hook_function) (void *hook_data,
      void *fn_data,
      void *data);

typedef struct scm_t_c_hook_entry {
  struct scm_t_c_hook_entry *next;
  scm_t_c_hook_function func;
  void *data;
} scm_t_c_hook_entry;

typedef struct scm_t_c_hook {
  scm_t_c_hook_entry *first;
  scm_t_c_hook_type type;
  void *data;
} scm_t_c_hook;

extern void scm_c_hook_init (scm_t_c_hook *hook,
         void *hook_data,
         scm_t_c_hook_type type);
extern void scm_c_hook_add (scm_t_c_hook *hook,
        scm_t_c_hook_function func,
        void *fn_data,
        int appendp);
extern void scm_c_hook_remove (scm_t_c_hook *hook,
    scm_t_c_hook_function func,
    void *fn_data);
extern void *scm_c_hook_run (scm_t_c_hook *hook, void *data);
# 27 "../libguile/gc.h" 2
# 36 "../libguile/gc.h"
typedef struct scm_t_cell
{
  SCM word_0;
  SCM word_1;
} scm_t_cell;
# 89 "../libguile/gc.h"
extern unsigned long scm_gc_ports_collected;

extern SCM scm_after_gc_hook;

extern scm_t_c_hook scm_before_gc_c_hook;
extern scm_t_c_hook scm_before_mark_c_hook;
extern scm_t_c_hook scm_before_sweep_c_hook;
extern scm_t_c_hook scm_after_sweep_c_hook;
extern scm_t_c_hook scm_after_gc_c_hook;



extern SCM scm_set_debug_cell_accesses_x (SCM flag);


extern SCM scm_object_address (SCM obj);
extern SCM scm_gc_enable (void);
extern SCM scm_gc_disable (void);
extern SCM scm_gc_dump (void);
extern SCM scm_gc_stats (void);
extern SCM scm_gc (void);
extern void scm_i_gc (const char *what);
extern void scm_gc_mark (SCM p);
extern void scm_gc_sweep (void);

extern void scm_gc_register_allocation (size_t size);

extern void *scm_malloc (size_t size) __attribute__ ((__malloc__));
extern void *scm_calloc (size_t size) __attribute__ ((__malloc__));
extern void *scm_realloc (void *mem, size_t size);
extern char *scm_strdup (const char *str) __attribute__ ((__malloc__));
extern char *scm_strndup (const char *str, size_t n) __attribute__ ((__malloc__));
extern void scm_gc_register_collectable_memory (void *mem, size_t size,
       const char *what);
extern void scm_gc_unregister_collectable_memory (void *mem, size_t size,
         const char *what);
extern void *scm_gc_malloc_pointerless (size_t size, const char *what)
  __attribute__ ((__malloc__));
extern void *scm_gc_calloc (size_t size, const char *what)
  __attribute__ ((__malloc__));
extern void *scm_gc_malloc (size_t size, const char *what)
  __attribute__ ((__malloc__));
extern void *scm_gc_realloc (void *mem, size_t old_size,
         size_t new_size, const char *what);
extern void scm_gc_free (void *mem, size_t size, const char *what);
extern char *scm_gc_strdup (const char *str, const char *what)
  __attribute__ ((__malloc__));
extern char *scm_gc_strndup (const char *str, size_t n, const char *what)
  __attribute__ ((__malloc__));
# 151 "../libguile/gc.h"
extern __inline__ __attribute__ ((__gnu_inline__)) SCM scm_cell (scm_t_bits car, scm_t_bits cdr);
extern __inline__ __attribute__ ((__gnu_inline__)) SCM scm_double_cell (scm_t_bits car, scm_t_bits cbr,
                                scm_t_bits ccr, scm_t_bits cdr);
extern __inline__ __attribute__ ((__gnu_inline__)) SCM scm_words (scm_t_bits car, uint32_t n_words);



extern __inline__ __attribute__ ((__gnu_inline__)) SCM
scm_cell (scm_t_bits car, scm_t_bits cdr)
{
  SCM cell = (((SCM) ((scm_t_bits) (scm_gc_malloc (sizeof (scm_t_cell), 
# 161 "../libguile/gc.h" 3 4
            ((void *)0)
# 161 "../libguile/gc.h"
            )))));





  (((((SCM *)((scm_t_cell *) (((scm_t_bits *) (((scm_t_bits) (0? (*(volatile SCM *)0=((cell))): (cell)))))))) [(1)]) = (((SCM) (cdr)))));
  (((((SCM *)((scm_t_cell *) (((scm_t_bits *) (((scm_t_bits) (0? (*(volatile SCM *)0=((cell))): (cell)))))))) [(0)]) = (((SCM) (car)))));

  return cell;
}

extern __inline__ __attribute__ ((__gnu_inline__)) SCM
scm_double_cell (scm_t_bits car, scm_t_bits cbr,
   scm_t_bits ccr, scm_t_bits cdr)
{
  SCM z;

  z = (((SCM) ((scm_t_bits) (scm_gc_malloc (2 * sizeof (scm_t_cell), 
# 179 "../libguile/gc.h" 3 4
     ((void *)0)
# 179 "../libguile/gc.h"
     )))));





  (((((SCM *)((scm_t_cell *) (((scm_t_bits *) (((scm_t_bits) (0? (*(volatile SCM *)0=((z))): (z)))))))) [(1)]) = (((SCM) (cbr)))));
  (((((SCM *)((scm_t_cell *) (((scm_t_bits *) (((scm_t_bits) (0? (*(volatile SCM *)0=((z))): (z)))))))) [(2)]) = (((SCM) (ccr)))));
  (((((SCM *)((scm_t_cell *) (((scm_t_bits *) (((scm_t_bits) (0? (*(volatile SCM *)0=((z))): (z)))))))) [(3)]) = (((SCM) (cdr)))));
  (((((SCM *)((scm_t_cell *) (((scm_t_bits *) (((scm_t_bits) (0? (*(volatile SCM *)0=((z))): (z)))))))) [(0)]) = (((SCM) (car)))));
# 203 "../libguile/gc.h"
  __asm__ volatile ("" : : : "memory");






  return z;
}

extern __inline__ __attribute__ ((__gnu_inline__)) SCM
scm_words (scm_t_bits car, uint32_t n_words)
{
  SCM z;

  z = (((SCM) ((scm_t_bits) (scm_gc_malloc (sizeof (scm_t_bits) * n_words, 
# 218 "../libguile/gc.h" 3 4
     ((void *)0)
# 218 "../libguile/gc.h"
     )))));
  (((((SCM *)((scm_t_cell *) (((scm_t_bits *) (((scm_t_bits) (0? (*(volatile SCM *)0=((z))): (z)))))))) [(0)]) = (((SCM) (car)))));
# 236 "../libguile/gc.h"
  __asm__ volatile ("" : : : "memory");






  return z;
}



extern void scm_remember_upto_here_1 (SCM obj);
extern void scm_remember_upto_here_2 (SCM obj1, SCM obj2);
extern void scm_remember_upto_here (SCM obj1, ...);
# 276 "../libguile/gc.h"
extern SCM scm_return_first (SCM elt, ...);
extern int scm_return_first_int (int x, ...);
extern SCM scm_permanent_object (SCM obj);
extern SCM scm_gc_protect_object (SCM obj);
extern SCM scm_gc_unprotect_object (SCM obj);
extern void scm_gc_register_root (SCM *p);
extern void scm_gc_unregister_root (SCM *p);
extern void scm_gc_register_roots (SCM *b, unsigned long n);
extern void scm_gc_unregister_roots (SCM *b, unsigned long n);
extern void scm_gc_after_nonlocal_exit (void);
extern void scm_storage_prehistory (void);
extern void scm_init_gc_protect_object (void);
extern void scm_init_gc (void);
# 27 "pairs.h" 2
# 143 "pairs.h"
extern __inline__ __attribute__ ((__gnu_inline__)) int scm_is_pair (SCM x);
extern __inline__ __attribute__ ((__gnu_inline__)) SCM scm_cons (SCM x, SCM y);
extern __inline__ __attribute__ ((__gnu_inline__)) SCM scm_car (SCM x);
extern __inline__ __attribute__ ((__gnu_inline__)) SCM scm_cdr (SCM x);





extern __inline__ __attribute__ ((__gnu_inline__)) SCM
scm_cons (SCM x, SCM y)
{
  return scm_cell (((scm_t_bits) (0? (*(volatile SCM *)0=(x)): x)), ((scm_t_bits) (0? (*(volatile SCM *)0=(y)): y)));
}

extern __inline__ __attribute__ ((__gnu_inline__)) int
scm_is_pair (SCM x)
{
# 182 "pairs.h"
  return (!(6 & ((scm_t_bits) (0? (*(volatile SCM *)0=(x)): x))) && ((1 & (((scm_t_bits) (0? (*(volatile SCM *)0=((((SCM *)((scm_t_cell *) (((scm_t_bits *) (((scm_t_bits) (0? (*(volatile SCM *)0=((((x))))): (((x)))))))))) [((0))]))): (((SCM *)((scm_t_cell *) (((scm_t_bits *) (((scm_t_bits) (0? (*(volatile SCM *)0=((((x))))): (((x)))))))))) [((0))]))))) == 0));
}

extern __inline__ __attribute__ ((__gnu_inline__)) SCM
scm_car (SCM x)
{
  if (__builtin_expect ((!scm_is_pair (x)), 0))
    scm_wrong_type_arg_msg ("car", 0, x, "pair");
  return (((((SCM *)((scm_t_cell *) (((scm_t_bits *) (((scm_t_bits) (0? (*(volatile SCM *)0=(((x)))): ((x))))))))) [(0)])));
}

extern __inline__ __attribute__ ((__gnu_inline__)) SCM
scm_cdr (SCM x)
{
  if (__builtin_expect ((!scm_is_pair (x)), 0))
    scm_wrong_type_arg_msg ("cdr", 0, x, "pair");
  return (((((SCM *)((scm_t_cell *) (((scm_t_bits *) (((scm_t_bits) (0? (*(volatile SCM *)0=(((x)))): ((x))))))))) [(1)])));
}
# 226 "pairs.h"
extern SCM scm_cons2 (SCM w, SCM x, SCM y);
extern SCM scm_pair_p (SCM x);
extern SCM scm_set_car_x (SCM pair, SCM value);
extern SCM scm_set_cdr_x (SCM pair, SCM value);

extern SCM scm_cddr (SCM x);
extern SCM scm_cdar (SCM x);
extern SCM scm_cadr (SCM x);
extern SCM scm_caar (SCM x);
extern SCM scm_cdddr (SCM x);
extern SCM scm_cddar (SCM x);
extern SCM scm_cdadr (SCM x);
extern SCM scm_cdaar (SCM x);
extern SCM scm_caddr (SCM x);
extern SCM scm_cadar (SCM x);
extern SCM scm_caadr (SCM x);
extern SCM scm_caaar (SCM x);
extern SCM scm_cddddr (SCM x);
extern SCM scm_cdddar (SCM x);
extern SCM scm_cddadr (SCM x);
extern SCM scm_cddaar (SCM x);
extern SCM scm_cdaddr (SCM x);
extern SCM scm_cdadar (SCM x);
extern SCM scm_cdaadr (SCM x);
extern SCM scm_cdaaar (SCM x);
extern SCM scm_cadddr (SCM x);
extern SCM scm_caddar (SCM x);
extern SCM scm_cadadr (SCM x);
extern SCM scm_cadaar (SCM x);
extern SCM scm_caaddr (SCM x);
extern SCM scm_caadar (SCM x);
extern SCM scm_caaadr (SCM x);
extern SCM scm_caaaar (SCM x);

extern void scm_init_pairs (void);
# 34 "weak-vector.c" 2
# 1 "vectors.h" 1
# 25 "vectors.h"
# 1 "../libguile/array-handle.h" 1
# 27 "../libguile/array-handle.h"
# 1 "../libguile/numbers.h" 1
# 25 "../libguile/numbers.h"
# 1 "/usr/include/gmp.h" 1 3 4
# 59 "/usr/include/gmp.h" 3 4
# 1 "/usr/include/gmp-x86_64.h" 1 3 4
# 55 "/usr/include/gmp-x86_64.h" 3 4
# 1 "/usr/lib/gcc/x86_64-redhat-linux/9/include/stddef.h" 1 3 4
# 56 "/usr/include/gmp-x86_64.h" 2 3 4
# 1 "/usr/lib/gcc/x86_64-redhat-linux/9/include/limits.h" 1 3 4
# 57 "/usr/include/gmp-x86_64.h" 2 3 4
# 141 "/usr/include/gmp-x86_64.h" 3 4

# 141 "/usr/include/gmp-x86_64.h" 3 4
typedef unsigned long int mp_limb_t;
typedef long int mp_limb_signed_t;


typedef unsigned long int mp_bitcnt_t;




typedef struct
{
  int _mp_alloc;

  int _mp_size;


  mp_limb_t *_mp_d;
} __mpz_struct;




typedef __mpz_struct MP_INT;
typedef __mpz_struct mpz_t[1];

typedef mp_limb_t * mp_ptr;
typedef const mp_limb_t * mp_srcptr;







typedef long int mp_size_t;
typedef long int mp_exp_t;


typedef struct
{
  __mpz_struct _mp_num;
  __mpz_struct _mp_den;
} __mpq_struct;

typedef __mpq_struct MP_RAT;
typedef __mpq_struct mpq_t[1];

typedef struct
{
  int _mp_prec;



  int _mp_size;


  mp_exp_t _mp_exp;
  mp_limb_t *_mp_d;
} __mpf_struct;


typedef __mpf_struct mpf_t[1];


typedef enum
{
  GMP_RAND_ALG_DEFAULT = 0,
  GMP_RAND_ALG_LC = GMP_RAND_ALG_DEFAULT
} gmp_randalg_t;


typedef struct
{
  mpz_t _mp_seed;
  gmp_randalg_t _mp_alg;
  union {
    void *_mp_lc;
  } _mp_algdata;
} __gmp_randstate_struct;
typedef __gmp_randstate_struct gmp_randstate_t[1];



typedef const __mpz_struct *mpz_srcptr;
typedef __mpz_struct *mpz_ptr;
typedef const __mpf_struct *mpf_srcptr;
typedef __mpf_struct *mpf_ptr;
typedef const __mpq_struct *mpq_srcptr;
typedef __mpq_struct *mpq_ptr;
# 473 "/usr/include/gmp-x86_64.h" 3 4
 void __gmp_set_memory_functions (void *(*) (size_t),
          void *(*) (void *, size_t, size_t),
          void (*) (void *, size_t)) ;


 void __gmp_get_memory_functions (void *(**) (size_t),
          void *(**) (void *, size_t, size_t),
          void (**) (void *, size_t)) ;


 extern const int __gmp_bits_per_limb;


 extern int __gmp_errno;


 extern const char * const __gmp_version;






 void __gmp_randinit (gmp_randstate_t, gmp_randalg_t, ...);


 void __gmp_randinit_default (gmp_randstate_t);


 void __gmp_randinit_lc_2exp (gmp_randstate_t, mpz_srcptr, unsigned long int, mp_bitcnt_t);


 int __gmp_randinit_lc_2exp_size (gmp_randstate_t, mp_bitcnt_t);


 void __gmp_randinit_mt (gmp_randstate_t);


 void __gmp_randinit_set (gmp_randstate_t, const __gmp_randstate_struct *);


 void __gmp_randseed (gmp_randstate_t, mpz_srcptr);


 void __gmp_randseed_ui (gmp_randstate_t, unsigned long int);


 void __gmp_randclear (gmp_randstate_t);


 unsigned long __gmp_urandomb_ui (gmp_randstate_t, unsigned long);


 unsigned long __gmp_urandomm_ui (gmp_randstate_t, unsigned long);





 int __gmp_asprintf (char **, const char *, ...);



 int __gmp_fprintf (FILE *, const char *, ...);
# 550 "/usr/include/gmp-x86_64.h" 3 4
 int __gmp_printf (const char *, ...);


 int __gmp_snprintf (char *, size_t, const char *, ...);


 int __gmp_sprintf (char *, const char *, ...);
# 588 "/usr/include/gmp-x86_64.h" 3 4
 int __gmp_fscanf (FILE *, const char *, ...);



 int __gmp_scanf (const char *, ...);


 int __gmp_sscanf (const char *, const char *, ...);
# 617 "/usr/include/gmp-x86_64.h" 3 4
 void *__gmpz_realloc (mpz_ptr, mp_size_t);



 void __gmpz_abs (mpz_ptr, mpz_srcptr);



 void __gmpz_add (mpz_ptr, mpz_srcptr, mpz_srcptr);


 void __gmpz_add_ui (mpz_ptr, mpz_srcptr, unsigned long int);


 void __gmpz_addmul (mpz_ptr, mpz_srcptr, mpz_srcptr);


 void __gmpz_addmul_ui (mpz_ptr, mpz_srcptr, unsigned long int);


 void __gmpz_and (mpz_ptr, mpz_srcptr, mpz_srcptr);


 void __gmpz_array_init (mpz_ptr, mp_size_t, mp_size_t);


 void __gmpz_bin_ui (mpz_ptr, mpz_srcptr, unsigned long int);


 void __gmpz_bin_uiui (mpz_ptr, unsigned long int, unsigned long int);


 void __gmpz_cdiv_q (mpz_ptr, mpz_srcptr, mpz_srcptr);


 void __gmpz_cdiv_q_2exp (mpz_ptr, mpz_srcptr, mp_bitcnt_t);


 unsigned long int __gmpz_cdiv_q_ui (mpz_ptr, mpz_srcptr, unsigned long int);


 void __gmpz_cdiv_qr (mpz_ptr, mpz_ptr, mpz_srcptr, mpz_srcptr);


 unsigned long int __gmpz_cdiv_qr_ui (mpz_ptr, mpz_ptr, mpz_srcptr, unsigned long int);


 void __gmpz_cdiv_r (mpz_ptr, mpz_srcptr, mpz_srcptr);


 void __gmpz_cdiv_r_2exp (mpz_ptr, mpz_srcptr, mp_bitcnt_t);


 unsigned long int __gmpz_cdiv_r_ui (mpz_ptr, mpz_srcptr, unsigned long int);


 unsigned long int __gmpz_cdiv_ui (mpz_srcptr, unsigned long int) __attribute__ ((__pure__));


 void __gmpz_clear (mpz_ptr);


 void __gmpz_clears (mpz_ptr, ...);


 void __gmpz_clrbit (mpz_ptr, mp_bitcnt_t);


 int __gmpz_cmp (mpz_srcptr, mpz_srcptr) __attribute__ ((__pure__));


 int __gmpz_cmp_d (mpz_srcptr, double) __attribute__ ((__pure__));


 int __gmpz_cmp_si (mpz_srcptr, signed long int) __attribute__ ((__pure__));


 int __gmpz_cmp_ui (mpz_srcptr, unsigned long int) __attribute__ ((__pure__));


 int __gmpz_cmpabs (mpz_srcptr, mpz_srcptr) __attribute__ ((__pure__));


 int __gmpz_cmpabs_d (mpz_srcptr, double) __attribute__ ((__pure__));


 int __gmpz_cmpabs_ui (mpz_srcptr, unsigned long int) __attribute__ ((__pure__));


 void __gmpz_com (mpz_ptr, mpz_srcptr);


 void __gmpz_combit (mpz_ptr, mp_bitcnt_t);


 int __gmpz_congruent_p (mpz_srcptr, mpz_srcptr, mpz_srcptr) __attribute__ ((__pure__));


 int __gmpz_congruent_2exp_p (mpz_srcptr, mpz_srcptr, mp_bitcnt_t) __attribute__ ((__pure__));


 int __gmpz_congruent_ui_p (mpz_srcptr, unsigned long, unsigned long) __attribute__ ((__pure__));


 void __gmpz_divexact (mpz_ptr, mpz_srcptr, mpz_srcptr);


 void __gmpz_divexact_ui (mpz_ptr, mpz_srcptr, unsigned long);


 int __gmpz_divisible_p (mpz_srcptr, mpz_srcptr) __attribute__ ((__pure__));


 int __gmpz_divisible_ui_p (mpz_srcptr, unsigned long) __attribute__ ((__pure__));


 int __gmpz_divisible_2exp_p (mpz_srcptr, mp_bitcnt_t) __attribute__ ((__pure__));


 void __gmpz_dump (mpz_srcptr);


 void *__gmpz_export (void *, size_t *, int, size_t, int, size_t, mpz_srcptr);


 void __gmpz_fac_ui (mpz_ptr, unsigned long int);


 void __gmpz_2fac_ui (mpz_ptr, unsigned long int);


 void __gmpz_mfac_uiui (mpz_ptr, unsigned long int, unsigned long int);


 void __gmpz_primorial_ui (mpz_ptr, unsigned long int);


 void __gmpz_fdiv_q (mpz_ptr, mpz_srcptr, mpz_srcptr);


 void __gmpz_fdiv_q_2exp (mpz_ptr, mpz_srcptr, mp_bitcnt_t);


 unsigned long int __gmpz_fdiv_q_ui (mpz_ptr, mpz_srcptr, unsigned long int);


 void __gmpz_fdiv_qr (mpz_ptr, mpz_ptr, mpz_srcptr, mpz_srcptr);


 unsigned long int __gmpz_fdiv_qr_ui (mpz_ptr, mpz_ptr, mpz_srcptr, unsigned long int);


 void __gmpz_fdiv_r (mpz_ptr, mpz_srcptr, mpz_srcptr);


 void __gmpz_fdiv_r_2exp (mpz_ptr, mpz_srcptr, mp_bitcnt_t);


 unsigned long int __gmpz_fdiv_r_ui (mpz_ptr, mpz_srcptr, unsigned long int);


 unsigned long int __gmpz_fdiv_ui (mpz_srcptr, unsigned long int) __attribute__ ((__pure__));


 void __gmpz_fib_ui (mpz_ptr, unsigned long int);


 void __gmpz_fib2_ui (mpz_ptr, mpz_ptr, unsigned long int);


 int __gmpz_fits_sint_p (mpz_srcptr) __attribute__ ((__pure__));


 int __gmpz_fits_slong_p (mpz_srcptr) __attribute__ ((__pure__));


 int __gmpz_fits_sshort_p (mpz_srcptr) __attribute__ ((__pure__));



 int __gmpz_fits_uint_p (mpz_srcptr) __attribute__ ((__pure__));




 int __gmpz_fits_ulong_p (mpz_srcptr) __attribute__ ((__pure__));




 int __gmpz_fits_ushort_p (mpz_srcptr) __attribute__ ((__pure__));



 void __gmpz_gcd (mpz_ptr, mpz_srcptr, mpz_srcptr);


 unsigned long int __gmpz_gcd_ui (mpz_ptr, mpz_srcptr, unsigned long int);


 void __gmpz_gcdext (mpz_ptr, mpz_ptr, mpz_ptr, mpz_srcptr, mpz_srcptr);


 double __gmpz_get_d (mpz_srcptr) __attribute__ ((__pure__));


 double __gmpz_get_d_2exp (signed long int *, mpz_srcptr);


 long int __gmpz_get_si (mpz_srcptr) __attribute__ ((__pure__));


 char *__gmpz_get_str (char *, int, mpz_srcptr);



 unsigned long int __gmpz_get_ui (mpz_srcptr) __attribute__ ((__pure__));




 mp_limb_t __gmpz_getlimbn (mpz_srcptr, mp_size_t) __attribute__ ((__pure__));



 mp_bitcnt_t __gmpz_hamdist (mpz_srcptr, mpz_srcptr) __attribute__ ((__pure__));


 void __gmpz_import (mpz_ptr, size_t, int, size_t, int, size_t, const void *);


 void __gmpz_init (mpz_ptr);


 void __gmpz_init2 (mpz_ptr, mp_bitcnt_t);


 void __gmpz_inits (mpz_ptr, ...);


 void __gmpz_init_set (mpz_ptr, mpz_srcptr);


 void __gmpz_init_set_d (mpz_ptr, double);


 void __gmpz_init_set_si (mpz_ptr, signed long int);


 int __gmpz_init_set_str (mpz_ptr, const char *, int);


 void __gmpz_init_set_ui (mpz_ptr, unsigned long int);



 size_t __gmpz_inp_raw (mpz_ptr, FILE *);




 size_t __gmpz_inp_str (mpz_ptr, FILE *, int);



 int __gmpz_invert (mpz_ptr, mpz_srcptr, mpz_srcptr);


 void __gmpz_ior (mpz_ptr, mpz_srcptr, mpz_srcptr);


 int __gmpz_jacobi (mpz_srcptr, mpz_srcptr) __attribute__ ((__pure__));




 int __gmpz_kronecker_si (mpz_srcptr, long) __attribute__ ((__pure__));


 int __gmpz_kronecker_ui (mpz_srcptr, unsigned long) __attribute__ ((__pure__));


 int __gmpz_si_kronecker (long, mpz_srcptr) __attribute__ ((__pure__));


 int __gmpz_ui_kronecker (unsigned long, mpz_srcptr) __attribute__ ((__pure__));


 void __gmpz_lcm (mpz_ptr, mpz_srcptr, mpz_srcptr);


 void __gmpz_lcm_ui (mpz_ptr, mpz_srcptr, unsigned long);




 void __gmpz_lucnum_ui (mpz_ptr, unsigned long int);


 void __gmpz_lucnum2_ui (mpz_ptr, mpz_ptr, unsigned long int);


 int __gmpz_millerrabin (mpz_srcptr, int) __attribute__ ((__pure__));


 void __gmpz_mod (mpz_ptr, mpz_srcptr, mpz_srcptr);




 void __gmpz_mul (mpz_ptr, mpz_srcptr, mpz_srcptr);


 void __gmpz_mul_2exp (mpz_ptr, mpz_srcptr, mp_bitcnt_t);


 void __gmpz_mul_si (mpz_ptr, mpz_srcptr, long int);


 void __gmpz_mul_ui (mpz_ptr, mpz_srcptr, unsigned long int);



 void __gmpz_neg (mpz_ptr, mpz_srcptr);



 void __gmpz_nextprime (mpz_ptr, mpz_srcptr);



 size_t __gmpz_out_raw (FILE *, mpz_srcptr);




 size_t __gmpz_out_str (FILE *, int, mpz_srcptr);



 int __gmpz_perfect_power_p (mpz_srcptr) __attribute__ ((__pure__));



 int __gmpz_perfect_square_p (mpz_srcptr) __attribute__ ((__pure__));




 mp_bitcnt_t __gmpz_popcount (mpz_srcptr) __attribute__ ((__pure__));



 void __gmpz_pow_ui (mpz_ptr, mpz_srcptr, unsigned long int);


 void __gmpz_powm (mpz_ptr, mpz_srcptr, mpz_srcptr, mpz_srcptr);


 void __gmpz_powm_sec (mpz_ptr, mpz_srcptr, mpz_srcptr, mpz_srcptr);


 void __gmpz_powm_ui (mpz_ptr, mpz_srcptr, unsigned long int, mpz_srcptr);


 int __gmpz_probab_prime_p (mpz_srcptr, int) __attribute__ ((__pure__));


 void __gmpz_random (mpz_ptr, mp_size_t);


 void __gmpz_random2 (mpz_ptr, mp_size_t);


 void __gmpz_realloc2 (mpz_ptr, mp_bitcnt_t);


 mp_bitcnt_t __gmpz_remove (mpz_ptr, mpz_srcptr, mpz_srcptr);


 int __gmpz_root (mpz_ptr, mpz_srcptr, unsigned long int);


 void __gmpz_rootrem (mpz_ptr, mpz_ptr, mpz_srcptr, unsigned long int);


 void __gmpz_rrandomb (mpz_ptr, gmp_randstate_t, mp_bitcnt_t);


 mp_bitcnt_t __gmpz_scan0 (mpz_srcptr, mp_bitcnt_t) __attribute__ ((__pure__));


 mp_bitcnt_t __gmpz_scan1 (mpz_srcptr, mp_bitcnt_t) __attribute__ ((__pure__));


 void __gmpz_set (mpz_ptr, mpz_srcptr);


 void __gmpz_set_d (mpz_ptr, double);


 void __gmpz_set_f (mpz_ptr, mpf_srcptr);



 void __gmpz_set_q (mpz_ptr, mpq_srcptr);



 void __gmpz_set_si (mpz_ptr, signed long int);


 int __gmpz_set_str (mpz_ptr, const char *, int);


 void __gmpz_set_ui (mpz_ptr, unsigned long int);


 void __gmpz_setbit (mpz_ptr, mp_bitcnt_t);



 size_t __gmpz_size (mpz_srcptr) __attribute__ ((__pure__));



 size_t __gmpz_sizeinbase (mpz_srcptr, int) __attribute__ ((__pure__));


 void __gmpz_sqrt (mpz_ptr, mpz_srcptr);


 void __gmpz_sqrtrem (mpz_ptr, mpz_ptr, mpz_srcptr);


 void __gmpz_sub (mpz_ptr, mpz_srcptr, mpz_srcptr);


 void __gmpz_sub_ui (mpz_ptr, mpz_srcptr, unsigned long int);


 void __gmpz_ui_sub (mpz_ptr, unsigned long int, mpz_srcptr);


 void __gmpz_submul (mpz_ptr, mpz_srcptr, mpz_srcptr);


 void __gmpz_submul_ui (mpz_ptr, mpz_srcptr, unsigned long int);


 void __gmpz_swap (mpz_ptr, mpz_ptr) ;


 unsigned long int __gmpz_tdiv_ui (mpz_srcptr, unsigned long int) __attribute__ ((__pure__));


 void __gmpz_tdiv_q (mpz_ptr, mpz_srcptr, mpz_srcptr);


 void __gmpz_tdiv_q_2exp (mpz_ptr, mpz_srcptr, mp_bitcnt_t);


 unsigned long int __gmpz_tdiv_q_ui (mpz_ptr, mpz_srcptr, unsigned long int);


 void __gmpz_tdiv_qr (mpz_ptr, mpz_ptr, mpz_srcptr, mpz_srcptr);


 unsigned long int __gmpz_tdiv_qr_ui (mpz_ptr, mpz_ptr, mpz_srcptr, unsigned long int);


 void __gmpz_tdiv_r (mpz_ptr, mpz_srcptr, mpz_srcptr);


 void __gmpz_tdiv_r_2exp (mpz_ptr, mpz_srcptr, mp_bitcnt_t);


 unsigned long int __gmpz_tdiv_r_ui (mpz_ptr, mpz_srcptr, unsigned long int);


 int __gmpz_tstbit (mpz_srcptr, mp_bitcnt_t) __attribute__ ((__pure__));


 void __gmpz_ui_pow_ui (mpz_ptr, unsigned long int, unsigned long int);


 void __gmpz_urandomb (mpz_ptr, gmp_randstate_t, mp_bitcnt_t);


 void __gmpz_urandomm (mpz_ptr, gmp_randstate_t, mpz_srcptr);



 void __gmpz_xor (mpz_ptr, mpz_srcptr, mpz_srcptr);


 mp_srcptr __gmpz_limbs_read (mpz_srcptr);


 mp_ptr __gmpz_limbs_write (mpz_ptr, mp_size_t);


 mp_ptr __gmpz_limbs_modify (mpz_ptr, mp_size_t);


 void __gmpz_limbs_finish (mpz_ptr, mp_size_t);


 mpz_srcptr __gmpz_roinit_n (mpz_ptr, mp_srcptr, mp_size_t);







 void __gmpq_abs (mpq_ptr, mpq_srcptr);



 void __gmpq_add (mpq_ptr, mpq_srcptr, mpq_srcptr);


 void __gmpq_canonicalize (mpq_ptr);


 void __gmpq_clear (mpq_ptr);


 void __gmpq_clears (mpq_ptr, ...);


 int __gmpq_cmp (mpq_srcptr, mpq_srcptr) __attribute__ ((__pure__));


 int __gmpq_cmp_si (mpq_srcptr, long, unsigned long) __attribute__ ((__pure__));


 int __gmpq_cmp_ui (mpq_srcptr, unsigned long int, unsigned long int) __attribute__ ((__pure__));


 int __gmpq_cmp_z (mpq_srcptr, mpz_srcptr) __attribute__ ((__pure__));


 void __gmpq_div (mpq_ptr, mpq_srcptr, mpq_srcptr);


 void __gmpq_div_2exp (mpq_ptr, mpq_srcptr, mp_bitcnt_t);


 int __gmpq_equal (mpq_srcptr, mpq_srcptr) __attribute__ ((__pure__));


 void __gmpq_get_num (mpz_ptr, mpq_srcptr);


 void __gmpq_get_den (mpz_ptr, mpq_srcptr);


 double __gmpq_get_d (mpq_srcptr) __attribute__ ((__pure__));


 char *__gmpq_get_str (char *, int, mpq_srcptr);


 void __gmpq_init (mpq_ptr);


 void __gmpq_inits (mpq_ptr, ...);



 size_t __gmpq_inp_str (mpq_ptr, FILE *, int);



 void __gmpq_inv (mpq_ptr, mpq_srcptr);


 void __gmpq_mul (mpq_ptr, mpq_srcptr, mpq_srcptr);


 void __gmpq_mul_2exp (mpq_ptr, mpq_srcptr, mp_bitcnt_t);



 void __gmpq_neg (mpq_ptr, mpq_srcptr);




 size_t __gmpq_out_str (FILE *, int, mpq_srcptr);



 void __gmpq_set (mpq_ptr, mpq_srcptr);


 void __gmpq_set_d (mpq_ptr, double);


 void __gmpq_set_den (mpq_ptr, mpz_srcptr);


 void __gmpq_set_f (mpq_ptr, mpf_srcptr);


 void __gmpq_set_num (mpq_ptr, mpz_srcptr);


 void __gmpq_set_si (mpq_ptr, signed long int, unsigned long int);


 int __gmpq_set_str (mpq_ptr, const char *, int);


 void __gmpq_set_ui (mpq_ptr, unsigned long int, unsigned long int);


 void __gmpq_set_z (mpq_ptr, mpz_srcptr);


 void __gmpq_sub (mpq_ptr, mpq_srcptr, mpq_srcptr);


 void __gmpq_swap (mpq_ptr, mpq_ptr) ;





 void __gmpf_abs (mpf_ptr, mpf_srcptr);


 void __gmpf_add (mpf_ptr, mpf_srcptr, mpf_srcptr);


 void __gmpf_add_ui (mpf_ptr, mpf_srcptr, unsigned long int);

 void __gmpf_ceil (mpf_ptr, mpf_srcptr);


 void __gmpf_clear (mpf_ptr);


 void __gmpf_clears (mpf_ptr, ...);


 int __gmpf_cmp (mpf_srcptr, mpf_srcptr) __attribute__ ((__pure__));


 int __gmpf_cmp_z (mpf_srcptr, mpz_srcptr) __attribute__ ((__pure__));


 int __gmpf_cmp_d (mpf_srcptr, double) __attribute__ ((__pure__));


 int __gmpf_cmp_si (mpf_srcptr, signed long int) __attribute__ ((__pure__));


 int __gmpf_cmp_ui (mpf_srcptr, unsigned long int) __attribute__ ((__pure__));


 void __gmpf_div (mpf_ptr, mpf_srcptr, mpf_srcptr);


 void __gmpf_div_2exp (mpf_ptr, mpf_srcptr, mp_bitcnt_t);


 void __gmpf_div_ui (mpf_ptr, mpf_srcptr, unsigned long int);


 void __gmpf_dump (mpf_srcptr);


 int __gmpf_eq (mpf_srcptr, mpf_srcptr, mp_bitcnt_t) __attribute__ ((__pure__));


 int __gmpf_fits_sint_p (mpf_srcptr) __attribute__ ((__pure__));


 int __gmpf_fits_slong_p (mpf_srcptr) __attribute__ ((__pure__));


 int __gmpf_fits_sshort_p (mpf_srcptr) __attribute__ ((__pure__));


 int __gmpf_fits_uint_p (mpf_srcptr) __attribute__ ((__pure__));


 int __gmpf_fits_ulong_p (mpf_srcptr) __attribute__ ((__pure__));


 int __gmpf_fits_ushort_p (mpf_srcptr) __attribute__ ((__pure__));


 void __gmpf_floor (mpf_ptr, mpf_srcptr);


 double __gmpf_get_d (mpf_srcptr) __attribute__ ((__pure__));


 double __gmpf_get_d_2exp (signed long int *, mpf_srcptr);


 mp_bitcnt_t __gmpf_get_default_prec (void) __attribute__ ((__pure__));


 mp_bitcnt_t __gmpf_get_prec (mpf_srcptr) __attribute__ ((__pure__));


 long __gmpf_get_si (mpf_srcptr) __attribute__ ((__pure__));


 char *__gmpf_get_str (char *, mp_exp_t *, int, size_t, mpf_srcptr);


 unsigned long __gmpf_get_ui (mpf_srcptr) __attribute__ ((__pure__));


 void __gmpf_init (mpf_ptr);


 void __gmpf_init2 (mpf_ptr, mp_bitcnt_t);


 void __gmpf_inits (mpf_ptr, ...);


 void __gmpf_init_set (mpf_ptr, mpf_srcptr);


 void __gmpf_init_set_d (mpf_ptr, double);


 void __gmpf_init_set_si (mpf_ptr, signed long int);


 int __gmpf_init_set_str (mpf_ptr, const char *, int);


 void __gmpf_init_set_ui (mpf_ptr, unsigned long int);



 size_t __gmpf_inp_str (mpf_ptr, FILE *, int);



 int __gmpf_integer_p (mpf_srcptr) __attribute__ ((__pure__));


 void __gmpf_mul (mpf_ptr, mpf_srcptr, mpf_srcptr);


 void __gmpf_mul_2exp (mpf_ptr, mpf_srcptr, mp_bitcnt_t);


 void __gmpf_mul_ui (mpf_ptr, mpf_srcptr, unsigned long int);


 void __gmpf_neg (mpf_ptr, mpf_srcptr);



 size_t __gmpf_out_str (FILE *, int, size_t, mpf_srcptr);



 void __gmpf_pow_ui (mpf_ptr, mpf_srcptr, unsigned long int);


 void __gmpf_random2 (mpf_ptr, mp_size_t, mp_exp_t);


 void __gmpf_reldiff (mpf_ptr, mpf_srcptr, mpf_srcptr);


 void __gmpf_set (mpf_ptr, mpf_srcptr);


 void __gmpf_set_d (mpf_ptr, double);


 void __gmpf_set_default_prec (mp_bitcnt_t) ;


 void __gmpf_set_prec (mpf_ptr, mp_bitcnt_t);


 void __gmpf_set_prec_raw (mpf_ptr, mp_bitcnt_t) ;


 void __gmpf_set_q (mpf_ptr, mpq_srcptr);


 void __gmpf_set_si (mpf_ptr, signed long int);


 int __gmpf_set_str (mpf_ptr, const char *, int);


 void __gmpf_set_ui (mpf_ptr, unsigned long int);


 void __gmpf_set_z (mpf_ptr, mpz_srcptr);


 size_t __gmpf_size (mpf_srcptr) __attribute__ ((__pure__));


 void __gmpf_sqrt (mpf_ptr, mpf_srcptr);


 void __gmpf_sqrt_ui (mpf_ptr, unsigned long int);


 void __gmpf_sub (mpf_ptr, mpf_srcptr, mpf_srcptr);


 void __gmpf_sub_ui (mpf_ptr, mpf_srcptr, unsigned long int);


 void __gmpf_swap (mpf_ptr, mpf_ptr) ;


 void __gmpf_trunc (mpf_ptr, mpf_srcptr);


 void __gmpf_ui_div (mpf_ptr, unsigned long int, mpf_srcptr);


 void __gmpf_ui_sub (mpf_ptr, unsigned long int, mpf_srcptr);


 void __gmpf_urandomb (mpf_t, gmp_randstate_t, mp_bitcnt_t);
# 1461 "/usr/include/gmp-x86_64.h" 3 4
 mp_limb_t __gmpn_add (mp_ptr, mp_srcptr, mp_size_t, mp_srcptr, mp_size_t);




 mp_limb_t __gmpn_add_1 (mp_ptr, mp_srcptr, mp_size_t, mp_limb_t) ;



 mp_limb_t __gmpn_add_n (mp_ptr, mp_srcptr, mp_srcptr, mp_size_t);


 mp_limb_t __gmpn_addmul_1 (mp_ptr, mp_srcptr, mp_size_t, mp_limb_t);



 int __gmpn_cmp (mp_srcptr, mp_srcptr, mp_size_t) __attribute__ ((__pure__));




 int __gmpn_zero_p (mp_srcptr, mp_size_t) __attribute__ ((__pure__));



 void __gmpn_divexact_1 (mp_ptr, mp_srcptr, mp_size_t, mp_limb_t);





 mp_limb_t __gmpn_divexact_by3c (mp_ptr, mp_srcptr, mp_size_t, mp_limb_t);





 mp_limb_t __gmpn_divrem (mp_ptr, mp_size_t, mp_ptr, mp_size_t, mp_srcptr, mp_size_t);


 mp_limb_t __gmpn_divrem_1 (mp_ptr, mp_size_t, mp_srcptr, mp_size_t, mp_limb_t);


 mp_limb_t __gmpn_divrem_2 (mp_ptr, mp_size_t, mp_ptr, mp_size_t, mp_srcptr);


 mp_limb_t __gmpn_div_qr_1 (mp_ptr, mp_limb_t *, mp_srcptr, mp_size_t, mp_limb_t);


 mp_limb_t __gmpn_div_qr_2 (mp_ptr, mp_ptr, mp_srcptr, mp_size_t, mp_srcptr);


 mp_size_t __gmpn_gcd (mp_ptr, mp_ptr, mp_size_t, mp_ptr, mp_size_t);


 mp_limb_t __gmpn_gcd_1 (mp_srcptr, mp_size_t, mp_limb_t) __attribute__ ((__pure__));


 mp_limb_t __gmpn_gcdext_1 (mp_limb_signed_t *, mp_limb_signed_t *, mp_limb_t, mp_limb_t);


 mp_size_t __gmpn_gcdext (mp_ptr, mp_ptr, mp_size_t *, mp_ptr, mp_size_t, mp_ptr, mp_size_t);


 size_t __gmpn_get_str (unsigned char *, int, mp_ptr, mp_size_t);


 mp_bitcnt_t __gmpn_hamdist (mp_srcptr, mp_srcptr, mp_size_t) __attribute__ ((__pure__));


 mp_limb_t __gmpn_lshift (mp_ptr, mp_srcptr, mp_size_t, unsigned int);


 mp_limb_t __gmpn_mod_1 (mp_srcptr, mp_size_t, mp_limb_t) __attribute__ ((__pure__));


 mp_limb_t __gmpn_mul (mp_ptr, mp_srcptr, mp_size_t, mp_srcptr, mp_size_t);


 mp_limb_t __gmpn_mul_1 (mp_ptr, mp_srcptr, mp_size_t, mp_limb_t);


 void __gmpn_mul_n (mp_ptr, mp_srcptr, mp_srcptr, mp_size_t);


 void __gmpn_sqr (mp_ptr, mp_srcptr, mp_size_t);



 mp_limb_t __gmpn_neg (mp_ptr, mp_srcptr, mp_size_t);



 void __gmpn_com (mp_ptr, mp_srcptr, mp_size_t);


 int __gmpn_perfect_square_p (mp_srcptr, mp_size_t) __attribute__ ((__pure__));


 int __gmpn_perfect_power_p (mp_srcptr, mp_size_t) __attribute__ ((__pure__));


 mp_bitcnt_t __gmpn_popcount (mp_srcptr, mp_size_t) __attribute__ ((__pure__));


 mp_size_t __gmpn_pow_1 (mp_ptr, mp_srcptr, mp_size_t, mp_limb_t, mp_ptr);



 mp_limb_t __gmpn_preinv_mod_1 (mp_srcptr, mp_size_t, mp_limb_t, mp_limb_t) __attribute__ ((__pure__));


 void __gmpn_random (mp_ptr, mp_size_t);


 void __gmpn_random2 (mp_ptr, mp_size_t);


 mp_limb_t __gmpn_rshift (mp_ptr, mp_srcptr, mp_size_t, unsigned int);


 mp_bitcnt_t __gmpn_scan0 (mp_srcptr, mp_bitcnt_t) __attribute__ ((__pure__));


 mp_bitcnt_t __gmpn_scan1 (mp_srcptr, mp_bitcnt_t) __attribute__ ((__pure__));


 mp_size_t __gmpn_set_str (mp_ptr, const unsigned char *, size_t, int);


 size_t __gmpn_sizeinbase (mp_srcptr, mp_size_t, int);


 mp_size_t __gmpn_sqrtrem (mp_ptr, mp_ptr, mp_srcptr, mp_size_t);



 mp_limb_t __gmpn_sub (mp_ptr, mp_srcptr, mp_size_t, mp_srcptr, mp_size_t);




 mp_limb_t __gmpn_sub_1 (mp_ptr, mp_srcptr, mp_size_t, mp_limb_t) ;



 mp_limb_t __gmpn_sub_n (mp_ptr, mp_srcptr, mp_srcptr, mp_size_t);


 mp_limb_t __gmpn_submul_1 (mp_ptr, mp_srcptr, mp_size_t, mp_limb_t);


 void __gmpn_tdiv_qr (mp_ptr, mp_ptr, mp_size_t, mp_srcptr, mp_size_t, mp_srcptr, mp_size_t);


 void __gmpn_and_n (mp_ptr, mp_srcptr, mp_srcptr, mp_size_t);

 void __gmpn_andn_n (mp_ptr, mp_srcptr, mp_srcptr, mp_size_t);

 void __gmpn_nand_n (mp_ptr, mp_srcptr, mp_srcptr, mp_size_t);

 void __gmpn_ior_n (mp_ptr, mp_srcptr, mp_srcptr, mp_size_t);

 void __gmpn_iorn_n (mp_ptr, mp_srcptr, mp_srcptr, mp_size_t);

 void __gmpn_nior_n (mp_ptr, mp_srcptr, mp_srcptr, mp_size_t);

 void __gmpn_xor_n (mp_ptr, mp_srcptr, mp_srcptr, mp_size_t);

 void __gmpn_xnor_n (mp_ptr, mp_srcptr, mp_srcptr, mp_size_t);


 void __gmpn_copyi (mp_ptr, mp_srcptr, mp_size_t);

 void __gmpn_copyd (mp_ptr, mp_srcptr, mp_size_t);

 void __gmpn_zero (mp_ptr, mp_size_t);


 mp_limb_t __gmpn_cnd_add_n (mp_limb_t, mp_ptr, mp_srcptr, mp_srcptr, mp_size_t);

 mp_limb_t __gmpn_cnd_sub_n (mp_limb_t, mp_ptr, mp_srcptr, mp_srcptr, mp_size_t);


 mp_limb_t __gmpn_sec_add_1 (mp_ptr, mp_srcptr, mp_size_t, mp_limb_t, mp_ptr);

 mp_size_t __gmpn_sec_add_1_itch (mp_size_t) __attribute__ ((__pure__));


 mp_limb_t __gmpn_sec_sub_1 (mp_ptr, mp_srcptr, mp_size_t, mp_limb_t, mp_ptr);

 mp_size_t __gmpn_sec_sub_1_itch (mp_size_t) __attribute__ ((__pure__));


 void __gmpn_cnd_swap (mp_limb_t, volatile mp_limb_t *, volatile mp_limb_t *, mp_size_t);


 void __gmpn_sec_mul (mp_ptr, mp_srcptr, mp_size_t, mp_srcptr, mp_size_t, mp_ptr);

 mp_size_t __gmpn_sec_mul_itch (mp_size_t, mp_size_t) __attribute__ ((__pure__));


 void __gmpn_sec_sqr (mp_ptr, mp_srcptr, mp_size_t, mp_ptr);

 mp_size_t __gmpn_sec_sqr_itch (mp_size_t) __attribute__ ((__pure__));


 void __gmpn_sec_powm (mp_ptr, mp_srcptr, mp_size_t, mp_srcptr, mp_bitcnt_t, mp_srcptr, mp_size_t, mp_ptr);

 mp_size_t __gmpn_sec_powm_itch (mp_size_t, mp_bitcnt_t, mp_size_t) __attribute__ ((__pure__));


 void __gmpn_sec_tabselect (volatile mp_limb_t *, volatile const mp_limb_t *, mp_size_t, mp_size_t, mp_size_t);


 mp_limb_t __gmpn_sec_div_qr (mp_ptr, mp_ptr, mp_size_t, mp_srcptr, mp_size_t, mp_ptr);

 mp_size_t __gmpn_sec_div_qr_itch (mp_size_t, mp_size_t) __attribute__ ((__pure__));

 void __gmpn_sec_div_r (mp_ptr, mp_size_t, mp_srcptr, mp_size_t, mp_ptr);

 mp_size_t __gmpn_sec_div_r_itch (mp_size_t, mp_size_t) __attribute__ ((__pure__));


 int __gmpn_sec_invert (mp_ptr, mp_ptr, mp_srcptr, mp_size_t, mp_bitcnt_t, mp_ptr);

 mp_size_t __gmpn_sec_invert_itch (mp_size_t) __attribute__ ((__pure__));
# 1707 "/usr/include/gmp-x86_64.h" 3 4
extern __inline__ __attribute__ ((__gnu_inline__)) void
__gmpz_abs (mpz_ptr __gmp_w, mpz_srcptr __gmp_u)
{
  if (__gmp_w != __gmp_u)
    __gmpz_set (__gmp_w, __gmp_u);
  __gmp_w->_mp_size = ((__gmp_w->_mp_size) >= 0 ? (__gmp_w->_mp_size) : -(__gmp_w->_mp_size));
}
# 1731 "/usr/include/gmp-x86_64.h" 3 4
extern __inline__ __attribute__ ((__gnu_inline__))

int
__gmpz_fits_uint_p (mpz_srcptr __gmp_z)
{
  mp_size_t __gmp_n = __gmp_z->_mp_size; mp_ptr __gmp_p = __gmp_z->_mp_d; return (__gmp_n == 0 || (__gmp_n == 1 && __gmp_p[0] <= (0x7fffffff * 2U + 1U)));;
}




extern __inline__ __attribute__ ((__gnu_inline__))

int
__gmpz_fits_ulong_p (mpz_srcptr __gmp_z)
{
  mp_size_t __gmp_n = __gmp_z->_mp_size; mp_ptr __gmp_p = __gmp_z->_mp_d; return (__gmp_n == 0 || (__gmp_n == 1 && __gmp_p[0] <= (0x7fffffffffffffffL * 2UL + 1UL)));;
}




extern __inline__ __attribute__ ((__gnu_inline__))

int
__gmpz_fits_ushort_p (mpz_srcptr __gmp_z)
{
  mp_size_t __gmp_n = __gmp_z->_mp_size; mp_ptr __gmp_p = __gmp_z->_mp_d; return (__gmp_n == 0 || (__gmp_n == 1 && __gmp_p[0] <= (0x7fff * 2 + 1)));;
}




extern __inline__ __attribute__ ((__gnu_inline__))

unsigned long
__gmpz_get_ui (mpz_srcptr __gmp_z)
{
  mp_ptr __gmp_p = __gmp_z->_mp_d;
  mp_size_t __gmp_n = __gmp_z->_mp_size;
  mp_limb_t __gmp_l = __gmp_p[0];






  return (__gmp_n != 0 ? __gmp_l : 0);
# 1787 "/usr/include/gmp-x86_64.h" 3 4
}




extern __inline__ __attribute__ ((__gnu_inline__))

mp_limb_t
__gmpz_getlimbn (mpz_srcptr __gmp_z, mp_size_t __gmp_n)
{
  mp_limb_t __gmp_result = 0;
  if (__builtin_expect ((__gmp_n >= 0 && __gmp_n < ((__gmp_z->_mp_size) >= 0 ? (__gmp_z->_mp_size) : -(__gmp_z->_mp_size))) != 0, 1))
    __gmp_result = __gmp_z->_mp_d[__gmp_n];
  return __gmp_result;
}



extern __inline__ __attribute__ ((__gnu_inline__)) void
__gmpz_neg (mpz_ptr __gmp_w, mpz_srcptr __gmp_u)
{
  if (__gmp_w != __gmp_u)
    __gmpz_set (__gmp_w, __gmp_u);
  __gmp_w->_mp_size = - __gmp_w->_mp_size;
}




extern __inline__ __attribute__ ((__gnu_inline__))

int
__gmpz_perfect_square_p (mpz_srcptr __gmp_a)
{
  mp_size_t __gmp_asize;
  int __gmp_result;

  __gmp_asize = __gmp_a->_mp_size;
  __gmp_result = (__gmp_asize >= 0);
  if (__builtin_expect ((__gmp_asize > 0) != 0, 1))
    __gmp_result = __gmpn_perfect_square_p (__gmp_a->_mp_d, __gmp_asize);
  return __gmp_result;
}




extern __inline__ __attribute__ ((__gnu_inline__))

mp_bitcnt_t
__gmpz_popcount (mpz_srcptr __gmp_u)
{
  mp_size_t __gmp_usize;
  mp_bitcnt_t __gmp_result;

  __gmp_usize = __gmp_u->_mp_size;
  __gmp_result = (__gmp_usize < 0 ? (0x7fffffffffffffffL * 2UL + 1UL) : 0);
  if (__builtin_expect ((__gmp_usize > 0) != 0, 1))
    __gmp_result = __gmpn_popcount (__gmp_u->_mp_d, __gmp_usize);
  return __gmp_result;
}




extern __inline__ __attribute__ ((__gnu_inline__))

void
__gmpz_set_q (mpz_ptr __gmp_w, mpq_srcptr __gmp_u)
{
  __gmpz_tdiv_q (__gmp_w, (&((__gmp_u)->_mp_num)), (&((__gmp_u)->_mp_den)));
}




extern __inline__ __attribute__ ((__gnu_inline__))

size_t
__gmpz_size (mpz_srcptr __gmp_z)
{
  return ((__gmp_z->_mp_size) >= 0 ? (__gmp_z->_mp_size) : -(__gmp_z->_mp_size));
}






extern __inline__ __attribute__ ((__gnu_inline__)) void
__gmpq_abs (mpq_ptr __gmp_w, mpq_srcptr __gmp_u)
{
  if (__gmp_w != __gmp_u)
    __gmpq_set (__gmp_w, __gmp_u);
  __gmp_w->_mp_num._mp_size = ((__gmp_w->_mp_num._mp_size) >= 0 ? (__gmp_w->_mp_num._mp_size) : -(__gmp_w->_mp_num._mp_size));
}



extern __inline__ __attribute__ ((__gnu_inline__)) void
__gmpq_neg (mpq_ptr __gmp_w, mpq_srcptr __gmp_u)
{
  if (__gmp_w != __gmp_u)
    __gmpq_set (__gmp_w, __gmp_u);
  __gmp_w->_mp_num._mp_size = - __gmp_w->_mp_num._mp_size;
}
# 2129 "/usr/include/gmp-x86_64.h" 3 4
extern __inline__ __attribute__ ((__gnu_inline__))

mp_limb_t
__gmpn_add (mp_ptr __gmp_wp, mp_srcptr __gmp_xp, mp_size_t __gmp_xsize, mp_srcptr __gmp_yp, mp_size_t __gmp_ysize)
{
  mp_limb_t __gmp_c;
  do { mp_size_t __gmp_i; mp_limb_t __gmp_x; __gmp_i = (__gmp_ysize); if (__gmp_i != 0) { if (__gmpn_add_n (__gmp_wp, __gmp_xp, __gmp_yp, __gmp_i)) { do { if (__gmp_i >= (__gmp_xsize)) { (__gmp_c) = 1; goto __gmp_done; } __gmp_x = (__gmp_xp)[__gmp_i]; } while ((((__gmp_wp)[__gmp_i++] = (__gmp_x + 1) & ((~ ((mp_limb_t) (0))) >> 0)) == 0)); } } if ((__gmp_wp) != (__gmp_xp)) do { mp_size_t __gmp_j; ; for (__gmp_j = (__gmp_i); __gmp_j < (__gmp_xsize); __gmp_j++) (__gmp_wp)[__gmp_j] = (__gmp_xp)[__gmp_j]; } while (0); (__gmp_c) = 0; __gmp_done: ; } while (0);
  return __gmp_c;
}




extern __inline__ __attribute__ ((__gnu_inline__))

mp_limb_t
__gmpn_add_1 (mp_ptr __gmp_dst, mp_srcptr __gmp_src, mp_size_t __gmp_size, mp_limb_t __gmp_n)
{
  mp_limb_t __gmp_c;
  do { mp_size_t __gmp_i; mp_limb_t __gmp_x, __gmp_r; __gmp_x = (__gmp_src)[0]; __gmp_r = __gmp_x + (__gmp_n); (__gmp_dst)[0] = __gmp_r; if (((__gmp_r) < ((__gmp_n)))) { (__gmp_c) = 1; for (__gmp_i = 1; __gmp_i < (__gmp_size);) { __gmp_x = (__gmp_src)[__gmp_i]; __gmp_r = __gmp_x + 1; (__gmp_dst)[__gmp_i] = __gmp_r; ++__gmp_i; if (!((__gmp_r) < (1))) { if ((__gmp_src) != (__gmp_dst)) do { mp_size_t __gmp_j; ; for (__gmp_j = (__gmp_i); __gmp_j < (__gmp_size); __gmp_j++) (__gmp_dst)[__gmp_j] = (__gmp_src)[__gmp_j]; } while (0); (__gmp_c) = 0; break; } } } else { if ((__gmp_src) != (__gmp_dst)) do { mp_size_t __gmp_j; ; for (__gmp_j = (1); __gmp_j < (__gmp_size); __gmp_j++) (__gmp_dst)[__gmp_j] = (__gmp_src)[__gmp_j]; } while (0); (__gmp_c) = 0; } } while (0);
  return __gmp_c;
}




extern __inline__ __attribute__ ((__gnu_inline__))

int
__gmpn_cmp (mp_srcptr __gmp_xp, mp_srcptr __gmp_yp, mp_size_t __gmp_size)
{
  int __gmp_result;
  do { mp_size_t __gmp_i; mp_limb_t __gmp_x, __gmp_y; (__gmp_result) = 0; __gmp_i = (__gmp_size); while (--__gmp_i >= 0) { __gmp_x = (__gmp_xp)[__gmp_i]; __gmp_y = (__gmp_yp)[__gmp_i]; if (__gmp_x != __gmp_y) { (__gmp_result) = (__gmp_x > __gmp_y ? 1 : -1); break; } } } while (0);
  return __gmp_result;
}




extern __inline__ __attribute__ ((__gnu_inline__))

int
__gmpn_zero_p (mp_srcptr __gmp_p, mp_size_t __gmp_n)
{

    do {
      if (__gmp_p[--__gmp_n] != 0)
 return 0;
    } while (__gmp_n != 0);
  return 1;
}




extern __inline__ __attribute__ ((__gnu_inline__))

mp_limb_t
__gmpn_sub (mp_ptr __gmp_wp, mp_srcptr __gmp_xp, mp_size_t __gmp_xsize, mp_srcptr __gmp_yp, mp_size_t __gmp_ysize)
{
  mp_limb_t __gmp_c;
  do { mp_size_t __gmp_i; mp_limb_t __gmp_x; __gmp_i = (__gmp_ysize); if (__gmp_i != 0) { if (__gmpn_sub_n (__gmp_wp, __gmp_xp, __gmp_yp, __gmp_i)) { do { if (__gmp_i >= (__gmp_xsize)) { (__gmp_c) = 1; goto __gmp_done; } __gmp_x = (__gmp_xp)[__gmp_i]; } while ((((__gmp_wp)[__gmp_i++] = (__gmp_x - 1) & ((~ ((mp_limb_t) (0))) >> 0)), __gmp_x == 0)); } } if ((__gmp_wp) != (__gmp_xp)) do { mp_size_t __gmp_j; ; for (__gmp_j = (__gmp_i); __gmp_j < (__gmp_xsize); __gmp_j++) (__gmp_wp)[__gmp_j] = (__gmp_xp)[__gmp_j]; } while (0); (__gmp_c) = 0; __gmp_done: ; } while (0);
  return __gmp_c;
}




extern __inline__ __attribute__ ((__gnu_inline__))

mp_limb_t
__gmpn_sub_1 (mp_ptr __gmp_dst, mp_srcptr __gmp_src, mp_size_t __gmp_size, mp_limb_t __gmp_n)
{
  mp_limb_t __gmp_c;
  do { mp_size_t __gmp_i; mp_limb_t __gmp_x, __gmp_r; __gmp_x = (__gmp_src)[0]; __gmp_r = __gmp_x - (__gmp_n); (__gmp_dst)[0] = __gmp_r; if (((__gmp_x) < ((__gmp_n)))) { (__gmp_c) = 1; for (__gmp_i = 1; __gmp_i < (__gmp_size);) { __gmp_x = (__gmp_src)[__gmp_i]; __gmp_r = __gmp_x - 1; (__gmp_dst)[__gmp_i] = __gmp_r; ++__gmp_i; if (!((__gmp_x) < (1))) { if ((__gmp_src) != (__gmp_dst)) do { mp_size_t __gmp_j; ; for (__gmp_j = (__gmp_i); __gmp_j < (__gmp_size); __gmp_j++) (__gmp_dst)[__gmp_j] = (__gmp_src)[__gmp_j]; } while (0); (__gmp_c) = 0; break; } } } else { if ((__gmp_src) != (__gmp_dst)) do { mp_size_t __gmp_j; ; for (__gmp_j = (1); __gmp_j < (__gmp_size); __gmp_j++) (__gmp_dst)[__gmp_j] = (__gmp_src)[__gmp_j]; } while (0); (__gmp_c) = 0; } } while (0);
  return __gmp_c;
}




extern __inline__ __attribute__ ((__gnu_inline__))

mp_limb_t
__gmpn_neg (mp_ptr __gmp_rp, mp_srcptr __gmp_up, mp_size_t __gmp_n)
{
  while (*__gmp_up == 0)
    {
      *__gmp_rp = 0;
      if (!--__gmp_n)
 return 0;
      ++__gmp_up; ++__gmp_rp;
    }

  *__gmp_rp = (- *__gmp_up) & ((~ ((mp_limb_t) (0))) >> 0);

  if (--__gmp_n)
    __gmpn_com (++__gmp_rp, ++__gmp_up, __gmp_n);

  return 1;
}
# 2309 "/usr/include/gmp-x86_64.h" 3 4
enum
{
  GMP_ERROR_NONE = 0,
  GMP_ERROR_UNSUPPORTED_ARGUMENT = 1,
  GMP_ERROR_DIVISION_BY_ZERO = 2,
  GMP_ERROR_SQRT_OF_NEGATIVE = 4,
  GMP_ERROR_INVALID_ARGUMENT = 8
};
# 60 "/usr/include/gmp.h" 2 3 4
# 26 "../libguile/numbers.h" 2



# 1 "../libguile/print.h" 1
# 25 "../libguile/print.h"
# 1 "../libguile/chars.h" 1
# 66 "../libguile/chars.h"

# 66 "../libguile/chars.h"
extern SCM scm_char_p (SCM x);
extern SCM scm_char_eq_p (SCM x, SCM y);
extern SCM scm_char_less_p (SCM x, SCM y);
extern SCM scm_char_leq_p (SCM x, SCM y);
extern SCM scm_char_gr_p (SCM x, SCM y);
extern SCM scm_char_geq_p (SCM x, SCM y);
extern SCM scm_char_ci_eq_p (SCM x, SCM y);
extern SCM scm_char_ci_less_p (SCM x, SCM y);
extern SCM scm_char_ci_leq_p (SCM x, SCM y);
extern SCM scm_char_ci_gr_p (SCM x, SCM y);
extern SCM scm_char_ci_geq_p (SCM x, SCM y);
extern SCM scm_char_alphabetic_p (SCM chr);
extern SCM scm_char_numeric_p (SCM chr);
extern SCM scm_char_whitespace_p (SCM chr);
extern SCM scm_char_upper_case_p (SCM chr);
extern SCM scm_char_lower_case_p (SCM chr);
extern SCM scm_char_is_both_p (SCM chr);
extern SCM scm_char_to_integer (SCM chr);
extern SCM scm_integer_to_char (SCM n);
extern SCM scm_char_upcase (SCM chr);
extern SCM scm_char_downcase (SCM chr);
extern SCM scm_char_titlecase (SCM chr);
extern SCM scm_char_general_category (SCM chr);

extern __inline__ __attribute__ ((__gnu_inline__)) SCM scm_c_make_char (scm_t_wchar c);
extern scm_t_wchar scm_c_upcase (scm_t_wchar c);
extern scm_t_wchar scm_c_downcase (scm_t_wchar c);
extern scm_t_wchar scm_c_titlecase (scm_t_wchar c);

extern const char *scm_i_charname (SCM chr);
extern SCM scm_i_charname_to_char (const char *charname,
                                         size_t charname_len);
extern void scm_init_chars (void);


extern __inline__ __attribute__ ((__gnu_inline__)) SCM
scm_c_make_char (scm_t_wchar c)
{
  return ((c) <= 1 ? (((SCM) (((((scm_t_bits) (unsigned char) (c)) << 8) + scm_tc8_char)))) : (((SCM) (((((scm_t_bits) (c)) << 8) + scm_tc8_char)))));
}
# 26 "../libguile/print.h" 2


# 1 "../libguile/options.h" 1
# 29 "../libguile/options.h"
typedef struct scm_t_option
{
  unsigned int type;
  const char *name;
  scm_t_bits val;
  char *doc;
} scm_t_option;







extern SCM scm_options_try (SCM args, scm_t_option options[], const char *s, int dry_run);
extern SCM scm_options (SCM, scm_t_option [], const char*);
extern void scm_init_opts (SCM (*) (SCM), scm_t_option []);
extern void scm_init_options (void);
# 29 "../libguile/print.h" 2
# 64 "../libguile/print.h"
typedef struct scm_print_state {
  SCM handle;
  int revealed;
  unsigned long writingp;
  unsigned long fancyp;
  unsigned long level;
  unsigned long length;
  SCM hot_ref;
  unsigned long list_offset;
  unsigned long top;
  unsigned long ceiling;
  SCM ref_vect;


  SCM highlight_objects;
} scm_print_state;

extern SCM scm_print_state_vtable;

extern scm_t_bits scm_tc16_port_with_ps;

extern SCM scm_print_options (SCM setting);
extern SCM scm_make_print_state (void);
extern void scm_free_print_state (SCM print_state);
extern SCM scm_i_port_with_print_state (SCM port, SCM print_state);
extern void scm_intprint (intmax_t n, int radix, SCM port);
extern void scm_uintprint (uintmax_t n, int radix, SCM port);
extern void scm_ipruk (char *hdr, SCM ptr, SCM port);
extern void scm_iprlist (char *hdr, SCM exp, int tlr, SCM port, scm_print_state *pstate);
extern void scm_print_symbol_name (const char *str, size_t len, SCM port);
extern void scm_prin1 (SCM exp, SCM port, int writingp);
extern void scm_iprin1 (SCM exp, SCM port, scm_print_state *pstate);
extern SCM scm_write (SCM obj, SCM port);
extern SCM scm_display (SCM obj, SCM port);
extern SCM scm_simple_format (SCM port, SCM message, SCM args);
extern SCM scm_newline (SCM port);
extern SCM scm_write_char (SCM chr, SCM port);
extern SCM scm_printer_apply (SCM proc, SCM exp, SCM port, scm_print_state *);
extern SCM scm_port_with_print_state (SCM port, SCM pstate);
extern SCM scm_get_print_state (SCM port);
extern int scm_valid_oport_value_p (SCM val);
extern void scm_init_print (void);
# 30 "../libguile/numbers.h" 2
# 40 "../libguile/numbers.h"
typedef long scm_t_inum;
# 99 "../libguile/numbers.h"
# 1 "/usr/lib/gcc/x86_64-redhat-linux/9/include/float.h" 1 3 4
# 100 "../libguile/numbers.h" 2
# 166 "../libguile/numbers.h"
typedef struct scm_t_double
{
  SCM type;



  double real;
} scm_t_double;

typedef struct scm_t_complex
{
  SCM type;



  double real;
  double imag;
} scm_t_complex;




extern SCM scm_exact_p (SCM x);
extern int scm_is_exact (SCM x);
extern SCM scm_odd_p (SCM n);
extern SCM scm_even_p (SCM n);
extern SCM scm_finite_p (SCM x);
extern SCM scm_inf_p (SCM x);
extern SCM scm_nan_p (SCM x);
extern SCM scm_inf (void);
extern SCM scm_nan (void);
extern SCM scm_abs (SCM x);
extern SCM scm_quotient (SCM x, SCM y);
extern SCM scm_remainder (SCM x, SCM y);
extern SCM scm_modulo (SCM x, SCM y);
extern void scm_euclidean_divide (SCM x, SCM y, SCM *q, SCM *r);
extern SCM scm_euclidean_quotient (SCM x, SCM y);
extern SCM scm_euclidean_remainder (SCM x, SCM y);
extern void scm_floor_divide (SCM x, SCM y, SCM *q, SCM *r);
extern SCM scm_floor_quotient (SCM x, SCM y);
extern SCM scm_floor_remainder (SCM x, SCM y);
extern void scm_ceiling_divide (SCM x, SCM y, SCM *q, SCM *r);
extern SCM scm_ceiling_quotient (SCM x, SCM y);
extern SCM scm_ceiling_remainder (SCM x, SCM y);
extern void scm_truncate_divide (SCM x, SCM y, SCM *q, SCM *r);
extern SCM scm_truncate_quotient (SCM x, SCM y);
extern SCM scm_truncate_remainder (SCM x, SCM y);
extern void scm_centered_divide (SCM x, SCM y, SCM *q, SCM *r);
extern SCM scm_centered_quotient (SCM x, SCM y);
extern SCM scm_centered_remainder (SCM x, SCM y);
extern void scm_round_divide (SCM x, SCM y, SCM *q, SCM *r);
extern SCM scm_round_quotient (SCM x, SCM y);
extern SCM scm_round_remainder (SCM x, SCM y);
extern SCM scm_gcd (SCM x, SCM y);
extern SCM scm_lcm (SCM n1, SCM n2);
extern SCM scm_logand (SCM n1, SCM n2);
extern SCM scm_logior (SCM n1, SCM n2);
extern SCM scm_logxor (SCM n1, SCM n2);
extern SCM scm_logtest (SCM n1, SCM n2);
extern SCM scm_logbit_p (SCM n1, SCM n2);
extern SCM scm_lognot (SCM n);
extern SCM scm_modulo_expt (SCM n, SCM k, SCM m);
extern SCM scm_integer_expt (SCM z1, SCM z2);
extern SCM scm_ash (SCM n, SCM count);
extern SCM scm_round_ash (SCM n, SCM count);
extern SCM scm_bit_extract (SCM n, SCM start, SCM end);
extern SCM scm_logcount (SCM n);
extern SCM scm_integer_length (SCM n);

extern SCM scm_i_euclidean_divide (SCM x, SCM y);
extern SCM scm_i_floor_divide (SCM x, SCM y);
extern SCM scm_i_ceiling_divide (SCM x, SCM y);
extern SCM scm_i_truncate_divide (SCM x, SCM y);
extern SCM scm_i_centered_divide (SCM x, SCM y);
extern SCM scm_i_round_divide (SCM x, SCM y);

extern SCM scm_i_gcd (SCM x, SCM y, SCM rest);
extern SCM scm_i_lcm (SCM x, SCM y, SCM rest);
extern SCM scm_i_logand (SCM x, SCM y, SCM rest);
extern SCM scm_i_logior (SCM x, SCM y, SCM rest);
extern SCM scm_i_logxor (SCM x, SCM y, SCM rest);

extern size_t scm_iint2str (intmax_t num, int rad, char *p);
extern size_t scm_iuint2str (uintmax_t num, int rad, char *p);
extern SCM scm_number_to_string (SCM x, SCM radix);
extern int scm_print_real (SCM sexp, SCM port, scm_print_state *pstate);
extern int scm_print_complex (SCM sexp, SCM port, scm_print_state *pstate);
extern int scm_bigprint (SCM exp, SCM port, scm_print_state *pstate);
extern SCM scm_c_locale_stringn_to_number (const char *mem, size_t len,
         unsigned int radix);
extern SCM scm_i_string_to_number (SCM str, unsigned int radix);
extern SCM scm_string_to_number (SCM str, SCM radix);
extern SCM scm_bigequal (SCM x, SCM y);
extern SCM scm_real_equalp (SCM x, SCM y);
extern SCM scm_complex_equalp (SCM x, SCM y);
extern int scm_i_heap_numbers_equal_p (SCM x, SCM y);
extern SCM scm_number_p (SCM x);
extern SCM scm_complex_p (SCM x);
extern SCM scm_real_p (SCM x);
extern SCM scm_rational_p (SCM z);
extern SCM scm_integer_p (SCM x);
extern SCM scm_exact_integer_p (SCM x);
extern SCM scm_inexact_p (SCM x);
extern int scm_is_inexact (SCM x);
extern SCM scm_num_eq_p (SCM x, SCM y);
extern SCM scm_less_p (SCM x, SCM y);
extern SCM scm_gr_p (SCM x, SCM y);
extern SCM scm_leq_p (SCM x, SCM y);
extern SCM scm_geq_p (SCM x, SCM y);
extern SCM scm_zero_p (SCM z);
extern SCM scm_positive_p (SCM x);
extern SCM scm_negative_p (SCM x);
extern SCM scm_max (SCM x, SCM y);
extern SCM scm_min (SCM x, SCM y);
extern SCM scm_sum (SCM x, SCM y);
extern SCM scm_oneplus (SCM x);
extern SCM scm_difference (SCM x, SCM y);
extern SCM scm_oneminus (SCM x);
extern SCM scm_product (SCM x, SCM y);
extern SCM scm_divide (SCM x, SCM y);
extern SCM scm_floor (SCM x);
extern SCM scm_ceiling (SCM x);
extern double scm_c_truncate (double x);
extern double scm_c_round (double x);
extern SCM scm_truncate_number (SCM x);
extern SCM scm_round_number (SCM x);
extern SCM scm_expt (SCM z1, SCM z2);
extern SCM scm_sin (SCM z);
extern SCM scm_cos (SCM z);
extern SCM scm_tan (SCM z);
extern SCM scm_sinh (SCM z);
extern SCM scm_cosh (SCM z);
extern SCM scm_tanh (SCM z);
extern SCM scm_asin (SCM z);
extern SCM scm_acos (SCM z);
extern SCM scm_atan (SCM x, SCM y);
extern SCM scm_sys_asinh (SCM z);
extern SCM scm_sys_acosh (SCM z);
extern SCM scm_sys_atanh (SCM z);
extern SCM scm_make_rectangular (SCM z1, SCM z2);
extern SCM scm_make_polar (SCM z1, SCM z2);
extern SCM scm_real_part (SCM z);
extern SCM scm_imag_part (SCM z);
extern SCM scm_magnitude (SCM z);
extern SCM scm_angle (SCM z);
extern SCM scm_exact_to_inexact (SCM z);
extern SCM scm_inexact_to_exact (SCM z);
extern SCM scm_trunc (SCM x);
extern SCM scm_log (SCM z);
extern SCM scm_log10 (SCM z);
extern SCM scm_exp (SCM z);
extern SCM scm_sqrt (SCM z);
extern void scm_exact_integer_sqrt (SCM k, SCM *s, SCM *r);

extern SCM scm_i_min (SCM x, SCM y, SCM rest);
extern SCM scm_i_max (SCM x, SCM y, SCM rest);
extern SCM scm_i_sum (SCM x, SCM y, SCM rest);
extern SCM scm_i_difference (SCM x, SCM y, SCM rest);
extern SCM scm_i_product (SCM x, SCM y, SCM rest);
extern SCM scm_i_divide (SCM x, SCM y, SCM rest);
extern SCM scm_i_exact_integer_sqrt (SCM k);


extern SCM scm_i_mkbig (void);
extern SCM scm_i_normbig (SCM x);
extern int scm_i_bigcmp (SCM a, SCM b);
extern SCM scm_i_dbl2big (double d);
extern SCM scm_i_dbl2num (double d);
extern double scm_i_big2dbl (SCM b);
extern SCM scm_i_long2big (long n);
extern SCM scm_i_ulong2big (unsigned long n);
extern SCM scm_i_clonebig (SCM src_big, int same_sign_p);


extern SCM scm_rationalize (SCM x, SCM err);
extern SCM scm_numerator (SCM z);
extern SCM scm_denominator (SCM z);


extern double scm_i_fraction2double (SCM z);
extern SCM scm_i_fraction_equalp (SCM x, SCM y);
extern int scm_i_print_fraction (SCM sexp, SCM port, scm_print_state *pstate);


extern void scm_i_print_double (double val, SCM port);
extern void scm_i_print_complex (double real, double imag, SCM port);



extern int scm_is_integer (SCM val);
extern int scm_is_exact_integer (SCM val);
extern int scm_is_signed_integer (SCM val,
       intmax_t min, intmax_t max);
extern int scm_is_unsigned_integer (SCM val,
         uintmax_t min, uintmax_t max);

extern SCM scm_from_signed_integer (intmax_t val);
extern SCM scm_from_unsigned_integer (uintmax_t val);

extern intmax_t scm_to_signed_integer (SCM val,
         intmax_t min,
         intmax_t max);
extern uintmax_t scm_to_unsigned_integer (SCM val,
            uintmax_t min,
            uintmax_t max);

extern int8_t scm_to_int8 (SCM x);
extern SCM scm_from_int8 (int8_t x);

extern uint8_t scm_to_uint8 (SCM x);
extern SCM scm_from_uint8 (uint8_t x);

extern int16_t scm_to_int16 (SCM x);
extern SCM scm_from_int16 (int16_t x);

extern uint16_t scm_to_uint16 (SCM x);
extern SCM scm_from_uint16 (uint16_t x);

extern int32_t scm_to_int32 (SCM x);
extern SCM scm_from_int32 (int32_t x);

extern uint32_t scm_to_uint32 (SCM x);
extern SCM scm_from_uint32 (uint32_t x);

extern scm_t_wchar scm_to_wchar (SCM x);
extern SCM scm_from_wchar (scm_t_wchar x);

extern int64_t scm_to_int64 (SCM x);
extern SCM scm_from_int64 (int64_t x);

extern uint64_t scm_to_uint64 (SCM x);
extern SCM scm_from_uint64 (uint64_t x);

extern void scm_to_mpz (SCM x, mpz_t rop);
extern SCM scm_from_mpz (mpz_t rop);
# 571 "../libguile/numbers.h"
extern int scm_is_real (SCM val);
extern int scm_is_rational (SCM val);
extern double scm_to_double (SCM val);
extern SCM scm_from_double (double val);



extern int scm_is_complex (SCM val);
extern SCM scm_c_make_rectangular (double re, double im);
extern SCM scm_c_make_polar (double mag, double ang);
extern double scm_c_real_part (SCM z);
extern double scm_c_imag_part (SCM z);
extern double scm_c_magnitude (SCM z);
extern double scm_c_angle (SCM z);

extern int scm_is_number (SCM val);


extern int scm_install_gmp_memory_functions;

extern void scm_init_numbers (void);
# 28 "../libguile/array-handle.h" 2



typedef SCM (*scm_t_vector_ref) (SCM, size_t);
typedef void (*scm_t_vector_set) (SCM, size_t, SCM);

typedef struct scm_t_array_dim
{
  ssize_t lbnd;
  ssize_t ubnd;
  ssize_t inc;
} scm_t_array_dim;

typedef enum
  {
    SCM_ARRAY_ELEMENT_TYPE_SCM = 0,
    SCM_ARRAY_ELEMENT_TYPE_CHAR = 1,
    SCM_ARRAY_ELEMENT_TYPE_BIT = 2,
    SCM_ARRAY_ELEMENT_TYPE_VU8 = 3,
    SCM_ARRAY_ELEMENT_TYPE_U8 = 4,
    SCM_ARRAY_ELEMENT_TYPE_S8 = 5,
    SCM_ARRAY_ELEMENT_TYPE_U16 = 6,
    SCM_ARRAY_ELEMENT_TYPE_S16 = 7,
    SCM_ARRAY_ELEMENT_TYPE_U32 = 8,
    SCM_ARRAY_ELEMENT_TYPE_S32 = 9,
    SCM_ARRAY_ELEMENT_TYPE_U64 = 10,
    SCM_ARRAY_ELEMENT_TYPE_S64 = 11,
    SCM_ARRAY_ELEMENT_TYPE_F32 = 12,
    SCM_ARRAY_ELEMENT_TYPE_F64 = 13,
    SCM_ARRAY_ELEMENT_TYPE_C32 = 14,
    SCM_ARRAY_ELEMENT_TYPE_C64 = 15,
    SCM_ARRAY_ELEMENT_TYPE_LAST = 15
  } scm_t_array_element_type;

extern SCM scm_i_array_element_types[];


typedef struct scm_t_array_handle {
  SCM array;







  size_t base;
  size_t ndims;
  scm_t_array_dim *dims;
  scm_t_array_dim dim0;
  scm_t_array_element_type element_type;
  const void *elements;
  void *writable_elements;


  SCM vector;
  scm_t_vector_ref vref;
  scm_t_vector_set vset;
} scm_t_array_handle;




extern void scm_array_get_handle (SCM array, scm_t_array_handle *h);
extern ssize_t scm_array_handle_pos (scm_t_array_handle *h, SCM indices);
extern ssize_t scm_array_handle_pos_1 (scm_t_array_handle *h, ssize_t idx0);
extern ssize_t scm_array_handle_pos_2 (scm_t_array_handle *h, ssize_t idx0, ssize_t idx1);
extern SCM scm_array_handle_element_type (scm_t_array_handle *h);
extern void scm_array_handle_release (scm_t_array_handle *h);
extern const SCM* scm_array_handle_elements (scm_t_array_handle *h);
extern SCM* scm_array_handle_writable_elements (scm_t_array_handle *h);


extern __inline__ __attribute__ ((__gnu_inline__)) SCM scm_array_handle_ref (scm_t_array_handle *h, ssize_t pos);
extern __inline__ __attribute__ ((__gnu_inline__)) void scm_array_handle_set (scm_t_array_handle *h, ssize_t pos, SCM val);




extern __inline__ __attribute__ ((__gnu_inline__)) SCM
scm_array_handle_ref (scm_t_array_handle *h, ssize_t p)
{
  if (__builtin_expect ((p < 0 && ((size_t)-p) > h->base), 0))

    scm_out_of_range (
# 112 "../libguile/array-handle.h" 3 4
                     ((void *)0)
# 112 "../libguile/array-handle.h"
                         , scm_from_int64 (p));

  return h->vref (h->vector, h->base + p);
}

extern __inline__ __attribute__ ((__gnu_inline__)) void
scm_array_handle_set (scm_t_array_handle *h, ssize_t p, SCM v)
{
  if (__builtin_expect ((p < 0 && ((size_t)-p) > h->base), 0))

    scm_out_of_range (
# 122 "../libguile/array-handle.h" 3 4
                     ((void *)0)
# 122 "../libguile/array-handle.h"
                         , scm_from_int64 (p));

  h->vset (h->vector, h->base + p, v);
}




extern void scm_init_array_handle (void);
# 26 "vectors.h" 2





extern SCM scm_vector_p (SCM x);
extern SCM scm_vector_length (SCM v);
extern SCM scm_vector (SCM l);
extern SCM scm_vector_ref (SCM v, SCM k);
extern SCM scm_vector_set_x (SCM v, SCM k, SCM obj);
extern SCM scm_make_vector (SCM k, SCM fill);
extern SCM scm_vector_to_list (SCM v);
extern SCM scm_vector_fill_x (SCM v, SCM fill_x);
extern SCM scm_vector_move_left_x (SCM vec1, SCM start1, SCM end1,
        SCM vec2, SCM start2);
extern SCM scm_vector_move_right_x (SCM vec1, SCM start1, SCM end1,
         SCM vec2, SCM start2);
extern SCM scm_vector_copy (SCM vec);

extern int scm_is_vector (SCM obj);
extern int scm_is_simple_vector (SCM obj);
extern SCM scm_c_make_vector (size_t len, SCM fill);
extern size_t scm_c_vector_length (SCM vec);
extern SCM scm_c_vector_ref (SCM vec, size_t k);
extern void scm_c_vector_set_x (SCM vec, size_t k, SCM obj);
extern const SCM *scm_vector_elements (SCM vec,
     scm_t_array_handle *h,
     size_t *lenp, ssize_t *incp);
extern SCM *scm_vector_writable_elements (SCM vec,
        scm_t_array_handle *h,
        size_t *lenp, ssize_t *incp);
# 90 "vectors.h"
extern SCM scm_i_vector_equal_p (SCM x, SCM y);


extern void scm_init_vectors (void);
# 35 "weak-vector.c" 2
# 1 "version.h" 1
# 34 "version.h"
extern SCM scm_major_version (void);
extern SCM scm_minor_version (void);
extern SCM scm_micro_version (void);
extern SCM scm_effective_version (void);
extern SCM scm_version (void);
extern void scm_init_version (void);
# 36 "weak-vector.c" 2

# 1 "weak-vector.h" 1
# 32 "weak-vector.h"
extern SCM scm_make_weak_vector (SCM len, SCM fill);
extern SCM scm_weak_vector (SCM l);
extern SCM scm_weak_vector_p (SCM x);
extern SCM scm_weak_vector_length (SCM v);
extern SCM scm_weak_vector_ref (SCM v, SCM k);
extern SCM scm_weak_vector_set_x (SCM v, SCM k, SCM x);

extern SCM scm_c_make_weak_vector (size_t len, SCM fill);
extern int scm_is_weak_vector (SCM obj);
extern size_t scm_c_weak_vector_length (SCM vec);
extern SCM scm_c_weak_vector_ref (SCM v, size_t k);
extern void scm_c_weak_vector_set_x (SCM v, size_t k, SCM x);

extern void scm_init_weak_vectors (void);
# 38 "weak-vector.c" 2
# 47 "weak-vector.c"
SCM
scm_c_make_weak_vector (size_t len, SCM fill)

{
  SCM wv;
  size_t j;

  do { if (__builtin_expect ((!(len <= (
# 54 "weak-vector.c" 3 4
 (18446744073709551615UL) 
# 54 "weak-vector.c"
 >> 8))), 0)) scm_out_of_range_pos ("make-weak-vector", scm_from_uint64 (len), scm_from_int32 (1)); } while (0);

  if (((((scm_t_bits) (0? (*(volatile SCM *)0=((fill))): (fill))) == ((scm_t_bits) (0? (*(volatile SCM *)0=(((SCM) ((((((9)) << 8) + scm_tc8_flag)))))): ((SCM) ((((((9)) << 8) + scm_tc8_flag)))))))))
    fill = ((SCM) ((((((8)) << 8) + scm_tc8_flag))));

  wv = (((SCM) ((scm_t_bits) (scm_gc_malloc_pointerless ((len + 1) * sizeof (SCM), "weak vector")))))
                                                          ;

  (((((SCM *)((scm_t_cell *) (((scm_t_bits *) (((scm_t_bits) (0? (*(volatile SCM *)0=((((wv))))): (((wv)))))))))) [((0))]) = (((SCM) ((((len << 8) | 0x0f)))))));

  if (((!(6 & ((scm_t_bits) (0? (*(volatile SCM *)0=(fill)): fill))))))
    {
      memset (((&(((SCM *)((scm_t_cell *) (((scm_t_bits *) (((scm_t_bits) (0? (*(volatile SCM *)0=((wv))): (wv)))))))) [(1)]))), 0, len * sizeof (SCM));
      for (j = 0; j < len; j++)
        scm_c_weak_vector_set_x (wv, j, fill);
    }
  else
    for (j = 0; j < len; j++)
      ((((&(((SCM *)((scm_t_cell *) (((scm_t_bits *) (((scm_t_bits) (0? (*(volatile SCM *)0=((wv))): (wv)))))))) [(1)]))))[j]=(fill));

  return wv;
}


^^ { cname scm_make_weak_vector ^^ fname "make-weak-vector" ^^ type primitive ^^ location "weak-vector.c" 78 ^^ arglist (SCM size, SCM fill) ^^ argsig 1 1 0 ^^ "Return a weak vector with @var{size} elements. If the optional\n" "argument @var{fill} is given, all entries in the vector will be\n" "set to @var{fill}. The default value for @var{fill} is the\n" "empty list." ^^ }






{
  return scm_c_make_weak_vector (scm_to_uint64 (size), fill);
}



 ^^ { cname scm_weak_vector ^^ fname "list->weak-vector" ^^ type register ^^ location "weak-vector.c" 91 ^^ arglist () ^^ argsig 1 0 0 ^^ "implemented by the C function \"" "scm_weak_vector" "\"" ^^ };

^^ { cname scm_weak_vector ^^ fname "weak-vector" ^^ type primitive ^^ location "weak-vector.c" 93 ^^ arglist (SCM lst) ^^ argsig 0 0 1 ^^ "@deffnx {Scheme Procedure} list->weak-vector lst\n" "Construct a weak vector from a list: @code{weak-vector} uses\n" "the list of its arguments while @code{list->weak-vector} uses\n" "its only argument @var{l} (a list) to construct a weak vector\n" "the same way @code{list->vector} would." ^^ }







{
  SCM wv;
  size_t i;
  long c_size;

  do { c_size = scm_ilength (lst); ^^ argpos lst 1 106 ^^; } while (0);

  wv = scm_c_make_weak_vector ((size_t) c_size, ((SCM) ((((((0)) << 8) + scm_tc8_flag)))));

  for (i = 0; scm_is_pair (lst); lst = (((((SCM *)((scm_t_cell *) (((scm_t_bits *) (((scm_t_bits) (0? (*(volatile SCM *)0=(((lst)))): ((lst))))))))) [(1)]))), i++)
    scm_c_weak_vector_set_x (wv, i, (((((SCM *)((scm_t_cell *) (((scm_t_bits *) (((scm_t_bits) (0? (*(volatile SCM *)0=(((lst)))): ((lst))))))))) [(0)]))));

  return wv;
}



^^ { cname scm_weak_vector_p ^^ fname "weak-vector?" ^^ type primitive ^^ location "weak-vector.c" 118 ^^ arglist (SCM obj) ^^ argsig 1 0 0 ^^ "Return @code{#t} if @var{obj} is a weak vector. Note that all\n" "weak hashes are also weak vectors." ^^ }




{
  return ((scm_is_weak_vector (obj)) ? ((SCM) ((((((4)) << 8) + scm_tc8_flag)))) : ((SCM) ((((((0)) << 8) + scm_tc8_flag)))));
}



int
scm_is_weak_vector (SCM obj)

{
  return ((((!(6 & ((scm_t_bits) (0? (*(volatile SCM *)0=(obj)): obj)))) && (0x7f & (((scm_t_bits) (0? (*(volatile SCM *)0=((((SCM *)((scm_t_cell *) (((scm_t_bits *) (((scm_t_bits) (0? (*(volatile SCM *)0=((((obj))))): (((obj)))))))))) [((0))]))): (((SCM *)((scm_t_cell *) (((scm_t_bits *) (((scm_t_bits) (0? (*(volatile SCM *)0=((((obj))))): (((obj)))))))))) [((0))]))))) == (0x0f))));
}







^^ { cname scm_weak_vector_length ^^ fname "weak-vector-length" ^^ type primitive ^^ location "weak-vector.c" 142 ^^ arglist (SCM wvect) ^^ argsig 1 0 0 ^^ "Like @code{vector-length}, but for weak vectors." ^^ }



{
  return scm_from_uint64 (scm_c_weak_vector_length (wvect));
}



size_t
scm_c_weak_vector_length (SCM wvect)

{
  do { do { if (__builtin_expect ((!(((((!(6 & ((scm_t_bits) (0? (*(volatile SCM *)0=(wvect)): wvect)))) && (0x7f & (((scm_t_bits) (0? (*(volatile SCM *)0=((((SCM *)((scm_t_cell *) (((scm_t_bits *) (((scm_t_bits) (0? (*(volatile SCM *)0=((((wvect))))): (((wvect)))))))))) [((0))]))): (((SCM *)((scm_t_cell *) (((scm_t_bits *) (((scm_t_bits) (0? (*(volatile SCM *)0=((((wvect))))): (((wvect)))))))))) [((0))]))))) == (0x0f)))))), 0)) scm_wrong_type_arg_msg(s_scm_weak_vector_length, 1, wvect, "weak vector"); } while (0); } while (0);
  return (((size_t) (((scm_t_bits) (0? (*(volatile SCM *)0=((((SCM *)((scm_t_cell *) (((scm_t_bits *) (((scm_t_bits) (0? (*(volatile SCM *)0=((((wvect))))): (((wvect)))))))))) [((0))]))): (((SCM *)((scm_t_cell *) (((scm_t_bits *) (((scm_t_bits) (0? (*(volatile SCM *)0=((((wvect))))): (((wvect)))))))))) [((0))]))))) >> 8);
}



^^ { cname scm_weak_vector_ref ^^ fname "weak-vector-ref" ^^ type primitive ^^ location "weak-vector.c" 162 ^^ arglist (SCM wvect, SCM k) ^^ argsig 2 0 0 ^^ "Like @code{vector-ref}, but for weak vectors." ^^ }



{
  return scm_c_weak_vector_ref (wvect, scm_to_uint64 (k));
}



struct weak_vector_ref_data
{
  SCM wv;
  size_t k;
};

static void*
weak_vector_ref (void *data)
{
  struct weak_vector_ref_data *d = data;

  return (void *) ((scm_t_bits) (0? (*(volatile SCM *)0=(((((const SCM *) ((&(((SCM *)((scm_t_cell *) (((scm_t_bits *) (((scm_t_bits) (0? (*(volatile SCM *)0=((d->wv))): (d->wv)))))))) [(1)])))))[d->k]))): ((((const SCM *) ((&(((SCM *)((scm_t_cell *) (((scm_t_bits *) (((scm_t_bits) (0? (*(volatile SCM *)0=((d->wv))): (d->wv)))))))) [(1)])))))[d->k])));
}

SCM
scm_c_weak_vector_ref (SCM wv, size_t k)

{
  struct weak_vector_ref_data d;
  void *ret;

  do { do { if (__builtin_expect ((!(((((!(6 & ((scm_t_bits) (0? (*(volatile SCM *)0=(wv)): wv)))) && (0x7f & (((scm_t_bits) (0? (*(volatile SCM *)0=((((SCM *)((scm_t_cell *) (((scm_t_bits *) (((scm_t_bits) (0? (*(volatile SCM *)0=((((wv))))): (((wv)))))))))) [((0))]))): (((SCM *)((scm_t_cell *) (((scm_t_bits *) (((scm_t_bits) (0? (*(volatile SCM *)0=((((wv))))): (((wv)))))))))) [((0))]))))) == (0x0f)))))), 0)) scm_wrong_type_arg_msg(s_scm_weak_vector_ref, 1, wv, "weak vector"); } while (0); } while (0);

  d.wv = wv;
  d.k = k;

  if (k >= (((size_t) (((scm_t_bits) (0? (*(volatile SCM *)0=((((SCM *)((scm_t_cell *) (((scm_t_bits *) (((scm_t_bits) (0? (*(volatile SCM *)0=((((wv))))): (((wv)))))))))) [((0))]))): (((SCM *)((scm_t_cell *) (((scm_t_bits *) (((scm_t_bits) (0? (*(volatile SCM *)0=((((wv))))): (((wv)))))))))) [((0))]))))) >> 8))
    scm_out_of_range ("weak-vector-ref", scm_from_uint64 (k));

  ret = GC_call_with_alloc_lock (weak_vector_ref, &d);

  if (ret)
    return (((SCM) ((scm_t_bits) (ret))));
  else
    return ((SCM) ((((((0)) << 8) + scm_tc8_flag))));
}



^^ { cname scm_weak_vector_set_x ^^ fname "weak-vector-set!" ^^ type primitive ^^ location "weak-vector.c" 211 ^^ arglist (SCM wvect, SCM k, SCM obj) ^^ argsig 3 0 0 ^^ "Like @code{vector-set!}, but for weak vectors." ^^ }



{
  scm_c_weak_vector_set_x (wvect, scm_to_uint64 (k), obj);

  return ((SCM) ((((((8)) << 8) + scm_tc8_flag))));
}



void
scm_c_weak_vector_set_x (SCM wv, size_t k, SCM x)

{
  SCM *elts;
  struct weak_vector_ref_data d;
  void *prev;

  do { do { if (__builtin_expect ((!(((((!(6 & ((scm_t_bits) (0? (*(volatile SCM *)0=(wv)): wv)))) && (0x7f & (((scm_t_bits) (0? (*(volatile SCM *)0=((((SCM *)((scm_t_cell *) (((scm_t_bits *) (((scm_t_bits) (0? (*(volatile SCM *)0=((((wv))))): (((wv)))))))))) [((0))]))): (((SCM *)((scm_t_cell *) (((scm_t_bits *) (((scm_t_bits) (0? (*(volatile SCM *)0=((((wv))))): (((wv)))))))))) [((0))]))))) == (0x0f)))))), 0)) scm_wrong_type_arg_msg(s_scm_weak_vector_set_x, 1, wv, "weak vector"); } while (0); } while (0);

  d.wv = wv;
  d.k = k;

  if (k >= (((size_t) (((scm_t_bits) (0? (*(volatile SCM *)0=((((SCM *)((scm_t_cell *) (((scm_t_bits *) (((scm_t_bits) (0? (*(volatile SCM *)0=((((wv))))): (((wv)))))))))) [((0))]))): (((SCM *)((scm_t_cell *) (((scm_t_bits *) (((scm_t_bits) (0? (*(volatile SCM *)0=((((wv))))): (((wv)))))))))) [((0))]))))) >> 8))
    scm_out_of_range ("weak-vector-set!", scm_from_uint64 (k));

  prev = GC_call_with_alloc_lock (weak_vector_ref, &d);

  elts = ((&(((SCM *)((scm_t_cell *) (((scm_t_bits *) (((scm_t_bits) (0? (*(volatile SCM *)0=((wv))): (wv)))))))) [(1)])));

  if (prev && ((!(6 & ((scm_t_bits) (0? (*(volatile SCM *)0=((((SCM) ((scm_t_bits) (prev)))))): (((SCM) ((scm_t_bits) (prev))))))))))
    GC_unregister_disappearing_link ((void **) &elts[k]);

  elts[k] = x;

  if (((!(6 & ((scm_t_bits) (0? (*(volatile SCM *)0=(x)): x))))))
    SCM_I_REGISTER_DISAPPEARING_LINK ((void **) &elts[k],
                                      ((scm_t_cell *) (((scm_t_bits *) (((scm_t_bits) (0? (*(volatile SCM *)0=(x)): x)))))));
}




static void
scm_init_weak_vector_builtins (void)
{

# 1 "weak-vector.x" 1

scm_c_define_gsubr (s_scm_make_weak_vector, 1, 1, 0, (scm_t_subr) scm_make_weak_vector);;
scm_c_define_gsubr (s_list_to_weak_vector, 1, 0, 0, (scm_t_subr) scm_weak_vector);;
scm_c_define_gsubr (s_scm_weak_vector, 0, 0, 1, (scm_t_subr) scm_weak_vector);;
scm_c_define_gsubr (s_scm_weak_vector_p, 1, 0, 0, (scm_t_subr) scm_weak_vector_p);;
scm_c_define_gsubr (s_scm_weak_vector_length, 1, 0, 0, (scm_t_subr) scm_weak_vector_length);;
scm_c_define_gsubr (s_scm_weak_vector_ref, 2, 0, 0, (scm_t_subr) scm_weak_vector_ref);;
scm_c_define_gsubr (s_scm_weak_vector_set_x, 3, 0, 0, (scm_t_subr) scm_weak_vector_set_x);;
# 261 "weak-vector.c" 2

}

void
scm_init_weak_vectors ()
{
  scm_c_register_extension ("libguile-" "3.0",
                            "scm_init_weak_vector_builtins",
                            (scm_t_extension_init_func)scm_init_weak_vector_builtins,
                            
# 270 "weak-vector.c" 3 4
                           ((void *)0)
# 270 "weak-vector.c"
                               );
}
