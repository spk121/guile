dnl check for gcc's "labels as values" feature
AC_DEFUN([AC_C_LABELS_AS_VALUES],
[AC_CACHE_CHECK([labels as values], ac_cv_labels_as_values,
[AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
int foo(int);
int foo(int i) {
static void *label[] = { &&l1, &&l2 };
goto *label[i];
l1: return 1;
l2: return 2;
}
]],
[[foo(1);]])],
[ac_cv_labels_as_values=yes],
[ac_cv_labels_as_values=no])])
if test "$ac_cv_labels_as_values" = yes; then
AC_DEFINE([HAVE_LABELS_AS_VALUES], [],
          [Define if compiler supports gcc's "labels as values" (aka computed goto)
           feature, used to speed up instruction dispatch in the interpreter.])
fi
])
