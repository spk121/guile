SCM_BENCHMARKS =				\
  %D%/benchmarks/0-reference.bm			\
  %D%/benchmarks/arithmetic.bm			\
  %D%/benchmarks/bytevector-io.bm		\
  %D%/benchmarks/bytevectors.bm			\
  %D%/benchmarks/chars.bm			\
  %D%/benchmarks/continuations.bm		\
  %D%/benchmarks/hash.bm			\
  %D%/benchmarks/if.bm				\
  %D%/benchmarks/logand.bm			\
  %D%/benchmarks/ports.bm			\
  %D%/benchmarks/r6rs-arithmetic.bm		\
  %D%/benchmarks/read.bm			\
  %D%/benchmarks/srfi-1.bm			\
  %D%/benchmarks/srfi-13.bm			\
  %D%/benchmarks/structs.bm			\
  %D%/benchmarks/subr.bm			\
  %D%/benchmarks/vectors.bm			\
  %D%/benchmarks/vlists.bm			\
  %D%/benchmarks/write.bm			\
  %D%/benchmarks/strings.bm

EXTRA_DIST +=					\
  %D%/guile-benchmark				\
  %D%/benchmark-suite/lib.scm			\
  $(SCM_BENCHMARKS)				\
  %D%/ChangeLog-2008				\
  %D%/README
