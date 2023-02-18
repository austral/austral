#include <stdint.h>
#include <stddef.h>
#include <stdarg.h>

/*
 * Austral types
 */

typedef uint8_t   au_unit_t;
typedef uint8_t   au_bool_t;
typedef uint8_t   au_nat8_t;
typedef int8_t    au_int8_t;
typedef uint16_t  au_nat16_t;
typedef int16_t   au_int16_t;
typedef uint32_t  au_nat32_t;
typedef int32_t   au_int32_t;
typedef uint64_t  au_nat64_t;
typedef int64_t   au_int64_t;
typedef size_t    au_index_t;
typedef void*     au_fnptr_t;
typedef uint8_t   au_region_t;

#define nil   0
#define false 0
#define true  1

/*
 * A little hack
 */

#define AU_STORE(ptr, val) (*(ptr) = (val), nil)

/*
 * Pervasive
 */

typedef struct {
  void*  data;
  au_index_t size;
} au_array_t;

au_array_t au_make_array_from_string(const char* data, size_t size) {
  return (au_array_t){ .data = (void*) data, .size = size };
}

void* au_stderr() {
#if defined(__APPLE__)
  extern void* __stderrp;
  return __stderrp;
#elif defined(__FreeBSD__)
  extern void* __stderrp;
  return __stderrp;
#elif defined(__OpenBSD__)
  extern void *__sF;
  return &__sF[2];
#else
  extern void* stderr;
  return stderr;
#endif
}

au_unit_t au_abort_internal(const char* message) {
  extern int fprintf(void* stream, const char* format, ...);
  extern int fflush(void* stream);
  extern void _Exit(int status);

  void* stderr = au_stderr();

  fprintf(stderr, "%s\n", message);
  fflush(stderr);
  _Exit(-1);

  return nil;
}

au_unit_t au_abort(au_array_t message) {
  extern int fprintf(void* stream, const char* format, ...);
  extern int fflush(void* stream);
  extern void _Exit(int status);

  void* stderr = au_stderr();

  fprintf(stderr, "%s\n", (char*) message.data);
  fflush(stderr);
  _Exit(-1);
  return nil;
}

void* au_index_array(au_array_t array, au_index_t index, au_index_t elem_size) {
  if (index >= array.size) {
    au_abort_internal("Array index out of bounds.");
  }
  au_index_t offset = 0;
  if (__builtin_mul_overflow(index, elem_size, &offset)) {
    au_abort_internal("Multiplication overflow in array indexing operation.");
  }
  char* data = (char*) array.data;
  char* ptr = data + offset;
  return (void*)(ptr);
}

au_unit_t au_printf(const char* format, ...) {
  extern int vprintf(const char* format, va_list arg);

  va_list args;
  va_start(args, format);
  vprintf(format, args);
  va_end(args);
  return nil;
}

/*
 * Memory functions
 */

void* au_calloc(size_t size, size_t count) {
  extern void* calloc(size_t count, size_t size);

  return calloc(size, count);
}

void* au_realloc(void* ptr, size_t count) {
  extern void* realloc(void *ptr, size_t size);

  return realloc(ptr, count);
}

void* au_memmove(void* destination, void* source, size_t count) {
  extern void* memmove(void* destination, void* source, size_t count);

  return memmove(destination, source, count);
}

void* au_memcpy(void* destination, void* source, size_t count) {
  extern void* memcpy(void* destination, void* source, size_t count);

  return memcpy(destination, source, count);
}

au_unit_t au_free(void* ptr) {
  extern void free(void* ptr);

  free(ptr);
  return nil;
};

/*
 * CLI functions
 */

static int _au_argc = -1;

static char** _au_argv = NULL;

void au_store_cli_args(int argc, char** argv) {
  // Sanity checks.
  if (argc < 0) {
    au_abort_internal("Entrypoint error: argc is negative.");
  }
  if (argv == NULL) {
    au_abort_internal("Entrypoint error: argv is NULL.");
  }
  // Store values.
  _au_argc = argc;
  _au_argv = argv;
}

size_t au_get_argc() {
  // Sanity check.
  if (_au_argc == -1) {
    au_abort_internal("Prelude error: argc was not set.");
  }
  // Correctness argument: if _au_argc is non-negative, being an `int`, it
  // should fit inside `size_t`.
  size_t argc = (size_t)(_au_argc);
  return argc;
}

size_t _au_bounded_strlen(char* string, size_t bound) {
  size_t size = 0;
  for(size_t idx = 0; idx <= bound; idx++) {
    if (string[idx] == '\0') {
      return size;
    }
    size++;
  }
  au_abort_internal("Command line argument exceeds maximum length of 10 kibibytes.");
}

/* One kibibyte in bytes. */
#define AU_KIBIBYTE 1024
/* The maximum size of each CLI arg. */
#define AU_MAX_ARG_SIZE (10*AU_KIBIBYTE)

au_array_t au_get_nth_arg(size_t n) {
  // Sanity check.
  if (_au_argv == NULL) {
    au_abort_internal("Prelude error: argv was not set.");
  }
  size_t argc = au_get_argc();
  // Check array bounds.
  if (n >= argc) {
    au_abort_internal("Command line argument access out of bounds.");
  }
  // Retrieve the nth argument.
  char* arg = _au_argv[n];
  // Check non-null.
  if (arg == NULL) {
    au_abort_internal("Prelude error: command-line argument is NULL.");
  }
  // Measure the length.
  size_t size = _au_bounded_strlen(arg, AU_MAX_ARG_SIZE);
  // Otherwise, return it.
  au_array_t arg_array = ((au_array_t){ .data = (void*)arg, .size = size });
  return arg_array;
}

au_unit_t au_swap(void* a, void* b, size_t size) {
  char* c = a;
  char* d = b;
  char* e = c + size;
  while (c < e) {
    char t = *c;
    *c = *d;
    *d = t;
    c++;
    d++;
  }
}