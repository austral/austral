#include <stdint.h>
#include <stddef.h>
#include <stdarg.h>

/*
 * Austral types
 */

typedef unsigned char  au_unit_t;
typedef unsigned char  au_bool_t;
typedef unsigned char  au_nat8_t;
typedef signed   char  au_int8_t;
typedef unsigned short au_nat16_t;
typedef signed   short au_int16_t;
typedef unsigned int   au_nat32_t;
typedef signed   int   au_int32_t;
typedef unsigned long  au_nat64_t;
typedef signed   long  au_int64_t;
typedef size_t         au_index_t;
typedef void*          au_fnptr_t;
typedef unsigned char  au_region_t;

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

au_unit_t au_print_error(const char * message) 
{
  extern int fprintf(void* stream, const char* format, ...);
  extern int fflush(void* stream);
  void* stderr = au_stderr();
  fprintf(stderr, "%s", message);
  fflush(stderr);
  return nil;
}


au_unit_t au_abort_internal(const char* message) {
  extern void _Exit(int status);
  au_print_error(message);
  au_print_error("\n");
  _Exit(-1);
  return nil;
}

au_unit_t au_abort(au_array_t message) {
  return au_abort_internal((const char *)message.data);
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

/*
 * Conversion functions
 */

au_nat8_t convert_nat8_to_nat8(au_nat8_t value) {
  return (au_nat8_t)(value);
}


au_int8_t convert_nat8_to_int8(au_nat8_t value) {
  return (au_int8_t)(value);
}


au_nat16_t convert_nat8_to_nat16(au_nat8_t value) {
  return (au_nat16_t)(value);
}


au_int16_t convert_nat8_to_int16(au_nat8_t value) {
  return (au_int16_t)(value);
}


au_nat32_t convert_nat8_to_nat32(au_nat8_t value) {
  return (au_nat32_t)(value);
}


au_int32_t convert_nat8_to_int32(au_nat8_t value) {
  return (au_int32_t)(value);
}


au_nat64_t convert_nat8_to_nat64(au_nat8_t value) {
  return (au_nat64_t)(value);
}


au_int64_t convert_nat8_to_int64(au_nat8_t value) {
  return (au_int64_t)(value);
}


float convert_nat8_to_float(au_nat8_t value) {
  return (float)(value);
}


double convert_nat8_to_double(au_nat8_t value) {
  return (double)(value);
}


au_nat8_t convert_int8_to_nat8(au_int8_t value) {
  if (value < 0) {
    au_abort_internal("Error when converting Int8 to Nat8: value is less than zero, but target type is a natural number.");
  }
  return (au_nat8_t)(value);
}


au_int8_t convert_int8_to_int8(au_int8_t value) {
  return (au_int8_t)(value);
}


au_nat16_t convert_int8_to_nat16(au_int8_t value) {
  if (value < 0) {
    au_abort_internal("Error when converting Int8 to Nat16: value is less than zero, but target type is a natural number.");
  }
  return (au_nat16_t)(value);
}


au_int16_t convert_int8_to_int16(au_int8_t value) {
  return (au_int16_t)(value);
}


au_nat32_t convert_int8_to_nat32(au_int8_t value) {
  if (value < 0) {
    au_abort_internal("Error when converting Int8 to Nat32: value is less than zero, but target type is a natural number.");
  }
  return (au_nat32_t)(value);
}


au_int32_t convert_int8_to_int32(au_int8_t value) {
  return (au_int32_t)(value);
}


au_nat64_t convert_int8_to_nat64(au_int8_t value) {
  if (value < 0) {
    au_abort_internal("Error when converting Int8 to Nat64: value is less than zero, but target type is a natural number.");
  }
  return (au_nat64_t)(value);
}


au_int64_t convert_int8_to_int64(au_int8_t value) {
  return (au_int64_t)(value);
}


float convert_int8_to_float(au_int8_t value) {
  return (float)(value);
}


double convert_int8_to_double(au_int8_t value) {
  return (double)(value);
}


au_nat8_t convert_nat16_to_nat8(au_nat16_t value) {
  if (value > UINT8_MAX) {
    au_abort_internal("Error when converting Nat16 to Nat8: value is larger than the maximum value of the Nat8 type.");
  }
  return (au_nat8_t)(value);
}


au_int8_t convert_nat16_to_int8(au_nat16_t value) {
  if (value > INT8_MAX) {
    au_abort_internal("Error when converting Nat16 to Int8: value is larger than the maximum value of the Int8 type.");
  }
  return (au_int8_t)(value);
}


au_nat16_t convert_nat16_to_nat16(au_nat16_t value) {
  return (au_nat16_t)(value);
}


au_int16_t convert_nat16_to_int16(au_nat16_t value) {
  return (au_int16_t)(value);
}


au_nat32_t convert_nat16_to_nat32(au_nat16_t value) {
  return (au_nat32_t)(value);
}


au_int32_t convert_nat16_to_int32(au_nat16_t value) {
  return (au_int32_t)(value);
}


au_nat64_t convert_nat16_to_nat64(au_nat16_t value) {
  return (au_nat64_t)(value);
}


au_int64_t convert_nat16_to_int64(au_nat16_t value) {
  return (au_int64_t)(value);
}


float convert_nat16_to_float(au_nat16_t value) {
  return (float)(value);
}


double convert_nat16_to_double(au_nat16_t value) {
  return (double)(value);
}


au_nat8_t convert_int16_to_nat8(au_int16_t value) {
  if (value < 0) {
    au_abort_internal("Error when converting Int16 to Nat8: value is less than zero, but target type is a natural number.");
  }
  if (value > UINT8_MAX) {
    au_abort_internal("Error when converting Int16 to Nat8: value is larger than the maximum value of the Nat8 type.");
  }
  return (au_nat8_t)(value);
}


au_int8_t convert_int16_to_int8(au_int16_t value) {
  if (value > INT8_MAX) {
    au_abort_internal("Error when converting Int16 to Int8: value is larger than the maximum value of the Int8 type.");
  }
  return (au_int8_t)(value);
}


au_nat16_t convert_int16_to_nat16(au_int16_t value) {
  if (value < 0) {
    au_abort_internal("Error when converting Int16 to Nat16: value is less than zero, but target type is a natural number.");
  }
  return (au_nat16_t)(value);
}


au_int16_t convert_int16_to_int16(au_int16_t value) {
  return (au_int16_t)(value);
}


au_nat32_t convert_int16_to_nat32(au_int16_t value) {
  if (value < 0) {
    au_abort_internal("Error when converting Int16 to Nat32: value is less than zero, but target type is a natural number.");
  }
  return (au_nat32_t)(value);
}


au_int32_t convert_int16_to_int32(au_int16_t value) {
  return (au_int32_t)(value);
}


au_nat64_t convert_int16_to_nat64(au_int16_t value) {
  if (value < 0) {
    au_abort_internal("Error when converting Int16 to Nat64: value is less than zero, but target type is a natural number.");
  }
  return (au_nat64_t)(value);
}


au_int64_t convert_int16_to_int64(au_int16_t value) {
  return (au_int64_t)(value);
}


float convert_int16_to_float(au_int16_t value) {
  return (float)(value);
}


double convert_int16_to_double(au_int16_t value) {
  return (double)(value);
}


au_nat8_t convert_nat32_to_nat8(au_nat32_t value) {
  if (value > UINT8_MAX) {
    au_abort_internal("Error when converting Nat32 to Nat8: value is larger than the maximum value of the Nat8 type.");
  }
  return (au_nat8_t)(value);
}


au_int8_t convert_nat32_to_int8(au_nat32_t value) {
  if (value > INT8_MAX) {
    au_abort_internal("Error when converting Nat32 to Int8: value is larger than the maximum value of the Int8 type.");
  }
  return (au_int8_t)(value);
}


au_nat16_t convert_nat32_to_nat16(au_nat32_t value) {
  if (value > UINT16_MAX) {
    au_abort_internal("Error when converting Nat32 to Nat16: value is larger than the maximum value of the Nat16 type.");
  }
  return (au_nat16_t)(value);
}


au_int16_t convert_nat32_to_int16(au_nat32_t value) {
  if (value > INT16_MAX) {
    au_abort_internal("Error when converting Nat32 to Int16: value is larger than the maximum value of the Int16 type.");
  }
  return (au_int16_t)(value);
}


au_nat32_t convert_nat32_to_nat32(au_nat32_t value) {
  return (au_nat32_t)(value);
}


au_int32_t convert_nat32_to_int32(au_nat32_t value) {
  return (au_int32_t)(value);
}


au_nat64_t convert_nat32_to_nat64(au_nat32_t value) {
  return (au_nat64_t)(value);
}


au_int64_t convert_nat32_to_int64(au_nat32_t value) {
  return (au_int64_t)(value);
}


float convert_nat32_to_float(au_nat32_t value) {
  return (float)(value);
}


double convert_nat32_to_double(au_nat32_t value) {
  return (double)(value);
}


au_nat8_t convert_int32_to_nat8(au_int32_t value) {
  if (value < 0) {
    au_abort_internal("Error when converting Int32 to Nat8: value is less than zero, but target type is a natural number.");
  }
  if (value > UINT8_MAX) {
    au_abort_internal("Error when converting Int32 to Nat8: value is larger than the maximum value of the Nat8 type.");
  }
  return (au_nat8_t)(value);
}


au_int8_t convert_int32_to_int8(au_int32_t value) {
  if (value > INT8_MAX) {
    au_abort_internal("Error when converting Int32 to Int8: value is larger than the maximum value of the Int8 type.");
  }
  return (au_int8_t)(value);
}


au_nat16_t convert_int32_to_nat16(au_int32_t value) {
  if (value < 0) {
    au_abort_internal("Error when converting Int32 to Nat16: value is less than zero, but target type is a natural number.");
  }
  if (value > UINT16_MAX) {
    au_abort_internal("Error when converting Int32 to Nat16: value is larger than the maximum value of the Nat16 type.");
  }
  return (au_nat16_t)(value);
}


au_int16_t convert_int32_to_int16(au_int32_t value) {
  if (value > INT16_MAX) {
    au_abort_internal("Error when converting Int32 to Int16: value is larger than the maximum value of the Int16 type.");
  }
  return (au_int16_t)(value);
}


au_nat32_t convert_int32_to_nat32(au_int32_t value) {
  if (value < 0) {
    au_abort_internal("Error when converting Int32 to Nat32: value is less than zero, but target type is a natural number.");
  }
  return (au_nat32_t)(value);
}


au_int32_t convert_int32_to_int32(au_int32_t value) {
  return (au_int32_t)(value);
}


au_nat64_t convert_int32_to_nat64(au_int32_t value) {
  if (value < 0) {
    au_abort_internal("Error when converting Int32 to Nat64: value is less than zero, but target type is a natural number.");
  }
  return (au_nat64_t)(value);
}


au_int64_t convert_int32_to_int64(au_int32_t value) {
  return (au_int64_t)(value);
}


float convert_int32_to_float(au_int32_t value) {
  return (float)(value);
}


double convert_int32_to_double(au_int32_t value) {
  return (double)(value);
}


au_nat8_t convert_nat64_to_nat8(au_nat64_t value) {
  if (value > UINT8_MAX) {
    au_abort_internal("Error when converting Nat64 to Nat8: value is larger than the maximum value of the Nat8 type.");
  }
  return (au_nat8_t)(value);
}


au_int8_t convert_nat64_to_int8(au_nat64_t value) {
  if (value > INT8_MAX) {
    au_abort_internal("Error when converting Nat64 to Int8: value is larger than the maximum value of the Int8 type.");
  }
  return (au_int8_t)(value);
}


au_nat16_t convert_nat64_to_nat16(au_nat64_t value) {
  if (value > UINT16_MAX) {
    au_abort_internal("Error when converting Nat64 to Nat16: value is larger than the maximum value of the Nat16 type.");
  }
  return (au_nat16_t)(value);
}


au_int16_t convert_nat64_to_int16(au_nat64_t value) {
  if (value > INT16_MAX) {
    au_abort_internal("Error when converting Nat64 to Int16: value is larger than the maximum value of the Int16 type.");
  }
  return (au_int16_t)(value);
}


au_nat32_t convert_nat64_to_nat32(au_nat64_t value) {
  if (value > UINT32_MAX) {
    au_abort_internal("Error when converting Nat64 to Nat32: value is larger than the maximum value of the Nat32 type.");
  }
  return (au_nat32_t)(value);
}


au_int32_t convert_nat64_to_int32(au_nat64_t value) {
  if (value > INT32_MAX) {
    au_abort_internal("Error when converting Nat64 to Int32: value is larger than the maximum value of the Int32 type.");
  }
  return (au_int32_t)(value);
}


au_nat64_t convert_nat64_to_nat64(au_nat64_t value) {
  return (au_nat64_t)(value);
}

au_nat64_t convert_natindex_to_nat64(au_nat64_t value) {
  return (au_nat64_t)(value);
}


au_int64_t convert_nat64_to_int64(au_nat64_t value) {
  return (au_int64_t)(value);
}


float convert_nat64_to_float(au_nat64_t value) {
  return (float)(value);
}


double convert_nat64_to_double(au_nat64_t value) {
  return (double)(value);
}


au_nat8_t convert_int64_to_nat8(au_int64_t value) {
  if (value < 0) {
    au_abort_internal("Error when converting Int64 to Nat8: value is less than zero, but target type is a natural number.");
  }
  if (value > UINT8_MAX) {
    au_abort_internal("Error when converting Int64 to Nat8: value is larger than the maximum value of the Nat8 type.");
  }
  return (au_nat8_t)(value);
}


au_int8_t convert_int64_to_int8(au_int64_t value) {
  if (value > INT8_MAX) {
    au_abort_internal("Error when converting Int64 to Int8: value is larger than the maximum value of the Int8 type.");
  }
  return (au_int8_t)(value);
}


au_nat16_t convert_int64_to_nat16(au_int64_t value) {
  if (value < 0) {
    au_abort_internal("Error when converting Int64 to Nat16: value is less than zero, but target type is a natural number.");
  }
  if (value > UINT16_MAX) {
    au_abort_internal("Error when converting Int64 to Nat16: value is larger than the maximum value of the Nat16 type.");
  }
  return (au_nat16_t)(value);
}


au_int16_t convert_int64_to_int16(au_int64_t value) {
  if (value > INT16_MAX) {
    au_abort_internal("Error when converting Int64 to Int16: value is larger than the maximum value of the Int16 type.");
  }
  return (au_int16_t)(value);
}


au_nat32_t convert_int64_to_nat32(au_int64_t value) {
  if (value < 0) {
    au_abort_internal("Error when converting Int64 to Nat32: value is less than zero, but target type is a natural number.");
  }
  if (value > UINT32_MAX) {
    au_abort_internal("Error when converting Int64 to Nat32: value is larger than the maximum value of the Nat32 type.");
  }
  return (au_nat32_t)(value);
}


au_int32_t convert_int64_to_int32(au_int64_t value) {
  if (value > INT32_MAX) {
    au_abort_internal("Error when converting Int64 to Int32: value is larger than the maximum value of the Int32 type.");
  }
  return (au_int32_t)(value);
}


au_nat64_t convert_int64_to_nat64(au_int64_t value) {
  if (value < 0) {
    au_abort_internal("Error when converting Int64 to Nat64: value is less than zero, but target type is a natural number.");
  }
  return (au_nat64_t)(value);
}


au_int64_t convert_int64_to_int64(au_int64_t value) {
  return (au_int64_t)(value);
}


float convert_int64_to_float(au_int64_t value) {
  return (float)(value);
}


double convert_int64_to_double(au_int64_t value) {
  return (double)(value);
}


au_nat8_t convert_float_to_nat8(float value) {
  return (au_nat8_t)(value);
}


au_int8_t convert_float_to_int8(float value) {
  return (au_int8_t)(value);
}


au_nat16_t convert_float_to_nat16(float value) {
  return (au_nat16_t)(value);
}


au_int16_t convert_float_to_int16(float value) {
  return (au_int16_t)(value);
}


au_nat32_t convert_float_to_nat32(float value) {
  return (au_nat32_t)(value);
}


au_int32_t convert_float_to_int32(float value) {
  return (au_int32_t)(value);
}


au_nat64_t convert_float_to_nat64(float value) {
  return (au_nat64_t)(value);
}


au_int64_t convert_float_to_int64(float value) {
  return (au_int64_t)(value);
}


float convert_float_to_float(float value) {
  return (float)(value);
}


double convert_float_to_double(float value) {
  return (double)(value);
}


au_nat8_t convert_double_to_nat8(double value) {
  return (au_nat8_t)(value);
}


au_int8_t convert_double_to_int8(double value) {
  return (au_int8_t)(value);
}


au_nat16_t convert_double_to_nat16(double value) {
  return (au_nat16_t)(value);
}


au_int16_t convert_double_to_int16(double value) {
  return (au_int16_t)(value);
}


au_nat32_t convert_double_to_nat32(double value) {
  return (au_nat32_t)(value);
}


au_int32_t convert_double_to_int32(double value) {
  return (au_int32_t)(value);
}


au_nat64_t convert_double_to_nat64(double value) {
  return (au_nat64_t)(value);
}


au_int64_t convert_double_to_int64(double value) {
  return (au_int64_t)(value);
}


float convert_double_to_float(double value) {
  return (float)(value);
}


double convert_double_to_double(double value) {
  return (double)(value);
}
