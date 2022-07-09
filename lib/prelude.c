#include <stdint.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
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

#define nil   0
#define false 0
#define true  1

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

au_unit_t au_abort_internal(const char* message) {
    fprintf(stderr, "%s\n", message);
    fflush(stderr);
    _Exit(-1);
    return nil;
}

au_unit_t au_abort(au_array_t message) {
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
  return calloc(size, count);
}

void* au_realloc(void* ptr, size_t count) {
  return realloc(ptr, count);
}

void* au_memmove(void* destination, void* source, size_t count) {
  return memmove(destination, source, count);
}

void* au_memcpy(void* destination, void* source, size_t count) {
  return memcpy(destination, source, count);
}

au_unit_t au_free(void* ptr) {
  free(ptr);
  return nil;
};

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
