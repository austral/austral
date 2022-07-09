/* --- BEGIN prelude.h --- */
#include <stdint.h>
#include <stddef.h>

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
 * A little hack
 */

#define AU_STORE(ptr, val) (*(ptr) = (val), nil)

/*
 * Pervasive
 */

typedef struct {
  void*  data;
  size_t size;
} au_array_t;

extern void* au_index_array(au_array_t array, au_index_t index, au_index_t elem_size);

extern au_array_t au_make_array_from_string(const char* data, size_t size);

extern au_unit_t au_abort(au_array_t message);

extern au_unit_t au_printf(const char* format, ...);

/*
 * Memory functions
 */

extern void* au_calloc(size_t size, size_t count);

extern void* au_realloc(void* ptr, size_t count);

extern void* au_memmove(void* destination, void* source, size_t count);

extern void* au_memcpy(void* destination, void* source, size_t count);

extern au_unit_t au_free(void* ptr);

/*
 * Conversion functions
 */

extern au_nat8_t convert_nat8_to_nat8(au_nat8_t value);
extern au_int8_t convert_nat8_to_int8(au_nat8_t value);
extern au_nat16_t convert_nat8_to_nat16(au_nat8_t value);
extern au_int16_t convert_nat8_to_int16(au_nat8_t value);
extern au_nat32_t convert_nat8_to_nat32(au_nat8_t value);
extern au_int32_t convert_nat8_to_int32(au_nat8_t value);
extern au_nat64_t convert_nat8_to_nat64(au_nat8_t value);
extern au_int64_t convert_nat8_to_int64(au_nat8_t value);
extern float convert_nat8_to_float(au_nat8_t value);
extern double convert_nat8_to_double(au_nat8_t value);
extern au_nat8_t convert_int8_to_nat8(au_int8_t value);
extern au_int8_t convert_int8_to_int8(au_int8_t value);
extern au_nat16_t convert_int8_to_nat16(au_int8_t value);
extern au_int16_t convert_int8_to_int16(au_int8_t value);
extern au_nat32_t convert_int8_to_nat32(au_int8_t value);
extern au_int32_t convert_int8_to_int32(au_int8_t value);
extern au_nat64_t convert_int8_to_nat64(au_int8_t value);
extern au_int64_t convert_int8_to_int64(au_int8_t value);
extern float convert_int8_to_float(au_int8_t value);
extern double convert_int8_to_double(au_int8_t value);
extern au_nat8_t convert_nat16_to_nat8(au_nat16_t value);
extern au_int8_t convert_nat16_to_int8(au_nat16_t value);
extern au_nat16_t convert_nat16_to_nat16(au_nat16_t value);
extern au_int16_t convert_nat16_to_int16(au_nat16_t value);
extern au_nat32_t convert_nat16_to_nat32(au_nat16_t value);
extern au_int32_t convert_nat16_to_int32(au_nat16_t value);
extern au_nat64_t convert_nat16_to_nat64(au_nat16_t value);
extern au_int64_t convert_nat16_to_int64(au_nat16_t value);
extern float convert_nat16_to_float(au_nat16_t value);
extern double convert_nat16_to_double(au_nat16_t value);
extern au_nat8_t convert_int16_to_nat8(au_int16_t value);
extern au_int8_t convert_int16_to_int8(au_int16_t value);
extern au_nat16_t convert_int16_to_nat16(au_int16_t value);
extern au_int16_t convert_int16_to_int16(au_int16_t value);
extern au_nat32_t convert_int16_to_nat32(au_int16_t value);
extern au_int32_t convert_int16_to_int32(au_int16_t value);
extern au_nat64_t convert_int16_to_nat64(au_int16_t value);
extern au_int64_t convert_int16_to_int64(au_int16_t value);
extern float convert_int16_to_float(au_int16_t value);
extern double convert_int16_to_double(au_int16_t value);
extern au_nat8_t convert_nat32_to_nat8(au_nat32_t value);
extern au_int8_t convert_nat32_to_int8(au_nat32_t value);
extern au_nat16_t convert_nat32_to_nat16(au_nat32_t value);
extern au_int16_t convert_nat32_to_int16(au_nat32_t value);
extern au_nat32_t convert_nat32_to_nat32(au_nat32_t value);
extern au_int32_t convert_nat32_to_int32(au_nat32_t value);
extern au_nat64_t convert_nat32_to_nat64(au_nat32_t value);
extern au_int64_t convert_nat32_to_int64(au_nat32_t value);
extern float convert_nat32_to_float(au_nat32_t value);
extern double convert_nat32_to_double(au_nat32_t value);
extern au_nat8_t convert_int32_to_nat8(au_int32_t value);
extern au_int8_t convert_int32_to_int8(au_int32_t value);
extern au_nat16_t convert_int32_to_nat16(au_int32_t value);
extern au_int16_t convert_int32_to_int16(au_int32_t value);
extern au_nat32_t convert_int32_to_nat32(au_int32_t value);
extern au_int32_t convert_int32_to_int32(au_int32_t value);
extern au_nat64_t convert_int32_to_nat64(au_int32_t value);
extern au_int64_t convert_int32_to_int64(au_int32_t value);
extern float convert_int32_to_float(au_int32_t value);
extern double convert_int32_to_double(au_int32_t value);
extern au_nat8_t convert_nat64_to_nat8(au_nat64_t value);
extern au_int8_t convert_nat64_to_int8(au_nat64_t value);
extern au_nat16_t convert_nat64_to_nat16(au_nat64_t value);
extern au_int16_t convert_nat64_to_int16(au_nat64_t value);
extern au_nat32_t convert_nat64_to_nat32(au_nat64_t value);
extern au_int32_t convert_nat64_to_int32(au_nat64_t value);
extern au_nat64_t convert_nat64_to_nat64(au_nat64_t value);
extern au_int64_t convert_nat64_to_int64(au_nat64_t value);
extern float convert_nat64_to_float(au_nat64_t value);
extern double convert_nat64_to_double(au_nat64_t value);
extern au_nat8_t convert_int64_to_nat8(au_int64_t value);
extern au_int8_t convert_int64_to_int8(au_int64_t value);
extern au_nat16_t convert_int64_to_nat16(au_int64_t value);
extern au_int16_t convert_int64_to_int16(au_int64_t value);
extern au_nat32_t convert_int64_to_nat32(au_int64_t value);
extern au_int32_t convert_int64_to_int32(au_int64_t value);
extern au_nat64_t convert_int64_to_nat64(au_int64_t value);
extern au_int64_t convert_int64_to_int64(au_int64_t value);
extern float convert_int64_to_float(au_int64_t value);
extern double convert_int64_to_double(au_int64_t value);
extern au_nat8_t convert_float_to_nat8(float value);
extern au_int8_t convert_float_to_int8(float value);
extern au_nat16_t convert_float_to_nat16(float value);
extern au_int16_t convert_float_to_int16(float value);
extern au_nat32_t convert_float_to_nat32(float value);
extern au_int32_t convert_float_to_int32(float value);
extern au_nat64_t convert_float_to_nat64(float value);
extern au_int64_t convert_float_to_int64(float value);
extern float convert_float_to_float(float value);
extern double convert_float_to_double(float value);
extern au_nat8_t convert_double_to_nat8(double value);
extern au_int8_t convert_double_to_int8(double value);
extern au_nat16_t convert_double_to_nat16(double value);
extern au_int16_t convert_double_to_int16(double value);
extern au_nat32_t convert_double_to_nat32(double value);
extern au_int32_t convert_double_to_int32(double value);
extern au_nat64_t convert_double_to_nat64(double value);
extern au_int64_t convert_double_to_int64(double value);
extern float convert_double_to_float(double value);
extern double convert_double_to_double(double value);

/* --- END prelude.h --- */
