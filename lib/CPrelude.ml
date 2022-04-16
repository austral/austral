(** The C prelude. *)

(** The source code of the C prelude. *)
let prelude_source: string = {code|
/* --- BEGIN PRELUDE --- */
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

#define nil   0
#define false 0
#define true  1

/*
 * A little hack
 */

#define AU_STORE(ptr, val) (*(ptr) = (val), nil)

/*
 * Conversion functions
 */

extern uint8_t convert_uint8_to_uint8(uint8_t value);
extern uint16_t convert_uint8_to_uint16(uint8_t value);
extern uint32_t convert_uint8_to_uint32(uint8_t value);
extern uint64_t convert_uint8_to_uint64(uint8_t value);
extern int8_t convert_uint8_to_int8(uint8_t value);
extern int16_t convert_uint8_to_int16(uint8_t value);
extern int32_t convert_uint8_to_int32(uint8_t value);
extern int64_t convert_uint8_to_int64(uint8_t value);
extern size_t convert_uint8_to_usize(uint8_t value);
extern float convert_uint8_to_f(uint8_t value);
extern double convert_uint8_to_d(uint8_t value);
extern uint8_t convert_uint16_to_uint8(uint16_t value);
extern uint16_t convert_uint16_to_uint16(uint16_t value);
extern uint32_t convert_uint16_to_uint32(uint16_t value);
extern uint64_t convert_uint16_to_uint64(uint16_t value);
extern int8_t convert_uint16_to_int8(uint16_t value);
extern int16_t convert_uint16_to_int16(uint16_t value);
extern int32_t convert_uint16_to_int32(uint16_t value);
extern int64_t convert_uint16_to_int64(uint16_t value);
extern size_t convert_uint16_to_usize(uint16_t value);
extern float convert_uint16_to_f(uint16_t value);
extern double convert_uint16_to_d(uint16_t value);
extern uint8_t convert_uint32_to_uint8(uint32_t value);
extern uint16_t convert_uint32_to_uint16(uint32_t value);
extern uint32_t convert_uint32_to_uint32(uint32_t value);
extern uint64_t convert_uint32_to_uint64(uint32_t value);
extern int8_t convert_uint32_to_int8(uint32_t value);
extern int16_t convert_uint32_to_int16(uint32_t value);
extern int32_t convert_uint32_to_int32(uint32_t value);
extern int64_t convert_uint32_to_int64(uint32_t value);
extern size_t convert_uint32_to_usize(uint32_t value);
extern float convert_uint32_to_f(uint32_t value);
extern double convert_uint32_to_d(uint32_t value);
extern uint8_t convert_uint64_to_uint8(uint64_t value);
extern uint16_t convert_uint64_to_uint16(uint64_t value);
extern uint32_t convert_uint64_to_uint32(uint64_t value);
extern uint64_t convert_uint64_to_uint64(uint64_t value);
extern int8_t convert_uint64_to_int8(uint64_t value);
extern int16_t convert_uint64_to_int16(uint64_t value);
extern int32_t convert_uint64_to_int32(uint64_t value);
extern int64_t convert_uint64_to_int64(uint64_t value);
extern size_t convert_uint64_to_usize(uint64_t value);
extern float convert_uint64_to_f(uint64_t value);
extern double convert_uint64_to_d(uint64_t value);
extern uint8_t convert_int8_to_uint8(int8_t value);
extern uint16_t convert_int8_to_uint16(int8_t value);
extern uint32_t convert_int8_to_uint32(int8_t value);
extern uint64_t convert_int8_to_uint64(int8_t value);
extern int8_t convert_int8_to_int8(int8_t value);
extern int16_t convert_int8_to_int16(int8_t value);
extern int32_t convert_int8_to_int32(int8_t value);
extern int64_t convert_int8_to_int64(int8_t value);
extern size_t convert_int8_to_usize(int8_t value);
extern float convert_int8_to_f(int8_t value);
extern double convert_int8_to_d(int8_t value);
extern uint8_t convert_int16_to_uint8(int16_t value);
extern uint16_t convert_int16_to_uint16(int16_t value);
extern uint32_t convert_int16_to_uint32(int16_t value);
extern uint64_t convert_int16_to_uint64(int16_t value);
extern int8_t convert_int16_to_int8(int16_t value);
extern int16_t convert_int16_to_int16(int16_t value);
extern int32_t convert_int16_to_int32(int16_t value);
extern int64_t convert_int16_to_int64(int16_t value);
extern size_t convert_int16_to_usize(int16_t value);
extern float convert_int16_to_f(int16_t value);
extern double convert_int16_to_d(int16_t value);
extern uint8_t convert_int32_to_uint8(int32_t value);
extern uint16_t convert_int32_to_uint16(int32_t value);
extern uint32_t convert_int32_to_uint32(int32_t value);
extern uint64_t convert_int32_to_uint64(int32_t value);
extern int8_t convert_int32_to_int8(int32_t value);
extern int16_t convert_int32_to_int16(int32_t value);
extern int32_t convert_int32_to_int32(int32_t value);
extern int64_t convert_int32_to_int64(int32_t value);
extern size_t convert_int32_to_usize(int32_t value);
extern float convert_int32_to_f(int32_t value);
extern double convert_int32_to_d(int32_t value);
extern uint8_t convert_int64_to_uint8(int64_t value);
extern uint16_t convert_int64_to_uint16(int64_t value);
extern uint32_t convert_int64_to_uint32(int64_t value);
extern uint64_t convert_int64_to_uint64(int64_t value);
extern int8_t convert_int64_to_int8(int64_t value);
extern int16_t convert_int64_to_int16(int64_t value);
extern int32_t convert_int64_to_int32(int64_t value);
extern int64_t convert_int64_to_int64(int64_t value);
extern size_t convert_int64_to_usize(int64_t value);
extern float convert_int64_to_f(int64_t value);
extern double convert_int64_to_d(int64_t value);
extern uint8_t convert_usize_to_uint8(size_t value);
extern uint16_t convert_usize_to_uint16(size_t value);
extern uint32_t convert_usize_to_uint32(size_t value);
extern uint64_t convert_usize_to_uint64(size_t value);
extern int8_t convert_usize_to_int8(size_t value);
extern int16_t convert_usize_to_int16(size_t value);
extern int32_t convert_usize_to_int32(size_t value);
extern int64_t convert_usize_to_int64(size_t value);
extern size_t convert_usize_to_usize(size_t value);
extern float convert_usize_to_f(size_t value);
extern double convert_usize_to_d(size_t value);
extern uint8_t convert_f_to_uint8(float value);
extern uint16_t convert_f_to_uint16(float value);
extern uint32_t convert_f_to_uint32(float value);
extern uint64_t convert_f_to_uint64(float value);
extern int8_t convert_f_to_int8(float value);
extern int16_t convert_f_to_int16(float value);
extern int32_t convert_f_to_int32(float value);
extern int64_t convert_f_to_int64(float value);
extern size_t convert_f_to_usize(float value);
extern float convert_f_to_f(float value);
extern double convert_f_to_d(float value);
extern uint8_t convert_d_to_uint8(double value);
extern uint16_t convert_d_to_uint16(double value);
extern uint32_t convert_d_to_uint32(double value);
extern uint64_t convert_d_to_uint64(double value);
extern int8_t convert_d_to_int8(double value);
extern int16_t convert_d_to_int16(double value);
extern int32_t convert_d_to_int32(double value);
extern int64_t convert_d_to_int64(double value);
extern size_t convert_d_to_usize(double value);
extern float convert_d_to_f(double value);
extern double convert_d_to_d(double value);

/*
 * Pervasive
 */

typedef struct {
    void*  data;
    size_t size;
} au_array_t;

extern au_array_t au_make_array_from_string(const char* data, size_t size);

extern au_unit_t au_abort(au_array_t message);

/*
 * Memory functions
 */

extern void* au_calloc(size_t size, size_t count);

extern void* au_realloc(void* ptr, size_t count);

extern void* au_memmove(void* destination, void* source, size_t count);

extern void* au_memcpy(void* destination, void* source, size_t count);

extern au_unit_t au_free(void* ptr);

/* --- END PRELUDE --- */
|code}
