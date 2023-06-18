/*
    Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
    See LICENSE file for details.

    SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*/
/* --- BEGIN prelude.h --- */
#include <stdint.h>
#include <stddef.h>

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
  void* data;
  size_t size;
} au_span_t;

extern au_span_t au_make_span(void* data, size_t size);

extern au_span_t au_make_span_from_string(const char* data, size_t size);

extern void* au_array_index(au_span_t* array, size_t index, size_t elem_size);

extern void* au_stdout();

extern void* au_stderr();

extern void* au_stdin();

extern au_unit_t au_abort(au_span_t message);

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
 * CLI functions
 */

void au_store_cli_args(int argc, char** argv);

size_t au_get_argc();

au_span_t au_get_nth_arg(size_t n);

/* --- END prelude.h --- */
