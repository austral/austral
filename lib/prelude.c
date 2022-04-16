#include <stdint.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

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
 * Conversion functions
 */

uint8_t convert_uint8_to_uint8(uint8_t value) { return (uint8_t)(value); }
uint16_t convert_uint8_to_uint16(uint8_t value) { return (uint16_t)(value); }
uint32_t convert_uint8_to_uint32(uint8_t value) { return (uint32_t)(value); }
uint64_t convert_uint8_to_uint64(uint8_t value) { return (uint64_t)(value); }
int8_t convert_uint8_to_int8(uint8_t value) { return (int8_t)(value); }
int16_t convert_uint8_to_int16(uint8_t value) { return (int16_t)(value); }
int32_t convert_uint8_to_int32(uint8_t value) { return (int32_t)(value); }
int64_t convert_uint8_to_int64(uint8_t value) { return (int64_t)(value); }
float convert_uint8_to_f(uint8_t value) { return (float)(value); }
double convert_uint8_to_d(uint8_t value) { return (double)(value); }
uint8_t convert_uint16_to_uint8(uint16_t value) { return (uint8_t)(value); }
uint16_t convert_uint16_to_uint16(uint16_t value) { return (uint16_t)(value); }
uint32_t convert_uint16_to_uint32(uint16_t value) { return (uint32_t)(value); }
uint64_t convert_uint16_to_uint64(uint16_t value) { return (uint64_t)(value); }
int8_t convert_uint16_to_int8(uint16_t value) { return (int8_t)(value); }
int16_t convert_uint16_to_int16(uint16_t value) { return (int16_t)(value); }
int32_t convert_uint16_to_int32(uint16_t value) { return (int32_t)(value); }
int64_t convert_uint16_to_int64(uint16_t value) { return (int64_t)(value); }
float convert_uint16_to_f(uint16_t value) { return (float)(value); }
double convert_uint16_to_d(uint16_t value) { return (double)(value); }
uint8_t convert_uint32_to_uint8(uint32_t value) { return (uint8_t)(value); }
uint16_t convert_uint32_to_uint16(uint32_t value) { return (uint16_t)(value); }
uint32_t convert_uint32_to_uint32(uint32_t value) { return (uint32_t)(value); }
uint64_t convert_uint32_to_uint64(uint32_t value) { return (uint64_t)(value); }
int8_t convert_uint32_to_int8(uint32_t value) { return (int8_t)(value); }
int16_t convert_uint32_to_int16(uint32_t value) { return (int16_t)(value); }
int32_t convert_uint32_to_int32(uint32_t value) { return (int32_t)(value); }
int64_t convert_uint32_to_int64(uint32_t value) { return (int64_t)(value); }
float convert_uint32_to_f(uint32_t value) { return (float)(value); }
double convert_uint32_to_d(uint32_t value) { return (double)(value); }
uint8_t convert_uint64_to_uint8(uint64_t value) { return (uint8_t)(value); }
uint16_t convert_uint64_to_uint16(uint64_t value) { return (uint16_t)(value); }
uint32_t convert_uint64_to_uint32(uint64_t value) { return (uint32_t)(value); }
uint64_t convert_uint64_to_uint64(uint64_t value) { return (uint64_t)(value); }
int8_t convert_uint64_to_int8(uint64_t value) { return (int8_t)(value); }
int16_t convert_uint64_to_int16(uint64_t value) { return (int16_t)(value); }
int32_t convert_uint64_to_int32(uint64_t value) { return (int32_t)(value); }
int64_t convert_uint64_to_int64(uint64_t value) { return (int64_t)(value); }
float convert_uint64_to_f(uint64_t value) { return (float)(value); }
double convert_uint64_to_d(uint64_t value) { return (double)(value); }
uint8_t convert_int8_to_uint8(int8_t value) { return (uint8_t)(value); }
uint16_t convert_int8_to_uint16(int8_t value) { return (uint16_t)(value); }
uint32_t convert_int8_to_uint32(int8_t value) { return (uint32_t)(value); }
uint64_t convert_int8_to_uint64(int8_t value) { return (uint64_t)(value); }
int8_t convert_int8_to_int8(int8_t value) { return (int8_t)(value); }
int16_t convert_int8_to_int16(int8_t value) { return (int16_t)(value); }
int32_t convert_int8_to_int32(int8_t value) { return (int32_t)(value); }
int64_t convert_int8_to_int64(int8_t value) { return (int64_t)(value); }
float convert_int8_to_f(int8_t value) { return (float)(value); }
double convert_int8_to_d(int8_t value) { return (double)(value); }
uint8_t convert_int16_to_uint8(int16_t value) { return (uint8_t)(value); }
uint16_t convert_int16_to_uint16(int16_t value) { return (uint16_t)(value); }
uint32_t convert_int16_to_uint32(int16_t value) { return (uint32_t)(value); }
uint64_t convert_int16_to_uint64(int16_t value) { return (uint64_t)(value); }
int8_t convert_int16_to_int8(int16_t value) { return (int8_t)(value); }
int16_t convert_int16_to_int16(int16_t value) { return (int16_t)(value); }
int32_t convert_int16_to_int32(int16_t value) { return (int32_t)(value); }
int64_t convert_int16_to_int64(int16_t value) { return (int64_t)(value); }
float convert_int16_to_f(int16_t value) { return (float)(value); }
double convert_int16_to_d(int16_t value) { return (double)(value); }
uint8_t convert_int32_to_uint8(int32_t value) { return (uint8_t)(value); }
uint16_t convert_int32_to_uint16(int32_t value) { return (uint16_t)(value); }
uint32_t convert_int32_to_uint32(int32_t value) { return (uint32_t)(value); }
uint64_t convert_int32_to_uint64(int32_t value) { return (uint64_t)(value); }
int8_t convert_int32_to_int8(int32_t value) { return (int8_t)(value); }
int16_t convert_int32_to_int16(int32_t value) { return (int16_t)(value); }
int32_t convert_int32_to_int32(int32_t value) { return (int32_t)(value); }
int64_t convert_int32_to_int64(int32_t value) { return (int64_t)(value); }
float convert_int32_to_f(int32_t value) { return (float)(value); }
double convert_int32_to_d(int32_t value) { return (double)(value); }
uint8_t convert_int64_to_uint8(int64_t value) { return (uint8_t)(value); }
uint16_t convert_int64_to_uint16(int64_t value) { return (uint16_t)(value); }
uint32_t convert_int64_to_uint32(int64_t value) { return (uint32_t)(value); }
uint64_t convert_int64_to_uint64(int64_t value) { return (uint64_t)(value); }
int8_t convert_int64_to_int8(int64_t value) { return (int8_t)(value); }
int16_t convert_int64_to_int16(int64_t value) { return (int16_t)(value); }
int32_t convert_int64_to_int32(int64_t value) { return (int32_t)(value); }
int64_t convert_int64_to_int64(int64_t value) { return (int64_t)(value); }
float convert_int64_to_f(int64_t value) { return (float)(value); }
double convert_int64_to_d(int64_t value) { return (double)(value); }
uint8_t convert_f_to_uint8(float value) { return (uint8_t)(value); }
uint16_t convert_f_to_uint16(float value) { return (uint16_t)(value); }
uint32_t convert_f_to_uint32(float value) { return (uint32_t)(value); }
uint64_t convert_f_to_uint64(float value) { return (uint64_t)(value); }
int8_t convert_f_to_int8(float value) { return (int8_t)(value); }
int16_t convert_f_to_int16(float value) { return (int16_t)(value); }
int32_t convert_f_to_int32(float value) { return (int32_t)(value); }
int64_t convert_f_to_int64(float value) { return (int64_t)(value); }
float convert_f_to_f(float value) { return (float)(value); }
double convert_f_to_d(float value) { return (double)(value); }
uint8_t convert_d_to_uint8(double value) { return (uint8_t)(value); }
uint16_t convert_d_to_uint16(double value) { return (uint16_t)(value); }
uint32_t convert_d_to_uint32(double value) { return (uint32_t)(value); }
uint64_t convert_d_to_uint64(double value) { return (uint64_t)(value); }
int8_t convert_d_to_int8(double value) { return (int8_t)(value); }
int16_t convert_d_to_int16(double value) { return (int16_t)(value); }
int32_t convert_d_to_int32(double value) { return (int32_t)(value); }
int64_t convert_d_to_int64(double value) { return (int64_t)(value); }
float convert_d_to_f(double value) { return (float)(value); }
double convert_d_to_d(double value) { return (double)(value); }

/*
 * Pervasive
 */

typedef struct {
    void*  data;
    size_t size;
} au_array_t;

au_array_t au_make_array_from_string(const char* data, size_t size) {
    return (au_array_t){ .data = (void*) data, .size = size };
}

au_unit_t au_abort(au_array_t message) {
    fprintf(stderr, "%s\n", (char*) message.data);
    fflush(stderr);
    _Exit(-1);
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
