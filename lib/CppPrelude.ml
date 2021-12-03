let prelude = {code|#include <stdint.h>
#include <stddef.h>

namespace Austral__Core {
    extern "C" void* stderr;
    extern "C" int fprintf(void* stream, const char* format, ...);
    extern "C" int fflush(void* stream);
    extern "C" void _Exit(int exit_code);

    template<typename T>
    struct Array;

    template<typename T>
    Array<T> Make_Array(size_t size, T* data);

    bool Abort(Array<uint8_t> message);

    template<typename T>
    struct Array {
        size_t size;
        T* data;

        T &operator[](int index) {
            if (index < 0) {
               Abort(Make_Array<uint8_t>(15, (uint8_t*)"Negative index."));
            }
            if (((size_t)(index)) >= size) {
               Abort(Make_Array<uint8_t>(26, (uint8_t*)"Array index out of bounds."));
            }
            return data[index];
        }
    };

    template<typename T>
    Array<T> Make_Array(size_t size, T* data) {
        return {
            .size = size,
            .data = data
        };
    }

    bool Abort(Array<uint8_t> message) {
        fprintf(stderr, "%s\n", message.data);
        fflush(stderr);
        _Exit(-1);
        return false;
    }

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
}

|code}

let austral_memory_code = {code|

namespace A_Austral__Memory {
    extern "C" void* malloc(size_t size);
    extern "C" void* calloc(size_t num, size_t size);
    extern "C" void free(void* ptr);

    template<typename T>
    T* A_Allocate(T value) {
        // Note: we use malloc rather than calloc here
        // because we're going to write to the pointer
        // immediately.
        T* ptr = (T*) malloc(sizeof(T));
        if (ptr != NULL) {
            *ptr = value;
        }
        return ptr;
    }

    template<typename T>
    T A_Load(T* pointer) {
        return *pointer;
    }

    template<typename T>
    bool A_Store(T* pointer, T value) {
        *pointer = value;
        return false;
    }

    template<typename T>
    bool A_Deallocate(T* pointer) {
        free(pointer);
        return false;
    }

    template<typename T>
    T* A_Load_Read_Reference(T** ref) {
        return *ref;
    }

    template<typename T>
    T* A_Load_Write_Reference(T** ref) {
        return *ref;
    }

    using namespace A_Austral__Pervasive;

    template <typename T>
    A_Option<T> Make_Some(T value) {
        return {
            .tag = A_Option_Tag::A_Some,
            .data = { .A_Some = { .A_value = value } }
        };
    }

    template <typename T>
    A_Option<T> Make_None() {
        return {
            .tag = A_Option_Tag::A_None,
            .data = { .A_None = {} }
        };
    }


    template<typename T>
    T* A_Allocate_Array(uint64_t number) {
        unsigned long long elem_size = sizeof(T);
        unsigned long long num = number;
        unsigned long long array_size = 0;
        bool has_overflowed = __builtin_umulll_overflow(elem_size, num, &array_size);
        if (has_overflowed) {
            return NULL;
        } else {
            T* ptr = (T*) calloc(num, array_size);
            return ptr;
        }
    }

    template<typename T>
    A_Option<Austral__Core::Array<T>> A_Resize_Array(Austral__Core::Array<T> array, uint64_t new_number) {
        unsigned long long elem_size = sizeof(T);
        unsigned long long num = new_number;
        unsigned long long array_size = 0;
        bool has_overflowed = __builtin_umulll_overflow(elem_size, num, &array_size);
        if (has_overflowed) {
            A_Option<Austral__Core::Array<T>> result = Make_None<Austral__Core::Array<T>>();
            return result;
        } else {
            T* new_ptr = realloc(array.data, array_size);
            if (new_ptr == NULL) {
                A_Option<Austral__Core::Array<T>> result = Make_None<Austral__Core::Array<T>>();
                return result;
            } else {
                Austral__Core::Array<T> arr = Austral__Core::Make_Array(new_number, new_ptr);
                A_Option<Austral__Core::Array<T>> result = Make_Some(arr);
                return result;
            }
        }
    }

    template<typename T>
    bool A_Deallocate_Array(Austral__Core::Array<T> array) {
        free(array.data);
        return false;
    }

    template<typename T>
    size_t A_Heap_Array_Size(Austral__Core::Array<T> arr) {
        return arr.size;
    }
}
|code}
