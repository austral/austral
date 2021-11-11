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
    A_Option<Austral__Core::Array<T>> A_Allocate_Array(uint64_t number) {
        unsigned long long elem_size = sizeof(T);
        unsigned long long num = number;
        unsigned long long array_size = 0;
        bool has_overflowed = __builtin_umulll_overflow(elem_size, num, &array_size);
        if (has_overflowed) {
            A_Option<Austral__Core::Array<T>> result = Make_None<Austral__Core::Array<T>>();
            return result;
        } else {
            T* ptr = (T*) calloc(num, array_size);
            if (ptr == NULL) {
                A_Option<Austral__Core::Array<T>> result = Make_None<Austral__Core::Array<T>>();
                return result;
            } else {
                Austral__Core::Array<T> arr = Austral__Core::Make_Array(number, ptr);
                A_Option<Austral__Core::Array<T>> result = Make_Some(arr);
                return result;
            }
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
