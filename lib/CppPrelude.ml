let prelude = {code|#include <cstdlib>
#include <cstdint>
#include <cstdio>

namespace Austral__Core {
    void Abort(const char* message, size_t size) {
        fwrite(message, 1, size, stderr);
        fputc('\n', stderr);
        _Exit(EXIT_FAILURE);
    }

    template<typename T>
    struct Array {
        size_t size;
        T* data;
    };

    template<typename T>
    Array<T> Make_Array(size_t size, T* data) {
        return {
            .size = size,
            .data = data
        };
    }

    template<typename T>
    T Array_Nth(Array<T> array, size_t index) {
        if (index >= array.size) {
            Abort("Array index out of bounds.", 26);
        }
        return array.data[index];
    }
}

namespace A_Austral__Pervasive {
    enum A_Option_Tag {
        A_None, A_Some
    };

    template<typename A_T>
    struct A_Option {
        A_Option_Tag tag;
        union {
            struct {} A_None;
            struct {
                A_T A_value;
            } A_Some;
        } data;
    };

    template <typename T>
    A_Option<T> Some(T value) {
        return {
            .tag = A_Option_Tag::A_Some,
            .data = { .A_Some = { .A_value = value } }
        };
    }

    template <typename T>
    A_Option<T> None() {
        return {
            .tag = A_Option_Tag::A_None,
            .data = { .A_None = {} }
        };
    }
}

namespace A_Austral__Memory {
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
    A_Austral__Pervasive::A_Option<Austral__Core::Array<T>> A_Allocate_Array(size_t number) {
        unsigned long long elem_size = sizeof(T);
        unsigned long long num = number;
        unsigned long long array_size = 0;
        bool has_overflowed = __builtin_umulll_overflow(elem_size, num, &array_size);
        if (has_overflowed) {
            A_Austral__Pervasive::A_Option<T> result = A_Austral__Pervasive::None<T>();
            return result;
        } else {
            T* ptr = calloc(num, array_size);
            if (ptr == NULL) {
                A_Austral__Pervasive::A_Option<T> result = A_Austral__Pervasive::None<T>();
                return result;
            } else {
                Austral__Core::Array<T> arr = Austral__Core::Make_Array(number, ptr);
                A_Austral__Pervasive::A_Option<T> result = A_Austral__Pervasive::Some(arr);
                return result;
            }
        }
    }

    template<typename T>
    A_Austral__Pervasive::A_Option<Austral__Core::Array<T>> A_Resize_Array(Austral__Core::Array<T> array, size_t new_number) {
        unsigned long long elem_size = sizeof(T);
        unsigned long long num = new_number;
        unsigned long long array_size = 0;
        bool has_overflowed = __builtin_umulll_overflow(elem_size, num, &array_size);
        if (has_overflowed) {
            A_Austral__Pervasive::A_Option<T> result = A_Austral__Pervasive::None<T>();
            return result;
        } else {
            T* new_ptr = realloc(array.data, array_size);
            if (new_ptr == NULL) {
                A_Austral__Pervasive::A_Option<T> result = A_Austral__Pervasive::None<T>();
                return result;
            } else {
                Austral__Core::Array<T> arr = Austral__Core::Make_Array(new_number, new_ptr);
                A_Austral__Pervasive::A_Option<T> result = A_Austral__Pervasive::Some(arr);
                return result;
            }
        }
    }

    template<typename T>
    bool A_Deallocate_Array(Austral__Core::Array<T> array) {
        free(array.data);
        return false;
    }
}
|code}
