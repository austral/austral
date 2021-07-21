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

namespace A_Austral__Memory {
    template<typename T>
    T* A_Allocate(T value) {
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
}
|code}
