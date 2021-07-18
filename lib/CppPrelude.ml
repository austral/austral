let prelude = {code|#include <cstdlib>
#include <cstdint>

namespace Austral__Core {
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
}

namespace A_Austral__Memory {
    template<typename T>
    T* A_Allocate(T value) {
        T* ptr = (T*) malloc(sizeof(T));
        *ptr = value;
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
