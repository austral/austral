let prelude = {code|#include <stdlib.h>

namespace Austral__Memory {
    template<typename T>
    T* Allocate(T value) {
        T* ptr = (T*) malloc(sizeof(T));
        return ptr;
    }

    template<typename T>
    T Load(T* pointer) {
        return *pointer;
    }

    template<typename T>
    bool Store(T* pointer, T value) {
        *pointer = value;
        return false;
    }

    template<typename T>
    bool Deallocate(T* pointer) {
        free(pointer);
        return false;
    }
};
|code}
