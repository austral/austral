#include <cinttypes>
#include <iostream>
#include <tuple>

/* Checked arithmetic */

#if __has_builtin(__builtin_add_overflow)

template<typename T>
std::tuple<T, bool> austral_checked_add(T lhs, T rhs) {
  T result = 0;
  bool overflowed = __builtin_add_overflow(lhs, rhs, &result);
  return std::make_tuple(result, overflowed);
}

template<typename T>
std::tuple<T, bool> austral_checked_sub(T lhs, T rhs) {
  T result = 0;
  bool overflowed = __builtin_sub_overflow(lhs, rhs, &result);
  return std::make_tuple(result, overflowed);
}

#else
#error "Checked arithmetic intrinstics not found"
#endif
