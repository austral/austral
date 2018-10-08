#include <cinttypes>
#include <tuple>

/* Checked arithmetic */

#if __has_builtin(__builtin_add_overflow)

std::tuple<uint8_t, bool> austral_checked_add(uint8_t lhs, uint8_t rhs) {
  unsigned char result = 0;
  bool overflowed = __builtin_add_overflow(lhs, rhs, &result);
  return std::make_tuple(result, overflowed);
}

#else
#error "Checked arithmetic intrinstics not found"
#endif
