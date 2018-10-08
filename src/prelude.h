#include <cinttypes>
#include <tuple>

/* Checked arithmetic */

#if __has_builtin(__builtin_add_overflow)

std::tuple<uint8_t, bool> austral_checked_add(uint8_t lhs, uint8_t rhs) {
  uint8_t result = 0;
  bool overflowed = __builtin_add_overflow(lhs, rhs, &result);
  return std::make_tuple(result, overflowed);
}

std::tuple<int8_t, bool> austral_checked_add(int8_t lhs, int8_t rhs) {
  int8_t result = 0;
  bool overflowed = __builtin_add_overflow(lhs, rhs, &result);
  return std::make_tuple(result, overflowed);
}

std::tuple<uint16_t, bool> austral_checked_add(uint16_t lhs, uint16_t rhs) {
  uint16_t result = 0;
  bool overflowed = __builtin_add_overflow(lhs, rhs, &result);
  return std::make_tuple(result, overflowed);
}

std::tuple<int16_t, bool> austral_checked_add(int16_t lhs, int16_t rhs) {
  int16_t result = 0;
  bool overflowed = __builtin_add_overflow(lhs, rhs, &result);
  return std::make_tuple(result, overflowed);
}

#else
#error "Checked arithmetic intrinstics not found"
#endif
