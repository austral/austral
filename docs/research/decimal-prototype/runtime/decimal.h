/* Throwaway prototype of the Zustral Decimal runtime (issue #19). Not production code.

   A Decimal[P, S] value is its coefficient: value = coefficient * 10^-S,
   with |coefficient| < 10^P and 1 <= P <= 38, 0 <= S <= P.
   Every operation is defined on the exact mathematical result: round once
   to the target scale, then compare with the target precision. */

typedef __int128 au_decimal_t;

/* The order is the case order of the RoundingMode union. */
typedef enum {
  ZD_NEAREST_EVEN,
  ZD_NEAREST_AWAY_FROM_ZERO,
  ZD_NEAREST_TOWARD_ZERO,
  ZD_AWAY_FROM_ZERO,
  ZD_TOWARD_ZERO,
  ZD_TOWARD_GREATER,
  ZD_TOWARD_LESSER
} zd_mode;

/* Checked forms return 1 and write *out on success, 0 on failure. */
int zd_add(au_decimal_t a, au_decimal_t b, int p, au_decimal_t *out);
int zd_sub(au_decimal_t a, au_decimal_t b, int p, au_decimal_t *out);
/* Exact. The compiler guarantees P1 + P2 <= 38. */
au_decimal_t zd_mul(au_decimal_t a, au_decimal_t b);
/* a: scale s1, b: scale s2, result: Decimal[p3, s3]. Fails on a zero divisor or overflow. */
int zd_div(au_decimal_t a, int s1, au_decimal_t b, int s2, int p3, int s3, zd_mode mode, au_decimal_t *out);
/* x: scale s, result: Decimal[p2, s2]. Fails on overflow after rounding. */
int zd_round(au_decimal_t x, int s, int p2, int s2, zd_mode mode, au_decimal_t *out);
/* Fails on overflow or when a nonzero digit would be dropped. */
int zd_exact(au_decimal_t x, int s, int p2, int s2, au_decimal_t *out);
/* Cannot fail. The compiler guarantees s2 >= s and that the result fits. */
au_decimal_t zd_widen(au_decimal_t x, int s, int s2);
/* Rounds the exact binary value of f. Fails on NaN, infinity or overflow. */
int zd_from_double(double f, int p2, int s2, zd_mode mode, au_decimal_t *out);
/* The Float64 nearest to the value. */
double zd_to_double(au_decimal_t x, int s);

/* Trapping forms for generated code. They abort the program on failure. */
au_decimal_t au_dec_add(au_decimal_t a, au_decimal_t b, int p);
au_decimal_t au_dec_sub(au_decimal_t a, au_decimal_t b, int p);
au_decimal_t au_dec_div(au_decimal_t a, int s1, au_decimal_t b, int s2, int p3, int s3, int mode);
au_decimal_t au_dec_round(au_decimal_t x, int s, int p2, int s2, int mode);
au_decimal_t au_dec_exact(au_decimal_t x, int s, int p2, int s2);
au_decimal_t au_dec_from_double(double f, int p2, int s2, int mode);

/* A coefficient literal: (negative ? -1 : 1) * (hi * 10^19 + lo). C has no 128-bit literals. */
au_decimal_t au_dec_lit(int negative, unsigned long long hi, unsigned long long lo);
/* Prototype-only output: the value with exactly s fractional digits, then a newline. */
void au_dec_debug_print(au_decimal_t x, int s);
