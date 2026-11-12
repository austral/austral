/* Throwaway prototype of the Zustral Decimal runtime (issue #19). Not production code.
   Implements decimal.h with unsigned 128-bit integers only. */

typedef unsigned __int128 zd_u128;

unsigned char au_abort_internal(const char *message);

static zd_u128 zd_pow10(int n) {
  zd_u128 r = 1;
  while (n-- > 0) r *= 10;
  return r;
}

static zd_u128 zd_abs(au_decimal_t x) {
  return x < 0 ? -(zd_u128)x : (zd_u128)x;
}

/* What was cut off below the last kept digit, relative to half a unit of it. */
typedef enum { ZD_NOTHING, ZD_BELOW_HALF, ZD_HALF, ZD_ABOVE_HALF } zd_discarded;

static int zd_rounds_away(zd_mode mode, int negative, int kept_is_odd, zd_discarded d) {
  if (d == ZD_NOTHING) return 0;
  switch (mode) {
    case ZD_NEAREST_EVEN:           return d == ZD_ABOVE_HALF || (d == ZD_HALF && kept_is_odd);
    case ZD_NEAREST_AWAY_FROM_ZERO: return d != ZD_BELOW_HALF;
    case ZD_NEAREST_TOWARD_ZERO:    return d == ZD_ABOVE_HALF;
    case ZD_AWAY_FROM_ZERO:         return 1;
    case ZD_TOWARD_ZERO:            return 0;
    case ZD_TOWARD_GREATER:         return !negative;
    case ZD_TOWARD_LESSER:          return negative;
  }
  return 0;
}

/* 0 <= rem < divisor <= 10^38, so 2 * rem cannot wrap. */
static zd_discarded zd_classify(zd_u128 rem, zd_u128 divisor) {
  if (rem == 0) return ZD_NOTHING;
  zd_u128 twice = rem * 2;
  return twice < divisor ? ZD_BELOW_HALF : twice == divisor ? ZD_HALF : ZD_ABOVE_HALF;
}

static int zd_finish(zd_u128 kept, zd_discarded d, int negative, int p, zd_mode mode, au_decimal_t *out) {
  if (zd_rounds_away(mode, negative, (int)(kept & 1), d)) kept += 1;
  if (kept >= zd_pow10(p)) return 0;
  *out = negative ? -(au_decimal_t)kept : (au_decimal_t)kept;
  return 1;
}

int zd_add(au_decimal_t a, au_decimal_t b, int p, au_decimal_t *out) {
  au_decimal_t r;
  if (__builtin_add_overflow(a, b, &r)) return 0;
  if (zd_abs(r) >= zd_pow10(p)) return 0;
  *out = r;
  return 1;
}

int zd_sub(au_decimal_t a, au_decimal_t b, int p, au_decimal_t *out) {
  au_decimal_t r;
  if (__builtin_sub_overflow(a, b, &r)) return 0;
  if (zd_abs(r) >= zd_pow10(p)) return 0;
  *out = r;
  return 1;
}

au_decimal_t zd_mul(au_decimal_t a, au_decimal_t b) {
  return a * b;
}

int zd_div(au_decimal_t a, int s1, au_decimal_t b, int s2, int p3, int s3, zd_mode mode, au_decimal_t *out) {
  if (b == 0) return 0;
  int negative = (a < 0) != (b < 0);
  zd_u128 na = zd_abs(a), nb = zd_abs(b);
  zd_u128 q = na / nb, r = na % nb;
  /* The result coefficient is (a / b) * 10^k. */
  int k = s2 + s3 - s1;
  if (k >= 0) {
    /* Long division, one decimal digit at a time. 10 * r can exceed 128 bits,
       so each digit is found by adding r ten times; the running sum stays below 2 * nb. */
    for (int i = 0; i < k; i++) {
      if (q >= zd_pow10(37)) return 0;
      zd_u128 acc = 0;
      unsigned digit = 0;
      for (int j = 0; j < 10; j++) {
        acc += r;
        if (acc >= nb) { acc -= nb; digit++; }
      }
      q = q * 10 + digit;
      r = acc;
    }
    return zd_finish(q, zd_classify(r, nb), negative, p3, mode, out);
  }
  /* k < 0: drop -k digits of the integer quotient. The fraction r / nb only
     matters for telling an exact half from more than half. */
  zd_u128 unit = zd_pow10(-k);
  zd_u128 low = q % unit;
  zd_discarded d = zd_classify(low, unit);
  if (r != 0) {
    if (d == ZD_NOTHING) d = ZD_BELOW_HALF;
    else if (d == ZD_HALF) d = ZD_ABOVE_HALF;
  }
  return zd_finish(q / unit, d, negative, p3, mode, out);
}

static int zd_rescale(au_decimal_t x, int s, int p2, int s2, zd_mode mode, int must_be_exact, au_decimal_t *out) {
  int negative = x < 0;
  zd_u128 n = zd_abs(x);
  if (s2 >= s) {
    int up = s2 - s;
    if (n != 0 && (up > p2 || n >= zd_pow10(p2 - up))) return 0;
    n *= zd_pow10(up);
    *out = negative ? -(au_decimal_t)n : (au_decimal_t)n;
    return 1;
  }
  zd_u128 unit = zd_pow10(s - s2);
  zd_discarded d = zd_classify(n % unit, unit);
  if (must_be_exact && d != ZD_NOTHING) return 0;
  return zd_finish(n / unit, d, negative, p2, mode, out);
}

int zd_round(au_decimal_t x, int s, int p2, int s2, zd_mode mode, au_decimal_t *out) {
  return zd_rescale(x, s, p2, s2, mode, 0, out);
}

int zd_exact(au_decimal_t x, int s, int p2, int s2, au_decimal_t *out) {
  return zd_rescale(x, s, p2, s2, ZD_TOWARD_ZERO, 1, out);
}

au_decimal_t zd_widen(au_decimal_t x, int s, int s2) {
  return x * (au_decimal_t)zd_pow10(s2 - s);
}

int zd_from_double(double f, int p2, int s2, zd_mode mode, au_decimal_t *out) {
  union { double f; unsigned long long bits; } u;
  u.f = f;
  int negative = (int)(u.bits >> 63);
  int biased = (int)((u.bits >> 52) & 0x7ff);
  unsigned long long fraction = u.bits & 0xfffffffffffffULL;
  if (biased == 0x7ff) return 0;
  /* f = mantissa * 2^exponent exactly. */
  unsigned long long mantissa = biased == 0 ? fraction : fraction | (1ULL << 52);
  int exponent = biased == 0 ? -1074 : biased - 1075;
  if (mantissa == 0) { *out = 0; return 1; }

  if (exponent >= 0) {
    if (exponent > 74) return 0;
    zd_u128 n = (zd_u128)mantissa << exponent;
    if (n >= zd_pow10(38)) return 0;
    return zd_rescale(negative ? -(au_decimal_t)n : (au_decimal_t)n, 0, p2, s2, mode, 0, out);
  }

  int shift = -exponent;
  unsigned long long whole = shift < 64 ? mantissa >> shift : 0;
  unsigned long long rest = shift < 64 ? mantissa - (whole << shift) : mantissa;
  if (whole >= zd_pow10(p2 - s2)) return 0;

  /* scaled = rest * 10^s2 as four 64-bit limbs; the kept digits are scaled >> shift. */
  zd_u128 unit = zd_pow10(s2);
  zd_u128 lo = (zd_u128)rest * (unsigned long long)unit;
  zd_u128 hi = (zd_u128)rest * (unsigned long long)(unit >> 64) + (lo >> 64);
  unsigned long long w[4] = { (unsigned long long)lo, (unsigned long long)hi, (unsigned long long)(hi >> 64), 0 };

  zd_u128 kept = 0;
  int below_half_bit = 0, half_bit = 0;
  for (int bit = 255; bit >= 0; bit--) {
    int set = (int)((w[bit / 64] >> (bit % 64)) & 1);
    if (bit >= shift) kept = (kept << 1) | (zd_u128)set;
    else if (bit == shift - 1) half_bit = set;
    else below_half_bit |= set;
  }
  zd_discarded d = half_bit ? (below_half_bit ? ZD_ABOVE_HALF : ZD_HALF)
                            : (below_half_bit ? ZD_BELOW_HALF : ZD_NOTHING);
  return zd_finish((zd_u128)whole * unit + kept, d, negative, p2, mode, out);
}

/* Writes the value with exactly s fractional digits; returns the length. */
static int zd_format(au_decimal_t x, int s, char *buf) {
  char digits[40];
  int count = 0;
  zd_u128 n = zd_abs(x);
  do { digits[count++] = (char)('0' + (int)(n % 10)); n /= 10; } while (n != 0);
  while (count <= s) digits[count++] = '0';
  int len = 0;
  if (x < 0) buf[len++] = '-';
  for (int i = count - 1; i >= 0; i--) {
    buf[len++] = digits[i];
    if (i == s && s != 0) buf[len++] = '.';
  }
  buf[len] = 0;
  return len;
}

double zd_to_double(au_decimal_t x, int s) {
  extern double strtod(const char *text, char **end);
  char buf[48];
  zd_format(x, s, buf);
  return strtod(buf, 0);
}

au_decimal_t au_dec_add(au_decimal_t a, au_decimal_t b, int p) {
  au_decimal_t r = 0;
  if (!zd_add(a, b, p, &r)) au_abort_internal("Overflow in Decimal addition");
  return r;
}

au_decimal_t au_dec_sub(au_decimal_t a, au_decimal_t b, int p) {
  au_decimal_t r = 0;
  if (!zd_sub(a, b, p, &r)) au_abort_internal("Overflow in Decimal subtraction");
  return r;
}

au_decimal_t au_dec_div(au_decimal_t a, int s1, au_decimal_t b, int s2, int p3, int s3, int mode) {
  au_decimal_t r = 0;
  if (b == 0) au_abort_internal("Division by zero in Decimal divide");
  if (!zd_div(a, s1, b, s2, p3, s3, (zd_mode)mode, &r)) au_abort_internal("Overflow in Decimal divide");
  return r;
}

au_decimal_t au_dec_round(au_decimal_t x, int s, int p2, int s2, int mode) {
  au_decimal_t r = 0;
  if (!zd_round(x, s, p2, s2, (zd_mode)mode, &r)) au_abort_internal("Overflow in Decimal round");
  return r;
}

au_decimal_t au_dec_exact(au_decimal_t x, int s, int p2, int s2) {
  au_decimal_t r = 0;
  if (!zd_exact(x, s, p2, s2, &r)) au_abort_internal("Value not representable in Decimal exact");
  return r;
}

au_decimal_t au_dec_from_double(double f, int p2, int s2, int mode) {
  au_decimal_t r = 0;
  if (!zd_from_double(f, p2, s2, (zd_mode)mode, &r)) au_abort_internal("Value not representable in Decimal round from Float64");
  return r;
}

au_decimal_t au_dec_lit(int negative, unsigned long long hi, unsigned long long lo) {
  zd_u128 n = (zd_u128)hi * 10000000000000000000ULL + lo;
  return negative ? -(au_decimal_t)n : (au_decimal_t)n;
}

/* The embedder defines zd_write_line. Declaring puts here would clash with a
   program that imports puts under its own C prototype. */
void au_dec_debug_print(au_decimal_t x, int s) {
  char buf[48];
  zd_format(x, s, buf);
  zd_write_line(buf);
}
