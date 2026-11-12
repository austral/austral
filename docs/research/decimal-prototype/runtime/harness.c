/* Reads one operation per line on stdin and prints its result. Driven by check.py. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "decimal.h"

unsigned char au_abort_internal(const char *message) {
  fprintf(stderr, "%s\n", message);
  exit(3);
}

#include "decimal.c"

static au_decimal_t parse(const char *text) {
  int negative = *text == '-';
  if (negative) text++;
  unsigned __int128 n = 0;
  for (; *text; text++) n = n * 10 + (unsigned)(*text - '0');
  return negative ? -(au_decimal_t)n : (au_decimal_t)n;
}

static void show(int ok, au_decimal_t x) {
  char buf[48];
  if (!ok) { puts("fail"); return; }
  zd_format(x, 0, buf);
  puts(buf);
}

int main(void) {
  char line[512];
  char *t[10];
  while (fgets(line, sizeof line, stdin)) {
    int n = 0;
    for (char *tok = strtok(line, " \n"); tok && n < 10; tok = strtok(0, " \n")) t[n++] = tok;
    if (n == 0) continue;
    au_decimal_t r = 0;
    if (!strcmp(t[0], "add")) { int ok = zd_add(parse(t[1]), parse(t[2]), atoi(t[3]), &r); show(ok, r); }
    else if (!strcmp(t[0], "sub")) { int ok = zd_sub(parse(t[1]), parse(t[2]), atoi(t[3]), &r); show(ok, r); }
    else if (!strcmp(t[0], "mul")) show(1, zd_mul(parse(t[1]), parse(t[2])));
    else if (!strcmp(t[0], "div")) { int ok = zd_div(parse(t[1]), atoi(t[2]), parse(t[3]), atoi(t[4]), atoi(t[5]), atoi(t[6]), (zd_mode)atoi(t[7]), &r); show(ok, r); }
    else if (!strcmp(t[0], "round")) { int ok = zd_round(parse(t[1]), atoi(t[2]), atoi(t[3]), atoi(t[4]), (zd_mode)atoi(t[5]), &r); show(ok, r); }
    else if (!strcmp(t[0], "exact")) { int ok = zd_exact(parse(t[1]), atoi(t[2]), atoi(t[3]), atoi(t[4]), &r); show(ok, r); }
    else if (!strcmp(t[0], "widen")) show(1, zd_widen(parse(t[1]), atoi(t[2]), atoi(t[3])));
    else if (!strcmp(t[0], "fromdouble")) {
      union { double f; unsigned long long bits; } u;
      u.bits = strtoull(t[1], 0, 16);
      { int ok = zd_from_double(u.f, atoi(t[2]), atoi(t[3]), (zd_mode)atoi(t[4]), &r); show(ok, r); }
    } else if (!strcmp(t[0], "todouble")) {
      union { double f; unsigned long long bits; } u;
      u.f = zd_to_double(parse(t[1]), atoi(t[2]));
      printf("%016llx\n", u.bits);
    } else if (!strcmp(t[0], "lit")) show(1, au_dec_lit(atoi(t[1]), strtoull(t[2], 0, 10), strtoull(t[3], 0, 10)));
    else if (!strcmp(t[0], "print")) au_dec_debug_print(parse(t[1]), atoi(t[2]));
    else { puts("unknown"); }
  }
  return 0;
}
