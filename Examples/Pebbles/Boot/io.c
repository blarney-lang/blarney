#include <stdarg.h>
#include "pebbles.h"
#include <io.h>

int putchar(int c)
{
  while (! uartCanPut());
  uartPut(c);
  return c;
}

int puts(const char* s)
{
  int count = 0;
  while (*s) { putchar(*s); s++; count++; }
  return count;
}

int puthex(unsigned x)
{
  int count = 0;

  for (count = 0; count < 8; count++) {
    unsigned nibble = x >> 28;
    putchar(nibble > 9 ? ('a'-10)+nibble : '0'+nibble);
    x = x << 4;
  }

  return 8;
}

int printf(const char* fmt, ...)
{
  int count = 0;
  va_list args;

  va_start(args, fmt);

  while (*fmt) {
    if (*fmt == '%') {
      fmt++;
      if (*fmt == '\0') break;
      if (*fmt == 's') count += puts(va_arg(args, char*));
      if (*fmt == 'x') count += puthex(va_arg(args, unsigned));
    }
    else { putchar(*fmt); count++; }
    fmt++;
  }

  va_end(args);

  return count;
}
