#include <pebbles.h>

int main()
{
  const char* str = "hello world\n";
  for (int i = 0; i < 12; i++) {
    while (!uartCanPut());
    uartPut(str[i]);
  }
  finish();

  return 0;
}
