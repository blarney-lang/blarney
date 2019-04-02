#ifndef _PEBBLES_H_
#define _PEBBLES_H_

// Control/status registers
#define CSR_SimEmit        "0x800"
#define CSR_SimFinish      "0x801"
#define CSR_UARTCanPut     "0x802"
#define CSR_UARTPut        "0x803"
#define CSR_UARTCanGet     "0x804"
#define CSR_UARTGet        "0x805"

// Emit char to console (simulation only)
inline void emit(char c)
{
  asm volatile("csrw " CSR_SimEmit ", %0\n" : : "r"(c));
}

// Terminate simulator (simulation only)
inline void finish()
{
  asm volatile("csrw " CSR_SimFinish ", zero\n" : :);
}

// Can write to UART?
inline int uartCanPut()
{
  int x;
  asm volatile("csrrw %0, " CSR_UARTCanPut ", zero" : "=r"(x));
  return x;
}

// Write to UART
inline void uartPut(char c)
{
  asm volatile("csrw " CSR_UARTPut ", %0\n" : : "r"(c));
}

// Can write to UART?
inline int uartCanGet()
{
  int x;
  asm volatile("csrrw %0, " CSR_UARTCanGet ", zero" : "=r"(x));
  return x;
}

// Can write to UART?
inline int uartGet()
{
  int x;
  asm volatile ("csrrw %0, " CSR_UARTGet ", zero" : "=r"(x));
  return x;
}

#endif
