#ifndef _IO_H_
#define _IO_H_

#ifdef __cplusplus
extern "C" {
#endif

int putchar(int c);
int puts(const char* s);
int puthex(unsigned x);
int printf(const char* fmt, ...);

#ifdef __cplusplus
}
#endif

#endif
