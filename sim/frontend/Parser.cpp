// Blarney Netlist Parser

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>
#include <stdarg.h>
#include "Parser.h"
#include "Netlist.h"
#include "Seq.h"

// Constructor
Parser::Parser()
{
  inputStream = NULL;
  next = 0;
  lineNumber = 1;
}

// Destructor
Parser::~Parser()
{
  if (inputStream != NULL) free(inputStream);
}

// Read file for parsing
void Parser::readFile(const char* filename)
{
  FILE* fp = fopen(filename, "rt");
  if (fp == NULL) parseError("Can't open file '%s'", filename);
  fseek(fp, 0L, SEEK_END);
  long size = ftell(fp);
  rewind(fp);
  inputStream = (char*) malloc(size+1);
  if (fread(inputStream, size, 1, fp) != 1)
    parseError("Error reading %li bytes from file '%s'", size, filename);
  inputStream[size] = '\0';
  fclose(fp);
}

// Exit with an error message and a line number
void Parser::parseError(const char* fmt, ...)
{
  va_list args;
  va_start(args, fmt);
  fprintf(stderr, "Parse error on line %i:\n", lineNumber);
  vfprintf(stderr, fmt, args);
  fprintf(stderr, "\n");
  va_end(args);
  exit(EXIT_FAILURE);
}

// Consume next character from input stream
inline char Parser::getChar()
{
  char c = inputStream[next];
  if (c == '\0') return c;
  if (c == '\n') lineNumber++;
  next++;
  return c;
}

// Undo getChar()
inline void Parser::ungetChar()
{
  assert(next > 0);
  next--;
  if (inputStream[next] == '\n') lineNumber--;
}

// Rewind stream pointer to given position
void Parser::rewindTo(int pos)
{
  for (int i = next-1; i >= pos; i--)
    if (inputStream[i] == '\n') lineNumber--;
  next = pos;
}

// Have we reached the end of the input stream?
bool Parser::isEnd()
{
  return (inputStream[next] == '\0');
}

// Consume as much whitespace as possible
void Parser::spaces()
{
  bool comment = false;
  for (;;) {
    char c = getChar();
    if (c == '\0') return;
    if (c == '\n') comment = false;
    if (c == ';') comment = true;
    if (!comment && !isspace(c)) {
      ungetChar();
      return;
    }
  }
}

// Try to consume s
bool Parser::eat(const char* s)
{
  if (isEnd()) return false;
  int begin = next;
  while (*s != '\0') {
    if (*s != getChar()) {
      rewindTo(begin);
      return false;
    }
    s++;
  }
  spaces();
  return true;
}

// Consume s or exit with a parse error
void Parser::demand(const char* s)
{
  if (!eat(s)) parseError("Expecting '%s'\n", s);
}

// Try to consume a non-negative integer
bool Parser::parseNat(unsigned* n)
{
  char nat[10];
  int i;
  if (isEnd()) return false;
  for (i = 0; i < 10; i++) {
    char c = getChar();
    if (isdigit(c))
      nat[i] = c;
    else {
      nat[i] = '\0';
      ungetChar();
      break;
    }
  }
  if (i == 0) return false;
  if (i == 10) return false;
  *n = (unsigned) atoi(nat);
  spaces();
  return true;
}

// Consume a non-negative integer or exit with a parse error
void Parser::demandNat(unsigned* n)
{
  if (!parseNat(n)) parseError("Expected number");
}

// Copy last n chars from input stream
char* Parser::copyLast(int n)
{
  char* s = (char*) malloc(n+1);
  memcpy(s, &inputStream[next-n], n);
  s[n] = '\0';
  return s;
}

// Try to consume a decimal literal
// (Allocates buffer for literal)
bool Parser::parseLit(char** s)
{
  int begin = next;
  eat("-");
  char c = getChar();
  if (! isdigit(c)) {
    ungetChar();
    return false;
  }
  for (;;) {
    c = getChar();
    if (! isdigit(c)){
      ungetChar();
      *s = copyLast(next-begin);
      spaces();
      return true;
    }
  }
}

inline bool isSpecial(char c)
{
  return c == '\0' || c == '[' || c == ']' ||
         c == ')'  || c == '(' || c == ',' ||
         c == '"';
}

// Parse a word
bool Parser::parseWord(char** word)
{
  int begin = next;
  char c = getChar();
  if (isSpecial(c)) {
    ungetChar();
    return false;
  }
  for (;;) {
    c = getChar();
    if (isspace(c) || isSpecial(c)) {
      ungetChar();
      *word = copyLast(next-begin);
      spaces();
      return true;
    }
  }
}

// Parse a word or exit with a parse error
void Parser::demandWord(char** s)
{
  if (! parseWord(s)) parseError("Expected word");
}

// Demand a quoted string or exit with a parse error
void Parser::demandStr(char** s)
{
  int begin = next;
  demand("\"");
  bool escape = false;
  for (;;) {
    char c = getChar();
    if (!escape && c == '\\') escape = true;
    else if (c == '"' && !escape) {
      *s = copyLast(next-begin);
      spaces();
      return;
    }
    else escape = false;
  }
}

// Parse a net or exit with a parse error
void Parser::demandNet(Net* net)
{
  demandNat(&net->id);
  demandWord(&net->prim);
  demandInputs(&net->inputs);
  demandParams(&net->params);
  demandNat(&net->width);
}

// Parse net inputs or exit with a parse error
void Parser::demandInputs(Seq<NetInput>* inputs)
{
  NetInput input;
  demand("[");
  if (eat("]")) return;
  do {
    demand("(");
    demandNat(&input.id);
    demand(",");
    demandNat(&input.pin);
    demand(")");
    inputs->append(input);
  } while (eat(","));
  demand("]");
}

// Parse net parameters or exit with a parse error
void Parser::demandParams(Seq<NetParam>* params)
{
  NetParam param;
  demand("[");
  if (eat("]")) return;
  do {
    demandStr(&param.key);
    demand(":->");
    demandStr(&param.val);
    params->append(param);
  } while (eat(","));
  demand("]");
}

// Parse a netlist or exit with a parse error
void Parser::demandNetlist(Netlist* netlist)
{
  spaces();
  while (!isEnd()) {
    netlist->extend();
    demandNet(&netlist->elems[netlist->numElems-1]);
  }
}
