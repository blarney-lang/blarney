// Blarney Netlist Parser

#ifndef _PARSER_H_
#define _PARSER_H_

#include "Netlist.h"

class Parser {
  public:

    // State
    // -----

    // Input stream
    char* inputStream;

    // Stream pointer 
    int next;

    // Current line number
    int lineNumber;

    // Methods
    // -------

    // Constructor
    Parser();

    // Destructor
    ~Parser();

    // Read file for parsing
    void readFile(const char* filename);

    // Exit with a parse error
    void parseError(const char* fmt, ...);

    // Consume next character from input stream
    inline char getChar();

    // Undo getChar()
    inline void ungetChar();

    // Rewind stream pointer to given position
    void rewindTo(int pos);

    // Have we reached the end of the input stream?
    bool isEnd();

    // Consume as much whitespace as possible
    void spaces();

    // Try to consume s
    bool eat(const char* s);

    // Consume s or exit with a parse error
    void demand(const char* s);

    // Try to consume a non-negative integer
    bool parseNat(unsigned* n);

    // Consume a non-negative integer or exit with a parse error
    void demandNat(unsigned* n);

    // Copy last n chars from input stream
    char* copyLast(int n);

    // Try to consume a decimal literal
    // (Allocates buffer for literal)
    bool parseLit(char** s);

    // Parse a word
    bool parseWord(char** s);

    // Parse a word or exit with a parse error
    void demandWord(char** s);

    // Demand a quoted string or exit with a parse error
    void demandStr(char** s);

    // Parse net inputs or exit with a parse error
    void demandInputs(Seq<NetInput>* inputs);

    // Parse net parameters or exit with a parse error
    void demandParams(Seq<NetParam>* params);

    // Parse a net or exit with a parse error
    void demandNet(Net* net);

    // Parse a netlist or exit with a parse error
    void demandNetlist(Netlist* netlist);
};

#endif
