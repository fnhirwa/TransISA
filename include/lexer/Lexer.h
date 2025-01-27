#ifndef LEXER_H
#define LEXER_H

#include <string>
#include <vector>
#include "lexer/Token.h"

class Lexer {
    public:
        explicit Lexer(const std::string& source);
        std::vector<Token> tokenize();
    private:
        std::string source;
        size_t position = 0;
        size_t line = 1;
        size_t column = 1;

        char peek();
        char advance();
        void skipWhitespace();
        Token readInstruction();
        Token readRegister();
        Token readImmediate();
        Token readPunctuation();
};

#endif // LEXER_H
