#ifndef TOKEN_H
#define TOKEN_H

#include <string>

enum class TokenType {
    INSTRUCTION,
    REGISTER,
    IMMEDIATE,
    PUNCTUATION,
    END_OF_FILE
};

struct Token {
    TokenType type;
    std::string value;
    size_t line;
    size_t column;
};

#endif // TOKEN_H
