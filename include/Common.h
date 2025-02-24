/*
The Common.h header file conains utility functions that are used by multiple
parts of the program. This includes loggers and error handling functions.
*/
#ifndef COMMON_H
#define COMMON_H

#include <string>

void logError(const std::string& message);
void logInfo(const std::string& message);

#endif // COMMON_H