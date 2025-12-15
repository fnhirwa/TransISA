#ifndef TRANSISA_UTILS_LOGGER_H
#define TRANSISA_UTILS_LOGGER_H

#include <iostream>
#include <string>

namespace TransISA::Utils {

enum class LogLevel { Info, Warn, Error };

void log(LogLevel Level, const std::string& Message);

} // namespace TransISA::Utils

#endif // TRANSISA_UTILS_LOGGER_H