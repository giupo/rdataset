#ifndef __UTILS_HPP__
#define __UTILS_HPP__

#include <string>
#include <vector>

#include <Rcpp.h>

using namespace std;
using namespace Rcpp;

typedef std::string Nome;
typedef std::vector<Nome> ListaNomi;

typedef struct Periodo {
  unsigned int anno;
  unsigned int periodo;
} Periodo;


// unsigned int periodoToInt(Periodo p);
// Periodo intToPeriodo(int period);
Periodo floatToPeriodo(const float floatPeriod, const unsigned int freq);
// float periodoToFloat(const Periodo p, const unsigned int freq);

// trim from start
static inline std::string &ltrim(std::string &s) {
  s.erase(s.begin(), 
          std::find_if(s.begin(), s.end(), 
                       std::not1(std::ptr_fun<int, int>(std::isspace))));
  return s;
}
// trim from end
static inline std::string &rtrim(std::string &s) {
  s.erase(std::find_if(s.rbegin(), 
                       s.rend(), 
                       std::not1(std::ptr_fun<int, int>(std::isspace))).base(), 
          s.end());
  return s;
}

// trim from both ends
static inline std::string &trim(std::string &s) {
  return ltrim(rtrim(s));
}

NumericVector createTimeSeries(double anno, double periodo, 
                               double freq, std::vector<double> dati0);
NumericVector tsRead_nativo(std::string path);

void tsWrite_nativo(NumericVector series, std::string path);

#endif
