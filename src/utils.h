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


unsigned int periodoToInt(Periodo p);
Periodo intToPeriodo(int period);
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

string quote(string s);
vector<string> quote(vector<string> v);
string join(vector<string> v, char j);

std::string touppercase(std::string);
NumericVector createTimeSeries(double anno, double periodo, 
                               double freq, std::vector<double> dati0);
NumericVector tsRead_nativo(std::string path);

typedef struct filename_ {
  std::string name;
  std::string ext;
} filename_t;

filename_t getFilenameFromPath(std::string path);
vector<string> get_csv_from_path(std::string path);
char* gnu_basename(char* path);

#endif
