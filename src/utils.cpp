#include "utils.h"

#include <cmath>
#include <cctype>
#include <locale>
#include <dirent.h>
#include <fstream>
#include <cerrno>
#include <algorithm>
#include <functional> 

NumericVector createTimeSeries(double anno, double periodo, 
                                 double freq, std::vector<double> dati0) {

  double start = anno + periodo/freq - 1/freq;
  double end = start + dati0.size()/freq - 1/freq; 

  NumericVector dati(dati0.begin(), dati0.end());

  dati.attr("tsp") = NumericVector::create(start, end, freq);
  dati.attr("class") = "ts";
  return dati; 
}


//' tsWrite nativo
//'
//' Scrive una timeseries in formato CSV (legacy support)
//' @name tsWrite_nativo
//' @param series timeSeries
//' @param path percorso del file
//' @import Rcpp
//' @useDynLib rdataset
//
// [[Rcpp::export]]

void tsWrite_nativo(NumericVector series, std::string path) {
  std::ofstream output;
  output.open(path.c_str());
  NumericVector tsp = series.attr("tsp");
  unsigned int freq = tsp[2];
  float start = tsp[0];
  Periodo p = floatToPeriodo(start, freq);
  
  output << p.anno << '\n' << p.periodo << '\n' << freq << '\n';
  
  for(NumericVector::iterator it = series.begin(); it != series.end(); ++it) {
    output << *it << '\n';
  }
  output.close();
}

//' tsRead implementato nativamente.
//'
//' @name tsRead_nativo
//' @param path a path to the CSV file representing a timeseries
//' @return a `ts` object
//' @import Rcpp
//' @useDynLib rdataset
//
// [[Rcpp::export]]

NumericVector tsRead_nativo(std::string path) {
  std::vector<double> dati;
  std::vector<double> numeri;    
  std::ifstream fin(path.c_str());
  std::string line;
  double value = 0.;
  int anno = 0;
  int periodo = 0;
  int freq = 0;
  unsigned int lineCounter = 0;
  while(getline(fin, line)) {
    line = trim(line);          
    if(lineCounter > 2) {
      if(line.compare("?") == 0 || line.compare("NA") == 0) {
        value = NA_REAL;
      } else {
        value = atof(line.c_str());
     }      
      dati.push_back(value);       
    } else if(lineCounter == 0) {
      anno = atoi(line.c_str());
    } else if(lineCounter==1) {     
      periodo = atoi(line.c_str());
    } else if(lineCounter==2) {
      freq = atoi(line.c_str());
    }   
    lineCounter++;
  }
  fin.close();
  return createTimeSeries(anno, periodo, freq, dati); 
}

/*
unsigned int periodoToInt(Periodo p) {
  return p.anno * 1000 + p.periodo;
}

/*
Periodo intToPeriodo(int intperiod) {
  unsigned int anno = floor(intperiod/1000);
  unsigned int period = intperiod - ( anno * 1000 );
  
  Periodo p = {
    anno,
    period
  };
  return p;
}
*/

Periodo floatToPeriodo(const float floatPeriod, const unsigned int freq) {
  unsigned int anno = floor(floatPeriod);
  float diff = floatPeriod - float(anno);
  unsigned int period = 1 + int(diff * freq);

  if(period > freq) {
    period = 1;
    ++anno;
  }
  
  Periodo p = {
    anno,
    period
  };

  
  return p;
}

/*
float periodoToFloat(const Periodo periodo, const unsigned int freq) {
  return float(periodo.anno) + float(periodo.periodo)/freq;
}
*/
