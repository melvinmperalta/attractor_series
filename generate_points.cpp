#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
DataFrame clifford(int n, double a, double b, double c, double d, double e) {
  NumericVector x(n);
  NumericVector y(n);
  NumericVector z(n);
  x[0] = 0;
  y[0] = 0;
  z[0] = e;
  for(int i = 0; i < n - 1; i++) {
    x[i+1] = sin(a*y[i] + z[i]) + c*cos(a*x[i]);
    y[i+1] = sin(b*x[i]) + d*cos(b*y[i] + z[i]);
    z[i+1] = z[i] + e;
  }
  DataFrame df = DataFrame::create(_["x"] = x, _["y"] = y, _["z"] = z);
  return df;
}

// [[Rcpp::export]]
DataFrame ikeda(int n, double x0, double y0, double u, double r, double p) {
  NumericVector x(n);
  NumericVector y(n);
  x[0] = x0;
  y[0] = y0;
  double t;
  for(int i = 0; i < n - 1; i++) {
    t = r - p/(1 + x[i]*x[i] + y[i]*y[i]);
    x[i+1] = 1 + u*(x[i]*cos(t) - y[i]*sin(t));
    y[i+1] = u*(x[i]*sin(t) + y[i]*cos(t));
  }
  DataFrame df = DataFrame::create(_["x"] = x, _["y"] = y);
  return df;
}

// [[Rcpp::export]]
DataFrame ikeda_time(int n, double x0, double y0, double u, double r, double p, double c) {
  NumericVector x(n);
  NumericVector y(n);
  NumericVector z(n);
  x[0] = x0;
  y[0] = y0;
  z[0] = 1;
  double t;
  for(int i = 0; i < n - 1; i++) {
    t = r - p/(1 + x[i]*x[i] + y[i]*y[i]);
    x[i+1] = 1 + u*(x[i]*cos(t*z[i]) - y[i]*sin(t));
    y[i+1] = u*(x[i]*sin(t) + y[i]*cos(t));
    z[i+1] = z[i] + c;
  }
  DataFrame df = DataFrame::create(_["x"] = x, _["y"] = y, _["z"] = z);
  return df;
}

// [[Rcpp::export]]
DataFrame ikeda_time2(int n, double x0, double y0, double u, double r, double p, double c) {
  NumericVector x(n);
  NumericVector y(n);
  NumericVector z(n);
  x[0] = x0;
  y[0] = y0;
  z[0] = 1;
  double t;
  for(int i = 0; i < n - 1; i++) {
    t = r - p/(1 + x[i]*x[i] + y[i]*y[i]);
    x[i+1] = 1 + u*(x[i]*cos(t*z[i]) - y[i]*sin(t));
    y[i+1] = u*(x[i]*sin(t) + y[i]*cos(t));
    z[i+1] = z[i]*c;
  }
  DataFrame df = DataFrame::create(_["x"] = x, _["y"] = y, _["z"] = z);
  return df;
}

// [[Rcpp::export]]
DataFrame ikeda_time3(int n, double x0, double y0, double u, double r, double p, double c) {
  NumericVector x(n);
  NumericVector y(n);
  NumericVector z(n);
  x[0] = x0;
  y[0] = y0;
  z[0] = 1;
  double t;
  for(int i = 0; i < n - 1; i++) {
    t = r - p/(1 + x[i]*x[i] + y[i]*y[i]);
    x[i+1] = 1 + u*(x[i]*cos(t) - y[i]*sin(t));
    y[i+1] = u*(x[i]*sin(t) + y[i]*cos(t*z[i]));
    z[i+1] = z[i]*c;
  }
  DataFrame df = DataFrame::create(_["x"] = x, _["y"] = y, _["z"] = z);
  return df;
}

// [[Rcpp::export]]
DataFrame ikeda_time4(int n, double x0, double y0, double u, double r, double p, double c) {
  NumericVector x(n);
  NumericVector y(n);
  NumericVector z(n);
  x[0] = x0;
  y[0] = y0;
  z[0] = 1;
  double t;
  for(int i = 0; i < n - 1; i++) {
    t = r - p/(1 + x[i]*x[i] + y[i]*y[i]);
    x[i+1] = 1 + u*(x[i]*z[i]*cos(t) - y[i]*sin(t));
    y[i+1] = u*(x[i]*sin(t) + y[i]*cos(t));
    z[i+1] = z[i]*c;
  }
  DataFrame df = DataFrame::create(_["x"] = x, _["y"] = y, _["z"] = z);
  return df;
}

// [[Rcpp::export]]
DataFrame ikeda_time5(int n, double x0, double y0, double u, double r, double p, double c) {
  NumericVector x(n);
  NumericVector y(n);
  NumericVector z(n);
  x[0] = x0;
  y[0] = y0;
  z[0] = 1;
  double t;
  for(int i = 0; i < n - 1; i++) {
    t = r*z[i] - p/(1 + x[i]*x[i] + y[i]*y[i]);
    x[i+1] = 1 + u*(x[i]*cos(t) - y[i]*sin(t));
    y[i+1] = u*(x[i]*sin(t) + y[i]*cos(t));
    z[i+1] = z[i] + c;
  }
  DataFrame df = DataFrame::create(_["x"] = x, _["y"] = y, _["z"] = z);
  return df;
}

// [[Rcpp::export]]
DataFrame ikeda_time6(int n, double x0, double y0, double u, double r, double p, double c) {
  NumericVector x(n);
  NumericVector y(n);
  NumericVector z(n);
  x[0] = x0;
  y[0] = y0;
  z[0] = 1;
  double t;
  for(int i = 0; i < n - 1; i++) {
    t = r - p/(1 + x[i]*x[i] + y[i]*y[i]) + z[i];
    x[i+1] = 1 + u*(x[i]*cos(t) - y[i]*sin(t));
    y[i+1] = u*(x[i]*sin(t) + y[i]*cos(t));
    z[i+1] = z[i] + c;
  }
  DataFrame df = DataFrame::create(_["x"] = x, _["y"] = y, _["z"] = z);
  return df;
}

// [[Rcpp::export]]
DataFrame ikeda_shape(int n, double x0, double y0, double u, double r, double p, double c, int m) {
  NumericVector x(n);
  NumericVector y(n);
  NumericVector z(n);
  x[0] = x0;
  y[0] = y0;
  z[0] = 1;
  double t;
  for(int i = 0; i < n - 1; i++) {
    t = r*z[i] - p/(1 + x[i]*x[i] + y[i]*y[i]);
    x[i+1] = 1 + u*(x[i]*cos(fmod(t, m)) - y[i]*sin(t));
    y[i+1] = u*(x[i]*sin(t) + y[i]*cos(t));
    z[i+1] = z[i] + c;
  }
  DataFrame df = DataFrame::create(_["x"] = x, _["y"] = y, _["z"] = z);
  return df;
}

// [[Rcpp::export]]
DataFrame ikeda_shape2(int n, double x0, double y0, double u, double r, double p, double c, int m) {
  NumericVector x(n);
  NumericVector y(n);
  NumericVector z(n);
  x[0] = x0;
  y[0] = y0;
  z[0] = 1;
  double t;
  int s = 5;
  for(int i = 0; i < n - 1; i++) {
    t = r*z[i] - p/(1 + x[i]*x[i] + y[i]*y[i]);
    x[i+1] = 1 + u*(cos(3.14/s)*cos(3.14/s*(2*t+1))-(2*t-2*floor(t)-1)*sin(3.14/s)*sin(3.14/s*(2*t+1)));
    y[i+1] = u*(cos(3.14/s)*sin(3.14/s*(2*t+1))+(2*t-2*floor(t)-1)*sin(3.14/s)*cos(3.14/s*(2*t+1)));
    z[i+1] = z[i] + c;
  }
  DataFrame df = DataFrame::create(_["x"] = x, _["y"] = y, _["z"] = z);
  return df;
}
  
// [[Rcpp::export]]
DataFrame ikeda_error(int n, double x0, double y0, double u, double r, double p, double c) {
  NumericVector x(n);
  NumericVector y(n);
  NumericVector z(n);
  x[0] = x0;
  y[0] = y0;
  z[0] = 0;
  double t;
  for(int i = 0; i < n - 1; i++) {
    t = r - p/(1 + x[i]*x[i] + y[i]*y[i]);
    x[i+1] = 1 + u*(x[i]*cos(t*z[i]) - y[i]*sin(t));
    y[i+1] = u*(x[i]*sin(t) + y[i]*cos(t));
    z[i+1] = z[i] + c;
  }
  DataFrame df = DataFrame::create(_["x"] = x, _["y"] = y, _["z"] = z);
  return df;
}

// [[Rcpp::export]]
DataFrame quadratic_map(int n, double x0, double y0, double a1, double a2, double a3, double a4, double a5, double a6, double a7, double a8, double a9, double a10, double a11, double a12) {
  NumericVector x(n);
  NumericVector y(n);
  NumericVector z(n);
  x[0] = x0;
  y[0] = y0;
  z[0] = 0;
  for(int i = 0; i < n - 1; i++) {
    x[i+1] = a1 + a2*x[i] + a3*x[i]*x[i] + a4*x[i]*y[i] + a5*y[i] + a6*y[i]*y[i];
    y[i+1] = a7 + a8*x[i] + a9*x[i]*x[i] + a10*x[i]*y[i] + a11*y[i] + a12*y[i]*y[i];
    z[i+1] = z[i] + 0.01;
  }
  DataFrame df = DataFrame::create(_["x"] = x, _["y"] = y, _["z"] = z);
  return df;
}

// [[Rcpp::export]]
DataFrame cubic_map(int n, double x0, double y0, double a1, double a2, double a3, double a4, double a5, double a6, double a7, double a8, double a9, double a10, double a11, double a12, double a13, double a14, double a15, double a16, double a17, double a18, double a19, double a20) {
  NumericVector x(n);
  NumericVector y(n);
  NumericVector z(n);
  x[0] = x0;
  y[0] = y0;
  z[0] = 0;
  for(int i = 0; i < n - 1; i++) {
    x[i+1] = a1 + a2*x[i] + a3*x[i]*x[i] + a4*x[i]*x[i]*x[i] + a5*x[i]*x[i]*y[i] + a6*x[i]*y[i] + a7*x[i]*y[i]*y[i] + a8*y[i] + a9*y[i]*y[i] + a10*y[i]*y[i]*y[i];
    y[i+1] = a11 + a12*x[i] + a13*x[i]*x[i] + a14*x[i]*x[i]*x[i] + a15*x[i]*x[i]*y[i] + a16*x[i]*y[i] + a17*x[i]*y[i]*y[i] + a18*y[i] + a19*y[i]*y[i] + a20*y[i]*y[i]*y[i];
    z[i+1] = z[i] + 0.01;
  }
  DataFrame df = DataFrame::create(_["x"] = x, _["y"] = y, _["z"] = z);
  return df;
}

// [[Rcpp::export]]
DataFrame tinkerbell(int n, double x0, double y0, double a, double b, double c, double d) {
  NumericVector x(n);
  NumericVector y(n);
  NumericVector z(n);
  x[0] = x0;
  y[0] = y0;
  z[0] = 0;
  for(int i = 0; i < n - 1; i++) {
    x[i+1] = x[i]*x[i] - y[i]*y[i] + a*x[i] + b*y[i];
    y[i+1] = 2*x[i]*y[i] + c*x[i] + d*y[i];
    z[i+1] = z[i] + 0.01;
  }
  DataFrame df = DataFrame::create(_["x"] = x, _["y"] = y, _["z"] = z);
  return df;
}

