#include <Rcpp.h>
using namespace Rcpp;

Function subset("[.data.frame");

// [[Rcpp::export]]
DataFrame subset_test(DataFrame x, int y) {
  return subset(x, y, R_MissingArg);
}

/*** R
Rcpp::sourceCpp("cpp_subset_function.cpp")
df <- data.frame(x=1:3, y=letters[1:3])
subset_test(df, 1)
*/
