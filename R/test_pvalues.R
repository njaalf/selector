#' Test for correct model specification with various test statistics.
#'
#' This function takes a lavaan fit object and calculates the p-values for various
#' test of correct model specification. The tests include normal-theory ml likelihood ratio test,
#' the satorra-bentler test and the scaled-and-shifte test. In addition less known tests
#' such as the scaled F test, and three tests based on eigenvalue block averaging.
#'  
#' @param f A lavaan object produced by sem() or cfa()
#' @return A set of pvalues associated with different test statistics
#' @examples 
#' @export

test_pvalues <- function(f){
  
  UG <- lavaan::lavInspect(f, "UGamma")
  df <- lavaan::fitmeasures(f, "df")
  tml <- lavaan::fitmeasures(f, "chisq")
  
  porigs <- c(ml = unname(fitmeasures(f, "pvalue")), 
              get_pvalues(tml,  Re(eigen(UG)$values[1:df])),
              ss = unname(get_ss_pvalue(tml, df, UG)))
  
  round(porigs,4)
}