#' Bootstrap selection of most appropriate test statistic.
#'
#' This function takes a fitted lavaan object and bootstraps various test statistics.
#' The test whose p-values are closest to a uniform distribution is selected.
#' 
#' 
#'
#' @param f Object obtained by fitting a model using sem() or cfa() in lavaan.
#' @param seed Random seed.
#' @param b.reps Number of bootstrap samples.
#' @return Plot of bootstrap p-values and the selected test statistic and its associated p-value
#' @export
selector <- function(f, R=1000, seed=1, method="AD"){
  #continuous case, we assume ML
  if(f@Options$estimator != "ML"){
    cat("Object f must be fitted with estimator= ML\n")
    return(NULL)
  }
  
  porigs <- test_pvalues(f)
  
  outputstring <- paste("\n Candidate p-values are: \n",paste(names(porigs), round(porigs,3), sep = ":", collapse = ",  "))
  cat(paste(outputstring, "\n"))
  
  ## run bootstrap 
  boot.df <- data.frame(lavaan::bootstrapLavaan(f, R=R, type="bollen.stine", 
                                                FUN=test_pvalues, warn=-1L))

  #anderson-darling
  metric.df <-  as.vector(sapply(boot.df,function(x) 
    goftest::ad.test(x, null="punif")$statistic ))
  #cramer mises
  metric.df <-  rbind(metric.df,as.vector(sapply(boot.df,function(x) 
    goftest::cvm.test(x, null="punif")$statistic )))
  #kolmogorov-smirnov
  metric.df <-  rbind(metric.df, as.vector(sapply(boot.df,function(x) 
    ks.test(x,"punif")$statistic )))
  ## EC5
  metric.df <-  rbind(metric.df,abs(colMeans(boot.df <0.05)-0.05))
  rownames(metric.df) <- NULL
  metric.df <- data.frame(metric.df)
  metric.df$metric <- c("AD", "CVM", "KS", "EC5")
  
  bootstrap.pvals <- as.vector(sapply(X=1:ncol(boot.df),function(X) 
    mean(boot.df[,X] < porigs[X])))
  names(bootstrap.pvals) <- colnames(boot.df)
  
  
  for(meth in method){
    selected <- which.min(metric.df[metric.df$metric==meth, -ncol(metric.df)])
    selectedname <- colnames(boot.df)[selected]
    cat("\n According to criterion", meth,"the selector selects: ", selectedname,
        " with p-value", round(porigs[selectedname],3), "\n")
  }
  
  melted = reshape2::melt(boot.df, id.vars = NULL)
  melted$statistic <- melted$variable
  
  p <- ggplot2::ggplot(melted, ggplot2::aes(x=value))+ggplot2::geom_histogram(binwidth=0.02)+
    ggplot2::facet_wrap(~statistic)+ggplot2::ggtitle("Bootstrapped p-values")
  print(p)
  invisible(list(metrics = metric.df, bootstrap.pvals =bootstrap.pvals))
}
  

  
  