get_pvalues<- function(tml, eigenvalues){
  pfull <- CompQuadForm::imhof(tml, eigenvalues)$Qq
  psb <- CompQuadForm::imhof(tml, rep(mean(eigenvalues), length(eigenvalues)))$Qq
  peba2 <- CompQuadForm::imhof(tml, eigen_block(eigenvalues, 2))$Qq
  peba4 <- CompQuadForm::imhof(tml, eigen_block(eigenvalues, 4))$Qq
  pwl <- scaled_F(tml, eigenvalues)
  return(c(sb=psb, eba2=peba2, eba4=peba4, ebafull=pfull, wl=pwl))
}
