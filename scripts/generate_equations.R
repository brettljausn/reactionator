generate_equations <- function(stoich_matrix){
  
  
  equation <- NA
  strings <- data.frame(stoich_matrix)
  for(i in 2:NCOL(strings)){
    strings[1:NROW(strings),i] <- paste(abs(as.numeric(strings[1:NROW(strings),i])), colnames(strings)[i])
  }
  
  for(i in 1:NROW(stoich_matrix)){
    positive <- which(stoich_matrix[i,-1] > 0)
    negative <- which(stoich_matrix[i,-1] < 0)
    equation_rhs <- paste(strings[i,positive+1], collapse = " + ")
    equation_lhs <- paste(strings[i,negative+1], collapse = " + ")
    equation[i] <- paste(equation_lhs, equation_rhs, sep = " &#8594 ")
  }
  
  return(equation)
}