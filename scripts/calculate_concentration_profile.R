# data for testing
concentrations <- data.table(species = c("c(A)","c(B)","c(C)"),
                             c = c(3,2,0))
rates <- data.table(reaction = c("k1", "k2"),
                    k = c(0.01,0.1))
stoich_matrix <- data.table(reaction = c(1,2),
                            A = c(-2,2),
                            B = c(-1,1),
                            C = c(2,-2))
endtime = 1
stepsize = 0.1

calc_concentrations <- function(concentrations, rates, stoich_matrix, endtime, stepsize){

time <- seq(0,endtime,stepsize)

concentrations <- transpose(concentrations)
colnames(concentrations) <- as.character(concentrations[1,])
concentrations <- concentrations[-1,]
concentrations <- concentrations[, names(concentrations) := lapply(.SD, as.numeric)]
concentrations <- cbind(t = 0, concentrations)
concentrations <- data.frame(concentrations)
stoich_matrix <- data.frame(stoich_matrix)
nu_lhs <- data.frame(stoich_matrix)

for(i in 2:NCOL(nu_lhs)){
  nu_lhs[nu_lhs[,i] > 0,i] <- 0
}

nu_lhs <- abs(nu_lhs)


for (i in 2:length(time)){
  for (j in 2:NCOL(nu_lhs)){
    concentrations[i,1] <- time[i]
    concentrations[i,j] <- concentrations[i-1,j]
    for (k in 1:NROW(nu_lhs)){
      delta_c <- (stoich_matrix[k,j] * rates$k[k] * prod(as.numeric(concentrations[i-1,2:NCOL(concentrations)])^as.numeric(nu_lhs[k,2:NCOL(nu_lhs)]))) * stepsize
      concentrations[i,j] <- concentrations[i,j] + delta_c
    }
  }
}

result <- melt(concentrations, id.vars = "t")
result$variable <- sub("\\.", "(", result$variable)
result$variable <- sub("\\.", ")", result$variable)


p <- ggplot(data = result, aes(x = t, y = value, color = variable)) +
  geom_line() +
  ylab(bquote('c / mol L'^-1)) +
  xlab("t / s") +
  labs(color = "") +
  theme_bw()

return(list(p,concentrations))
}
