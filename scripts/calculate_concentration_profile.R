# data for testing
# concentrations <- data.table(species = c("c(A)","c(B)","c(C)"),
#                              c = c(2,1,0.5))
# rates <- data.table(reaction = c("k1", "k2"),
#                     k = c(0.1,0.01))
# stoich_matrix <- data.table(reaction = c(1,2),
#                             A = c(-2,2),
#                             B = c(-1,1),
#                             C = c(2,-2))
# endtime = 2
# stepsize = 0.1
# algorithm = "rk"

calc_concentrations <-
  function(concentrations,
           rates,
           stoich_matrix,
           endtime,
           stepsize,
           algorithm) {
    
    # create time vector
    time <- seq(0, endtime, stepsize)
    
    # format concentrations
    concentrations <- transpose(concentrations)
    colnames(concentrations) <- as.character(concentrations[1, ])
    concentrations <- concentrations[-1, ]
    concentrations <-
      concentrations[, names(concentrations) := lapply(.SD, as.numeric)]
    concentrations <- cbind(t = 0, concentrations)
    concentrations <- data.frame(concentrations)
    
    # set stoichiometric matrix from data.table to data.frame (why?)
    stoich_matrix <- data.frame(stoich_matrix)
    
    # create stoichiometric matrix for educts
    nu_lhs <- data.frame(stoich_matrix)
    for (i in 2:NCOL(nu_lhs)) {
      nu_lhs[nu_lhs[, i] > 0, i] <- 0
    }
    nu_lhs <- abs(nu_lhs)
    
    # duplicate concentration matrix
    intermediate_concentrations <- concentrations
    aux <- concentrations
    
    # calculate using explicit euler method
    if ("euler" %in% algorithm) {
      for (i in 2:length(time)) {
        for (j in 2:NCOL(nu_lhs)) {
          concentrations[i, 1] <- time[i]
          concentrations[i, j] <- concentrations[i - 1, j]
          for (k in 1:NROW(nu_lhs)) {
            delta_c <-
              (stoich_matrix[k, j] * rates$k[k] * prod(as.numeric(concentrations[i -
                                                                                   1, 2:NCOL(concentrations)]) ^ as.numeric(nu_lhs[k, 2:NCOL(nu_lhs)]))) * stepsize
            concentrations[i, j] <- concentrations[i, j] + delta_c
          }
        }
      }
      
      result_euler <- melt(concentrations, id.vars = "t")
      result_euler$variable <- sub("\\.", "(", result_euler$variable)
      result_euler$variable <- sub("\\.", ")", result_euler$variable)
      result_euler$algorithm <- "Explicit Euler"
    }
    
    # calculate using runge-kutta 2nd order method
    if ("rk" %in% algorithm) {
      concentrations <- aux
      for (i in 2:length(time)) {
        for (j in 2:NCOL(nu_lhs)) {
          concentrations[i, 1] <- time[i]
          concentrations[i, j] <- concentrations[i - 1, j]
          abs_concentration_change <- 0
          for (k in 1:NROW(nu_lhs)) {
            delta_c <-
              (stoich_matrix[k, j] * rates$k[k] * prod(as.numeric(concentrations[i -
                                                                                   1, 2:NCOL(concentrations)]) ^ as.numeric(nu_lhs[k, 2:NCOL(nu_lhs)]))) * stepsize /
              2
            abs_concentration_change <-
              abs_concentration_change + delta_c
          }
          intermediate_concentrations[i, j] <-
            concentrations[i, j] + abs_concentration_change
        }
        for (j in 2:NCOL(nu_lhs)) {
          for (l in 1:NROW(nu_lhs)) {
            intermediate_dc <-
              (stoich_matrix[l, j] * rates$k[l] * prod(
                as.numeric(intermediate_concentrations[i, 2:NCOL(intermediate_concentrations)]) ^
                  as.numeric(nu_lhs[l, 2:NCOL(nu_lhs)])
              )) * stepsize
            concentrations[i, j] <-
              concentrations[i, j] + intermediate_dc
          }
        }
      }
      
      
      
      result_rk <- melt(concentrations, id.vars = "t")
      result_rk$variable <- sub("\\.", "(", result_rk$variable)
      result_rk$variable <- sub("\\.", ")", result_rk$variable)
      result_rk$algorithm <- "2nd Order Runge-Kutta"
    }
    
    # create results data.frame
    if (length(algorithm) == 2) {
      result <- rbind(result_rk, result_euler)
    } else if (algorithm == "euler") {
      result <- result_euler
    } else {
      result <- result_rk
    }
    
    # create plot
    p <-
      ggplot(data = result, aes(
        x = t,
        y = value,
        color = variable,
        lty = algorithm
      )) +
      geom_line() +
      ylab(bquote('c / mol L' ^ -1)) +
      xlab("t / s") +
      labs(color = "", lty = "Calculation Algorithm") +
      theme_bw() +
      ylim(0, NA)
    
    # return plot and data as list
    return(list(p, concentrations))
  }
