##  Data simulation and power analyses
##  Simulación de datos y análisis del poder de los tests


##  Power analysis empiríco ---------------------------------

#  Vamos a simular unos datos para ver que podríamos obtener
#  En primer lugar necesitamos saber más o menos las distribuciones
#  que nos vamos a encontrar.
#  Luego podemos hacer un power analysis preliminar
#  Mediante la formula eff.size^2*n obtenida de
#  http://stackoverflow.com/questions/2710609/power-analysis-in-r-for-two-way-anova
#  (en un comentario de Stéphane Laurent) obtengo el ncp (non centrality parameter):
#  lambda <- eff^2 * n
#  Por otro lado, utilizo el planteamiento de
#  https://stat.ethz.ch/pipermail/r-help/2011-April/273931.html
#  para obtener el poder del análisis para cada factor y para la interacción mediante:
#  pf(qf(1-alpha,Df1,Df2),Df1,Df2,ncp,lower.tail=FALSE)
#  Para obtener los grados de libertad y el ncp necesitamos un n y un effc.size,
#  con lo que hago simulaciones con diferentes n y effc.size para tener una idea de cuantas
#  réplicas necesitaríamos.
#  El effect.size se calcula en la página
#  http://www.uccs.edu/~lbecker/


#' Power analysis for a three-way ANOVA.
#'
#' \code{three_way_power} calculates the power of a three-way ANOVA.
#'
#' Preliminar (empirical) power analyisis. Non-centrality parameter (ncp) is
#' obtained from \url{http://stackoverflow.com/questions/2710609/power-analysis-in-r-for-two-way-anova}
#' (in a comment from Stéphane Laurent). Also, the power calculations are
#' obtained as explained in \url{https://stat.ethz.ch/pipermail/r-help/2011-April/273931.html}
#' The effect size values can be empirical or obtained as in
#' \url{http://www.uccs.edu/~lbecker/}.
#'
#' @family preliminar analyses
#'
#' @param N1,N2,N3 Number of levels for each factor. Must be a numeric value.
#' @param n_values Number of replicates. Can be a numeric value or a numeric vector for
#'   complex simulations.
#' @param eff_sizes Effect size. Can ve a numeric value or a numeric vector for
#'   complex simulations.
#' @param alpha Confidence level.
#'
#' @return Prints in console the results indicating the power of analisis for
#'   the given n's and effect sizes, and returns a data.frame with all the
#'   information to make easy to produce a plot (optimized for ggplot2).
#'
#' @examples
#' n_values <- c(3,4,5)
#' eff_sizes <- seq(.8,2.2,.2)
#' N1 <- 2
#' N2 <- 8
#' N3 <- 5
#' alpha <- .05
#'
#' foo.power <- three_way_power(N1,N2,N3,n_values,eff_sizes,alpha)
#' head(foo.power)
#'
#' @export
three_way_power <- function(N1,N2,N3,n_values,eff_sizes, alpha){
  # Helper function:
  pwr_test_internal <- function(N1,N2,N3,n,eff_size,alpha) {
    df1 <- N1-1  # degrees of freedom for factor 1
    df2 <- N2-1  # degrees of freedom for factor 2
    df3 <- N3-1  # degrees of freedom for factor 3
    Resdf <- (N1*N2*N3)*(n-1)  # Residuals degrees of freedom
    ncp <- (eff_size^2)*n  # Non-Centrality parameter
    # Now, power analyses for every factor and interaction
    pwr.F1 <- pf(qf(1-alpha, df1, Resdf),df1,Resdf,ncp,lower.tail=FALSE)
    pwr.F2 <- pf(qf(1-alpha, df2, Resdf),df2,Resdf,ncp,lower.tail=FALSE)
    pwr.F3 <- pf(qf(1-alpha, df3, Resdf),df3,Resdf,ncp,lower.tail=FALSE)
    # Quedarían por añadir las interacciones
    # Creatng a list with the results
    res.list <- list(Fac1=pwr.F1, Fac2=pwr.F2, Fac3=pwr.F3)
    # Retunrning the results list
    return(res.list)
  }
  # Now, the real function
  # Creating vectors to store values in every iteration in case of several
  # values for n and eff_size
  n.vector <- vector()
  effsize.vector <- vector()
  factor.vector <- vector()
  power.vector <- vector()
  iteration=-2
  for(i in n_values){
    for(k in eff_sizes){
      iteration=iteration+3
      cat(paste('\nPower for n = ',i,' & effect size = ',k,'\n', sep=''))
      res <- pwr_test_internal(N1,N2,N3,i,k,alpha)
      cat(paste('Factor 1: ',round(res$Fac1,3),
                ' Factor 2: ',round(res$Fac2,3),
                ' Factor 3: ',round(res$Fac3,3),'\n', sep=''))
      n.vector[iteration:(iteration+2)] <- i
      effsize.vector[iteration:(iteration+2)] <- k
      factor.vector[iteration] <- 'F1'
      factor.vector[iteration+1] <- 'F2'
      factor.vector[iteration+2] <- 'F3'
      power.vector[iteration] <- res$Fac1
      power.vector[iteration+1] <- res$Fac2
      power.vector[iteration+2] <- res$Fac3
    }
  }
  res.df <- data.frame(n=factor(n.vector), EffSize=effsize.vector,
                       Factor=factor.vector, Power=power.vector)
  return(res.df)
}

#######################################################
# GLASS TESTING
#######################################################
# n_values <- c(3,4,5)
# eff_sizes <- seq(.8,2.2,.2)
# N1 <- 2
# N2 <- 8
# N3 <- 5
# alpha <- .05
#
# power.test.custom(N1,N2,N3,n_values,eff_sizes,alpha)
#######################################################
