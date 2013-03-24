#' Genetic Algorithm setup
#' 
#' Setup a \code{GAPerm} object that can be used to perform a permutation-based optimization.
#' 
#' This is the function used to configure and fine-tune a permutation-based optimization.
#' The basic usage requires only the \code{FUN} parameter (function to be maximized),
#' together with \code{n} (the number of elements to permutate),
#' all the other parameters have sensible defaults.
#' 
#' The parameters \code{selection}, \code{crossover} and \code{mutation} can also take a custom
#' function as argument, which needs to be in the appropriate format (see the
#' examples). The text below explains the default behaviour for these parameters, which will
#' be usefull if you want to override one or more genetic operators.
#' 
#' \itemize{
#' \item \code{selection}: Foo bar
#' }
#' 
#' @param FUN The fitness function, which should take a vector as argument and return a numeric
#' value (See details).
#' @param n The number of elements to permutate.
#' @param popSize The population size.
#' @param mutRate The mutation rate, a numeric value between 0 and 1. When implementing a
#' custom mutation function, this value should be one of the parameters (see details and examples).
#' @param cxRate The crossover rate, a numeric value between 0 and 1. This parameter specifies
#' the probability of two individuals effectively exchange DNA during crossover. In case the
#' individuals didn't crossover, the offspring is a exact copy of the parents. When implementing
#' a custom crossover function, this value should be one of the arguments (see details and examples).
#' @param eliteRate A numeric value between 0 and 1. The \code{eliteRate * popSize} best-fitted
#' individuals will automatically be selected for the next generation.
#' @param selection The selection operator to be used. You can also implement a custom selection
#' function (see details and examples).
#' @param crossover The crossover operator to be used. You can also implement a custom crossover
#' function (see details and examples).
#' @param mutation The mutation operator to be used. You can also implement a custom mutation
#' function (see details and examples).
#' @return An object of class \code{GAperm}, which you can pass as an argument to \code{plot} or
#' \code{summary}. This object is a list with the following accessor functions:
#' \tabular{ll}{
#' \code{bestFit}: \tab Returns a vector with the best fitness achieved in each generation.\cr
#' \code{meanFit}: \tab Returns a vector with the mean fitness achieved in each generation.\cr
#' \code{bestIndividual}: \tab Returns a vector with the best solution found.\cr
#' \code{evolve(h)}: \tab This is the function you call to evolve your population. You need
#' to specify the number of generations to evolve.\cr
#' \code{population}: \tab Returns the current population matrix.
#' }
#' @export
#' @examples
#'  # TSP with 10 cities around a circular pattern
#'  n = 10
#'  R = 10
#'  angs = seq(0, 2*pi, length = n)
#'  xp = R * cos(angs) + rnorm(n)
#'  yp = R * sin(angs) + rnorm(n)
#'  xp = c(xp, xp[1])
#'  yp = c(yp, yp[1])
#'
#'  base.M = matrix(c(xp, yp), ncol = 2)
#'  dist.FUN = function(p)
#'  {
#'    p = c(p, p[1])
#'    M.diff = diff(base.M[p, ])
#'    dists = apply(M.diff, 1, function(x)x[1]^2 + x[2]^2)
#'    1/sum(dists)
#'  }
#'
#'  ga1 = GAPerm(dist.FUN, n, popSize = 100, mutRate = 0.3)
#'  ga1$evolve(100)
#'  print(ga1)
#'  plot(xp, yp, type = 'n', xlab = '', ylab = '')
#'  res = ga1$bestIndividual()
#'  res = c(res, res[1])
#'
#'  i = 1:n
#'  xi = base.M[res[i], 1]
#'  yi = base.M[res[i], 2]
#'  xf = base.M[res[i + 1], 1]
#'  yf = base.M[res[i + 1], 2]
#'
#'  arrows(xi, yi, xf, yf, col = 'red', angle = 10)
#'  text(base.M[res, 1], base.M[res, 2], 1:n, cex = 0.9, col = 'gray20')
#' 

GAPerm = function(FUN, n, popSize = 100, mutRate = 0.1, cxRate = 0.9, eliteRate = 0.4, 
=======
simpleGAP = function(FUN, n, popSize = 100, mutRate = 0.01, cxRate = 0.95, eliteRate = 0.4, 
>>>>>>> f4227ac025bc66125a87e6ea5dbc9d6909bffec1
			   selection = c('fitness', 'uniform'), crossover = c('pmx'), mutation = c('swap'))
{
  currentPopulation = NULL		
  bestFitnessVec = numeric()
  meanFitnessVec = numeric()
  elite = max(0, 2 * as.integer(eliteRate * popSize * 0.5))
  popSize = 2 * as.integer(popSize * 0.5)
  bestCX = rep(0, n)
  bestFit = NULL
  mutations = max(as.integer(mutRate * popSize), 1)
  iter = 0
  newPopulation = matrix(0, nrow = popSize, ncol = n)

  ############### BEG selection function definitions #########################################
  selection.FUN = NULL
  if (is.function(selection))
    selection.FUN = selection
  else
    selection.type = switch(match.arg(selection), fitness = 'fitness', uniform = 'uniform')
  ############### END selection function definitions #########################################
  
  
  ############### BEG crossover function definitions #########################################
  pmxCrossover = function(vec1, vec2, prob)
  {
    prob = runif(1)
    if (prob > cxRate)       
      return(matrix(c(vec1, vec2), nrow = 2, byrow = T))
    
    idxs = sample(1:length(vec1), 2)
    vec1.cp = vec1
    
    for (i in idxs)
    {
      other.val = vec2[i]
      vec.idx = which(vec1 == other.val)
      vec1[vec.idx] = vec1[i]
      vec1[i] = other.val
    }
    
    for (i in idxs)
    {
      other.val = vec1.cp[i]
      vec.idx = which(vec2 == other.val)
      vec2[vec.idx] = vec2[i]
      vec2[i] = other.val
    }
    
    matrix(c(vec1, vec2), nrow = 2, byrow = T)
  }
  
  applyCrossover = function(rowIdxs, M, FUN)
  {
    FUN.vec = function(rowVector, mat)
    {
      FUN(mat[rowVector[1], ], mat[rowVector[2], ], cxRate)
    }
    
    m1 = apply(rowIdxs, 1, FUN.vec, mat = M)
    matrix(t(m1), byrow = F, ncol = ncol(M))
  }
  
  crossover.FUN = NULL
  if (is.function(crossover))
    crossover.FUN = crossover
  else
    crossover.FUN = switch(match.arg(crossover), pmx = pmxCrossover) 
  ############### END crossover function definitions #########################################
  
  ############### BEG mutation function definitions ##########################################
  mutateSwap = function(M)
  {
    rows = sample(1:nrow(M), mutations, rep = FALSE)
    cols = t(replicate(mutations, sample(1:n, 2)))
    col1 = cols[, 1]
    col2 = cols[, 2]
    extM1 = matrix(c(rows, col1), ncol = 2)	
    extM2 = matrix(c(rows, col2), ncol = 2)
    tempCol = M[extM1]
    M[extM1] = M[extM2]
    M[extM2] = tempCol
    M
  }	
  
  mutation.FUN = NULL
  if (is.function(mutation))
    mutation.FUN = mutation
  else
    mutation.FUN = switch(match.arg(mutation), swap = mutateSwap)
  ############### END mutation function definitions #########################################
   
  initPopulation = function()
  {
    if (is.null(currentPopulation))
      currentPopulation <<- matrix(replicate(popSize,sample(1:n)), byrow = TRUE, ncol = n)
  }
  
  initPopulation()
  
  do.evolve = function()
  {
    iter <<- iter + 1      
    fitnessVec = apply(currentPopulation, 1, FUN)
    max.idx = which.max(fitnessVec)
    this.best = fitnessVec[max.idx]		
    bestFitnessVec[iter] <<- this.best			
    meanFitnessVec[iter] <<- mean(fitnessVec)
    
    if (is.null(bestFit) || (this.best > bestFit))
    {
      bestFit <<- this.best
      bestCX <<- currentPopulation[max.idx, ]
    }
    
    nLeft = popSize
    
    if (elite > 0)
    {
      # Maximization problem												
      nLeft = popSize - elite
      newPopulation[1:elite, ] = currentPopulation[order(fitnessVec, decreasing = TRUE)[1:elite], ] 
    }
    
    # crossover selection
    if (! is.null(selection.FUN))
      popIdxs = selection.FUN(currentPopulation, fitnessVec, nLeft)[1:nLeft]
    else
    { 
      if (identical(selection.type, 'fitness'))
        probVec = fitnessVec
      else if (identical(selection.type, 'uniform'))
        probVec = NULL
      
      popIdxs = sample(1:popSize, nLeft, replace = TRUE, prob = probVec)
    }
    
    popIdxsM = matrix(popIdxs, ncol = 2, byrow = T)
    offspring = applyCrossover(popIdxsM, currentPopulation, crossover.FUN)
    newPopulation[(elite+1):popSize, ] = offspring
    
    currentPopulation <<- mutation.FUN(newPopulation)
  }
  
  objs = list(
    population = function()
    {
      currentPopulation
    },								
    
    bestFit = function()
    {
      bestFitnessVec
    },
    
    meanFit = function()
    {
      meanFitnessVec
    },
    
    bestIndividual = function()
    {
      bestCX
    },
    
    evolve = function(h)
    {        	
      if (missing(h))
        stop('Please specify the number of generations to evolve.\n')
      
      length(bestFitnessVec) = length(bestFitnessVec) + h
      length(meanFitnessVec) = length(meanFitnessVec) + h
      invisible(replicate(h, do.evolve()))
    }
    
  )
  
<<<<<<< HEAD
  class(objs) = 'GAPerm'
  objs
}

#' Genetic Algorithm plot
#' 
#' A quick way to visualize the GA results.
#' 
#' @param x An object of class \code{GAPerm}.
#' @param xlab The label for the x-axis.
#' @param ylab The label for the y-axis.
#' @param main The plot title.
#' @param bestcol The color for the best fitness evolution line
#' @param meancol The color for the mean fitness evolution line
#' @param lwd The line width.
#' @param legend.pos The legend position, as a character vector.
#' @param ... Other parameters (will be ignored).
#' @aliases plot
#' @export
#' @examples
#' 
#' ga = GAPerm(function(x) sum(x), n = 10)
#' ga$evolve(200)
#' plot(ga)
#' 
# plot = function(x, ...)
# {
#   UseMethod('plot')
# }

#' @method plot GAPerm
#' @S3method plot GAPerm
#' @rdname plot_perm
plot.GAPerm = function(x, xlab = 'Generation', ylab = 'Fitness', main = 'GA optimization',
                       bestcol = 'steelblue', meancol = 'tomato', lwd = 2, 
                       legend.pos = c('bottomright', 'bottom', 'bottomleft', 'left', 'topleft', 'top',
                                      'topright', 'right', 'center'), ...)
{
  ymean = x$meanFit()
  if (length(ymean) == 0)
  {
    print(summary(x))
    return(NULL)
  }
  
  ybest = x$bestFit()
  ylim = c(min(ymean, ybest), max(ymean, ybest))
  plot(ybest, col = bestcol, panel.first = grid(col = '#A9A9A9'), xlab = xlab,
       ylab = ylab, main = main, lwd = lwd, type = 'l', ylim = ylim)
  
  lines(ymean, col = meancol, lwd = lwd)
  legend(legend.pos, legend = c('best', 'mean'), col = c(bestcol, meancol), lwd = lwd)
}

#' Genetic Algorithm summary
#' 
#' A simple summary of the GA results.
#' 
#' @return An object of class \code{summaryGAPerm}, which is a list that can be inspected or
#' printed on-screen.
#' @param object An object of class \code{GAPerm}.
#' @param ... Other parameters (will be ignored).
#' @export
#' @aliases summary summary.GAPerm
=======
  objs
}

# TSP cidades em volta de circunferencia
get.circle.cities = function(n, R = 100)
{
  delta = 360/n
  angs = seq(0, 360 - delta, length = n)
  angs	
  
  pontos = vector(mode = "list", length = n)
  
  for (i in 1:n)
  {
    rad = angs[i] * pi/180.0
    pontos[[i]] = list(x = R * sin(rad), y = R * cos(rad))
  }
  pontos
}

plot.circle = function(pontos)
{
  xp = numeric()
  yp = numeric()
  for (i in 1:length(pontos))
  {
    plot(-R:R, -R:R, type = 'n')
    xp = c(xp, pontos[[i]]$x)
    yp = c(yp, pontos[[i]]$y)
    
  }
  points(xp, yp, col = 'red', cex = 2)
}
>>>>>>> f4227ac025bc66125a87e6ea5dbc9d6909bffec1

#' @method summary GAPerm
#' @S3method summary GAPerm
#' @rdname summary_perm
summary.GAPerm = function(object, ...)
{
<<<<<<< HEAD
  n = length(object$bestFit())		
  if (n == 0)
  {
    sm.obj = list(evolved = FALSE)
    class(sm.obj) = 'summaryGAPerm'
    return(sm.obj)		
  }
  sm.obj = list(n = n, sm.mean = summary(object$meanFit()),
                sm.best = summary(object$bestFit()),
                best.cx = object$bestIndividual(),
                best.fit = max(object$bestFit()), 
                evolved = TRUE)
  class(sm.obj) = 'summaryGAPerm'
  sm.obj
}

#' Print GA results
#' 
#' Prints the GA results.
#' @param x An object of class \code{GAPerm} or \code{summaryGAPerm}
#' @param ... Other parameters (will be ignored).
#' @export
#' @aliases print print.GAPerm

#' @method print GAPerm
#' @S3method print GAPerm
#' @rdname print_perm
print.GAPerm = function(x, ...)
{
  print(summary(x))
}

#' @method print summaryGAPerm
#' @S3method print summaryGAPerm
#' @rdname print_perm
print.summaryGAPerm = function(x, ...)
{
  if (! x$evolved)
  {
    cat('Population ready to evolve.')
    cat('\nPlease, call myGA$evolve(h) to generate results.\n')
  }
  else
  {
    cat('Results for', x$n, 'Generations:')
    cat('\nMean Fitness:\n')
    print(x$sm.mean)
    cat('\nBest Fitness:\n')
    print(x$sm.best)
    cat('\nBest individual:\n')
    print(x$best.cx)
    cat('\nBest fitness value:\n')
    print(x$best.fit)
  }
}

run.test = function(popSize = 100, h = 200, mutRate = 0.3)
{
  eurodistmat <- as.matrix(eurodist)
  distance <- function(sq) 
  {  
      sq = c(sq, sq[1])
      sq2 <- embed(sq, 2)
      1/sum(eurodistmat[cbind(sq2[,2], sq2[,1])])
  }
  
  loc = -cmdscale(eurodist, add = TRUE)$points
  x = loc[, 1]
  y = loc[, 2]
  n = nrow(eurodistmat)
  
  set.seed(1)
  ga = GAPerm(distance, n, popSize = popSize, mutRate = mutRate)    
  ga$evolve(h)
  best = ga$bestIndividual()
  best = c(best, best[1])
  best.dist = 1/max(ga$bestFit())
  res = loc[best, ]
  i = 1:n
  
  plot.title = paste('Euro tour: TSP with 21 cities', '\nBest distance:', best.dist)
  plot(x, y, type = 'n', axes = FALSE, ylab = '', xlab = '', main = plot.title)
  arrows(res[i, 1], res[i, 2], res[i + 1, 1], res[i + 1, 2], col = 'red', angle = 10)
  text(x, y, labels(eurodist), cex = 0.8, col = 'gray20') 
  ga
}
=======
  dist = 0.0
  for (i in 1:(length(perm) - 1))
  {
    idx1 = perm[i]
    idx2 = perm[i + 1]
    x1 = pontos[[idx1]]$x
    y1 = pontos[[idx1]]$y
    x2 = pontos[[idx2]]$x
    y2 = pontos[[idx2]]$y
    
    dist = dist + sqrt((x1 - x2)^2 + (y1 - y2)^2)
  }
  
  idxf = tail(perm, 1)
  xf = pontos[[idxf]]$x
  yf = pontos[[idxf]]$y
  idxi = perm[1]
  xi = pontos[[idxi]]$x
  yi = pontos[[idxi]]$y
  
  dist + sqrt((xf - xi)^2 + (yf - yi)^2)
}

run.test = function(cities = 10, R = 100, pop = 100, h = 100, mr = 0.1, er = 0.5, cr = 0.5,
                    selection = 'fitness')
{
  pontos = get.circle.cities(cities, R)
  #plot.circle(pontos)
  dist = get.distance(pontos, 1:cities)
  cat('Optimal distance: ', dist, '\n')
  
  heur = function(perm)
  {
    1/get.distance(pontos, perm)
  }
  
  eurodistmat <- as.matrix(eurodist)
  distance <- function(sq) 
  {  
      sq = c(sq, sq[1])
      sq2 <- embed(sq, 2)
      1/sum(eurodistmat[cbind(sq2[,2],sq2[,1])])
  }
  
  gap = simpleGAP(heur, cities, selection = selection, cxRate = cr,
                  mutRate = mr, eliteRate = er)
  gap$evolve(h)
  cat('GAP best distance: ', 1/max(gap$bestFit()), '\n')
  best = gap$bestIndividual()
  plot(gap$bestFit(), type = 'l', col = 'gold', main = paste('TSP -', cities, 'cities'), lwd = 2)
  lines(gap$meanFit(), type = 'l', col = 'steelblue', lwd = 2)
  grid(col = 'darkgrey')
}