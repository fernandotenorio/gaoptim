GAReal = function (FUN, lb, ub,  popSize = 100, mutRate = 0.01, cxRate = 0.9, eliteRate = 0.4,
                     selection = c('fitness', 'uniform'), crossover = c('blend', 'two.points'),
                     mutation = c('noise'))
{		
  currentPopulation = NULL		
  bestFitnessVec = numeric()
  meanFitnessVec = numeric()
  elite = max(0, 2 * as.integer(eliteRate * popSize * 0.5))
  popSize = 2 * as.integer(popSize * 0.5)
  nvars = length(lb)
  bestCX = rep(0, nvars)
  bestFit = NULL
  mutations = as.integer(mutRate * popSize * nvars)
  iter = 0
  newPopulation = matrix(0, nrow = popSize, ncol = nvars)
  
  ############### BEG selection function definitions #########################################
  selection.FUN = NULL
  if (is.function(selection))
  	selection.FUN = selection
  else
  	selection.type = switch(match.arg(selection), fitness = 'fitness', uniform = 'uniform')
  ############### END selection function definitions #########################################
  	
  ############### BEG crossover function definitions #########################################
  twoPointsCrossover = function(x1, x2, prob)
  {	
  	if (runif(1) > prob)                  
      return(matrix(c(x1, x2), nrow = 2, byrow = T))

	p12 = sample(1:length(x1), 2)
	idxs = seq(p12[1], p12[2])
	temp1 = x1[idxs]
	x1[idxs] = x2[idxs]
	x2[idxs] = temp1
	matrix(c(x1, x2), nrow = 2, byrow = T)
  }
  
  blendCrossover = function(cr1, cr2, prob)
  {								
    if (runif(1) > prob)                  
      return(matrix(c(cr1, cr2), nrow = 2, byrow = T))
    
    beta = 0.5
    n = length(cr1)
    i = sample(1:n, 1)
    
    pm = cr1[i]
    pd = cr2[i]			
    ch1 = cr1
    ch2 = cr2			
    ch1[i:n] = cr2[i:n]
    ch2[i:n] = cr1[i:n]
    
    ch1[i] = pm - beta*(pm - pd)
    ch2[i] = pd + beta*(pm - pd)
    
    matrix(c(ch1, ch2), nrow = 2, byrow = T)			
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
  	crossover.FUN = switch(match.arg(crossover), blend = blendCrossover, two.points = twoPointsCrossover) 
  ############### END crossover function definitions #########################################
  
  ############### BEG mutation function definitions #########################################
  mutateNoise = function(x)
  {			
    rows = sample(1:nrow(x), mutations, rep = TRUE)
    cols = sample(1:ncol(x), mutations, rep = TRUE)
    noise = runif(mutations)
    
    ext = matrix(c(rows, cols), mutations, 2)
    x[ext] = noise
    x
  }
  
  	mutation.FUN = NULL
  	if (is.function(mutation))
  		mutation.FUN = mutation
  	else
  		mutation.FUN = switch(match.arg(mutation), noise = mutateNoise)
  ############### END mutation function definitions #########################################
  
    
  decode = function(x, lb, ub)
  {
    n = nrow(x)
    x * rep(ub - lb, each = n) + rep(lb, each=n) 	
  }
  
  initPopulation = function()
  {		
    if (is.null (currentPopulation))
    {
      eps = 10E-6
      if (length(lb) != length(ub))
        stop('Domain vectors must have the same length.\n')
      if (any (is.na (lb)) || any( is.na (ub)))
        stop('Missing values not allowed in Domain vectors.\n')
      if ( any(ub - lb < eps) )
        stop('Small difference detected int Domain vectors.\n')
      
      n = popSize * nvars
      currentPopulation <<- matrix (runif (n), nrow = popSize)				
    }			
  }
  
  initPopulation()
  
  do.evolve = function()
  {
    iter <<- iter + 1      
    decodedPop = decode(currentPopulation, lb, ub)
    fitnessVec = apply(decodedPop, 1, FUN)
    this.best = max(fitnessVec)			
    bestFitnessVec[iter] <<- this.best			
    meanFitnessVec[iter] <<- mean(fitnessVec)
    
    if (is.null(bestFit) || (this.best > bestFit))
    {
      bestFit <<- this.best
      # [1]: pode haver mais de 1 instancia do melhor individuo
      bestCX <<- decodedPop[which(fitnessVec == this.best)[1], ]
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
    	popIdxs = selection.FUN(decodedPop, fitnessVec, nLeft)[1:nLeft]
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
  
  objs = list (		
    population = function()
    {
      decode(currentPopulation, lb, ub)
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
  
  class(objs) = 'GAReal'
  objs
}

plot.GAReal = function(ga, xlab = 'Generation', ylab = 'Fitness', main = 'GA optimization',
		bestcol = 'steelblue', meancol = 'tomato', lwd = 2, 
		legend.pos = c('bottomright', 'bottom', 'bottomleft', 'left', 'topleft', 'top',
		'topright', 'right', 'center'))
{
	ymean = ga$meanFit()
	if (length(ymean) == 0)
	{
		print(summary(ga))
		return(NULL)
	}
	
	ybest = ga$bestFit()
	ylim = c(min(ymean, ybest), max(ymean, ybest))
	plot(ybest, col = bestcol, panel.first = grid(col = '#A9A9A9'), xlab = xlab,
	ylab = ylab, main = main, lwd = lwd, type = 'l', ylim = ylim)
	
	lines(ymean, col = meancol, lwd = lwd)
	legend(legend.pos, legend = c('best', 'mean'), col = c(bestcol, meancol), lwd = lwd)
}

summary.GAReal = function(ga)
{
	n = length(ga$bestFit())		
	if (n == 0)
	{
		sm.obj = list(evolved = FALSE)
		class(sm.obj) = 'summaryGAReal'
		return(sm.obj)		
	}
	sm.obj = list(n = n, sm.mean = summary(ga$meanFit()),
				sm.best = summary(ga$bestFit()),
				best.cx = ga$bestIndividual(),
				best.fit = max(ga$bestFit()), 
				evolved = TRUE)
	class(sm.obj) = 'summaryGAReal'
	sm.obj
}

print.GAReal = function(obj)
{
	print(summary(obj))
}

print.summaryGAReal = function(obj)
{
	if (! obj$evolved)
	{
		cat('Population ready to evolve.')
		cat('\nPlease, call myGA$evolve(h) to generate results.\n')
	}
	else
	{
		cat('Results for', obj$n, 'Generations:')
		cat('\nMean Fitness:\n')
		print(obj$sm.mean)
		cat('\nBest Fitness:\n')
		print(obj$sm.best)
		cat('\nBest individual:\n')
		print(obj$best.cx)
		cat('\nBest fitness value:\n')
		print(obj$best.fit)
	}
}