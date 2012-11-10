GAReal = function (FUN, lb, ub,  popSize = 100, mutRate = 0.01, cxRate = 0.9, eliteRate = 0.4,
                     selection = c('fitness', 'uniform'), crossover = c('blend'), mutation = c('noise'))
{		
  population = NULL		
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
  
  selection.FUN = NULL
  if (is.function(selection))
  	selection.FUN = selection
  else
  	selection.type = switch(match.arg(selection), fitness = 'fitness', uniform = 'uniform')
  	
  do.blendCrossover = function(rowIdxs, M, beta = 0.5)
  {
  	crossover.vec = function(rowVector, mat, beta)
  	{
    	blendCrossover(mat[rowVector[1], ], mat[rowVector[2], ], beta)			 
  	}
  	
    m1 = apply(rowIdxs, 1,  crossover.vec, mat = M, beta = beta)
    matrix(t(m1), byrow = F, ncol = ncol(M))
  }
  
  crossover.FUN = NULL
  if (is.function(crossover))
  	crossover.FUN = crossover
  else
  	crossover.FUN = do.blendCrossover
  	
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
  
  blendCrossover = function(cr1, cr2, beta)
  {								
    prob = runif(1)
    if (prob > cxRate)                  
      return(matrix(c(cr1, cr2), nrow = 2, byrow = T))
    
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
  
  decode = function(x, lb, ub)
  {
    n = nrow(x)
    x * rep(ub - lb, each = n) + rep(lb, each=n) 	
  }
  
  initPopulation = function()
  {		
    if (is.null (population))
    {
      eps = 10E-6
      if (length(lb) != length(ub))
        stop('Domain vectors must have the same length.\n')
      if (any (is.na (lb)) || any( is.na (ub)))
        stop('Missing values not allowed in Domain vectors.\n')
      if ( any(ub - lb < eps) )
        stop('Small difference detected int Domain vectors.\n')
      
      n = popSize * nvars
      population <<- matrix (runif (n), nrow = popSize)				
    }			
  }
  
  initPopulation()
  
  do.evolve = function()
  {
    iter <<- iter + 1      
    decodedPop = decode(population, lb, ub)
    fitnessVec = apply(decodedPop, 1, FUN)
    this.best = max(fitnessVec)			
    bestFitnessVec[iter] <<- this.best			
    meanFitnessVec[iter] <<- mean(fitnessVec)
    
    if (is.null(bestFit) || (this.best > bestFit))
    {
      bestFit <<- this.best
      bestCX <<- decodedPop[which(fitnessVec == this.best), ]
    }
    
    nLeft = popSize
    
    if (elite > 0)
    {
      # Maximization problem												
      nLeft = popSize - elite
      newPopulation[1:elite, ] = population[order(fitnessVec, decreasing = TRUE)[1:elite], ] 
    }
    
    # crossover selection
    if (identical(selection.type, 'fitness'))
      probVec = fitnessVec
    else if (identical(selection.type, 'uniform'))
      probVec = NULL
    else
      stop('Unknow selection type.\n')
    
    popIdxs = sample(1:popSize, nLeft, replace = TRUE, prob = probVec)
    popIdxsM = matrix(popIdxs, ncol = 2, byrow = T) 
    beta = 0.5
    
    offspring = crossover.FUN(popIdxsM, population, beta)
    newPopulation[(elite+1):popSize, ] = offspring
    
    population <<- mutation.FUN(newPopulation)
  }		
  
  objs = list (		
    get.population = function()
    {
      decode(population, lb, ub)
    },								
    
    get.bestfit.hist = function()
    {
      bestFitnessVec
    },
    
    get.meanfit.hist = function()
    {
      meanFitnessVec
    },
    
    get.best.cx = function()
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
  
  objs
}
