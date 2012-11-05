simpleGAP = function(f, n, popSize = 100, mutRate = 0.01, cxRate = 0.95, eliteRate = 0.4)
{
	population = NULL		
  	bestFitnessVec = numeric()
  	meanFitnessVec = numeric()
  	elite = max(0, 2 * as.integer(eliteRate * popSize * 0.5))
  	popSize = 2 * as.integer(popSize * 0.5)
  	bestCX = rep(0, n)
  	bestFit = NULL
  	iter = 0
	
	pmx = function(vec1, vec2)
	{
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

		list(vec1, vec2)
	}

	mutate = function(vec)
	{
		idxs = sample(1:length(vec), 2)
		val = vec[idxs[1]]
		vec[idxs[1]] = vec[idxs[2]]
		vec[idxs[2]] = val
		vec
	}	

	initPopulation = function()
	{
		if (is.null(population))
			population <<- matrix(replicate(popSize,sample(1:n)), byrow = TRUE, ncol = n)
	}

	initPopulation()
	
	do.evolve = function()
	{
		
	}

	objs = list(
	get.population = function()
    	{
      	population
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


