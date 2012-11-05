simpleGAP = function(FUN, n, popSize = 100, mutRate = 0.01, cxRate = 0.95, eliteRate = 0.4)
{
	population = NULL		
  	bestFitnessVec = numeric()
  	meanFitnessVec = numeric()
  	elite = max(0, 2 * as.integer(eliteRate * popSize * 0.5))
  	popSize = 2 * as.integer(popSize * 0.5)
  	bestCX = rep(0, n)
  	bestFit = NULL
	mutations = as.integer(mutRate * popSize)
  	iter = 0
	newPopulation = matrix(0, nrow = popSize, ncol = n)
	
	pmx = function(vec1, vec2)
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

	mutate = function(M)
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

	do.crossover = function(rowIdxs, M)
  	{
    		m1 = apply(rowIdxs, 1, crossover.vec, mat = M)
    		matrix(t(m1), byrow = F, ncol = ncol(M))
  	}
  
  	crossover.vec = function(rowVector, mat)
  	{
    		pmx(mat[rowVector[1], ], mat[rowVector[2], ])			 
  	}

	initPopulation = function()
	{
		if (is.null(population))
			population <<- matrix(replicate(popSize,sample(1:n)), byrow = TRUE, ncol = n)
	}

	initPopulation()
	
	do.evolve = function()
	{
		iter <<- iter + 1      
    		fitnessVec = apply(population, 1, FUN)
    		this.best = max(fitnessVec)			
    		bestFitnessVec[iter] <<- this.best			
    		meanFitnessVec[iter] <<- mean(fitnessVec)
    
    		if (is.null(bestFit) || (this.best > bestFit))
    		{
      		bestFit <<- this.best
      		bestCX <<- population[which(fitnessVec == this.best), ]
    		}
    
    		nLeft = popSize
    
    		if (elite > 0)
    		{
      		# Maximization problem												
     		 	nLeft = popSize - elite
      		newPopulation[1:elite, ] = population[order(fitnessVec, decreasing = TRUE)[1:elite], ] 
    		}
    
    		# Crossover selection
      	probVec = fitnessVec
    		popIdxs = sample(1:popSize, nLeft, replace = TRUE, prob = probVec)
    		popIdxsM = matrix(popIdxs, ncol = 2, byrow = T) 
    		
    		offspring = do.crossover(popIdxsM, population)
    		newPopulation[(elite+1):popSize, ] = offspring
    
   	 	population <<- mutate(newPopulation)
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


