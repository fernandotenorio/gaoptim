simpleGA = function (f, lowerBounds, upperBounds,  popSize = 100, mutRate = 0.05, cxRate = 0.9, eliteRate = 0.2)
{		
		population = NULL		
		bestFitnessVec = numeric()
		meanFitnessVec = numeric()
		elite = max(0, 2 * as.integer(eliteRate * popSize * 0.5))
		popSize = 2 * as.integer(popSize * 0.5)
		nvars = length(lowerBounds)
		bestCX = rep(0, nvars)
		bestFit = NA
		mutations = as.integer(mutRate * popSize * nvars)
		
		mutate = function(x)
		{			
			rows = sample(1:nrow(x), mutations, rep = TRUE)
			cols = sample(1:ncol(x), mutations, rep = TRUE)
			noise = runif(mutations)
	
			ext = matrix(c(rows, cols), mutations, 2)
			x[ext] = noise
			
			x
		}
		
		do.crossover = function(rowIdxs, M, beta = 0.5)
		{
			m1 = apply(rowIdxs, 1,  crossover.vec, mat = M, beta = beta)
			matrix(t(m1), byrow = F, ncol = ncol(M))
		}
		
		crossover.vec = function(rowVector, mat, beta)
		{
			  extrapolationCrossover(mat[rowVector[1], ], mat[rowVector[2], ], beta)			 
		}
		
		extrapolationCrossover = function(cr1, cr2, beta)
		{						
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
				if (length(lowerBounds) != length(upperBounds))
					stop('Boundary vectors must have the same length.')
				if (length(lowerBounds) == 0 || length(upperBounds) == 0)
					stop('Boundary vectors are empty.')
				if (any (is.na (lowerBounds)) || any( is.na (upperBounds)))
					stop('NA\'s not allowed in Boundary vectors.')
				if ( any(upperBounds - lowerBounds < eps) )
					stop('Small difference detected int upperBounds - lowerBounds. ')
							
			    n = popSize * nvars
				population <<- matrix (runif (n), nrow = popSize)				
			}			
		}
		
		initPopulation()
		
		do.evolve = function()
		{
			decodedPop = decode(population, lowerBounds, upperBounds)
			fitnessVec = apply(decodedPop, 1, f)
			this.best = max(fitnessVec)
			bestFitnessVec <<- c(bestFitnessVec, this.best)
			meanFitnessVec <<- c(meanFitnessVec, mean(fitnessVec))
			
			if (is.na(bestFit) || (this.best > bestFit))
            {
				bestFit <<- this.best
				bestCX <<- decodedPop[which(fitnessVec == this.best), ]
            }

			newPopulation = matrix(0, nrow = popSize, ncol = nvars)
			nLeft = popSize
			
			if (elite > 0)
			{
				# Maximization problem												
				nLeft = popSize - elite
				newPopulation[1:elite, ] = population[order(fitnessVec, decreasing = TRUE)[1:elite], ] 
			}
			
			# crossover
			popIdxs = sample(1:popSize, nLeft, replace = TRUE, prob = fitnessVec)
			popIdxsM = matrix(popIdxs, ncol = 2, byrow = T) 
			beta = 0.5
			
			offspring = do.crossover(popIdxsM, population, beta)
			newPopulation[(elite+1):popSize, ] = offspring
			
			population <<- mutate(newPopulation)
		}		
		
		objs = list (		
		get.population = function()
		{
			decode(population, lowerBounds, upperBounds)
		},								
		
		get.bestfit.hist = function()
		{
			bestFitnessVec
		},
		get.meanfit.hist = function()
		{
			meanFitnessVec
		},
		get.best = function()
        {
			bestCX
        },
        
        evolve = function(gens = 50)
        {        	
        		invisible(replicate(gens, do.evolve()))
        }
        
	)
	
	objs
}
