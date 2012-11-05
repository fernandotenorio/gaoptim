simpleGAP = function(f, n, popSize = 50, mutRate = 0.01, cxRate = 0.95)
{
	population = NULL
	bestFitnessVec = numeric()
	meanFitnessVec = numeric()

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

	objs = list(

	evolve = function(h)
	{
		
	}

	)

	objs
}


