
#makeCacheMatrix consists of set, get, setInverse, getInverse functions.
#This function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){ #set the matrix 
    x <<- y
    inv <<- NULL
  }
  get <- function() x #get the matrix 
  setInverse <- function(inverso) inv <<- inverso #set the inverse of the matrix
  getInverse <- function()inv #get the inverse of the matrix
  list(set = set, get = get,
       setInverse = setInverse, 
       getInverse = getInverse) #list with the functions 
}

#cacheSolve computes the inverse of the special matrix returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
#retrieve the inverse from the cache.

cacheSolve <- function(x, ...){ 
  inv <- x$getInverse()
  if(!is.null(inv)){ #examine if the inverse was calculated
    message("getting cached data")
    return(inv) #returns the inverse 
  }
  matriz <- x$get() #get our matrix
  inv <- solve(matriz, ...) #use solve function to calculate the inverse 
  x$setInverse(inv)
  inv #show the inverse 
}
