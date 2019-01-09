## makeCacheMatrix is a special function that can store a matrix and cache its
## inverse

makeCacheMatrix <- function(x = matrix())
 {
	inv <- NULL
	set <- function(y)
		{  x <<- y
		   inv <<- NULL
		}
      get <- function()x
      setInverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
 }


## cacheSolve finds the iverse of the matrix created by the above function 
##(makeCacheMatrix). In case a new computation is not done, obtain matrix from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	  inv <- x$getInverse()
        if (!is.null(inv))
          { message ("obtaining cached data")
	      return(inv)
           }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
