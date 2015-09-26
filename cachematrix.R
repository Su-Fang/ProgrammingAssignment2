## In this Programming Assignment will take advantage of the scoping rules 
## of the R language and how they can be manipulated to preserve state inside of an R object.

## To cache the inverse of a matrix (rather than compute it)

## makeCacheMatrix is to store the matrix
## <<- operator which can be used to assign a value to an object in an environment 
## that is different from the current environment.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
              x <<- y
              inv <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse) inv <<- inverse
      getInverse <- function() inv
      list( set = set,
            get = get, 
            setInverse = setInverse,
            getInverse = getInverse)
}

## cacheSolve is to calculate its inverse
## However, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message ("getting cached data")
                return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix, ...)
        x$setInverse(inv)
        inv
}
