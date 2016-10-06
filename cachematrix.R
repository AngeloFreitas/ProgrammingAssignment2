## The functions bellow verify if the inverse matrix had already been
## computed and return its value already retrieved in cache. The inverse
## matrix is computed only when it's necessary.

## The makeCacheMatrix function assigns values to x and i. It sets the values
## of a given matrix and its inverses. It also retrieves both matrices.

makeCacheMatrix <- function(x = matrix()) {
                   ## Initializes i and x
                   i <- NULL
                   set <- function(y) {
                        ## Caches x and i
                        x <<- y
                        i <<- NULL
                   }
                   ## Gets matrix x
                   get <- function() x
                   ## Sets inverse matrix and caches it in i
                   setinv <- function() i <<- solve(x)
                   ## Gets inverse matrix i
                   getinv <- function() i
                   ## List sets and gets and their respective names
                   list(set = set, get = get,
                        setinv = setinv,
                        getinv = getinv)
}


## This function checks if there's already an inverse already retrieved and
## returns the matrix in affirmative case. Otherwise, it computes the inverse
## and retrieves it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        ## Checks if inverse matrix has already been computed and returns it
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        ## Computes the inverse matrix when there isn't cached inverse matrix
        i <- solve(data,...)
        x$setinv()
        i
}
