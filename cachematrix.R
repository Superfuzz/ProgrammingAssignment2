## Functions: makeCacheMatrix, cacheSolve


## Write a short comment describing this function

##  makeCacheMatrix: This function creates a special 'matrix' object that can cache its inverse.

## makeCacheMatrix is based on makeVector, described in this week's assignment
## makeCacheMatrix receives a matrix as it's main parameter and returns a list that contains the same matrix 
makeCacheMatrix <- function(x = matrix()) {
        ## sets the value of m inside the current environment to NULL
        m <- NULL
        ## Stores in -set- the value of x -- sets the value of x and m in the parent environment to y and NULL respectively
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## stores the value of x in get
        get <- function() {
                x
        }
        ## stores the value of inverse on global m, and assignes it to setinverse 
        setinverse <- function(inverse) {
                m <<- inverse
        }
        # Retrieves the value of m and stores it in getinverse
        getinverse <- function() {
                m
        }
        # This is what's returned - a list containing the main argument and the other attributes
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve returns a matrix that is the inverse of 'x', 'x' being a "matrix" created with makeCacheMatrix
## cacheSolve is based on cachemean, described in this week's assignment

cacheSolve <- function(x, ...) {
        ## Recovers the value of 'getinverse' stored in 'x' and assigns it to 'm'
        m <- x$getinverse()
        ## Checks the value of 'm'. If 'm' isn't 'NULL' it returns its value
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## Recovers the value of the matrix stored in the 'x' list and stores it into 'data'
        data <- x$get()
        ## Computes the inverse matrix of 'data' and stores it into 'm'
        m <- solve(data, ...)
        ## Stores the value of the Solve function inside the "matrix" element x, so it can be re-used
        x$setinverse(m)
        ## prints 'm'
        m
}
