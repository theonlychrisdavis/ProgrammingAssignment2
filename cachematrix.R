## 
## makeCacheMatrix creates a cache through which matrix inversion operations are retained.
## cacheSolve uses stored results from matrix inversion, or solves the inversion itself.

## makeCacheMatrix create the cache in which the matrix and its inverse will be stored.
## It returns a list of functions used to work with the cache.
makeCacheMatrix <- function(x = matrix()) {
    
    ## Error check
    if (!is.matrix(x)) {
        message("You must supply a matrix")
        return()
    }
    
    ## Start fresh
    inv <- NULL
    
    ## Assigns the supplied matrix x to the cache, clearing the inverted matrix cache
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## Return the cached matrix
    get <- function() x
    
    ## Cache the supplied inverse
    setinverse <- function(inverse) inv <<- inverse
    
    ## Return the cached inverse, if there is one
    getinverse <- function() inv
    
    ## Finally, return the list of functions that represent the cached matrix x
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve returns the inverted matrix. If the inverse has already been calculated,
## it returns the cached solution. Otherwise, it inverts the matrix and stores the solution
## in the cache.
cacheSolve <- function(x, ...) {
    
    ## Ensure we have a list
    if (!is.list(x)) {
        message("Please supply the matrix cache")
        return()
    }
    
    ## Does the list have the appropriate functions?
    keys <- names(x)
    if ( !("set" %in% keys) || !("get" %in% keys) ||
         !("getinverse" %in% keys) || !("setinverse" %in% keys)) {
        message("List is not a matrix cache")
        return()
    }
    
    ## Get the inverted matrix from the cache
    inv <- x$getinverse()
    
    ## Return the cached solution if it exists
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }

    ## Get the matrix and invert it
    data <- x$get()
    inv <- solve(data)
    
    ## Store the solution in the cache
    x$setinverse(inv)
    
    ## Return the inverse
    inv
}
