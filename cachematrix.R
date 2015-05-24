## The following pair of functions calculates the inverse of a matrix and saves
## it to the cache so that the user can call it to return the saved value and
## avoid repeating the computation 

## makeCacheMatrix is a function that creates a special matrix object that 
## can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL  ## define the cache 'm'
        set <- function(y){
                x <<- y  ## assign the input matrix 'y' to the variable 'x' in 
                ## the parent environment
                m <<- NULL  ## restores 'm' to null in the parent environment
        }
        get <- function() x  ## return the matrix 'x'
        setinverse <- function(solve) m <<- solve  ## set the cache 'm' so that it
        ## is equal to the inverse of the matrix 'x'
        getinverse <- function() m  ## return the cached inverse of the matrix 'x'
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)  ## stores the 4 functions in the function
        ## makeCacheMatrix
}


## The following function calculates the inverse of the special "matrix" object 
## returned by the makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cacheSolve should
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$inverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setinverse(m)
        m
}