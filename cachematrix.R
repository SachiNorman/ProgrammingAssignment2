## Put comments here that give an overall description of what your
## functions do

## Create a Matrix with a cache that can store the value of the inverse Matrix
## returns a list containing 4 elements:
##  $get - a function returning the original Matrix
##  $set - a function to store the data from the original matrix
##  $getinverse - a function returning the cahced inverse matrix
##  $setinverse - a function to store the cahced inverse matrix


makeCacheMatrix <- function(x = matrix()) {
        ## m will be used to cahce the inverse matrix
        m <- NULL
        ## set a function to update the matrix data and erase any stored cahce
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        ## stores the calculated inverse matrix in the cahce
        setinverse <- function(solve) m <<- solve
        ## returns the cahced inverse matrix
        getinverse <- function() m
        ## the list of function returned 
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        ##checks if there is data stored in the cache and returns it if it exists
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## calculates the inverse if the cache is empty
        data <- x$get()
        m <- solve(data, ...)
        ## stores the inverse to the empty cache
        x$setinverse(m)
        m
}
