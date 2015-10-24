## These pair of functions will create a matrix, cache the inverse 
## and print the inverted matrix

## This function will grab a matrix input and inverse it

makeCacheMatrix <- function(x = matrix()) {
        ## empty for now
        inv <- NULL
        
        ##Sets the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        ##grab the matrix
        get <- function() x
        
        ##Set the inverse
        setinverse <- function(inverse) inv <<- inverse
        
        ##Get in the inverse
        getinverse <-function() inv
        
        ##Then enclose into a list
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}                
## This function will "solve" the matrix from the previous function
## caching it in another environment, and printing it on demand

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        
        ## Only run if there aren't any null values
        if(!is.null(inv)){
                
                ## Provide message if requested data is already cached 
                message("getting cached data...")
                return(inv)
        }
        ## Otherwise get data
        data <- x$get()
        
        ## solve the inverse
        inv <- solve(data, ...)
        
        ## Cache the results
        x$setinverse(inv)
        
        ## Return/Print the result
        inv
}
