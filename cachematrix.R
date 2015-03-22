## The two functions below will return the inverse of a matrix
## If the inverse has been calculated, this will be cached for quicker use
## in future

## makeCacheMatrix takes an input of a matrix 'x' and outputs a list of 4 functions:
## set, get, setinv, getinv

makeCacheMatrix <- function(x = matrix()) {
        ## initialise the inverse variable to NULL
        i <- NULL
        
        ## Function to set x to be the original matrix x and i to be NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        ## Function to return the matrix x
        get <- function() x
        
        ## Function to set i to be the inverse matrix passed in
        setinv <- function(inv) i <<- inv
        
        ## Function to return the inverse matrix
        getinv <- function() i
        
        ## Outputs a list of the 4 created functions
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve takes an input of a list 'x' from makeCacheMatrix and checks to see
## if the inverse of the matrix has been calculated.  If it has been calculated and
## stored in the cache then the function returns the inverse.  If it hasn't been
## calculated then it calculates the inverse using the solve() function, caches it
## and returns the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## Set i to the 'getinv' element of the list x passed in 
        i <- x$getinv()
        
        ## Check if the inverse has been cached
        if(!is.null(i)) {
                ## If it has, return the inverse
                message("getting cached data")
                return(i)
        }
        
        ## If it hasn't been cached, the inverse must be calculated
        ## Set data to the matrix
        data <- x$get()
        
        ## Calculate the inverse using the solve() function
        i <- solve(data, ...)
        
        ## Cache the inverse using the setinv() function of the list
        x$setinv(i)
        
        ## Print the inverse matrix
        i
}
