## Cacheing the inverse of a matrix ##
## Avoid recomputing the already computed inverse of a matrix from scratch ##
## Usage >>
## temp <- makeCacheMatrix(inputMatrix)
## cacheSolve(temp)

## R function 'makeCacheMatrix' creates cache for an input matrix ##
## the data into cache is stored as a list object containing 4 different functions ##
## one sets value of input matrix
## second one retrieves value of input matrix
## third one sets value of inverse of input matrix
## fourth one retrieves value of inverse of input matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInv <- function(solve) i <<- solve
        getInv <- function() i
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}



## R function 'cacheSolve' takes output of the function 'makeCacheMatrix' as input ##
## first check if the inverse has been calculated
## ... if the inverse is already calculated, the function doesn't recalculate the inverse of that matrix
## and reurns the cached output
## ... else it calculates the inverse and sets the value of the inverse in the cache via 'setInv' function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInv(i)
        i
}
