## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    # example call: a<-makeCacheMatrix(matrix(1:4,2,2))    
    
    m <- NULL   #initialisation of local variable m set to NULL          
    set <- function(y) {
        x <<- y      #cache variable x in the containing environment, for the cachesolve function 
        m <<- NULL   #variable m in the containing environment is set to NULL
    }
    get <- function() x     #Function for returning the input matrix
    setSolve <- function(solve) m <<- solve #Function called from caheSolve for caching the solve
    getSolve <- function() m                #FFunction called from cacheSolve for checking if solve is already cached
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getSolve()  # gets the cached solve. Function declared in makeCacheMatrix
    if(!is.null(m)) {  # after the second call, m will have a matrix
        message("getting cached data")
        return(m)
    }
    #The first time we will have to do the solve, because it isn't cached yet
    data <- x$get()  # get the input matrix. Function get() is declared in makeCacheMatrix
    m <- solve(data, ...)  # do the solve
    x$setSolve(m)          # cache output matrkix. Function declared in makeCacheMatrix
    m                      # print matrix
}
