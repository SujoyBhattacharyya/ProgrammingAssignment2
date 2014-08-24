## These two functions calculate the inverse of a matrix an cache it for later use. If the inverse has
## already been calculated, the function does no calculation but simply retrieves the cached value. 
## Honor Code: This code is heavily based on the example in the assignment description. Bill Hilton's post
## on the discussion forums was also helpful to understand how the code worked. 

## makeCacheMatrix creates a matrix object that will later be used by cacheSolve. It accepts as argument 
## a matrix, which we can do all sort of this to. m is a dummy variable that is set to NULL every time
## that makeCacheMatrix is used. The object has four methods, which are put in a list for later use. 
## "get" will return the raw matrix. "setInverse" will be used by cacheSolve to cache the inverse of the 
## matrix. "getInverse" will be used to retrieve the cached value. "set" is an additional method that allows
## you to reset the matrix of the object from the command line. 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL 
        get <- function() {x}
        setInverse <- function(inverse) {m <<- inverse}
        getInverse <- function() {m}
        set <- function(y) {
            x <<- y
            m <<- NULL
        }
        list(get = get, setInverse = setInverse, getInverse = getInverse, set = set)    
}


## cacheSolve checks to see if the inverse of the raw matrix in a matrix object (created by makeCacheMatrix)
## has been calculated and cached. If the inverse has been cached, then the function simply returns the
## cached value along with the message "getting cached data" so that the user knows what's going on. If the 
## inverse hasn't already been, then cacheSolve uses the methods of makeCacheMatrix to retrieve the raw matrix
## from the matrix object, solve for its inverse and cache it. 

cacheSolve <- function(x, ...) {
        m <- x$getInverse()             
        if(!is.null(m)) {                            
            message("getting cached data")
            return(m)
        } else  {                               
            data <- x$get()
            m <- solve(data,...)
            x$setInverse(m)
            m
        }
}
