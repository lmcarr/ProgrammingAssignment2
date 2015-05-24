
## makeCacheMatrix takes inputted matrices and calculates the inverse of them. 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() {
                x
        }
        setmatrix <- function(solve) {
                m <<- solve
        }
        getmatrix <- function() {
                m
        }
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)        
}


## cacheSolve returns the inverse of a square matrix. If the inverse has already been cached with makeCacheMatrix, 
##this function simply returns the matrix. If the inverse has yet to be cached, cacheSolve calculates the inverse then prints it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}
