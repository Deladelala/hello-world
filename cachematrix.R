## Below are two functions that are used to create a special object 
## that stores a square matrix and caches its inverse.

## The first function, makeVector creates a special "matrix", 
## which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        invert <- NULL
        set <- function(y){
                x <<- y
                invert <<- NULL
        }
        get <- function() x
        setinvert <- function(inv) invert <<- inv
        getinvert <- function() invert
        list(set = set, get = get,
             setinvert = setinvert,
             getinvert = getinvert)
}


## The following function calculates the mean of the special
## "matrix" created with the above function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invert <- x$getinvert()
        if(!is.null(invert)){
                message("getting cached data")
                return(invert)
        }
        data <- x$get()
        invert <- solve(data, ...)
        x$setinvert(invert)
        invert
}
