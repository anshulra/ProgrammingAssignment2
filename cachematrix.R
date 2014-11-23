##makeCacheMatrix: create a special matrix
##cacheSolve: calculate inverse of the special matrix

## makeCacheMatrix: creates a special matrix containing functions to:
## 1. set: set the matrix 
## 2. get: get the matrix
## 3. setInverse: set the inverse of the matrix
## 4. getInverse: get the inverse of teh matrix


makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y){
                x<<-y
                inverse<<-NULL
        }
        get <-function() x
        setInverse <- function(inv) inverse <<-inv
        getInverse <- function() inverse
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


##cacheSolve first checks if inverse of matrix has already been calculated
##if inverse already exists then it returns the cached inverse else it computes it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        if(!is.null(inverse)) {
                message("Getting cacehd data")
                return(inverse)
        }
        mat <- x$get()
        inverse <- solve(mat, ...)
        x$setInverse(inverse)
        inverse
}
