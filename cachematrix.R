## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#A<-makeCacheMatrix()
#matriz<-matrix(1:4, 2,2)
#solve(matriz)

#A$set(matriz)
#A$get()
#A$setInv(solve(matriz))
#A$getInv()

makeCacheMatrix <- function(x = matrix()) {
        Minv <- NULL
        set <- function(y) {
                x <<- y
                Minv <<- NULL
        }
        get <- function() x
        setInv <- function(Inv) Minv <<- Inv
        getInv <- function() Minv
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}



## Write a short comment describing this function
#cacheSolve(A)

cacheSolve <- function(x, ...) {
        Minv <- x$getInv()
        if(!is.null(Minv)) {
                message("getting cached data")
                return(Minv)
        }
        data <- x$get()
        Minv <- solve(data, ...)
        x$setInv(Minv)
        Minv
        
        ## Return a matrix that is the inverse of 'x'
}


