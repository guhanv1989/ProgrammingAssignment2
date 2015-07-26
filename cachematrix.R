## makeCacheMatrix() creates a list with 4 objects set,get,setinv, and getinv


makeCacheMatrix <- function(x = matrix()) 
{
        i <- NULL
        set <- function(y) 
        {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get, setinv = setinv, getinv = getinv)
        
}


## cacheSolve() checks if the inverse of the matrix is already present and 
## also checks if the matrix has changed. If the inverse is already present in the cache
## it retrieves the result from the cache, orelse computes a new inverse.

cacheSolve <- function(y, ...) {
        ## Return a matrix that is the inverse of 'x'
        matinv<-y$getinv()
        ## This condition statement checks if the inverse has already been calculated
        ## and if the matrix is the same
        if(!is.null(matinv)&& solve(y$get())==y$getinv())
        {
                print("Getting data from cache!")
                return(matinv)
        }
        mat<-y$get()
        matinv<-solve(mat)
        y$setinv(matinv)
        matinv
}
