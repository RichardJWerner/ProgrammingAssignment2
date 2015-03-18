## The following functions create a cache to store an
## inverse of a matrix to allow for inverse retrieval 
## where needed.

## Creating a special object to cache the inverse matrix...

makeCacheMatrix <- function(x = matrix()) {
        
        i <- NULL
        
        set <- function(y){
                x <<-y
                i <<- NULL
        }

        get <- function() x
        
        setinv <- function(solve) i <<- solve
        
        getinv <- function() i
        
        list(set = set, get = get, 
             setinv = setinv, getinv = getinv )
}


## Compute inverse of matrix if not computed or if matrix
## has changed; otherwise retrieve cached matrix...

cacheSolve <- function(x, ...) {       
        
        ## Return a matrix that is the inverse of 'x'
        i <- makeCacheMatrix(getinv)
        
        ## Return the cached matrix
        data <- makeCacheMatrix(get)

        ## check for cached inverse matrix
        if(!is.null(i)) {
                
                ## Check to see if matrix changed
                if(x == data) {
                
                        message("getting cached data")
                        return(i)
                }
                
                ## matrix changed; need to re-solve
                message("solving for changed matrix")
        }
        
        ## solve for matrix
        i <- solve(data, ...)
        
        makeCacheMatrix(setinv) <- i

        i
        
}
