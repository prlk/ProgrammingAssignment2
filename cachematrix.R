##A function that creates a matrix and a list of functions

makeCacheMatrix <- function(mat = matrix()) {
        
        #setting the inverse to null
        
        inv <- NULL
        
        #getmat: returns the matrix when it is asked for
        getmat <- function() mat
        
        #setmat: sets the value of the matrix and returns inv to NULL
        setmat <- function(y){
                mat <<- y
                inv <<- NULL
        }
        
        #getinv: returns the inverse of the matrix
        getinv <- function() inv
        
        #setinv: sets the value of the inverse
        setinv <- function(inverse){
                inv <<- inverse
        }
        
        #creating a list that stores the functions
        list(setmat = setmat,
             getmat = getmat,
             getinv = getinv,
             setinv = setinv)
}


## calculates the inverse for the previous matrix
## if the inverse already exists, it is retrieved from the cache

cacheSolve <- function(x, ...) {
        
        #retrieving the inverse
        inv <- x$getinv()
        
        #if the inverse already exists, it is returned
        if(!is.null(inv)){
                print("Inverse already exists, retrieving previous result")
                return(inv)
        }
        
        #if there is no inverse, it is calculated
        
        mat <- x$getmat()
        
        inv <- solve(mat)
        
        #caching the inverse
        
        x$setinv(inv)
        
        #returning the inverse
        
        inv
}
