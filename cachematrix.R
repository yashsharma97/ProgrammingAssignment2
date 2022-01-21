## Below are the two functions that cache the inverse of a matrix based on given constraints.

MatrixCache <- function( m = matrix() ) {      ## function to create a matrix object that is capable of caching its own inverse

	    i <- NULL

    
    set <- function( matrix ) {   ## setting up the required matrix
            m <<- matrix
            i <<- NULL
    }

    
    get <- function() {      ## retrieving the required matrix
    	
    	m
    }

    
    setInverse <- function(inverse) {     ## Finding the inverse
        i <<- inverse
    }

    
    getInverse <- function() {    ## Returning the inverse
        
        i
    }

      list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


cacheSolve <- function(x, ...) {

    
    m <- x$getInverse()   ## Returning inverse of matrix 'x'

    
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

    
    data <- x$get()

    
    m <- solve(data) %*% data   ## Calculating the inverse

    
    x$setInverse(m)

    
    m      ## output returned
}
