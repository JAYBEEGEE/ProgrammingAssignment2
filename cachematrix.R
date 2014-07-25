## makeCacheMatrix (mCM) is a function which creates a cache variable d
## and a set of functions that redefine d to the value of a var passed to 
## mCM, pass mCM value of d to a calling environment and also pass the
##  matrix argument of mCM to a calling environment



## makeCacheMatrix (mCM) defines a function which takes a matrix as input
## it initially sets a cache variable d to NULL - 1st pass
## and then defines a set of functions
## 1 get an empty function which returns the matrix passed to mCM
## 2 setparval which takes an argument parval and assigns to d 
##   the value of parval from this environment or parent environments
## 3 getinv an empty function which returns the value of the cache var d
## mCM then creates a list of the functions it defines
 

makeCacheMatrix <- function(x = matrix()) {
        d <- NULL                                  
        get <-  function() x                       
        setparval <- function(parval) d <<- parval 
                                                   
        getinv <-  function() d                    

        list(get = get,
             setparval = setparval,
             getinv = getinv)
}


## cacheSolve is a function which uses mCM as an argument
## it calls the mCM function getinv to assign the mCM value of the cache
## var d to m
## m is then checked to see if it is NULL, if not m is returned i.e. the 
## cached value
## if m is NULL, i.e. there is no cache value, the original matrix is 
## retrieved from mCM using the mCM get function and assigned th data
## data is inversed and assigned to m, this environment's value of m is
## cached using the mCM setparval function and then output


cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setparval(m)
        m
}
 