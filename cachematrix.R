
# # This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix  <-  function( x  =  matrix ()) {
                 inve<-NULL
                 set <- function(y) {
                                 x <<- y
                                 inve <<- NULL
                         }
                 get <- function() {x}
                 setinverse <- function(inverse){inve<<-inverse}
                 getinverse <- function(){inve}
                 list(set=set, get=get,
                      setinverse=setinverse, 
                      getinverse=getinverse)
}

 # # This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
                
cacheSolve  <- function ( x , ... ) {
  # # Returns an array that is the inverse of 'x'
                inve <- x$getinverse()
                
                 if(!is.null(inve)){
                        message("getting cached data")
                        return(inve)
                }
                matrix <- x$get()
                inve <- solve(matrix,...)
                x$setinverse(inve)
                inve
}
m1<- makeCacheMatrix(matrix(1:4,nrow = 2,ncol = 2))
m1$get()
m1$getinverse()
cacheSolve(m1)