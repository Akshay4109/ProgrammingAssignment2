## Calculating the inverse of the matrix iff there is no orior cached inverse being calculated

## makeCacheMatrix sets the matrix x and sets its inverse using setinverse()
## using get() and getinverse() we can retrive the values of the matrix and the inverse respectively
makeCacheMatrix <- function(x = matrix()) {
            i<-NULL
           set<-function(y){
               x<<-y
               inv<<-NULL
           }
           get <- function() x
           setinverse <- function(inverse) i<<-inverse
           getinverse <- function() i
           list(set = set,get = get, setinverse = setinverse, getinverse = getinverse)


}


## cacheSolve checks whether there is a cached inverse if not then it calculates the inverse and returns it

cacheSolve <- function(x, ...) {
         i <- x$getinverse()
         if(!is.null(i)){
          message("getting cached data")
          return(i)
         }
         data <- x$get()
         i  <-solve(data,...)
         x$setinverse(i)
         i

}
