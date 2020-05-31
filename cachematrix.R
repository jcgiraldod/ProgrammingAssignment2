## This two functions allows to not recalculate the inverse of a matrix that does not change in every loop
## Saving the results in the Caché and then using it instead recalculate

## Takes the same example as the makeVector matrix but instead of the mean review the cached data to calculate the inverse

makeCacheMatrix <- function(x = matrix()) {
inv<-null
set<-function(y){
  x<<-y
  inv<<-null
}
get<-function() x
inver<-solve(x)
inverse<-function(inver) inv<<-inver
get_inverse<-function() inv
list(inverse=inverse, get_inverse=get_inverse,
     inverse=inverse,
     get_inverse=get_inverse)
}


## This function allows to take the special Matrix created to obtain the inverse from cached data

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
inv<-x$get_inverse
if(!is.null(inv)){
  message("Getting inverse from cached data")
  return(inv)
}
data<-x$get()
inv<- solve(x)
x$get_inverse(inv)
inv
}
