

#The first function,makeCacheMatrix caches the inverse of input matrix and it
#is a list of functions for following tasks:
#1.set the value of matrix
#2.get the value of matrix
#3.set the value of inverse
#4.get the value of inverse

#The second function cacheSolve computes the value of inverse of matrix
#and if its already computed then it returns the cached value.


##For caching the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  if(!is.matrix(x))
    stop("Input should be a matrix.")
  x_inv<-NULL
  set<-function(y)
  {
    x<<-y
    x_inv<-NULL
  }
  
  get<-function() x
  set_inv<-function(invMat) x_inv<<-invMat
  get_inv<-function() x_inv
  list(set=set,
       get=get,
       set_inv=set_inv,
       get_inv=get_inv)
}


## For computing the inverse of matrix.

cacheSolve <- function(x, ...) {
  x_inv<-x$get_inv()
  if(!is.null(x_inv))
  {
    message("getting cached value")
    return(x_inv)
  }
  mat<-x$get()
  invMat<-solve(mat)
  x$set_inv(invMat)
  invMat
 
}
