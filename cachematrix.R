## Put comments here that give an overall description of what your
## functions do

## Creates a matrix that caches it's inverse.

makeCacheMatrix<-function(x=matrix())
{
  inv<-NULL
  set<-function(mat)
  {
    x<<-mat
    inv<<-NULL
  }
  
  get<-function()
  {
    x
  }
  
  setinverse<-function(inverse)
  {
    inv<<-inverse
  }
  
  getinverse<-function()
  {
    inv
  }
  
  list(set=set,
       get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}



## Calculates the inverse of the matrix object.

cacheSolve<-function(x,...)
{
  inv<-x$getinverse()
  
  if(!is.null(inv))
  {
    message("getting cached data")
    return(inv)
  }
  
  m<-x$get()
  s_m<- solve(m)%*%m
  x$setinverse(s_m)
  s_m
}