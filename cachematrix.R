makeCacheMatrix <- function(x = matrix()) {
  mat<-NULL
  set<-function(y){
    x<<-y
    mat<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) mat<<- solve
  getmatrix<-function() mat
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

cacheSolve <- function(x=matrix(), ...) {
  mat<-x$getmatrix()
  if(!is.null(mat)){
    message("getting cached data")
    return(mat)
  }
  matrix<-x$get()
  mat<-solve(matrix, ...)
  x$setmatrix(mat)
  mat
}
