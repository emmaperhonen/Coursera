
## Make a matrix that can cache the inverse
##list/function where the inverse is made or retrieved

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  ##set the matrix
  set_matrix<- function(y){
    x<<-y
    i<<-NULL
  }
  ##find matrix
  get_matrix<-function()x
  ##inverse
  set_m_inverse<-function(inverse) i<<-inverse
  get_m_inverse<-function() i
  list (set_matrix=set_matrix,
        get_matrix=get_matrix,
        set_m_inverse=set_m_inverse,
        get_m_inverse=get_m_inverse)
}


## Either find the inverse of matrix if not then compute inverse of matrix

cacheSolve <- function(x, ...) {
        ## step 1 find the inverse
  i<-x$get_m_inverse()
  if(!is.null(i)){
      message ("wait a second getting the inverse")
      return (i)
  }
  data<- x$get_matrix()
  ##solve computes the inverse
  i<- solve(data, ...)
  x$set_m_inverse(i)
  i
}

