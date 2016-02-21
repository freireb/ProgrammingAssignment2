## First function creates necessary matrix that will set up
##second function which will cache the inverse

## Creating matrix that will cache the inverse

makeCacheMatrix <- function(x = matrix()) {
    m<- NULL
    set <-function(y){
          x<<-y
          m<<-NULL
    }
    get<-function()x
    set_inverse<-function(inverse)m<<-inverse
    get_inverse<-function()m
    list(set = set,
         get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
    
}


## Calculates the inverse of the matrix above, but if
##inverse already solved for then the cache inverse
##will be called

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m<-x$get_inverse()
    if (!is.null(m)){
        return(m)
    }
    original<-x$get()
    m<-solve(original,...)
    x$set_inverse(m)
    m
}
