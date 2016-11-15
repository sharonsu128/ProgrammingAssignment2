##makeCacheMatrix specifies a matrix which waits to get its inverse

makeCacheMatrix <- function(x=matrix()){ 
        #set 'm' as null
        m <- NULL
        #define setter 
        set <- function(y){
                #set 'x' matrix
                x <<- y
                #clear 'm' 
                m <<- NULL
        }
        #set getter to get the matrix
        get <- function() x
        #set the inverse of 'x' matrix to be 'm' 
        setinverse <- function(solve) m <<- solve
        #get m matrix
        getinverse <- function() m
        #make a list of above functions and name them
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

##cacheSolve searches whether the inverse of this matrix has 
###already been calculated and retrieves the inverse
cacheSolve <-function(x,...){
        #set 'm' to the inverse of 'x' matrix
        m <- x$getinverse()
        #if there is already 'm' in the memory, call it
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        #if there is no memory of 'm', calculate the inverse
        data <- x$get()
        #calculate the inverse and call it 'm'
        m <- solve(data,...)
        #set the inverse of 'x' matrix to be 'm'
        x$setinverse(m)
        #print m
        m
}
