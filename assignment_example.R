# testing example functions

makeVector <- function(x = numeric()) {
    m <- NULL
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}

#first we should make a vector
a <- makeVector(rnorm(100000000))

#then we can just set the new value in it
a$set(rnorm(100000000))



cachemean <- function(x) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data)
    x$setmean(m)
    m
}



ptm <- proc.time()
cachemean(a)
proc.time() - ptm


