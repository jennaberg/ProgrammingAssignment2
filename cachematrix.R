## what follows are three functions, makeCacheMatrix(<matrix>), cacheSolve(<chached matrix>), and invert(<matrix>)
##the first two were required by the assignment and the third is a sub-proccess used in cacheSolve to calcualte the inverse of a matrix

##details on use of functions
      #to initially store a matrix M, use M<-makeCahceMatrix(M)
      #after the initial storing of a matrix M, you can change your stored matrix to a new one (call new matrix N) using M$set(N) 
      #after you've stored a matrix, you may calcualte and store the inverse of the matrix using the cacheSolve function (typing cacheSolve(M))
      #if you'd like to check in to see what matrix you have stored use M$getM()
      #note further, if cacheSolve is retrieving a stored inverse you will get the message "retreiving stored matrix inverse"
      #if cacheSolve is calulating the inverse of a newly stored matrix you will not see this message



##makeCacheMatrix is a function that not only stores a matrix for retrival, its output is a list  of functions - 
#$subsetting by these functions will allow you to view your stored matrix, and change the value of the stored matrix
##it also includes functions that will be used in the second function cacheSolve.


makeCacheMatrix<-function(x=matrix()){
      I<-NULL #internally sets the Inverse at NULL
      set<-function(y){
            x<<-y #this will store input as the matrix in the parent environment
            I<<-NULL #this resets the Inverse to NULL (since now you've a new matrix)
      }
      get<-function() x #this lets me know what my stored matrix is (well -it retrieves it)
      setInverse<-function(inverse) I<<-inverse #this sets the Inverse in the parent environment
      getInverse<-function() I #this retrieves the inverse that has been stored
      
      list(set=set, get=get,  setInverse=setInverse, getInverse=getInverse) #output list of functions to use to interact with stored matrix and its inverse
}


##cacheSolve is a funciton whose input is a stored matrix, store the inverse of the matrix, and save it for future retrival
##(if you input a matrix that hasn't been stored via makeCache Matrix you will get the error message
##"Error in x$getInverse : $ operator is invalid for atomic vectors"). 

cacheSolve<-function(x, ...){
      I<-x$getInverse() #tries to find sotred matrix
      if(!is.null(I)){ 
            message("retreiving stored inverse matrix")
            return(I)
      } #function which, if there is a stored inverse, retrieves it
      matrix<-x$getM() #retrieving stored matrix
      invert<-function(a=matrix){
            n<-nrow(a)
            b<-diag(n)
            solve(a,b)
      } #this is the function that will give me the inverse of a matrix
      I<-invert(matrix, ...) #calculating the inverse of the stored matrix
      x$setInverse(I) #setting (storing) the inverse of stored matrix
      I #returning the inverse of stored matrix
}



#the final function invert() takes as an input a matrix M, and has the output of the inverse of M.
#if you use a non-invertible matrix (to either invert or cahceSolve)
#you will get the error message "Error in solve.default(a, b) : 'a' (m x n) must be square  "

invert<-function(a=matrix){
      n<-nrow(a)
      b<-diag(n)
      solve(a,b)
}
