#' An S4 class for sparse matrix.
#' 
#' @slot i row index
#' @slot j colum index
#' @slot x value
#' @slot dims dimensions
#' @name sparse.matrix
#' @rdname sparse.matrix-methods
#' @export sparse.matrix
#' @docType methods
#' @import methods



sparse.matrix <- setClass("sparse.matrix",
                          slots=list(i="numeric", j="numeric",
                                     x="numeric",dims = "numeric"))

sparse.matrix<-function(i,j,x,dims=NULL){
  if(is.null(dims)){
    dims<-c(max(i),max(j))
  }
  a1<-new("sparse.matrix",i = i, j = j, x = x, dims = dims)
  return(a1)
}

#' t method for sparse.matrix 
#' 
#' @param x,y,e1,e2,i,j,dims a \code{sparse.matrix} object 
#' @docType methods 
#' @rdname sparse.matrix-methods
#' @aliases t,sparse.matrix,ANY-method
#' @usage \S4method{t}{sparse.matrix}(x) 
#' @inheritParams x from t function
#' 
setMethod("t", signature = "sparse.matrix",function(x){
  a <- data.frame(i = x@i, j = x@j, x = x@x)
  temp<-a$i
  a$i<-a$j
  a$j<-temp
  a<-a[order(a$i),]
  a <- sparse.matrix(i = a$i, j = a$j, x = a$x,
                     dims = c(x@dims[2], x@dims[1]))
  return(a)
})
 
#' + method for sparse.matrix 
#' 
#' @docType methods 
#' @rdname sparse.matrix-methods
#' @aliases +,sparse.matrix,sparse.matrix-method
#' @usage \S4method{+}{sparse.matrix,sparse.matrix}(e1,e2) 
#' @inheritParams e1,e2 from add function

setMethod("+",
          signature(e1= "sparse.matrix", e2 = "sparse.matrix"),
          function(e1, e2){
            if(sum(e1@dims != e2@dims) == 0){
              a <- data.frame(i = e1@i, j = e1@j, x = e1@x)
              b <- data.frame(i = e2@i, j = e2@j, x = e2@x)
              c <- merge(a, b, by = c("i", "j"), all = TRUE, suffixes = c("1", "2"))
              c$x1[is.na(c$x1)] <- 0
              c$x2[is.na(c$x2)] <- 0
              c$x <- c$x1 + c$x2
              c <- c[, c("i", "j", "x")]
              c <- c[order(c$j), ]
              rownames(c) <- (1:nrow(c))
              c <- sparse.matrix(i = c$i, j = c$j, x = c$x,
                                 dims = c(e1@dims[1], e2@dims[2]))
              return(c)
            }else{
              stop("Dimensions Error")
            }
          })

#' %*% method for sparse.matrix 
#' 
#' @docType methods 
#' @rdname sparse.matrix-methods
#' @aliases %*%,sparse.matrix,sparse.matrix-method
#' @usage \S4method{%*%}{sparse.matrix}(x,y) 
#' @inheritParams x,y from matrix multiplication function
#' 


setMethod("%*%",
          signature(x= "sparse.matrix", y = "sparse.matrix"),
          function(x, y){
            
            # check dim
            if(x@dims[2] == y@dims[1]){
              a <- data.frame(i = x@i, j = x@j, x = x@x)
              b <- data.frame(i = y@i, j = y@j, x = y@x)
              unique_a <- unique(a$i)
              unique_b <- unique(b$j)
              c_index <- expand.grid(unique_a, unique_b)
              colnames(c_index) <- c("i", "j")
              i <- c()
              j <- c()
              xv <- c()
              for(ida in unique_a){
                j_i <- a$j[which(a$i == ida)]
                birow <- b$i %in% j_i
                c_j <- unique(b$j[birow])
                a_x <- a$x[(a$i == ida) & (a$j == j_i)]
                for(idb in c_j){
                  b_x <- b$x[(b$i == j_i) & (b$j ==  idb)]
                  c_x <- sum(a_x * b_x)
                  i <- c(i, ida)
                  j <- c(j, idb)
                  xv <- c(xv, c_x)
                }
              }
              c <- data.frame(i = i, j = j, x = xv)
              c <- c[order(c$j), ]
              rownames(c) <- (1:nrow(c))
              c <- sparse.matrix(i = c$i, j = c$j, x = c$x,
                                 dims = c(x@dims[1], y@dims[2]))
              return(c)
            }else{
              stop("Dimensions Error")
            }
          })



# args(getGeneric("t"))

## Another document sparse-matrix2.R can also passed the test 
## but it cannot be shown simultaneouly with this file. 
## Therefore, I will just show that the new S4 class works well.
## Actually, the new S3 class also works well.

