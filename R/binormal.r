bivarnorm <- function(r=0){
   x <- seq(-3,3,length=50)
   y <- x
   z <- matrix(0, ncol=length(x), nrow=length(y))

   for(i in 1:length(x)){
      for(j in 1:length(y)){
         z[i,j] <- exp(-(x[i]^2 -2*r*x[i]*y[j] + y[j]^2)/(2*(1-r^2)))
      }
   }
   z <- z/(2*pi*sqrt(1-r^2))
   list(x=x,y=y,z=z)
}