#' @importFrom graphics plot
#' @importFrom graphics lines
silverman.plot <-
function(x,kmin=1,kmax=5,alpha=0.05,adjust=FALSE){

    pvalues = 0
    if(kmax<kmin){
    print("kmax<kmin")
    }
    else{
      for (i in kmin:kmax){
        if(i==1 && adjust==TRUE)
          temp = silverman.test(x,k=i,adjust=TRUE)
        else
          temp=  silverman.test(x,k=i)
        pvalues[i]=temp@p_value
      }

      plot(pvalues,ylim=c(0,1),pch=20,main="Plot of p-values",xlab="Number of modes in null hypothesis of silvermantest",ylab="p-value")
      lines(pvalues,lty=3)

      lines(rep(alpha,kmax),col="red",lty=3)
    }

}

