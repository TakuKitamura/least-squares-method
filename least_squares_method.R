
least_squares <- function(x,y,n){
  
  lim_minus = -1 * (max(abs(x),abs(y)) + 5)
  lim_plus = max(abs(x),abs(y)) + 5
  
  plot(x,y,xlim = c(8,16),ylim = c(5990,7160),pch=11,col=2,ann=F)
  
  par(new = T)
  
  #----------------------------------------------
  #(n+1)行 1行の0行列作成
  #解
  solution <- matrix(0,nrow = n + 1,ncol = 1)
  
  #----------------------------------------------
  #(n+1)行 1行の0行列作成
  #分子
  child <- matrix(0,nrow = n + 1,ncol = 1)
  
  for(i in 0:n){
    child[i + 1,1] <- sum((x^i)*y)
  }
  
  #----------------------------------------------
  
  #----------------------------------------------
  
  #(n+1)行 (n+1)行の0行列作成
  #分母
  
  mother <- matrix(0,nrow = n + 1,ncol = n + 1)
  
  k <- 0
  
  for(i in 0:n){
    for(j in 0:n){
      mother[i+1,j+1] <- sum(x^(j + k))
    }
    k <- k + 1
  }
  
  mother[1][1] <- n
  
  #----------------------------------------------
  
  #----------------------------------------------
  
  solution <- solve(mother,child)
  
  print(solution)
  
  #----------------------------------------------
  
  graph <- function(z){
    
    fun <- 0
    
    for(i in 0:n){
      fun <- fun + solution[i+1,1] * z^i
    }
    #n+1
    return (fun)
    
  }
  
  plot(graph,xlim = c(8,16),ylim = c(5990,7160),col = 1,lwd=5,ann=F)
  axis(side=1,tck=1.0,lty="dotted")
  axis(side=2,tck=1.0,lty="dotted",lwd=2)
  
}

x <- c(10  ,10.5,11  ,11.5,  12,12.5,13  ,13.5,14  ,14.5,15,15.5)
y <- c(5993,5956,6072,6064,6707,7156,7079,6855,6546,6457,6466,6161)

print(length(x))
print(length(y))

count <- 1

while(1){
  
  ret <- try(least_squares(x,y,count),silent=TRUE)
  if(class(ret)=="try-error"){
    count <- 0
    least_squares(x,y,count + 1)
  }
  #Sys.sleep(5)
  stop <- readline()
  count <- count + 1
}

