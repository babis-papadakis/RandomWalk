s= 0
steps = 200
exps = 200
walks = c()
plot(rep(0,steps+1), type = "l", ylim =c(-steps,steps),xlab="time", ylab = "Xn", main = "Random Walk")
#create walks & plot
for (i in 1:exps){
  D = sample(c(-1,1), steps, replace=TRUE)
  X= cumsum(D)
  lines(X, col =palette()[i%%8+1])
  walks = c(walks, X) #define walks matrix
  #lines(X, col= rgb(i/20,(i%%8)/8,0.5)) #pws ginetai to pallet
}
#statistic 
walks_mat2 = matrix(walks, steps , exps)
plot(apply(walks_mat2, 1 , mean), ylim =c(-1,1),
     ylab="mean", xlab= "time", main="Mean Value") #Mean
plot(apply(walks_mat2, 1 , sd)^2, ylim =c(0,steps+10), xlim=c(0,steps+10), 
     ylab="Standard Deviation", xlab= "time", main="Standard deviation") #STandard Deviation