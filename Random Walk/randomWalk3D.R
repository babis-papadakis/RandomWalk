library(plot3D)
steps = 20
exps = 200
disp = matrix (c(1,0,0,-1,0,0,0,1,0,0,-1,0,0,0,1,0,0,-1), 3, 6)
t_sum = c(0,0)
total_sd2 = c(0)
#create walks & plot
for (i in 1:exps){
  D = disp[,sample(6,steps, replace=TRUE)]
  D = cbind(c(0,0,0),D)
  R= apply(D,1,cumsum)
  if(i == 1)
    lines3D(R[,1], R[,2], R[,3], box=TRUE, pch =16, bty="b2",axes= TRUE, label= TRUE,
            colkey=FALSE,main="Random Walk 3D", xlab="x-position", ylab="y-position",
            zlab="z-position",xlim=c(-steps/2,steps/2), ylim=c(-steps/2,steps/2), 
            zlim=c(-steps/2,steps/2), phi= 40, theta = 40)
  else
    lines3D(R[,1], R[,2], R[,3], col =palette()[i%%8+1])
  t_sum = t_sum +  apply(R,2, sum)
  total_sd2 = total_sd2+ apply(R^2,1, sum)
}#for(i) end

#statistic 
#plot3D(matrix(t_sum/steps/exps,1,3), ylim=c(-2,2), xlim = c(0,2), main ="mean value", xlab= "x mean", ylab= "y mean")
plot(total_sd2/steps, main =" r^2 - time", xlab= "time", ylab= "r^2")