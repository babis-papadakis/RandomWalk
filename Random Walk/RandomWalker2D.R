steps = 20
exps = 1000
disp = matrix (c(1,0, -1,0, 0,1, 0,-1), 2, 4)
t_sum = c(0,0)
total_sd2 = c(0)
#create walks & plot
for (i in 1:exps){
  D = disp[,sample(4,steps, replace=TRUE)]
  D = cbind(c(0,0),D)
  xy= apply(D,1,cumsum)
  if(i == 1)
    plot(xy, type="l", main="Random Walk 2D", xlab="time", ylab="R",
         xlim=c(-steps/2,steps/2), ylim=c(-steps/2,steps/2))
  else
    points(xy, type="l", col =palette()[i%%8+1])
  t_sum = t_sum +  apply(xy,2, sum)
  total_sd2 = total_sd2+ apply(xy^2,1, sum)
}#for(i) end

#statistic 
plot(matrix(t_sum/steps/exps,2,1), ylim=c(-2,2), xlim = c(0,2), main ="mean value", xlab= "x mean", ylab= "y mean")
plot(total_sd2/steps, main =" r^2 - time", xlab= "time", ylab= "r^2")