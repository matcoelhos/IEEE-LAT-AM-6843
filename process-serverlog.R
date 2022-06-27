#Replace this with the path of your server log
setwd("./server")

data <- read.table('serverlog.txt', header = F)
data <- data - data$V1[1]

I1 <- (data$V2-data$V1)*1000
I2 <- (data$V3-data$V2)*1000

L <- I1 + I2

boxplot(I1,I2, outline = F)

p0 <- 1

iv <- vector(mode = 'numeric')
siv <- vector(mode = 'numeric')
tv <- vector(mode = 'numeric')

#moving average
while(p0 < length(L)-1000)
{
  iv <- c(iv,mean(L[p0:p0+1000]))
  siv <- c(siv,sd(L[p0:p0+1000]))
  tv <- c(tv,data$V1[p0])
  p0 <- p0 + 100
}

mv <- vector(mode = 'numeric', length = length(tv)) + mean(L)

#Plotting

par(mar = c(5,4,4,2)+0.5)
plot(tv[iv < 390],iv[iv < 390], col = 'dark gray', ylim = c(0,400), xlab = '', ylab = '', main = 'Server Time Constraint Experiment\nPatients Overload',
     cex.main = 1.8, cex.axis = 1.8)
par(new = T)
plot(tv[iv < 390],mv[iv < 390], col = 'red', type = 'l', lwd = 2, ylim = c(0,400), xlab = 'Elapsed time (s)', ylab = 'Average Task time (ms)',
     cex.lab = 1.8, cex.axis = 1.8)

plot(density(iv[iv < 390]),xlab = 'Average Task time (ms)', main = 'Density Plot\nPatients Overload',
     cex.main=1.8,cex.lab = 1.8, cex.axis = 1.8)
