setwd("~/Desktop/CSUF/Math 539 Statistical Consulting/Final Data/Cuts")
data = read.csv('2013-Zinc.csv')
#I'm just taking a subsection of the data to make it more manageable
#data = data[data$latitude > 33.5 & data$latitude < 34.0 & data$longitude >-119 & data$longitude < -118,]
library(ggplot2)
ggplot(data, aes(x=longitude, y=latitude)) +
  geom_point(aes(color=result), alpha=0.8) + scale_color_gradient(low = "black", high = "red")
###Select our Kernel...isotropic Gaussian seems fine...We're going to cross validate 10 separate times, Each with a disjoint partitioning of 10% of the data as test data.  
####STEP 1:  Partition Data into Training and Testing data....
n = dim(data)[1]
shuffle.index = sample(1:n,n,replace=F)
test.index = vector("list",10)
index.n = floor(n/10)
for(k in 1:9){
  test.index[[k]] = shuffle.index[(1+index.n*(k-1)):(index.n*k)]
}
test.index[[10]] = shuffle.index[(1+index.n*9):n]
minx = min(data$longitude)
maxx = max(data$longitude)
miny = min(data$latitude)
maxy = max(data$latitude)
kernel_regression = function(x, y, h){
  #get the values of the kernels
  densities = apply(cbind(data$longitude, data$latitude), 1, function(site){
    1/(2*pi*h)*exp(-((site[1]-x)^2+(site[2]-y)^2)/h)
  })
  value = sum((densities/sum(densities))*data$result)
  return(value)
}
parts = 100
x_seq = seq(minx, maxx, length.out=parts)
y_seq = seq(miny, maxy, length.out=parts)
regression = outer(x_seq, y_seq, Vectorize(kernel_regression), h=0.01)
image(x=x_seq, y=y_seq, z=regression,
      xlim=c(minx,maxx),ylim=c(miny,maxy),zlim = c(min(regression),max(regression)),
      col=heat.colors(100), xlab="", ylab="",
      main="Kernel Regression, Zinc")
points(data$longitude,data$latitude)