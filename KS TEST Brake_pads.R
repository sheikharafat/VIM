x<-c(22.2,23,24,28.6,21.8,17,26,23.2,18.9,21.9,27.3,13.8,24,20.1,15.7,26.8,27.9,15.3,28.8,16,23.6,53.8,21.7,28.8,17,16.5,15.7,28,13.3,16.5,24.2,17.6,27.8,18.3,17.7,20,13.2,16.9,14.9,15.5,7,15.8,15,38.3,11.2,38.2,26.7,17.1,29,18.3,18.4,18.2,15.9,16.4,23.6,19.2,23.3,20.4,20.9,28.5,23.2,17.9,46.1,39.3,11.8,17.7,30.9,22.4,45,18.2,30.2,21.8,18.2,23,27.2,10.9,25.5,12.4,39.9,17.7,26.3,14.1,21,11.2,10.8,25.7,32.4,13.6,19.1,16.1,53.3,57.3,36.5,19.7,20.8,30.8,20,39.6)



t=0
n1=length(x)
y<-as.numeric(n1)
j=1
for(i in 1:n1)
{
  if(x[i]>t) 
  {
    y[j]=x[i]
    j=j+1
  }
}
y
r=sort(y)
n=length(r)
a=(3*n)^-1
b=sum(1/r^2)
sigma=sqrt(a*b)
library(zipfR)
F1=(2/sqrt(pi))*(1-Igamma(3/2,1/(2*r^2*sigma^2)))
F2=(2/sqrt(pi))*(1-Igamma(3/2,1/(2*t^2*sigma^2)))
F=(F1-F2)/(1-F2)
u1=rep(0:(n-1))
u2=rep(1:n)
p1=u1/n
p2=u2/n
D1=F-p1
D2=F-p2
D3=abs(D1)
D4=abs(D2)
D<-as.numeric(n)
for(i in 1:n)
{
  if(D3[i]>D4[i])
    D[i]=D3[i]
  else
    D[i]=D4[i]
}

D
max(D)



#1% level of significance critical value is 
1.22/sqrt(n)

#5% level of significance critical value is 
1.36/sqrt(n)

#10% level of significance critical value is
1.63/sqrt(n)



CDF=(2/sqrt(pi))*(1-Igamma(3/2,1/(2*sort(x)^2*sigma^2)))
PDF=sqrt(2/pi)*(sigma)^-3*sort(x)^-4*exp(-(1/(2*sort(x)^2*sigma^2)))

plot(CDF,type="l")
plot(PDF,type="l")