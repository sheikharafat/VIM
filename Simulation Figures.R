tiff("Figure5MDPI.tiff", units="in", width=5.25, height=3, res=300)
par(mar=c(4,4,0.5,0.5))
set.seed(111)
###Figure 5
###VIM chart for IMD using probability limits (In control and Out of Control situation)
##Create a function to generate IMD random number
rIMD=
  function (N, sigma){
    rand.samples = rep(NA,N)
    for(i in 1:N){
      rand.samples[i] = 1/(sigma*sqrt(2*rgamma(1,3/2,1)))
    }
    rand.samples
  }
s<-as.numeric(24)
n.columns<-24
N=6
x<- matrix(, nrow = 6, ncol = n.columns)
for(i in 1:n.columns){
  x[,i] <- rIMD(6,100)
  s[i]<-((3*N)^-1)*sum(x[,i]^-2)
}

###n=6 and alpha=0.0027
L1=0.2848	
L2=2.2987
CL<-100^2
LCL<-L1*CL
UCL<-L2*CL
par(mar=c(4,4,1.75,0.75))
plot(s,type ="b", ylim=c(0,27500),xlim=c(1,24),ylab = "Plotting Statistic",xlab = "Sample number",col="springgreen3",pch=15,cex.axis=0.85,xaxt="n",yaxt="n",cex=0.80)
abline(h=LCL,col="blue",lwd=2,lty=2)
abline(h=UCL,col="green",lwd=2,lty=3)
abline(h=CL,col = "red", lty = 1, lwd = 1)
s1<-c(s[1:18],1.85*s[19:24])
lines(s1,type ="b",col="red",pch=16,cex=0.75)
xtick<-c(2,4,6,8,10,12,14,16,18,20,22,24)
ytick<-c(0,5000,10000,15000,20000,25000)
axis(side=1, at=xtick, labels = TRUE,cex.axis=0.85)
axis(side=2, at=ytick, labels = TRUE,cex.axis=0.80,tick = TRUE)
legend(1,28500,legend = expression(paste('V'[IM][~(In~Control)])),pch = 15,bty = "n",cex = 0.75,col ="springgreen3")
legend(5.5,28500,legend = expression(paste('V'[IM][~(Out~of~Control)])),pch = 16,bty = "n",cex = 0.75,col ="red")
legend(11,28500,
       c(expression('LPL'),
         expression('UPL'),
         expression('CL')),
       col = c("blue","green","red"),
       lwd = c(2,2,1),lty = c(2,3,1), bty = "n",horiz = T,cex = 0.75)
dev.off()  
###################################################################################
##################################################################################

###Figure 6
###VIM chart for IMD using L sigma limits (In control and Out of Control situation)
tiff("Figure6MDPI.tiff", units="in", width=5.25, height=3, res=300)
par(mar=c(4,4,0.5,0.5))
W1=0.0518	
W2=1.9483
CL1<-100^2
LCL1<-W1*CL1
UCL1<-W2*CL1
par(mar=c(4,4,1.75,0.75))
plot(s,type ="b", ylim=c(0,26000),xlim=c(1,24),ylab = "Plotting Statistic",xlab = "Sample number",col="springgreen3",pch=15,cex.axis=0.85,xaxt="n",cex=0.80)
abline(h=LCL1,col="blue",lwd=2,lty=2)
abline(h=UCL1,col="green",lwd=2,lty=3)
abline(h=CL1,col = "red", lty = 1, lwd = 1)
lines(s1,type ="b",col="red",pch=16,cex=0.75)
xtick<-c(2,4,6,8,10,12,14,16,18,20,22,24)
axis(side=1, at=xtick, labels = TRUE,cex.axis=0.85)

legend(1,27500,legend = expression(paste('V'[IM][~(In~Control)])),pch = 15,bty = "n",cex = 0.75,col ="springgreen3")
legend(5.5,27500,legend = expression(paste('V'[IM][~(Out~of~Control)])),pch = 16,bty = "n",cex = 0.75,col ="red")
legend(11,27500,
       c(expression('LCL'),
         expression('UCL'),
         expression('CL')),
       col = c("blue","green","red"),
       lwd = c(2,2,1),lty = c(2,3,1), bty = "n",horiz = T,cex = 0.75)
dev.off()  

