#tiff("Figure.tiff", units="in", width=4.5, height=4.5, res=300)
set.seed(558)

t=1:14
n=7

W1=0
W2=2.1367

x<-c(22.2,23,24,28.6,21.8,17,26,23.2,18.9,21.9,27.3,13.8,24,20.1,15.7,26.8,27.9,15.3,28.8,16,23.6,53.8,21.7,28.8,17,16.5,15.7,28,13.3,16.5,24.2,17.6,27.8,18.3,17.7,20,13.2,16.9,14.9,15.5,7,15.8,15,38.3,11.2,38.2,26.7,17.1,29,18.3,18.4,18.2,15.9,16.4,23.6,19.2,23.3,20.4,20.9,28.5,23.2,17.9,46.1,39.3,11.8,17.7,30.9,22.4,45,18.2,30.2,21.8,18.2,23,27.2,10.9,25.5,12.4,39.9,17.7,26.3,14.1,21,11.2,10.8,25.7,32.4,13.6,19.1,16.1,53.3,57.3,36.5,19.7,20.8,30.8,20,39.6)


r<- matrix(x, nrow = 7, ncol=14)
s=c()
for(i in 1:14){
  r[,i]<-sample(x,7,replace = T)
  s[i]<-((3*7)^-1)*sum(r[,i]^-2)
}


#VIM control limits

UCL=W2*mean(s)
CL=mean(s)
LCL=W1*mean(s)


#VIM control chart
par(mar=c(4,4,1.75,1.75))
plot((s),type ="b",ylim=c(LCL-.091328e-09,UCL+.251328e-09),xlim=c(1,14),ylab = expression('V'[IM]),xlab = "Sample number",col="brown4",pch=16,yaxt="n")

ytick<-c(round(LCL,11),round(CL,11),round(UCL,11))
axis(side=2, at=ytick, labels = FALSE)
text(par("usr")[1], ytick,  
     labels = ytick, srt = 45, pos = 2, xpd = TRUE)


abline(h =mean(s) , col = "red", lty = 2, lwd = 2)
abline(h=LCL,col="green",lwd=2,lty=2)
abline(h=UCL,col="blue",lwd=2,lty=2)


legend(2,UCL+.291328e-09,legend =expression('V'[IM]) ,pch = 16,bty = "n",cex = 0.9,col ="brown4")
legend(4,UCL+.291328e-09,
       c(expression('LCL'),
         expression('UCL'),
         expression('CL')),
       col = c("green","blue","red"),
       lwd = c(2,2,2),lty = c(2,2,2), bty = "n",horiz = T,cex = 0.9)

#dev.off()
