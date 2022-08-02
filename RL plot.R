L1 =0.0099; L2 =5.0294; n =1; alpha = .0027; del=1#shift
sg2 = del*0.009369711^2; lcl = L1*0.009369711^2; ucl = L2*0.009369711^2; 
p = rl1 =c()

for (j in 1:10000) {
  for (i in 1:10000) {
    r = 1/sqrt(rgamma(n, 1.5, scale = 2*sg2))#Inverse Maxwell Random Number  
    p[i] = sum(r^-2)/(3*n)
    if (lcl > p[i] | ucl < p[i]) 
    {
      rl1[j] = i
      print(cbind(length(rl1),rl1[j],mean(rl1), sqrt(var(rl1)),median(rl1)))
      break
    }
    else
    {
      rl1[j] = 100000
    }
    
  }
  
}

mean(rl1)# ARL
sqrt(var(rl1))
median(rl1)
quantile(rl1, probs = c(10, 25, 50, 75, 90)/100)


L1 =0.1380; L2 =3.0101; n =3; alpha = .0027; del=1#shift
sg2 = del*0.009369711^2; lcl = L1*0.009369711^2; ucl = L2*0.009369711^2; 
p = rl2 =c()

for (j in 1:10000) {
  for (i in 1:10000) {
    r = 1/sqrt(rgamma(n, 1.5, scale = 2*sg2))#Inverse Maxwell Random Number  
    p[i] = sum(r^-2)/(3*n)
    if (lcl > p[i] | ucl < p[i]) 
    {
      rl2[j] = i
      print(cbind(length(rl2),rl2[j],mean(rl2), sqrt(var(rl2)),median(rl2)))
      break
    }
    else
    {
      rl2[j] = 100000
    }
    
  }
  
}

mean(rl2)# ARL
sqrt(var(rl2))
median(rl2)
quantile(rl2, probs = c(10, 25, 50, 75, 90)/100)


L1 =0.2848; L2 =2.2987; n =6; alpha = .0027; del=1#shift
sg2 = del*0.009369711^2; lcl = L1*0.009369711^2; ucl = L2*0.009369711^2; 
p = rl3 =c()

for (j in 1:10000) {
  for (i in 1:10000) {
    r = 1/sqrt(rgamma(n, 1.5, scale = 2*sg2))#Inverse Maxwell Random Number  
    p[i] = sum(r^-2)/(3*n)
    if (lcl > p[i] | ucl < p[i]) 
    {
      rl3[j] = i
      print(cbind(length(rl3),rl3[j],mean(rl3), sqrt(var(rl3)),median(rl3)))
      break
    }
    else
    {
      rl3[j] = 100000
    }
    
  }
  
}

mean(rl3)# ARL
sqrt(var(rl3))
median(rl3)
quantile(rl3, probs = c(10, 25, 50, 75, 90)/100)



L1 =0.3984; L2 =1.9538; n =10; alpha = .0027; del=1#shift
sg2 = del*0.009369711^2; lcl = L1*0.009369711^2; ucl = L2*0.009369711^2; 
p = rl4 =c()

for (j in 1:10000) {
  for (i in 1:10000) {
    r = 1/sqrt(rgamma(n, 1.5, scale = 2*sg2))#Inverse Maxwell Random Number  
    p[i] = sum(r^-2)/(3*n)
    if (lcl > p[i] | ucl < p[i]) 
    {
      rl4[j] = i
      print(cbind(length(rl4),rl4[j],mean(rl4), sqrt(var(rl4)),median(rl4)))
      break
    }
    else
    {
      rl4[j] = 100000
    }
    
  }
  
}

mean(rl4)# ARL
sqrt(var(rl4))
median(rl4)
quantile(rl4, probs = c(10, 25, 50, 75, 90)/100)




plot(sort(rl1), cumsum(rl1)/sum(rl1), type = "l",
     xlab = "Run Length (RL)",
     ylab = "CDF of RL when d=1",
     col = "blue")
lines(sort(rl2), cumsum(rl2)/sum(rl2), type = "l",col="red")
lines(sort(rl3), cumsum(rl3)/sum(rl3), type = "l",col="green")
lines(sort(rl4), cumsum(rl4)/sum(rl4), type = "l",col="pink")
 legend("bottomright", c(expression("n=1"),
               expression("n=3"),
              expression("n=6"),
              expression("n=10")),
       col=c("blue","red","green", "pink"),
       lwd=c(1,2,1,2),lty=c(1,4,1,4),
       bty = "n", cex = 1)