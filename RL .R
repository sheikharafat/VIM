L1 =0.2848; L2 =2.2987; n = 6; alpha = .0027; del=1#shift
sg2 = del*0.009369711^2; lcl = L1*0.009369711^2; ucl = L2*0.009369711^2; 
p = rl =c()

for (j in 1:10000) {
  for (i in 1:10000) {
    r = 1/sqrt(rgamma(n, 1.5, scale = 2*sg2))#Inverse Maxwell Random Number  
    p[i] = sum(r^-2)/(3*n)
    if (lcl > p[i] | ucl < p[i]) 
    {
      rl[j] = i
      print(cbind(length(rl),rl[j],mean(rl), sqrt(var(rl)),median(rl)))
      break
    }
    else
    {
      rl[j] = 100000
    }
    
  }
  
}

mean(rl)# ARL
sqrt(var(rl))
median(rl)
quantile(rl, probs = c(10, 25, 50, 75, 90)/100)
plot(sort(rl), cumsum(rl)/sum(rl), type = "l",
     xlab = "Run Length (RL)",
     ylab = "CDF of RL",
     col = "blue")

