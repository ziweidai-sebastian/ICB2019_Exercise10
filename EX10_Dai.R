#Red line is mutant cells and black line is non-mutant cells
#When the drug is absent
rN = 0.1
rM = 0.1
Nt0 = 100 #initlal number of non-mutant cells when mutation happens
Mt0 = 0
timesteps = 500
K = 1000000

Nt = numeric(length=timesteps)
Nt[1] = Nt0
Mt = numeric(length=timesteps)
Mt[1] = Nt0

#Non-mutant cells simulation
for(t in 1:(timesteps-1)){
  Nt[t+1] <- Nt[t]+rN*Nt[t]*(1-(Nt[t]+Mt[t])/K)
}

#Mutant cells simulation
for(t in 1:(timesteps-1)){
  Mt[t+1] <- Mt[t]+rM*Mt[t]*(1-(Nt[t]+Mt[t])/K)
}

simdata <- data.frame(time = 1:timesteps, N = Nt, M = Mt)
ggplot(data = simdata)+
  geom_line(aes(x = time, y = N,), col = 'black')+
  geom_line(aes(x = time, y = M), col = 'red')+
  labs(
    x = "time",
    y = "Number of cells"
  )+theme_classic()

#When the drug is present
#Non-mutant cells simulation
rN_drug = -0.1
for(t in 1:(timesteps-1)){
  Nt[t+1] <- Nt[t]+rN_drug*Nt[t]*(1-(Nt[t]+Mt[t])/K)
}

#Mutant cells simulation
for(t in 1:(timesteps-1)){
  Mt[t+1] <- 0.5*(Mt[t]+rM*Mt[t]*(1-(Nt[t]+Mt[t])/K))
}
simdata <- data.frame(time = 1:timesteps, N = Nt, M = Mt)
ggplot(data = simdata)+
  geom_line(aes(x = time, y = N), col = 'black')+
  geom_line(aes(x = time, y = M), col = 'red')+
  labs(
    x = "time",
    y = "Number of cells"
  )+theme_classic()
