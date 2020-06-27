
#download MCMCpack 
download.packages('MCMCpack', destdir = "~/Desktop/")

MCMCregress(formula, data, burnin=1000, mcmc=10000,
            thin=1, verbose=0, seed=NA, beta.start=NA,
            b0=0, B0=0, c0=0.001, d0=0.001,
            sigma.mu=NA, sigma.var=NA,
            marginal.likelihood=c("none", "Laplace", "Chib95"))

#MCMCregress(Y1~X1, data=dat)
#MCMCregress(formula, data)

#Rでモンテカルロシミュレーションの実施
S=100
K=100
r=5/100
q=0.0
sigma=0.3
T=1

Call_MC<-function(sample)
{
  Call<-0
  for(i in 1:sample)
  {
    Call<-Call+max(S*exp((r-q-sigma^2/2)*T+sigma*sqrt(T)*rnorm(1))-K,0)
  }
  Call<-exp(-r*T)*Call/sample
    return(Call)
}

Call_MC(100000)


Call_MC2<-function(sample)
{
  x<-rnorm(sample)
  y<-S*exp((r-q-sigma^2/2)*T+sigma*sqrt(T)*x)-K
  Call<-exp(-r*T)*sum(y[y>0])/sample #y(Call価値)の正の部分のみを足す
  return(Call)
}

Call_MC2(100000)

#BS model; price=S0, Volatility=sigma, risk free rate = r, dividen=q, maturity=T, option price=K

BS_call = function(S, K, r, q, sigma, T)
{
  d1 = ( log(S/K) + (r - q + sigma^2/2)*T)/( sigma* sqrt(T))
  d2 = ( log(S/K) + (r - q - sigma^2/2)*T)/( sigma* sqrt(T))
  C0 = exp(-q*T)*S * pnorm(d1) - exp(-r*T)*K*pnorm(d2)
  return(C0)
}

S = 100
K = 100
T = 1
r = 5 / 100
q = 0 / 100
sigma = 30 / 100

BS_call(S, K, r, q, sigma, T)



S_series = seq(0,200,5)


IntrinsicValue=array(1:length(S_series))
for(i in 1:length(S_series)){
  IntrinsicValue[i]=max(S_series[i]-K,0)
}

BS=BS_call(S_series, K, r, q, sigma, T)

plot(BS,type="l",col="red",ylim=c(0,100),ann=F)
par(new=T) 

plot(IntrinsicValue,type="l",ylim=c(0,100),ann=F)


#CRR

S0   <- 100                  #現在の原資産価格
K    <- 100                  #権利行使価格
r    <- 0.05                 #無リスク金利
q    <- 0.00                 #配当率
sigma<- 0.3                  #ボラティリティ
T    <- 1.0                  #満期

binomial<-function(Step){
  Delta.t =T/Step                    #1Step当たりの期間
  u    <- exp(sigma*sqrt(Delta.t))       #上昇率
  d    <- exp(-sigma*sqrt(Delta.t))      #下落率
  p    <-(exp((r-q)*Delta.t)-d)/(u-d)    #リスク中立確率
  
  myMax=function(x)
  {
    myMax=max(x,0)
    return(myMax)
  }
  j=0:Step
  C0=sum(as.numeric(lapply(u^j*d^(Step-j)*S0-K,myMax))*
           choose(Step,j)*p^j*(1-p)^(Step-j))/(1+r)^T
  return( C0 )
}


binomial(1000)

BS_call <- function(S, K, r, q, sigma, T)
{
  d1 <- ( log(S/K) + (r - q + sigma^2/2)*T)/( sigma* sqrt(T))
  d2 <- ( log(S/K) + (r - q - sigma^2/2)*T)/( sigma* sqrt(T))
  C0 <- exp(-q*T)*S * pnorm(d1) - exp(-r*T)*K*pnorm(d2)
  return(C0)
}

(BS<-BS_call(S0,K,r,q,sigma,T))

plot(1:100,binomial_series,type="l",ylim=c(13,17),ann=F)
par(new=T)
plot(1:100,rep(BS,100),col="red",type="l",ylim=c(13,17),ann=F)

binomial(100)-BS
#[1] -0.01223184




