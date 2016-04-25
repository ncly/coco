## PROGRAM TO CREATE TWO (2) SEQUENCES OF RANDOM NUMBERS WITH A PREDETERMINED LENGTH (n) AND CORRELATION (rho) VIA THE CHOLESKY DECOMPOSITION ##


##

# "Same" random numbers
set.seed(1987)
# Check? Use: rnorm(10)

##

#Cholesky
# Time
T <- 5 # Years maturity

# Number of paths (for interest rates and assets)
npath <- 10

rho <- -0.2 # Correlation between interest rate and ln(x)-return

#CIR
kappa <- 0.114 # Mean reversion parameter for the interest rate
r.bar <- 0.069 # Equilibrium level for the interest rate
r0 <- 0.035 # Interest rate @ t=0 => same as r[1]
sigma_r <- 0.07 # Standard deviation for the interest rate

#Jumps
mu_Y <- -0.01 # Mean for jump size
sigma_Y <- 0.02 # Standard deviation for jump size
lambda <- c(1,0,1) # Frequency (1 = once per year)

#x
g <- c(0.5,0.5,0.25) # Mean reversion parameter for capital ratio target
x.hat <- 1.1 # Capital ratio target
b0 <- 0.04 # CCB-to-deposit ratio, B/D
p <- 1 # Conversion parameter: par=1, premium>1, discount<1
e.bar <- 0.02 # equity-to-deposit ratio
sigma_x <- 0.02 # Standard deviation for assets
#x0 <- 1.075 # Starting capital-to-deposit ratio
x0.low <- 1 + 0.065
x0.high <- 1 + 0.15
x0.nint <- 10 # number of intervals between x0.low and x0.high
B <- 1 # Principal
#c <- 0.05 # Coupon
c.low <- 0.04
c.high <- 0.058
c.nint <- 10 # number of intervals between c.low and c.high
c.fit.matrix <- matrix(0,x0.nint,length(lambda))

#PENN.fn <- function(T, npath, rho, r0, r.bar, sigma_r, kappa, mu_Y, sigma_Y, lambda, g, x.hat, b0, p, e.bar, sigma_x, x0.low, x0.high, x0.nint, B, c.low, c.high, c.nint)
#{
##############
n <- T*250 # 250 days per year
dt <- T/n # Time-step

## Cholesky START ##
# Building correlation-matrix, RHO
vect <- c(1,rho,rho,1)
RHO <- matrix(vect,nrow=2) # Correlation-matrix

# Cholesky-decomposition of RHO
chol.RHO <- t(chol(RHO))

# Two UNcorrelated BMs
dW_1 <- matrix(1,n,npath) # Making nxnpath-matrix
dW_2 <- matrix(1,n,npath) # Same
for(j in 1:npath) # use 'j' when npath
{
  dW_1[,j] <- rnorm(n)*sqrt(dt)
  dW_2[,j] <- rnorm(n)*sqrt(dt)
}

# New correlated process (using Cholesky-decomposition)
dW_2corr <- matrix(1,n,npath) # Making nxnpath-matrix
for(j in 1:npath)
{
  for(i in 1:n)
  {
    dW_2corr[i,j] <- dW_1[i,j]*chol.RHO[2,1]+dW_2[i,j]*chol.RHO[2,2]
  }
}

# Check/Output
# cor.vector <- 1:npath
# for(j in 1:npath)
#{
#    print(cor(dW_1[,j],dW_2corr[,j]))
#    cor.vector[j] <- cor(dW_1[,j],dW_2corr[,j])
#}
#matrix(c("Number of sequences", "Random numbers per sequence", "Number of paths", "Mean correlation", 2, n, npath, mean(cor.vector)), nrow=4)
## Cholesky END ##

##################

## PROGRAM TO SIMULATE A CIR-INTEREST RATE WITH A PREDETERMINED CORRELATION TO ASSETS ##

## RUN BEFOREHAND:
# PENN.CHOL

## Interest rate START ##
# The change in the default-free interest rate from day t to day t+dt (p. 17)

# Parametres for interest rate modeling (p. 18)
r <- matrix(r0,n+1,npath) # Making r a "(n+1)xnpath"-matrix, and effectively assigning r0 as first entry in every column

# Stochastic interest rate, CIR-process
for(j in 1:npath)
{
  for(i in 1:n)
  {
    r[i+1,j] <- r[i,j] + kappa*(r.bar-r[i,j])*dt+sigma_r*sqrt(r[i,j])*dW_2corr[i,j]
  }
}

## Interest rate END ##

##################

for(w in 1:length(lambda))
{
  ## PROGRAM TO SIMULATE A JUMPS IN ASSETS VALUE WITH A PREDETERMINED INTENSITY (lambda) AND SIZE (mu_Y) ##
  
  ## RUN BEFOREHAND:
  # PENN.CHOL(esky)
  # PENN.CIR.RATE
  
  ## Jumps START ##
  # Jump-process (p.17)
  
  # Parametres for jump modeling (p. 18)
  phi <- matrix(rbinom(n%*%npath,1,dt*lambda[w]),n,npath) # Jump or not? Binomial variable
  ln.Y <- matrix(rnorm(n%*%npath,mu_Y,sigma_Y),n,npath) # (Actual) jump size
  ## Jumps END ##
  
  ##################
  
  ##################
  
  ## PROGRAM TO SIMULATE THE DAILY RISK NEUTRAL PROCESS FOR THE LOG OF THE BANK'S ASSET-TO-DEPOSIT RATIO (p. 17) ##
  
  ## RUN BEFOREHAND:
  # PENN.CHOLESKY
  # PENN.CIR.RATE
  # PENN.JUMPS
  
  ## ln(x) START ##
  # Daily risk-neutral process for the log of the bank's asset-to-deposit ratio (p. 17)
  
  # Parametres for ln(x) modeling (p. 18)
  b <- matrix(b0,n+1,npath) # Making b a (n+1)xnpath matrix and effectively assigning b0 as the first entry of b (of all entries actually)
  x.bar0 <- 1+e.bar+p*b0 # Total capital-to-deposit ratio (conversion threshold)
  x.bar <- matrix(x.bar0,n+1,j)
  
  h <- matrix(1,n,npath) # Making h a nxnpath-matrix
  
  # For now:
  k <- exp(mu_Y+0.5*sigma_Y^2)-1 # k = E[Y-1], p. 14 (???)
  
  # Let the games begin
  # First: The regression part
  c <- seq(c.low, c.high, length=c.nint)
  x0 <- seq(x0.low,x0.high,length=x0.nint)
  reg.matrix <- matrix(0,c.nint, length(x0))
  
  reg.fit1 <- 1:x0.nint
  reg.fit2 <- 1:x0.nint
  c.fit <- 1:x0.nint
  
  for(l in 1:x0.nint) # Use l for x0
  {
    for(m in 1:c.nint) # Use m for c
    {
      x <- matrix(x0[l],n+1,npath) # Making x a nxnpath-matrix, and assigning x0 as the value of all entries in x (thus effectively making x0 the first value of x)
      ln.x0 <- matrix(log(x0[l]),n+1,npath) # Same
      ln.x <- ln.x0 # Same
      binom.c <- matrix(1,n+1,npath) # Making binom.c a nxnpath-matrix
      
      # Calculating: d_1, d_2, h, b, ln.x, x, binom.c
      for(j in 1:npath)
      {
        for(i in 1:n)
        {
          # d_1 & d_2
          d_1 <- (ln.x[i,j]+mu_Y)/sigma_Y
          d_2 <- d_1+sigma_Y
          
          # h
          h[i,j] <- lambda[w]*(pnorm(-d_1)-exp(ln.x[i,j])*exp(mu_Y+0.5*sigma_Y^2)*pnorm(-d_2))
          
          # b
          b[i+1,j] <- b[i,j]*exp(-g[w]*(exp(ln.x[i,j])-x.hat)*dt)
          
          # ln.x
          ln.x[i+1,j] <- ln.x[i,j] + ( (r[i,j]-lambda[w]*k) - (r[i,j]+h[i,j]+c[m]*b[i,j])/exp(ln.x[i,j]) - g[w]*(exp(ln.x[i,j])-x.hat) - 0.5*sigma_x^2 )*dt + sigma_x*dW_1[i,j] + ln.Y[i,j]*phi[i,j]
          
          # x
          x[i+1,j] <- exp(ln.x[i+1,j])
          
          # x.bar
          x.bar[i+1,j] <- 1+e.bar+p*b[i+1,j]
          
          # binom.c
          # Creating vector with 1 if payment (i.e. trigger point not reached) and 0 if no payment (i.e. trigger point reached)
          if(x[i+1,j]>=x.bar[i+1,j] && binom.c[i,j]>0.5)
          {
            binom.c[i+1,j] <- 1
          }else
          {
            binom.c[i+1,j] <- 0
          }
        }
      }
      
      # Creates vector(s) with payments
      # Finds the lump sum (lump.c) that will be paid in the event of trigger/no trigger
      payments <- matrix(c(rep(c[m]*dt, n-1),B), n, npath)*binom.c[1:n,] # OBS: dt multiplied here!!
      for(j in 1:npath)
      {
        for(i in 2:n)
        {
          if(payments[i,j] == 0 && p*b[sum(binom.c[,j])+1,j] <= x[sum(binom.c[,j])+1,j]-1 )
          {
            payments[i,j] <- p*B
            break
          }else if(payments[i,j] == 0 && 0 < x[sum(binom.c[,j])+1,j]-1 && x[sum(binom.c[,j])+1,j]-1 < p*b[sum(binom.c[,j])+1,j])
          {
            payments[i,j] <- (x[sum(binom.c[,j])+1,j]-1)*B/b[sum(binom.c[,j])+1,j]
            break
          }
          else
          {
            payments[i,j] <- payments[i,j]
          }
        }
      }
      
      # Plotting 1+x
      #plot(1:(n+1), x[,1], type="l", ylim=c(min(x),max(x)), ylab="1+x", #xlab="t")
      #for(j in 2:npath) lines(1:(n+1), x[,j], col=j)
      #legend("topleft",expression(ln(x_t)),lty=1,col="black")
      ## ln(x) END ##
      
      ##################
      
      ## PROGRAM TO VALUATE THE CONTINGENT CAPITAL FOR A GIVEN FIXED-COUPON RATE (p. 15) ##
      
      ## RUN BEFOREHAND:
      # PENN.CHOLESKY
      # PENN.CIR.RATE
      # PENN.JUMPS
      # PENN.LNX
      
      ## Fixed-coupon START ##
      
      # S exp(-S r ds) * v(t) dt
      vec.disc.v <- rep(0, npath) #disc = discounting
      for(j in 1:npath)
      {
        disc.v <- 0
        int.r <- 0
        
        for(i in 1:n)
        {
          int.r <- int.r + r[i,j]*dt
          disc.v <- disc.v + exp(-int.r)*payments[i,j]
          
        }
        
        vec.disc.v[j] <- disc.v
      }
      
      V0 <- mean(vec.disc.v)
      
      reg.matrix[m,l] <- V0
      ## Fixed-coupon END ##
      
    }
    
    reg.fit1[l] <- lm(reg.matrix[,l]~1+c)$coef[1]
    reg.fit2[l] <- lm(reg.matrix[,l]~1+c)$coef[2]
    
    c.fit[l] <- (B-reg.fit1[l])/reg.fit2[l]
  }
  c.fit.matrix[,w] <- c.fit
}
