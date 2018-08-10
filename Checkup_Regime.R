#-
###############################################################################
# Load Systematic Investor Toolbox (SIT)
# https://systematicinvestor.wordpress.com/systematic-investor-toolbox/
###############################################################################
con = gzcon(url('http://www.systematicportfolio.com/sit.gz', 'rb'))
source(con)
close(con)



#-
######################
##     INPUTS       ##
######################
n_Regimes <- 2
startDateTimePOSIX <- 1
# (date-time expressed in seconds since 01-01-1970 00:00:00)



#-
######################
##   TRAINING SET   ##
######################

#*****************************************************************
# Generate data as in the post
#*****************************************************************
# Example data
bull1 = rnorm( 100, 0.10, 0.15 )
bear  = rnorm( 100, -0.01, 0.20 )
bull2 = rnorm( 100, 0.10, 0.15 )

# "TRUE" States
true.states = c(rep(1,100),rep(2,100),rep(1,100))
returns = c( bull1, bear,  bull2 )

# Find regimes
y=returns
ResFit = HMMFit(y, nStates=n_Regimes)
VitPath = viterbi(ResFit, y)

# Forward-backward procedure; compute probabilities
fb = forwardBackward(ResFit, y)

# Plot probabilities and implied states
layout(1:2)
# 1
plot(VitPath$states, type='s', main='Implied States', xlab='', ylab='State')
legend(x='topright', c('ImpliedState1','ImpliedState2'),  fill=1:2, bty='n')
# 2
matplot(fb$Gamma, type='l', main='Smoothed Probabilities', ylab='Probability')
legend(x='topright', c('ProbabilityState1','ProbabilityState2'),  fill=1:2, bty='n')



#-
######################
##   TESTING SET    ##
######################

#*****************************************************************
# Add some data and see if the model is able to identify the regimes
#*****************************************************************
# (More) Example data
bear2  = rnorm( 100, -0.01, 0.20 )
bull3 = rnorm( 100, 0.10, 0.10 )
bear3  = rnorm( 100, -0.01, 0.25 )

# Find regimes
y = c(bull1, bear,  bull2, bear2, bull3, bear3)  # y = returns (???)
VitPath = viterbi(ResFit, y)$states



#-
######################
##   PREDICT ...    ##
######################

#*****************************************************************
# Plot regimes
#*****************************************************************

# Data
data = xts(y, as.Date(startDateTimePOSIX:len(y)))

# Disable warnings
DisableWarnings()

# Helper function
RegimeColor <- function(color) {
  for (i in 1:length(color)) {
    if(color[i]==3) {
      color[i] <- 2
    } else if (color[i]==2) {
      color[i] <- 3
    } else {
      color[i] <- color[i]
    }
  }
  color
}

# Plot same + Regime prediction results
layout(1:3)
# 1
plota.control$col.x.highlight = col.add.alpha(RegimeColor(true.states+1), 150)
plota(data, type='h', plotX=F, x.highlight=T)
plota.legend('Returns + True Regimes')
# 2
plota.control$col.x.highlight = col.add.alpha(RegimeColor(true.states+1), 150)
plota(cumprod(1+data/100), type='l', plotX=F, x.highlight=T)
plota.legend('Equity + True Regimes')
# 3
plota.control$col.x.highlight = col.add.alpha(VitPath+1, 150)
plota(data, type='h', x.highlight=T)
plota.legend('Returns + Detected Regimes')   

# Re-enable warnings
EnableWarnings()



