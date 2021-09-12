###############################################################################
##                           HW 2 Answer Sheet         			    		         ##
##            	        Name :______   NTU ID:________  	    	     	    	 ##
###############################################################################

##############
# Question 1 #
##############
#(a)# 

#(b)# 


##############
# Question 2 #
##############
#(a)# 

#(b)# 

#(c)#

#(d)# 

#(e)#
rm(list=ls(all=T))
data(mtcars)
d <- mtcars

x_1 = d$wt
x_2 = d$hp
x_3 = d$qsec
x_4 = d$vs
y = d$drat
#epsilon = rnorm(length(x_1), mean=0, sd=5)
unconstrained.X=cbind(1,x_1,x_2,x_3,x_4)
constrained.X = cbind(1,x_3,x_4)
unconstrained.beta_hat <- solve(t(unconstrained.X)%*%unconstrained.X)%*%t(unconstrained.X)%*%y
constrained.beta_hat <- solve(t(constrained.X)%*%constrained.X)%*%t(constrained.X)%*%y

constrained_y.pred = constrained.beta_hat[1] + constrained.beta_hat[2]*x_3 + constrained.beta_hat[3]*x_4
unconstrained_y.pred = unconstrained.beta_hat[1] + unconstrained.beta_hat[2]*x_1 + unconstrained.beta_hat[3]*x_2 + unconstrained.beta_hat[4]*x_3 + unconstrained.beta_hat[5]*x_4

unconstrained_model = lm(y~x_1+x_2+x_3+x_4)
constrained_model = lm(y~x_3+x_4)

print('R square of unconstrained model:')
ur.r <- as.numeric(summary(unconstrained_model)['r.squared'])
ur.r
print('R square of constrained model:')
r.r <- as.numeric(summary(constrained_model)['r.squared'])
r.r

F.statistics <- ((ur.r-r.r)/(2))/((1-ur.r)/(length(x_1)-4-1))
F.statistics

#(f)
constrained.RSS <- sum(constrained_model$residuals^2)
print('SSR of constrained model: ')
constrained.RSS
unconstrained.RSS <- sum(unconstrained_model$residuals^2)
print('SSR of unconstrained model: ')
unconstrained.RSS

F_RSS.statistics <- ((constrained.RSS-unconstrained.RSS)/2)/((unconstrained.RSS)/(length(x_1)-4-1))
print('F_RSS.statistics in question f is equal to F.statistics in question e')
F_RSS.statistics

#(g)

linearHypothesis(unconstrained_model,c("x_1 = 0", "x_2 = 0"))
print('In the Hypethesis test, the F value = 5.9064 which is equal to question e & f')

