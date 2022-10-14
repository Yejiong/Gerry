library(ggplot2)
library(dplyr)
library(ggsci)
library(qte)
library(CVXR)

# for tables
library(knitr)
library(kableExtra)


# simulate data
X=rbinom(n=1000,size=1,prob = 0.5)
T=rbinom(n=1000,size=1,prob=0.5+0.1*X)
Y=rnorm(n=1000,mean=-1*T+1*(1-T),sd=1)
mydat=data.frame(X=X,T=T,Y=Y)

mydat%>%ggplot()+
  geom_density(aes(x=Y,fill=factor(T),group=factor(T)),alpha=0.5)+
  scale_fill_startrek()

mydat%>%ggplot(aes(x=Y,col=factor(T)))+
  stat_ecdf(geom = "step")+  
  scale_color_startrek()

# QTE function
q <- ci.qte(Y ~ T,
             xformla=~X,
             data=mydat, se=TRUE, probs=seq(0.05, 0.95, 0.05))
summary(q)
ggqte(q)


#### Example from CVXR quantile regression
library(quantreg)
data(engel)
p <- ggplot(data = engel) +
  geom_point(mapping = aes(x = income, y = foodexp), color = "blue")
taus <- c(0.1, 0.25, 0.5, 0.75, 0.90, 0.95)
fits <- data.frame(
  coef(lm(foodexp ~ income, data = engel)),
  sapply(taus, function(x) coef(rq(formula = foodexp ~ income, data = engel, tau = x))))
names(fits) <- c("OLS", sprintf("$\\tau_{%0.2f}$", taus))

nf <- ncol(fits)
colors <- colorRampPalette(colors = c("black", "red"))(nf)
p <- p + geom_abline(intercept = fits[1, 1], slope = fits[2, 1], color = colors[1], size = 1.5)
for (i in seq_len(nf)[-1]) {
  p <- p + geom_abline(intercept = fits[1, i], slope = fits[2, i], color = colors[i])
}
p

knitr::kable(fits, format = "html", caption = "Fits from OLS and `quantreg`") %>%
  kable_styling("striped") %>%
  column_spec(1:8, background = "#ececec")

# build it yourself
X <- model.matrix(foodexp ~ income, data = engel)
y <- matrix(engel[, "foodexp"], ncol = 1)
beta <- Variable(2)
quant_loss <- function(u, tau) { 0.5 * abs(u) + (tau - 0.5) * u }
solutions <- sapply(taus, function(tau) {
  obj <- sum(quant_loss(y - X %*% beta, t = tau))
  prob <- Problem(Minimize(obj))
  ## THE OSQP solver returns an error for tau = 0.5
  solve(prob, solver = "ECOS")$getValue(beta)
})
fits <- data.frame(coef(lm(foodexp ~ income, data = engel)),
                   solutions)
names(fits) <- c("OLS", sprintf("$\\tau_{%0.2f}$", taus))

knitr::kable(fits, format = "html", caption = "Fits from OLS and `CVXR`") %>%
  kable_styling("striped") %>%
  column_spec(1:8, background = "#ececec")

###### My own Quantile treatment effect model


quantile=function(taus, y, wt){
 
  # minimize sum of check functions
  
  q <- Variable(1)
  quant_loss <- function(u, tau) { 0.5 * abs(u) + (tau - 0.5) * u }
  solutions <- sapply(taus, function(tau) {
    obj <- sum(wt*quant_loss(y-q,tau))
    prob <- Problem(Minimize(obj))
    ## THE OSQP solver returns an error for tau = 0.5
    solve(prob, solver = "ECOS")$getValue(q)
  })
  return(solutions)
}

QTE=function(dataset, taus){
  
  
  pscore.reg <- glm(T ~ X, data=dataset, family=binomial)
  pscore <- fitted(pscore.reg)
  
  q.trt=quantile(taus=taus, y=dataset$Y, wt=dataset$T/pscore)
  q.control=quantile(taus=taus, y=dataset$Y, wt=(1-dataset$T)/(1-pscore))
  return(q.trt-q.control)
  
}

QTE(dataset=mydat,taus=c(0.1, 0.25, 0.5, 0.75, 0.90, 0.95))
 



 

 