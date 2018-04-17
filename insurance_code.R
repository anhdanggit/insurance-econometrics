
#-----------------------------------------------#
#--- ECONOMETIRCS OF INSURANCE PROJECT ---------#
# Mai-Anh Dang
# M2 EEE - Toulouse School of Economics
#-----------------------------------------------#

library(dplyr)
library(tidyr)
library(psych) # for descriptive summary
library(stargazer)
library(MASS)
library(AER) # for dispersiontest
library(gamlss)

# (A) Priori Pricing ####

#------------------------------------------------
## (A.1) Cleaning data ####

dat1 = read.csv("./data/PG_2017_CLAIMS_YEAR0.csv")
dat2 = read.csv("./data/PG_2017_YEAR1.csv")

### sum claim_nb and claim_amount (as each client with each vehicle may claim several times)
dat1.sum = dat1 %>% group_by(id_client, id_vehicle) %>% summarize(claim_nb = sum(claim_nb), claim_amount = sum(claim_amount))

### merge data
merged.data <- left_join(dat2, dat1.sum, by=c('id_client', 'id_vehicle'))

### replace missing value by 0
i.na = is.na(merged.data$claim_nb)
merged.data$claim_nb[i.na] = 0

i.na = is.na(merged.data$claim_amount)
merged.data$claim_amount[i.na] = 0
write.csv(merged.data, "./data/merged_final_insurance.csv")

## Descriptive ####

hist(merged.data$claim_nb, main = paste("Histogram of claim_nb"),
     xlab = paste("claim_nb"))
hist(log(merged.data$claim_amount), main = paste("Histogram of log(claim_amount)"),
     xlab = paste("log(claim_amount)"))

describe(merged.data)
table(merged.data$claim_nb) # count the obs. by times of claim


#---------------------------------------------------
## (A.2) Poisson Distribution: Freq ####

dat = read.csv("./data/merged_final_insurance.csv")
attach(dat)

### no offset
model_poisson1 = glm(claim_nb ~ pol_payd + pol_usage + drv_drv2 + drv_age1 + drv_age2 + drv_sex1
                     + drv_sex2 + drv_age_lic1 + drv_age_lic2 + vh_age + vh_cyl + vh_din + vh_fuel
                     + vh_sale_begin + vh_sale_end + vh_speed + vh_type + vh_weight, 
                     family = poisson(link="log"), data = dat, weight = NULL) 
summary(model_poisson1)

### pol_duration as offset
model_poisson2 = glm(claim_nb ~ pol_payd + pol_usage + drv_drv2 + drv_age1 + drv_age2 + drv_sex1
                    + drv_sex2 + drv_age_lic1 + drv_age_lic2 + vh_age + vh_cyl + vh_din + vh_fuel
                    + vh_sale_begin + vh_sale_end + vh_speed + vh_type + vh_weight, 
                    family = poisson(link="log"), data = dat, weight = NULL, 
                    offset = log(pol_duration)) # exposure / offset variable is time interval in years
summary(model_poisson2)

### pol_sit_duration as offset
model_poisson3 = glm(claim_nb ~ pol_payd + pol_usage + drv_drv2 + drv_age1 + drv_age2 + drv_sex1
                     + drv_sex2 + drv_age_lic1 + drv_age_lic2 + vh_age + vh_cyl + vh_din + vh_fuel
                     + vh_sale_begin + vh_sale_end + vh_speed + vh_type + vh_weight, 
                     family = poisson(link="log"), data = dat, weight = NULL, 
                     offset = log(pol_sit_duration)) # exposure / offset variable 
summary(model_poisson3)


### Export table of results
stargazer(model_poisson1, model_poisson2, model_poisson3)


#-----------------------------------------------
## (A.3) Negbin Distribution: Freq ####

### no offset
model_negbin1 = glm(claim_nb ~ pol_payd + pol_usage + drv_drv2 +  drv_age1 + drv_age2 + drv_sex1
                     + drv_sex2 + drv_age_lic1 + drv_age_lic2 + vh_age + vh_cyl + vh_din + vh_fuel
                     + vh_sale_begin + vh_sale_end + vh_speed + vh_type + vh_weight, 
                     family = negative.binomial(theta=1, link="log"), data = dat, weight = NULL) 
summary(model_negbin1)

### pol_duration as offset
model_negbin2 = glm(claim_nb ~ pol_payd + pol_usage + drv_drv2 + drv_age1 + drv_age2 + drv_sex1
                     + drv_sex2 + drv_age_lic1 + drv_age_lic2 + vh_age + vh_cyl + vh_din + vh_fuel
                     + vh_sale_begin + vh_sale_end + vh_speed + vh_type + vh_weight, 
                     family = negative.binomial(theta=1, link="log"), data = dat, weight = NULL, 
                     offset = log(pol_duration)) # exposure / offset variable is time interval in years
summary(model_negbin2)

### pol_sit_duration as offset
model_negbin3 = glm(claim_nb ~ pol_payd + pol_usage + drv_drv2 + drv_age1 + drv_age2 + drv_sex1
                     + drv_sex2 + drv_age_lic1 + drv_age_lic2 + vh_age + vh_cyl + vh_din + vh_fuel
                     + vh_sale_begin + vh_sale_end + vh_speed + vh_type + vh_weight, 
                     family = negative.binomial(theta=1, link="log"), data = dat, weight = NULL, 
                     offset = log(pol_sit_duration)) # exposure / offset variable 
summary(model_negbin3)

### Export table of results
stargazer(model_negbin1, model_negbin2, model_negbin3)


#-----------------------------------------------
## (A.4) Negbin vs. Poisson Test ####

# LR ratio test of alpha = 0 ####
pchisq(2*(logLik(model_negbin1) - logLik(model_poisson1)), df=1, lower.tail = FALSE) # pval
as.numeric(2*(logLik(model_negbin1) - logLik(model_poisson1))) # lr stat

pchisq(2*(logLik(model_negbin2) - logLik(model_poisson2)), df=1, lower.tail = FALSE) # pval
as.numeric(2*(logLik(model_negbin2) - logLik(model_poisson2))) # lr stat

pchisq(2*(logLik(model_negbin3) - logLik(model_poisson3)), df=1, lower.tail = FALSE) # pval
as.numeric(2*(logLik(model_negbin3) - logLik(model_poisson3))) # lr stat


# overdispersion test #### 
AER::dispersiontest(model_poisson1,trafo=1, alternative = "greater")
AER::dispersiontest(model_poisson2,trafo=1) 
AER::dispersiontest(model_poisson3,trafo=1)

AER::dispersiontest(model_poisson1,trafo=2)
AER::dispersiontest(model_poisson2,trafo=2) 
AER::dispersiontest(model_poisson3,trafo=2)


#------------------------------------------------
## (A.5) Gamma Distribution: Severity ####

dat.amt = subset(dat, claim_amount > 0)

gamma.model = glm(claim_amount ~ pol_payd + pol_pay_freq + drv_age_lic1 + vh_age + vh_cyl + vh_din
              + vh_sale_begin + vh_sale_end,  offset=log(pol_duration), 
              data=dat.amt,family=Gamma(link="log"))
summary(gamma.model)


#------------------------------------------------
## (A.5) Log-normal Distribution: Severity ####

lnorm.model = glm(claim_amount ~ pol_payd + pol_pay_freq + drv_age_lic1 + vh_age + vh_cyl + vh_din
                  + vh_sale_begin + vh_sale_end,  offset=log(pol_duration), 
                  data=dat.amt,family=gaussian(link="log"))
summary(lnorm.model)


## Export the table
stargazer(gamma.model, lnorm.model)

### Anova test
anova(gamma.model, test = "Chisq")
anova(lnorm.model, test = "Chisq")
anova(gamma.model, lnorm.model, test = "Chisq")

#-------------------------------------------------
## (A.6) Compute the Premium 
dat$fit_nb = model_negbin2$fitted.values
dat.amt = subset(dat, claim_amount > 0)

dat.amt$ga_amt = gamma.model$fitted.values
dat.amt$lnorm_amt = lnorm.model$fitted.values

dat.amt$premium_gamma = dat.amt$fit_nb * dat.amt$ga_amt
dat.amt$premium_lnorm = dat.amt$fit_nb * dat.amt$lnorm_amt

## Export the table of results
premium.result = cbind(dat.amt$id_client, dat.amt$premium_gamma, dat.amt$premium_lnorm) %>% as.data.frame
names(premium.result) = c("id_client", "premium_gamma", "premium_lnorm")
write.csv(premium.result, './data/actuarial_premium.csv')

## Plot
premium_gamma =  data.frame(premium = dat.amt$premium_gamma)
premium_gamma$model = 'gamma'

premium_lnorm = data.frame(premium =dat.amt$premium_lnorm)
premium_lnorm$model = 'log-norm'

long = rbind(premium_lnorm, premium_gamma)
ggplot(long, aes(premium, fill = model)) + geom_density(alpha = 0.2) + xlim(c(0, 3000)) +
  ggtitle("Distribution of Estimated Actuarial Premium") +
  theme_classic()


#-------------------------------------------------
## Compute Posterior Premium (Bayes Rule)

'following assumptions:

- There are two types of risk in the population, low and high-risk types with a 50% of chances for each type. 
- The Number of claims N_it follows a Poisson distribution with an average frequency of λ_L=0.05 and λ_H=0.15
- The realizations of severity of claims are i.i.d. and on average equal to 1. 
- The insurance company uses the Bayesian rule to revise a priori pricing. 
'

### Compute the a posteriori fair premium E(N_i2 | N_i1=k ) for k=0,…,5.
k = 0:5
P.H.n_i1 = rep(0, 6)
P.L.n_i1 = rep(0, 6)
E.N_i2.n_i1 = rep(0, 6)

lambda_H = 0.15
lambda_L = 0.05


exp.func = function(lambda, k){
  exp(-lambda) * lambda^k / factorial(k)
}

for (i in k){
  P.H.n_i1[i+1] = exp.func(lambda_H, i) / ( exp.func(lambda_H, i) + exp.func(lambda_L, i))
  P.L.n_i1[i+1] = exp.func(lambda_L, i) / ( exp.func(lambda_H, i) + exp.func(lambda_L, i))
  E.N_i2.n_i1[i+1] = lambda_H * P.H.n_i1[i+1] + lambda_L * P.L.n_i1[i+1]
}

post.premium = data.frame(k = k, P.H.n_i1 = P.H.n_i1, P.L.n_i1 = P.L.n_i1, E.N_i2.n_i1 = E.N_i2.n_i1)
stargazer(post.premium, summary = FALSE, rownames = FALSE)
