# Independent In-Class Activity

### James Hada ###

library(brms)
library(readr)
library(marginaleffects)
library(ggplot2)
library(ggeffects)
library(dplyr)
library(performance)

rec <- read_csv("recordings.csv")
rec <- rec[-10, ]
sens <- read_csv("sensorinfo.csv")

plot(rec$boatactivity, rec$totsongs) # negative binom
plot(rec$boatactivity, rec$songlength) # probs gamma

mod.tot <- brm(totsongs ~ boatactivity, data = rec, family = negbinomial(link="log"))
        
mod.length <- brm(totsongs ~ boatactivity, data = rec, family = negbinomial(link="log"))        

summary(mod.tot)
summary(mod.length)

plot(mod.tot)
plot(mod.length)
mcmc_plot(mod.tot)
mcmc_plot(mod.length)

pp_check(mod.tot)
pp_check(mod.length)

################################################################################

# conditioning on watertemp closes noise<-watertemp->calls backdoor, boatactivity
# closes the noise<-boatactivity->calls, and the waterdepth and distshore backdoors

# Adjusting dataframe to scale variables
rec <- rec %>%
  mutate(
    boatactivity_scaled = scale(as.numeric(boatactivity))[, 1],
    watertemp_scaled = scale(as.numeric(watertemp))[, 1],
    boatnoise_scaled = scale(as.numeric(boatnoise))[, 1]
  )

mod.tot2 <- brm(totsongs ~ boatnoise_scaled + watertemp_scaled + boatactivity_scaled, data = rec, family = negbinomial(link="log"))
mod.length2 <- brm(songlength ~ boatnoise_scaled + watertemp_scaled + boatactivity_scaled, data = rec, family = Gamma(link = "log"))

# mod.tot2 used a negative binomial due to data overdispersion and integer 
# point data.mod.length2 used a Gamma distribution to approximate a non-normal
# distribution of a contiuous response variable. 

summary(mod.tot2)
mcmc_plot(mod.tot2)

summary(mod.length2)
mcmc_plot(mod.length2)

################################################################################
# Addressing pseudoreplication with hierachical model: 

rec.join <- rec %>%
  left_join(sens, by = "sensorid")

mod.tot.mix <- brm(
  totsongs ~ boatnoise_scaled + watertemp_scaled + boatactivity_scaled + 
    (1 | sensorid),
  data = rec.join,
  family = negbinomial(link = "log"),
  control = list(adapt_delta = 0.95),
)

mod.length.mix <- brm(
  songlength ~ boatnoise_scaled + watertemp_scaled + boatactivity_scaled + 
    (1 | sensorid),
  data = rec.join,
  family = Gamma(link = "log"),
  control = list(adapt_delta = 0.95),
)

plot(mod.tot.mix)
summary(mod.tot.mix)
mcmc_plot(mod.tot.mix, pars="^r")
mcmc_plot(mod.tot.mix)
bayes_R2(mod.tot.mix)
pp_check(mod.tot.mix)+ xlim(0, 200)

plot(mod.length.mix)
summary(mod.length.mix)
mcmc_plot(mod.length.mix, pars="^r")
mcmc_plot(mod.length.mix)
bayes_R2(mod.length.mix)
pp_check(mod.length.mix)+ xlim(0, 200)

# Boat noise has a negative effect on total whale songs and song duration.  
# boatnoise_noise had an estimate of -1.02 in mod.tot.mix and an estimate of 
# -0.68 in mod.length.mix. Watertemp_scaled had a significant positive effect
# on whale song length but not on the total number of songs. Mod.tot.mix had a
# Pseudo R2 of 0.536 with a 95% CI between 0.311 and 0.745; mod.length.mix had 
# an R2 of 0.322 with a 95% CI between 0.157 and 0.513. This suggests mod.tot.mix
# predicts a greater proportion of variation in total whale songs than 
# mod.length.mix predicts variation in song length. Both models contain a varying
# intercept for each sensor to address issues of non-independence in samples

pred <- ggpredict(mod.tot.mix,
                  terms = c("boatnoise_scaled", "boatactivity_scaled", "watertemp_scaled"),
                  interval = "prediction")
plot(pred)

pred <- ggpredict(mod.length.mix,
                  terms = c("boatnoise_scaled", "boatactivity_scaled", "watertemp_scaled"),
                  interval = "prediction")
plot(pred)
