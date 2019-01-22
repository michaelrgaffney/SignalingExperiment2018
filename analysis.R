library(signalingdata2018)
library(car)
library(effects)
#do in processing
library(tidyverse)
library(stringr)
# library(jtools)
# library(officer)
# library(flextable)
# library(huxtable)
library(devtools)
library(git2r)
#setwd("C:/Users/Michael/Desktop/WSU/MA Research/Vignette Study/SignalingExperiment2018")
#use_github(auth_token = "87e63dd6ab3e26c55ac1989b9330e3b3043a302a")
#cred <- cred_ssh_key(publickey = "~/../.ssh/id_rsa.pub", privatekey = "~/../.ssh/id_rsa")
#use_github(credentials = cred, auth_token = "87e63dd6ab3e26c55ac1989b9330e3b3043a302a")
#host = "https://github.com/michaelrgaffney/2018-Signal-Anaylsis.git")

signalingdata2018$signal <- str_replace(signalingdata2018$signal, pattern = ":Sister", replacement = "")
#d<- signalingdata2018[!signalingdata2018$exclusionopt1,]
signalingdata2018$conflict <- as.factor(signalingdata2018$conflict)
signalingdata2018$p_info <- ordered(signalingdata2018$p_info, levels = c("Cheating", "PrivateInformation", "Honest"))
signalingdata2018$signal <- as.factor(signalingdata2018$signal)
signalingdata2018$Sex <- as.factor(signalingdata2018$Sex)
d<- signalingdata2018[!signalingdata2018$exclusionopt1,]
e <- 
  d %>% 
  group_by(conflict, p_info, signal) %>% 
  summarize(n = n())
hist(e$n)


#variable key
# needsmoneyt1: Not at all - A great deal
# likelylendmoneyt1: Very unlikely - Very likely
# angryt1: Extemely angry - Not at all angry
# howsadt1: Devastated - Not sad
# satisfactiont1: No satisfaction - A great deal of satisfaction
# howreasonablet1: Extremely unreasonable - Extremely reasonable
# believehealtht1: Extremely unbelieving - Extremely believing
# daughterharmt1: A great deal - None at all
# believeneedt1: Im very suspicious - Im completely unsuspicous
# sisterbenefitt1: None at all - A great deal
# trustrepayt1: Extremely distrustful - Extremely trustful
#second set: less likely always 0 more always 100

#t1 outcomes based on conflict and p_info
# likelylendmoneyt1 comfortablelendingt1
cp1 <- lm(likelylendmoneyt1 ~ conflict + p_info, d)
#export_summs(cp1, to.file = "pdf", file.name = "test.pdf")
summary(cp1)
Anova(cp1, type = 2)
#use in paper
plot(allEffects(cp1), xlab= (""),ylab = "likelihood of lending money")
#plot(cp1)
hist(signalingdata2018$likelylendmoneyt1)

cp1glm <- glm(likelylendmoneyt1/100 ~ conflict + p_info, family = binomial, d)
summary(cp1glm)
Anova(cp1glm, type = 2)
#use in paper
plot(allEffects(cp1glm), xlab= (""), ylab = "likelihood of lending money")
#plot(cp1glm)

cp2 <- lm(comfortablelendingt1 ~ conflict + p_info, d)
summary(cp2)
Anova(cp2, type = 2)
plot(allEffects(cp2))
plot(cp2)
hist(signalingdata2018$comfortablelendingt1)

cp2glm <- glm(comfortablelendingt1/50000 ~ conflict + p_info, family = binomial, d)
summary(cp2glm)
Anova(cp2glm, type = 2)
#use in paper
plot(allEffects(cp2glm), xlab= (""), ylab = "amount of money comfortable lending")
#plot(cp2glm)

cp3glm <- glm(comfortablelendingt2/50000 ~ I(comfortablelendingt1/50000) + conflict + p_info + signal, family = binomial, d)
summary(cp3glm)
Anova(cp3glm, type = 2)
plot(allEffects(cp3glm))
plot(cp3glm)

library(emmeans)
ecp3glm <- emmeans(cp3glm, "signal")
pairs(ecp3glm)

m <- 
  d %>% 
  filter(signal %in% c("Depression", "VerbalRequest", "FacialSadnesswithCrying")) %>% 
  mutate(comfortablelendingt1 = comfortablelendingt1/50000,
         comfortablelendingt2 = comfortablelendingt2/50000, 
         signal = ordered(signal, levels = c("VerbalRequest", "FacialSadnesswithCrying", "Depression"))) %>% 
  glm(comfortablelendingt2 ~ comfortablelendingt1 + conflict + p_info + signal, family = binomial, data =.)
summary(m)

#em <- emmeans(m, "signal")
#pairs(em)
plot(allEffects(m))

mint <- 
  d %>% 
  filter(signal %in% c("Depression", "VerbalRequest", "FacialSadnesswithCrying", "DepressionwithSuicideThreat", "SuicideAttempt")) %>% 
  mutate(comfortablelendingt1 = comfortablelendingt1/50000,
         comfortablelendingt2 = comfortablelendingt2/50000#, 
         #signal = ordered(signal, levels = c("VerbalRequest", "FacialSadnesswithCrying", "Depression", "DepressionwithSuicideThreat", "SuicideAttempt"))
         ) %>% 
  glm(comfortablelendingt2 ~ comfortablelendingt1 + conflict*signal + p_info*signal, family = binomial, data =.)
summary(mint)
Anova(mint, type = 3)
plot(allEffects(mint))

mfint <- 
  d %>% 
  filter(signal %in% c("Depression", "VerbalRequest", "FacialSadnesswithCrying")) %>% 
  mutate(comfortablelendingt1 = comfortablelendingt1/50000,
         comfortablelendingt2 = comfortablelendingt2/50000, 
         signal = ordered(signal, levels = c("VerbalRequest", "FacialSadnesswithCrying", "Depression"))) %>% 
  glm(comfortablelendingt2 ~ comfortablelendingt1 + conflict * signal * p_info , family = binomial, data =.)
summary(mfint)
Anova(mfint, type = 3)
plot(allEffects(mfint))

mfints <- 
  d %>% 
  filter(signal %in% c("Depression", "VerbalRequest", "FacialSadnesswithCrying", "DepressionwithSuicideThreat")) %>% 
  mutate(comfortablelendingt1 = comfortablelendingt1/50000,
         comfortablelendingt2 = comfortablelendingt2/50000, 
         signal = ordered(signal, levels = c("VerbalRequest", "FacialSadnesswithCrying", "Depression", "DepressionwithSuicideThreat"))) %>% 
  glm(comfortablelendingt2 ~ comfortablelendingt1 + conflict * signal * p_info , family = binomial, data =.)
summary(mfints)
Anova(mfints, type = 3)
plot(allEffects(mfints))

angerm <- 
  d %>% 
  filter(signal %in% c("Depression", "VerbalRequest", "FacialSadnesswithCrying", "Anger")) %>% 
  mutate(comfortablelendingt1 = comfortablelendingt1/50000,
         comfortablelendingt2 = comfortablelendingt2/50000#, 
         #signal = ordered(signal, levels = c("VerbalRequest", "FacialSadnesswithCrying", "Depression", "DepressionwithSuicideThreat", "SuicideAttempt"))
  ) %>% 
  glm(comfortablelendingt2 ~ comfortablelendingt1 + conflict + signal + p_info +signal:Sex, family = binomial, data =.)
summary(angerm)
Anova(angerm, type = 3)
plot(allEffects(angerm))


library(visreg)
vcp3glm<- visreg(cp3glm, scale = "response")


cpAngerlm <- lm(angryt1 ~ conflict + p_info, d)
summary(cpAngerlm)
Anova(cpAngerlm, type = 2)
plot(allEffects(cpAngerlm))
plot(cpAngerlm)

cpbneedlm <- lm(believeneedt1 ~ conflict + p_info, d)
summary(cpbneedlm)
Anova(cpbneedlm, type = 2)
plot(allEffects(cpbneedlm))
plot(cpbneedlm)

cpdharmlm <- lm(daughterharmt1 ~ conflict + p_info, d)
summary(cpdharmlm)
Anova(cpdharmlm, type = 2)
plot(allEffects(cpdharmlm))
plot(cpdharmlm)



#####################################################################
#####################################################################
#signal relative to anger?
d<- signalingdata2018[!signalingdata2018$exclusionopt1,]
m <- lm(MO2.2_1 ~ conflict + p_info + signal, d)
summary(m)
Anova(m, type = 2)
plot(allEffects(m))
plot(Effect("signal", m))

options('contrasts')
#how we changed contrasts in the past
#contrasts(d$p_info) <- contr.sum(3) #sum- each level to average of all
# run treatment as well (private info as baseline)
levels(d$p_info)
levels(d$signal)
levels(d$conflict)

m2 <- lm(MO2.12_1 ~ conflict + p_info + signal, d)
summary(m2)
Anova(m2, type = 2)
plot(allEffects(m2))
plot(Effect("signal", m2))

m2b <- lm(MO2.12_1 ~ MO1.12_1 + conflict + p_info + signal, d)
summary(m2b)
Anova(m2b, type = 2)
plot(allEffects(m2b))
plot(Effect("signal", m2b))
#MO2.12_1

m3 <- lm(MO2.2_1 ~ conflict + p_info + signal + conflict:signal + p_info:signal + conflict:p_info, d)
summary(m3)
Anova(m3, type = 3)
plot(allEffects(m3))
plot(Effect("signal", m3))
#Delta Social Support ~ Signal Strength + Conflict + Information + Signal Strength:Conflict + Signal Strength:Information + Conflict:Information

m3b <- lm(MO2.12_1 ~ MO1.2_1 + conflict + p_info + signal + conflict:signal + p_info:signal + conflict:p_info, d)
summary(m3b)
Anova(m3b, type = 3)
plot(allEffects(m3b))
plot(Effect("signal", m3b))

levels(signalingdata2018$signal)
library(visreg)
visreg(m3b, by = "conflict", xvar = "signal", partial = FALSE)
visreg(m3b, by = "p_info", xvar = "signal", partial = FALSE)
visreg(m3b, by = "conflict", xvar = "p_info", partial = FALSE)
#visreg(m3b, by = "conflict", xvar = "p_info", partial = FALSE, cond = list("signal" = 0))

###########################################################################################################
#For the paper:
#Effects of conflict and p_info pre-signal: likliehood of lending money and amount comfortable lending
#Effect plot of each and a table with the combined summary statistics
#Is this enough to show that p_info and honest are significantly different

cp1glm <- glm(likelylendmoneyt1/100 ~ conflict + p_info, family = binomial, d)
summary(cp1glm)
#Anova(cp1glm, type = 2)
plot(allEffects(cp1glm), xlab= (""), ylab = "likelihood of lending money")

cp2glm <- glm(comfortablelendingt1/50000 ~ conflict + p_info, family = binomial, d)
summary(cp2glm)
#Anova(cp2glm, type = 2)
#use in paper
plot(allEffects(cp2glm), xlab= (""), ylab = "amount of money comfortable lending")

#main effects model for after the signal
#effect plot of main effects for all the signals and controls
#summary stats for models with only the signals too but which contrasts? currently treatment (alphabetical order)
#ordered factors?

#can get at change within a person
#d$signal <- factor(signal, levels = c("VerbalRequest", "FacialSadnesswithCrying", "Depression", "DepressionwithSuicideThreat", "SuicideAttempt"))
cp3glm <- glm(comfortablelendingt2/50000 ~ I(comfortablelendingt1/50000) + conflict + p_info + signal, family = binomial, d)
summary(cp3glm)
#library(multcomp)
#summary(glht(cp3glm, mcp(rank="Tukey")))
Anova(cp3glm, type = 2)
plot(allEffects(cp3glm))
#plot(cp3glm)

#can can only see change between people. still worth using?
cp4glm <- glm(likelylendmoneyt2/100 ~ I(likelylendmoneyt1/100) +conflict + p_info + signal, family = binomial, d)
summary(cp4glm)
library(multcomp)
summary(glht(cp4glm, mcp(rank="Tukey")))
Anova(cp4glm, type = 2)
plot(allEffects(cp4glm)) 
#plot(cp4glm)
library(visreg)
library(forcats)
p <- visreg(cp4glm, xvar= "signal", gg = FALSE, scale = "response")
p$fit$signal <- fct_reorder(p$fit$signal, p$fit$visregFit)
ggplot(p$fit, aes(visregFit, signal)) + geom_point() +geom_errorbarh(aes(xmin = visregLwr, xmax = visregUpr)) +theme_bw() +labs(x = "Proportion of max offer")
# p + coord_flip()
mfull <- 
  d %>% 
  filter(signal %in% c("Depression", "VerbalRequest", "FacialSadnesswithCrying", "DepressionwithSuicideThreat", "SuicideAttempt")) %>% 
  mutate(likelylendmoneyt2 = likelylendmoneyt2/100) %>% 
  glm(likelylendmoneyt2 ~  conflict + signal + p_info , family = binomial, data =.)
summary(mfull)
Anova(mfull, type = 3)
plot(allEffects(mfull))

mfullb <- 
  d %>% 
  filter(signal %in% c("Depression", "VerbalRequest", "FacialSadnesswithCrying", "DepressionwithSuicideThreat", "SuicideAttempt")) %>% 
  mutate(comfortablelendingt1 = comfortablelendingt1/50000,
               comfortablelendingt2 = comfortablelendingt2/50000)  %>% 
  glm(comfortablelendingt2 ~ comfortablelendingt1 + conflict + signal + p_info , family = binomial, data =.)
summary(mfullb)
Anova(mfullb, type = 3)
plot(allEffects(mfull))

#interactions
#effects plots and summaries for both?
mfullint <- 
  d %>% 
  filter(signal %in% c("Depression", "VerbalRequest", "FacialSadnesswithCrying", "DepressionwithSuicideThreat", "SuicideAttempt")) %>% 
  mutate(comfortablelendingt1 = comfortablelendingt1/50000,
         comfortablelendingt2 = comfortablelendingt2/50000, 
         signal = ordered(signal, levels = c("VerbalRequest", "FacialSadnesswithCrying", "Depression", "DepressionwithSuicideThreat", "SuicideAttempt"))) %>% 
  glm(comfortablelendingt2 ~ comfortablelendingt1 + conflict * signal * p_info , family = binomial, data =.)
summary(mfullint)
Anova(mfullint, type = 3)
plot(allEffects(mfullint))

mfullintb <- 
  d %>% 
  filter(signal %in% c("Depression", "VerbalRequest", "FacialSadnesswithCrying", "DepressionwithSuicideThreat", "SuicideAttempt")) %>% 
  mutate(likelylendmoneyt2 = likelylendmoneyt2/100,
         signal = ordered(signal, levels = c("VerbalRequest", "FacialSadnesswithCrying", "Depression", "DepressionwithSuicideThreat", "SuicideAttempt"))) %>% 
  glm(likelylendmoneyt2 ~  conflict * signal * p_info , family = binomial, data =.)
summary(mfullintb)
Anova(mfullintb, type = 3)
plot(allEffects(mfullintb))

#interaction model with only request, crying, and depression
#effects plot
#summary?
#ordered factor
mfint <- 
  d %>% 
  filter(signal %in% c("Depression", "VerbalRequest", "FacialSadnesswithCrying")) %>% 
  mutate(comfortablelendingt1 = comfortablelendingt1/50000,
         comfortablelendingt2 = comfortablelendingt2/50000, 
         signal = ordered(signal, levels = c("VerbalRequest", "FacialSadnesswithCrying", "Depression"))) %>% 
  glm(comfortablelendingt2 ~ comfortablelendingt1 + conflict * signal * p_info , family = binomial, data =.)
summary(mfint)
Anova(mfint, type = 3)
plot(allEffects(mfint))

mfint2 <- 
  d %>% 
  filter(signal %in% c("Depression", "VerbalRequest", "FacialSadnesswithCrying")) %>% 
  filter(p_info == "PrivateInformation") %>% 
  mutate(comfortablelendingt1 = comfortablelendingt1/50000,
         comfortablelendingt2 = comfortablelendingt2/50000, 
         signal = ordered(signal, levels = c("VerbalRequest", "FacialSadnesswithCrying", "Depression"))) %>% 
  glm(comfortablelendingt2 ~ comfortablelendingt1 + conflict * signal, family = binomial, data =.)
summary(mfint2)
Anova(mfint2, type = 3)
plot(allEffects(mfint2))
#use this
p3 <- visreg(mfint2, xvar = "signal", by = "conflict", gg = TRUE, scale = "response")
p3 + labs(y = "proportion of maximum offer") + theme_bw() + coord_flip()

mfint3 <- 
  d %>% 
  filter(signal %in% c("Depression", "VerbalRequest", "FacialSadnesswithCrying")) %>% 
  filter(p_info == "PrivateInformation") %>% 
  mutate(likelylendmoneyt1 = likelylendmoneyt1/100,
         likelylendmoneyt2 = likelylendmoneyt2/100, 
         signal = ordered(signal, levels = c("VerbalRequest", "FacialSadnesswithCrying", "Depression"))) %>% 
  glm(likelylendmoneyt2 ~ likelylendmoneyt1 + conflict * signal, family = binomial, data =.)
summary(mfint3)
Anova(mfint3, type = 3)
plot(allEffects(mfint3))
#show as well

##################################################################################
mfmain <- 
  d %>% 
  filter(signal %in% c("Depression", "VerbalRequest", "FacialSadnesswithCrying")) %>% 
  mutate(comfortablelendingt1 = comfortablelendingt1/50000,
         comfortablelendingt2 = comfortablelendingt2/50000, 
         signal = ordered(signal, levels = c("VerbalRequest", "FacialSadnesswithCrying", "Depression"))) %>% 
  glm(comfortablelendingt2 ~ comfortablelendingt1 + signal, family = binomial, data =.)
summary(mfmain)
Anova(mfmain, type = 2)
plot(allEffects(mfmain))
p2 <- visreg(mfmain, xvar = "signal", scale = "response", gg = TRUE)
p2 + labs(y = "proportion of maximum offer") + theme_bw()
#include anova tabl
#show p2 plus model summary

# anger compare based on sex? is there a sex difference controlling for overall liklihood of help


mangersex <- 
  d %>% 
  filter(signal %in% c("Depression", "VerbalRequest", "FacialSadnesswithCrying", "Anger")) %>% 
  filter(p_info == "PrivateInformation") %>% 
  mutate(likelylendmoneyt1 = likelylendmoneyt1/100,
         likelylendmoneyt2 = likelylendmoneyt2/100, 
         signal = factor(signal, levels = c("VerbalRequest", "FacialSadnesswithCrying", "Depression", "Anger"))) %>% 
  glm(likelylendmoneyt2 ~ likelylendmoneyt1 + conflict * signal * Sex, family = binomial, data =.)
summary(mangersex)
Anova(mangersex, type = 3)
plot(allEffects(mangersex))

#exploratory sex
d$Siblings2 <- factor(ifelse(d$Siblings != "0", "Yes", "No"))
d$Siblings3 <- as.numeric(ifelse(d$Siblings == "More than 10", "10", d$Siblings))

#education
d$Ed2 <- 
  ordered(
    d$Ed,
    levels = c(
      "Some high school (secondary school), no diploma",
      "GED",
      "High school (secondary school)",
      "Associate degree",
      "Bachelor's degree",
      "Professional degree",
      "Master's degree",
      "Doctorate"
    )
  )

mfullsex <- 
  d %>% 
  filter(signal %in% c("Depression", "VerbalRequest", "FacialSadnesswithCrying", "DepressionwithSuicideThreat", "SuicideAttempt")) %>% 
  mutate(likelylendmoneyt2 = likelylendmoneyt2/100,
        likelylendmoneyt1 = likelylendmoneyt1/100, 
        Ed = factor(Ed), 
        RelStat = factor(RelStat),
        BOrder = factor(BOrder)) %>% 
  glm(likelylendmoneyt2 ~ likelylendmoneyt1 + signal + MC1.1_1, family = binomial, data =.)
summary(mfullsex)
Anova(mfullsex, type = 2)
plot(allEffects(mfullsex))

mfullbsex <- 
  d %>% 
  filter(signal %in% c("Depression", "VerbalRequest", "FacialSadnesswithCrying", "DepressionwithSuicideThreat", "SuicideAttempt")) %>% 
  mutate(comfortablelendingt1 = comfortablelendingt1/50000,
         comfortablelendingt2 = comfortablelendingt2/50000)  %>% 
  glm(comfortablelendingt2 ~ comfortablelendingt1 + conflict + signal + p_info + Sex, family = binomial, data =.)
summary(mfullbsex)
Anova(mfullbsex, type = 2)
plot(allEffects(mfullsex))

cclose <- glm(daughterharmt1/100 ~ conflict + p_info, family = binomial, d)
summary(cclose)
plot(allEffects(cclose), xlab= (""), ylab = "")


cother <- glm(believehealtht1/100 ~ conflict + p_info, family = binomial, d)
summary(cother)
plot(allEffects(cother), xlab= (""), ylab = "")

cother2 <- glm(likelylendmoneyt2/100 ~ I(likelylendmoneyt1/100) + angryt2 + signal, family = binomial, d)
summary(cother2)
plot(allEffects(cother2), xlab= (""), ylab = "")

cang <- glm(angryt2/100 ~ I(angryt1/100) + conflict + p_info + signal, family = binomial, d)
summary(cang)
plot(allEffects(cang), xlab= (""), ylab = "")

cneed <- glm(needsmoneyt2/100 ~ I(needsmoneyt1/100) + conflict + p_info + signal, family = binomial, d)
summary(cneed)
plot(allEffects(cneed))

cneed2 <- glm(likelylendmoneyt2/100 ~ I(likelylendmoneyt1/100) + needsmoneyt2 + angryt2 + signal, family = binomial, d)
summary(cneed2)
plot(allEffects(cneed2))

cneed3 <- glm(satisfactiont2/100 ~ I(satisfactiont1/100) + signal, family = binomial, d)
summary(cneed3)
plot(allEffects(cneed3))

#needt2 and angert2 controls
########### 504 paper ############
#Effects of conflict and p_info pre-signal: comfortable lending
#Effect plot of each and a table with the combined summary statistics
cp1glm <- glm(likelylendmoneyt1/100 ~ conflict + p_info, family = binomial, d)
summary(cp1glm)
plot(allEffects(cp1glm), xlab= (""), ylab = "likelihood of lending money")

cp2glm <- glm(comfortablelendingt1/50000 ~ conflict + p_info, family = binomial, d)
summary(cp2glm)
plot(allEffects(cp2glm), xlab= (""), ylab = "proportion of maximum possible offer")

#Main Effects of signals
#show plots for signals and controls for both types
cp4glm <- glm(likelylendmoneyt2/100 ~ I(likelylendmoneyt1/100) + signal, family = binomial, d)
library(visreg)
library(forcats)
summary(cp4glm)
p <- visreg(cp4glm, xvar= "signal", gg = FALSE, scale = "response")
p$fit$signal <- fct_reorder(p$fit$signal, p$fit$visregFit)
ggplot(p$fit, aes(visregFit, signal)) + geom_point() +geom_errorbarh(aes(xmin = visregLwr, xmax = visregUpr)) +theme_bw() +labs(x = "Liklihood of helping", y = "Signal")
# p + coord_flip()

cp4bglm <- glm(likelylendmoneyt2/100 ~ I(likelylendmoneyt1/100) + signal, family = binomial, d)
summary(cp4bglm)


cp5glm <- glm(comfortablelendingt2/50000 ~ I(comfortablelendingt1/5000) + signal, family = binomial, d)
p5 <- visreg(cp5glm, xvar= "signal", gg = FALSE, scale = "response")
p5$fit$signal <- fct_reorder(p5$fit$signal, p5$fit$visregFit)
ggplot(p5$fit, aes(visregFit, signal)) + geom_point() +geom_errorbarh(aes(xmin = visregLwr, xmax = visregUpr)) +theme_bw() +labs(x = "Proportion of maximum amount comfortable lending", y = "Signal")

mainLend <- #originally just the 3
  d %>% 
  filter(signal %in% c("Depression", "VerbalRequest", "FacialSadnesswithCrying", "DepressionwithSuicideThreat", "SuicideAttempt")) %>% 
  mutate(comfortablelendingt1 = comfortablelendingt1/50000,
         comfortablelendingt2 = comfortablelendingt2/50000, 
         signal = factor(signal, levels = c("VerbalRequest", "FacialSadnesswithCrying", "Depression", "DepressionwithSuicideThreat", "SuicideAttempt"))) %>% 
  glm(comfortablelendingt2 ~ comfortablelendingt1 + signal, family = binomial, data =.)
summary(mainLend)
Anova(mainLend, type = 2)
#posthoc <- TukeyHSD(x=a_mainLend, 'd$signal', conf.level=0.95)
library(agricolae)
comparison <- HSD.test(mainLend,"signal", group=TRUE,
                       main="Effects of signal on sum lending")
library(emmeans)
mainLend.emm.s <- emmeans(mainLend, "signal")
pairs(mainLend.emm.s, adjust = "tukey")
plot(allEffects(mainLend))
p2 <- visreg(mainLend, xvar = "signal", scale = "response", gg = TRUE)
p2 + labs(y = "proportion of maximum offer") + theme_bw()
#include anova table
#show p2 plus model summary

mainLikely <- #originally just the 3
  d %>% 
  filter(signal %in% c("Depression", "VerbalRequest", "FacialSadnesswithCrying", "DepressionwithSuicideThreat", "SuicideAttempt")) %>% 
  mutate(likelylendmoneyt1 = likelylendmoneyt1/100,
         likelylendmoneyt2 = likelylendmoneyt2/100, 
         signal = factor(signal, levels = c("VerbalRequest", "FacialSadnesswithCrying", "Depression", "DepressionwithSuicideThreat", "SuicideAttempt"))) %>% 
  glm(likelylendmoneyt2 ~ likelylendmoneyt1 + signal, family = binomial, data =.)
summary(mainLikely)
Anova(mainLikely, type = 2)

mainLikely.emm.s <- emmeans(mainLikely, "signal")
pairs(mainLikely.emm.s, adjust = "tukey")
plot(allEffects(mainLikely))
p7 <- visreg(mainLikely, xvar = "signal", scale = "response", gg = TRUE)
p7 + labs(y = "proportion of maximum offer") + theme_bw()





#Depression and weaker signals interactions
mfint2 <- 
  d %>% 
  filter(signal %in% c("Depression", "VerbalRequest", "FacialSadnesswithCrying")) %>% 
  filter(p_info == "PrivateInformation") %>% 
  mutate(comfortablelendingt1 = comfortablelendingt1/50000,
         comfortablelendingt2 = comfortablelendingt2/50000, 
         signal = ordered(signal, levels = c("VerbalRequest", "FacialSadnesswithCrying", "Depression"))) %>% 
  glm(comfortablelendingt2 ~ comfortablelendingt1 + conflict * signal, family = binomial, data =.)
summary(mfint2)
Anova(mfint2, type = 3)
plot(allEffects(mfint2))
#use this
p3 <- visreg(mfint2, xvar = "signal", by = "conflict", gg = TRUE, scale = "response")
p3 + labs(y = "proportion of maximum amount comfortable lending") + theme_bw() + coord_flip()

mfint3 <- 
  d %>% 
  filter(signal %in% c("Depression", "VerbalRequest", "FacialSadnesswithCrying")) %>% 
  filter(p_info == "PrivateInformation") %>% 
  mutate(likelylendmoneyt1 = likelylendmoneyt1/100,
         likelylendmoneyt2 = likelylendmoneyt2/100, 
         signal = ordered(signal, levels = c("VerbalRequest", "FacialSadnesswithCrying", "Depression"))) %>% 
  glm(likelylendmoneyt2 ~ likelylendmoneyt1 + conflict * signal, family = binomial, data =.)
summary(mfint3)
Anova(mfint3, type = 3)
plot(allEffects(mfint3))
#show as well

p4 <- visreg(mfint3, xvar = "signal", by = "conflict", gg = TRUE, scale = "response")
p4 + labs(y = "proportion of maximum offer") + theme_bw() + coord_flip()

# 1/18 exploratory after mediation model
e1 <- 
  d %>% 
  filter(signal %in% c("Depression", "VerbalRequest")) %>% 
  mutate(likelylendmoneyt1 = likelylendmoneyt1/100,
         likelylendmoneyt2 = likelylendmoneyt2/100,
         needsmoneyt1 = needsmoneyt1/100,
         needsmoneyt2 = needsmoneyt2/100,
         signal = factor(signal, levels = c("VerbalRequest", "Depression"))) %>% 
  lm(likelylendmoneyt2 ~ likelylendmoneyt1 + signal + needsmoneyt2 + needsmoneyt1, family = binomial, data =.)
summary(e1)
plot(allEffects(e1))

e2 <- 
  d %>% 
  filter(signal %in% c("Depression", "VerbalRequest")) %>% 
  mutate(likelylendmoneyt1 = likelylendmoneyt1/100,
         likelylendmoneyt2 = likelylendmoneyt2/100,
         needsmoneyt1 = needsmoneyt1/100,
         needsmoneyt2 = needsmoneyt2/100,
         signal = factor(signal, levels = c("VerbalRequest", "Depression"))) %>% 
  lm(likelylendmoneyt2 ~ needsmoneyt2, family = binomial, data =.)
summary(e2)
plot(allEffects(e2))

e3 <- 
  d %>% 
  filter(signal %in% c("Depression", "VerbalRequest")) %>% 
  mutate(likelylendmoneyt1 = likelylendmoneyt1/100,
         likelylendmoneyt2 = likelylendmoneyt2/100,
         needsmoneyt1 = needsmoneyt1/100,
         needsmoneyt2 = needsmoneyt2/100,
         signal = factor(signal, levels = c("VerbalRequest", "Depression"))) %>% 
  lm(needsmoneyt2 ~ needsmoneyt1 + signal + MC2.1_1, family = binomial, data =.)
summary(e3)
plot(allEffects(e3))

#adding and removing either signal or needsmoney t2 shows r-squared is mostly drivin by signal (.17 compared to .08)
e4 <- 
  d %>% 
  filter(signal %in% c("Depression", "VerbalRequest")) %>% 
  mutate(likelylendmoneyt1 = likelylendmoneyt1/100,
         likelylendmoneyt2 = likelylendmoneyt2/100,
         needsmoneyt1 = needsmoneyt1/100,
         needsmoneyt2 = needsmoneyt2/100,
         signal = factor(signal, levels = c("VerbalRequest", "Depression"))) %>% 
  lm(needsmoneyt2 ~ needsmoneyt1 + signal, family = binomial, data =.)
summary(e4)
plot(allEffects(e4))

#anger alone r-squared of .42 but likely an outcome of the same variables that influence perceptions of need
#does anger mediate needsmoney?
e5 <- 
  d %>% 
  filter(signal %in% c("Depression", "VerbalRequest")) %>% 
  mutate(likelylendmoneyt1 = likelylendmoneyt1/100,
         likelylendmoneyt2 = likelylendmoneyt2/100,
         needsmoneyt1 = needsmoneyt1/100,
         needsmoneyt2 = needsmoneyt2/100,
         signal = factor(signal, levels = c("VerbalRequest", "Depression"))) %>% 
  lm(needsmoneyt2 ~ angryt2 + signal + needsmoneyt1, family = binomial, data =.)
summary(e5)
plot(allEffects(e5))

#how sizable the cost(second mc(t2 significant by very small increase in r-squared))
#beleive health
#how difficult putting oneself in this scenario
#how close felt (MC1) (r-squared .31)
e6 <- 
  d %>% 
  filter(signal %in% c("Depression", "VerbalRequest")) %>% 
  mutate(likelylendmoneyt1 = likelylendmoneyt1/100,
         likelylendmoneyt2 = likelylendmoneyt2/100,
         needsmoneyt1 = needsmoneyt1/100,
         needsmoneyt2 = needsmoneyt2/100,
         MC1.2_1 = MC1.2_1/100,
         MC2.2_1 = MC2.2_1/100,
         believehealtht1 = believehealtht1/100,
         believehealtht2 = believehealtht2/100,
         believeneedt1 = believeneedt1/100,
         believeneedt2 = believeneedt2/100,
         MC1.1_1 = MC1.1_1/100,
         MC2.1_1 = MC2.1_1/100,
         MC2.4_1 = MC2.4_1/100,
         daughterharmt1 = daughterharmt1/100,
         daughterharmt2 = daughterharmt2/100,
         signal = factor(signal, levels = c("VerbalRequest", "Depression"))) %>% 
  lm(needsmoneyt2 ~ MC2.1_1 + believeneedt2 + believehealtht2, data =.) #w/o signal .637
summary(e6)
plot(allEffects(e6))


