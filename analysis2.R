
#+ message=F,warning=F

library(signalingdata2018)
library(tidyverse)
library(mediation)
library(visreg)
library(car)
library(effects)
library(glmnet)
library(MASS)
library(ggmosaic)
library(ggcorrplot)
library(ggfortify)
library(naniar)
# library(hagenutils)

#+ message=F,warning=F,fig.width=10,fig.height=10

# Recoding MC2.3: How does your sister feel?

signalingdata2018 <- 
  signalingdata2018 %>%
  mutate(
    MC2.3 = ifelse(is.na(MC2.3), 'None', MC2.3),
    Angry = ifelse(str_detect(MC2.3, 'Angry'), 1, 0),
    Sad = ifelse(str_detect(MC2.3, 'Sad'), 1, 0),
    Suicidal = ifelse(str_detect(MC2.3, 'Suicidal'), 1, 0),
    MentallyIll = ifelse(str_detect(MC2.3, 'Mentally ill'), 1, 0),
    Depressed = ifelse(str_detect(MC2.3, 'Depressed'), 1, 0),
    OutOfOptions = ifelse(str_detect(MC2.3, 'Out of alternative options'), 1, 0),
    Happy = ifelse(str_detect(MC2.3, 'Happy'), 1, 0),
    Neutral = ifelse(str_detect(MC2.3, 'Neutral'), 1, 0),
    Scared = ifelse(str_detect(MC2.3, 'Scared'), 1, 0),
    Tired = ifelse(str_detect(MC2.3, 'Tired'), 1, 0),
    Distressed = ifelse(str_detect(MC2.3, 'Distressed'), 1, 0),
    NoneOfAbove = ifelse(str_detect(MC2.3, 'None'), 1, 0)
  ) %>% 
  dplyr::select(-MC2.3)

# All signals and conditions

signaldict <-
  c(
    'Schizophrenia:Sister' = 'Schizophrenia',
    'VerbalRequest:Sister' = 'VerbalRequest',
    'Anger:Sister' = 'Anger',
    'FacialSadnesswithCrying:Sister' = 'Crying',
    'Depression:Sister' = 'Depression',
    'DepressionwithSuicideThreat:Sister' = 'Depression&Suicidal',
    'SuicideAttempt:Sister' = 'Suicidal',
    'Control:Sister' = 'Control'
  )

d0 <-
  signalingdata2018 %>%
  filter(!exclusionopt1) %>%
  dplyr::select(
    -starts_with('Recipient'), 
    -ExternalReference,
    -starts_with('AC'),
    -starts_with('FL_')
    ) %>% 
  mutate(
    signal = factor(signaldict[signal]),
    p_info = ordered(p_info, levels = c('Cheating', 'PrivateInformation', 'Honest')),
    conflict = factor(conflict, levels = c('Conflict', 'Support')),
    needsmoneyt2 = ifelse(is.na(needsmoneyt2), 50, needsmoneyt2),
    likelylendmoneyt2 = ifelse(is.na(likelylendmoneyt2), 50, likelylendmoneyt2),
    angryt2 = ifelse(is.na(angryt2), 50, angryt2),
    satisfactiont2 = ifelse(is.na(satisfactiont2), 50, satisfactiont2),
    howreasonablet2 = ifelse(is.na(howreasonablet2), 50, howreasonablet2),
    believehealtht2 = ifelse(is.na(believehealtht2), 50, believehealtht2),
    believeneedt2 = ifelse(is.na(believeneedt2), 50, believeneedt2),
    sisterbenefitt2 = ifelse(is.na(sisterbenefitt2), 50, sisterbenefitt2),
    trustrepayt2 = ifelse(is.na(trustrepayt2), 50, trustrepayt2),
    daughterharmt2 = ifelse(is.na(daughterharmt2), 50, daughterharmt2),
    howsadt2 = ifelse(is.na(howsadt2), 50, howsadt2),
    delta_money = needsmoneyt2 - 50,
    delta_lend = likelylendmoneyt2 - 50
  )

m <- lm(delta_money ~ signal - 1, d0)
mc <- names(sort(coef(m)))
mc <- str_replace(mc, 'signal', '')
d0$signal2 <- factor(d0$signal, levels = mc)
m <- lm(delta_money ~ signal2 - 1, d0)
p <- visreg(m, partial=F, gg = T, rug = F)

p <-
  p +
  geom_hline(yintercept = 0, linetype = 'dotted') + 
  labs(
    title = "Mean change in perceived need across all conditions",
    subtitle = paste('N =', nobs(m)),
    x = '', 
    y = 'Change in perceived need') +
  coord_flip() + 
  theme_bw()
p

# likelylendmoneyt2 = likelylendmoneyt2/100,
# needsmoneyt1 = needsmoneyt1/100,
# needsmoneyt2 = needsmoneyt2/100,


# PCA of t1 vars

needvarsT1 <-
  c(
    "needsmoneyt1",
    "likelylendmoneyt1",
    "angryt1",
    "satisfactiont1",
    # "howsadt1",
    "howreasonablet1",
    "believehealtht1",
    # "daughterharmt1",
    "believeneedt1",
    "sisterbenefitt1",
    "trustrepayt1",
    "comfortablelendingt1",
    "MC1.1_1"
    )

cc <- complete.cases(d0[needvarsT1])
m <- prcomp(d0[cc, needvarsT1], scale. = T)

d0$PC1t1 <- NA
d0$PC1t1[cc] <- m$x[,1]

# pca_loadings_plot(m)

autoplot(
  m, 
  loadings = T, 
  loadings.label = T, 
  data = d0[cc,], 
  colour = 'conflict', 
  frame.type = 'norm'
) +
  theme_bw()

autoplot(
  m, 
  loadings = T, 
  loadings.label = T, 
  data = d0[cc,], 
  colour = 'p_info', 
  frame.type = 'norm'
  ) +
  theme_bw()

# Effect of t1 conflict and private info on perceived need and PC1t1

m <- lm(-PC1t1 ~ conflict + p_info, d0)
Anova(m)
plot(allEffects(m))

m <- lm(delta_money ~ conflict + p_info, d0)
Anova(m)
plot(allEffects(m))

# PCA of t2 vars

needvarsT2 <-
  c(
    "needsmoneyt2",
    "likelylendmoneyt2",
    "angryt2",
    "satisfactiont2",
    # "howsadt2",
    "howreasonablet2",
    "believehealtht2",
    # "daughterharmt2",
    "believeneedt2",
    "sisterbenefitt2",
    "trustrepayt2",
    "comfortablelendingt2",
    "MC2.1_1"
  )

cc <- complete.cases(d0[needvarsT2])
m <- prcomp(d0[cc, needvarsT2], scale. = T)

d0$PC1t2all <- NA
d0$PC1t2all[cc] <- -m$x[,1]

# Filter out Schizophrenia, Anger, Control, Depression&Suicidal, Cheating
d <-
  d0 %>% 
  dplyr::filter(
    ! signal %in% c('Schizophrenia', 'Anger', 'Control', 'Depression&Suicidal', 'Suicidal')
  ) %>% 
  mutate(signal = fct_drop(signal))

cc <- complete.cases(d[needvarsT2])
m <- prcomp(d[cc, needvarsT2], scale. = T)

d$PC1t2 <- NA
d$PC1t2[cc] <- -m$x[,1]

autoplot(m, loadings = T, loadings.label = T, data = d[cc,], colour = 'signal', frame.type = 'norm') + theme_bw()

# PCA of all need vars

df <- d0 %>% dplyr::filter(signal %in% c('VerbalRequest', 'Crying', 'Depression'))
# df <- d %>% dplyr::filter(signal %in% c('VerbalRequest', 'Suicidal'))

cc <- complete.cases(df[c(needvarsT1, needvarsT2)])
m <- prcomp(df[cc, c(needvarsT1, needvarsT2)], scale. = T)
# pca_loadings_plot(m)
autoplot(m, loadings = T, loadings.label = T, data = df[cc,], colour = 'signal', frame.type = 'norm') + theme_bw()

# Models

m <- lm(PC1t2 ~ PC1t1 + signal, d)
plot(Effect('signal', m), main = 'PC1t1 + signal')

# Retain only VerbalRequest, Crying, Depression
d2 <-
  d %>%
  dplyr::select(
    p_info,
    conflict,
    signal,
    needsmoneyt1,
    delta_money,
    delta_lend,
    PC1t1,
    PC1t2
    ) %>%
  dplyr::filter(
    p_info != 'Cheating',
    signal %in% c(
      'VerbalRequest',
      'Crying',
      'Depression')
    ) %>%
  mutate(
    p_info = ordered(as.character(p_info), levels = c('PrivateInformation', 'Honest')),
    signal = ordered(signal, levels = c('VerbalRequest', 'Crying', 'Depression')),
    delta_need = case_when(
      delta_money < -1 ~ -1,
      delta_money > 1 ~ 1,
      TRUE ~ 0
    ),
    delta_need2 = case_when(
      PC1t2 < (mean(PC1t2, na.rm=T) - sd(PC1t2, na.rm=T)/2) ~ -1,
      PC1t2 > mean(PC1t2, na.rm=T) + sd(PC1t2, na.rm=T)/2 ~ 1,
      TRUE ~ 0
    ),
    delta_need = ordered(delta_need),
    delta_need2 = ordered(delta_need2)
  ) %>% 
  na.omit

# Retain only Control, Depression
d2b <-
  d0 %>%
  dplyr::select(
    p_info,
    conflict,
    signal,
    needsmoneyt1,
    delta_money,
    delta_lend,
    PC1t1,
    PC1t2all
  ) %>%
  dplyr::filter(
    # p_info != 'Cheating',
    signal %in% c(
      'Control',
      'Depression')
  ) %>%
  mutate(
    p_info = ordered(as.character(p_info), levels = c('Cheating', 'PrivateInformation', 'Honest')),
    signal = factor(signal, levels = c('Control', 'Depression')),
    delta_need = case_when(
      delta_money < -1 ~ -1,
      delta_money > 1 ~ 1,
      TRUE ~ 0
    ),
    delta_need2 = case_when(
      PC1t2all < (mean(PC1t2all, na.rm=T) - sd(PC1t2all, na.rm=T)/2) ~ -1,
      PC1t2all > mean(PC1t2all, na.rm=T) + sd(PC1t2all, na.rm=T)/2 ~ 1,
      TRUE ~ 0
    ),
    delta_need = ordered(delta_need),
    delta_need2 = ordered(delta_need2)
  ) %>% 
  na.omit

# Scatterplots

ggplot(d2, aes(needsmoneyt1, delta_money, colour = signal)) +
  geom_point() +
  geom_smooth(span = 2) +
  scale_color_discrete() +
  facet_wrap(~conflict) +
  theme_bw()

ggplot(d2, aes(-PC1t1, PC1t2, colour = signal)) +
  geom_point() +
  geom_smooth(span = 2) +
  scale_color_discrete() +
  facet_wrap(~conflict) +
  theme_bw()

# Mediation

model.y <- glm(delta_lend ~ needsmoneyt1 + delta_money + signal, family = gaussian, data = d2)
model.m <- lm(delta_money ~ needsmoneyt1 + signal, data = d2)

# m <- mediate(model.m, model.y, treat = 'signal', mediator = 'delta_money', boot = T)
# plot(m)

# Exploratory models

# Main effects on PC1t2 only
m <- lm(PC1t2 ~ signal + conflict + p_info, data = d2)
Anova(m)
plot(allEffects(m))

# Main effects on PC1t2 only, controlling for PC1t1
m <- lm(PC1t2 ~ PC1t1 + signal + conflict + p_info, data = d2)
Anova(m)
plot(allEffects(m))

# Full interaction model
m <- lm(PC1t2 ~ signal * conflict * p_info, data = d2)
Anova(m, type = 3)
plot(allEffects(m))

# Full interaction model, controlling for PC1t1
m <- lm(PC1t2 ~ PC1t1 + signal * conflict * p_info, data = d2)
Anova(m, type = 3)
plot(Effect(c("signal", "conflict", "p_info"), m))

# Final exploratory model of PC1t2?
# signal * conflict interaction
# p_info main effect only

m <- lm(PC1t2 ~ signal * conflict + p_info, data = d2)
Anova(m, type = 3)
plot(allEffects(m))

m <- lm(PC1t2all ~ signal * conflict, data = d2b)
Anova(m, type = 3)
plot(allEffects(m))

# Ordinal logistic of delta_need (based on needsmoneyt2)

# Main effects only
m <- polr(delta_need ~ signal+p_info+conflict, d2, Hess = T)
plot(Effect('signal', m))

# Full interaction model
m <- polr(delta_need ~ signal*conflict + p_info, d2, Hess = T)
plot(allEffects(m))

# Mosaic plots

ggplot(data = d2) +
  geom_mosaic(aes(x = product(delta_need, signal), fill = delta_need)) +
  labs(x='', y='', title='Post signal reaction')

ggplot(data = d2) +
  geom_mosaic(aes(x = product(delta_need, signal), fill = delta_need)) +
  facet_grid(conflict~p_info) +
  labs(x='', y='', title='Post signal reaction')

# Heatmap

p <-
  d %>%
  dplyr::select(
    needvarsT1,
    needvarsT2,
    signal,
    conflict,
    p_info,
    Age,
    Sex,
    Siblings,
    `Duration (in seconds)`,
    LocationLatitude
    ) %>%
  dplyr::filter(
    signal %in% c(
      'VerbalRequest',
      'Crying',
      'Depression'
    )
  ) %>%
  mutate(
    conflict = as.numeric(conflict),
    p_info = as.numeric(p_info),
    signal = as.numeric(ordered(signal, levels = c('VerbalRequest', 'Crying', 'Depression'))),
    Sex = ifelse(Sex == 'Female', 0, 1),
    Siblings = ifelse(Siblings == '0', 0, 1),
    LocationLatitude = abs(LocationLatitude)
  ) %>%
  na.omit %>%
  cor(use = 'pairwise.complete.obs') %>%
  ggcorrplot(hc.order = T, hc.method = 'ward')
p

# Note: p_info correlates with T2 vars more than does conflict

# Elasticnet model of likelylendmoneyt2 as function of T1 need vars
x <- d[c(needvarsT1, 'likelylendmoneyt2')]
x <- as.matrix(na.omit(x))

cv <- cv.glmnet(x[,1:11], x[,12])
coef(cv)

d0 %>% 
  dplyr::select(Angry:NoneOfAbove) %>% 
  cor(use = 'pairwise.complete.obs') %>%
  ggcorrplot(hc.order = T, hc.method = 'ward')

d_mean_feel <-
  d0 %>% 
  dplyr::select(signal, Angry:NoneOfAbove) %>% 
  group_by(signal) %>% 
  summarise_all(mean, na.rm=T) 

mat <- as.matrix(d_mean_feel[-1])
rownames(mat) <- d_mean_feel$signal

heatmap(mat, scale = 'none')

lvls = names(d_mean_feel[-1])[order(as.numeric(d_mean_feel[2,-1]))]

d_mean_long <-
  d_mean_feel %>% 
  gather(key = Emotion, value = Mean, -signal) %>% 
  mutate(
    Emotion = factor(Emotion, levels = lvls)
  )

ggplot(d_mean_long, aes(Emotion, Mean, group = signal)) + 
  geom_line() + 
  facet_wrap(~signal, ncol=1)

# PCA of perceived emotions

emotions <-
  d0 %>% 
  dplyr::select(signal,Angry:NoneOfAbove)

m <- prcomp(emotions[-1], scale. = F)
plot(m)
# pca_loadings_plot(m)

autoplot(
  m, 
  loadings = T, 
  loadings.label = T, 
  data = emotions, 
  colour = 'signal',
  frame.type = 'norm'
) +
  theme_bw()

# Perceived emotions vs. PC1t2

d3 <-
  d0 %>% 
  mutate_at(vars(Angry:NoneOfAbove), factor)

m <- lm(PC1t2all ~ Depressed + Sad + MentallyIll, d3)
Anova(m)
plot(allEffects(m))
