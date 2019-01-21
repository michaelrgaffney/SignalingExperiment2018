
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

#+ message=F,warning=F,fig.width=10,fig.height=10

signaldict <-
  c(
    'VerbalRequest:Sister' = 'VerbalRequest',
    'Anger:Sister' = 'Anger',
    'FacialSadnesswithCrying:Sister' = 'Crying',
    'Depression:Sister' = 'Depression',
    'DepressionwithSuicideThreat:Sister' = 'Depression&Suicidal',
    'SuicideAttempt:Sister' = 'Suicidal'
  )

d <-
  signalingdata2018 %>%
  filter(
    !exclusionopt1,
    signal %in% c(
      'Anger:Sister',
      'Depression:Sister',
      'VerbalRequest:Sister',
      'FacialSadnesswithCrying:Sister',
      'DepressionwithSuicideThreat:Sister',
      'SuicideAttempt:Sister')
  ) %>%
  mutate(
    signal = factor(signaldict[signal], levels = signaldict),
    likelylendmoneyt2 = likelylendmoneyt2/100,
    needsmoneyt1 = needsmoneyt1/100,
    needsmoneyt2 = needsmoneyt2/100,
    p_info = ordered(p_info, levels = c('Cheating', 'PrivateInformation', 'Honest')),
    conflict = factor(conflict, levels = c('Conflict', 'Support'))
  )

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

cc <- complete.cases(d[needvarsT1])
m <- prcomp(d[cc, needvarsT1], scale. = T)

d$PC1t1 <- NA
d$PC1t1[cc] <- m$x[,1]

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

cc <- complete.cases(d[needvarsT2])
m <- prcomp(d[cc, needvarsT2], scale. = T)

d$PC1t2 <- NA
d$PC1t2[cc] <- -m$x[,1]

# PCA of all need vars

df <- d %>% dplyr::filter(signal %in% c('VerbalRequest', 'Crying', 'Depression'))
# df <- d %>% dplyr::filter(signal %in% c('VerbalRequest', 'Suicidal'))

cc <- complete.cases(df[c(needvarsT1, needvarsT2)])
m <- prcomp(df[cc, c(needvarsT1, needvarsT2)], scale. = T)
# pca_loadings_plot(m)
autoplot(m, loadings = T, loadings.label = T, data = df[cc,], colour = 'signal', frame.type = 'norm') + theme_bw()

# Scatterplots

ggplot(d2, aes(needsmoneyt1, needsmoneyt2, colour = signal)) +
  geom_point() +
  geom_smooth(span = 2) +
  scale_color_discrete() +
  facet_wrap(~conflict) +
  theme_bw()

ggplot(df, aes(-PC1t1, PC1t2, colour = signal)) +
  geom_point() +
  geom_smooth(span = 2) +
  scale_color_discrete() +
  facet_wrap(~conflict) +
  theme_bw()

d %>%
  filter(!signal %in% c('Anger', 'Depression&Suicidal')) %>%
  ggplot(aes(needsmoneyt1, needsmoneyt2)) +
  geom_point() +
  geom_density2d() +
  scale_color_discrete() +
  coord_fixed() +
  facet_grid(conflict~signal) +
  theme_bw()

d %>%
  filter(!signal %in% c('Anger', 'Depression&Suicidal')) %>%
  ggplot(aes(-PC1t1, PC1t2)) +
    geom_point() +
    geom_density2d() +
    scale_color_discrete() +
    coord_fixed() +
    facet_grid(conflict~signal) +
    theme_bw()


# Models

m <- lm(PC1t2 ~ PC1t1 + signal, d)
plot(Effect('signal', m), main = 'PC1t1 + signal')

# Retain only VerbalRequest, Crying, Depression. Omit Cheating.
d2 <-
  d %>%
  dplyr::select(
    p_info,
    conflict,
    signal,
    needsmoneyt1,
    needsmoneyt2,
    likelylendmoneyt2,
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
    p_info = factor(as.character(p_info), levels = c('PrivateInformation', 'Honest')),
    signal = ordered(signal, levels = c('VerbalRequest', 'Crying', 'Depression')),
    delta_need = case_when(
      needsmoneyt2 < 0.49 ~ -1,
      needsmoneyt2 > 0.51 ~ 1,
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

# Mediation

model.y <- glm(likelylendmoneyt2 ~ needsmoneyt1 + needsmoneyt2 + signal, family = gaussian, data = d2)
model.m <- lm(needsmoneyt2 ~ needsmoneyt1 + signal, data = d2)

m <- mediate(model.m, model.y, treat = 'signal', mediator = 'needsmoneyt2', boot = T)
plot(m)

# Exploratory models

# Main effects on PC1t2 only
m <- lm(PC1t2 ~ signal + conflict + p_info, data = d2)
plot(allEffects(m))

# Main effects on PC1t2 only, controlling for PC1t1
m <- lm(PC1t2 ~ PC1t1 + signal + conflict + p_info, data = d2)
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

# Ordinal logistic of delta_need (based on needsmoneyt2)

# Main effects only
m <- polr(delta_need ~ signal+p_info+conflict, d2, Hess = T)
plot(Effect('signal', m))

# Full interaction model
m <- polr(delta_need ~ signal*p_info*conflict, d2, Hess = T)
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

# Elasticnet model of likelylendmoneyt2 as function of T1 need vars
x <- d[c(needvarsT1, 'likelylendmoneyt2')]
x <- as.matrix(na.omit(x))

cv <- cv.glmnet(x[,1:11], x[,12])
coef(cv)
