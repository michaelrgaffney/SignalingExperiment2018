
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
library(broom)
library(hagenutils)
library(gapmap)
library(UpSetR)
library(patchwork)

#+ message=F,warning=F,fig.width=10,fig.height=10

# Recoding MC2.3: How does your sister feel?

signalingdata2018 <- 
  signalingdata2018 %>%
  dplyr::filter(State != 'I do not reside in the United States') %>% 
  mutate(
    closesistert1 = MC1.1_1,
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
    NoneOfAbove = ifelse(str_detect(MC2.3, 'None'), 1, 0),
    Siblings2 = as.numeric(ifelse(Siblings == "More than 10", "11", Siblings)),
    Ed2 <- ordered(
      Ed,
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
    ),
    angryt1 = 100 - angryt1,
    howsadt1 = 100 - howsadt1,
    daughterharmt1 = 100 - daughterharmt1
  ) %>% 
  dplyr::select(-MC2.3)

# Years of education

ed2years <-
  c(
  "Some high school (secondary school), no diploma" = 11,
  "GED" = 13,
  "High school (secondary school)" = 13,
  "Associate degree" = 15,
  "Bachelor's degree" = 17,
  "Professional degree" = 19,
  "Master's degree" = 19,
  "Doctorate" = 24
)

signalingdata2018$years_education <- ed2years[signalingdata2018$Ed]

# All signals and conditions

signaldict <-
  c(
    'Schizophrenia:Sister' = 'Schizophrenia',
    'VerbalRequest:Sister' = 'VerbalRequest',
    'Anger:Sister' = 'Anger',
    'FacialSadnesswithCrying:Sister' = 'Crying',
    'Depression:Sister' = 'Depression',
    'DepressionwithSuicideThreat:Sister' = 'Depression&Suicidal',
    'SuicideAttempt:Sister' = 'Suicide attempt',
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
    delta_needs_money = needsmoneyt2 - 50,
    delta_lend = likelylendmoneyt2 - 50
  )

m <- lm(delta_needs_money ~ signal - 1, d0)
mc <- names(sort(coef(m)))
mc <- str_replace(mc, 'signal', '')
d0$signal2 <- factor(d0$signal, levels = mc)
mT2need <- lm(delta_needs_money ~ signal2 - 1, d0)
pT2need <- visreg(mT2need, partial=F, gg = T, rug = F)

# PCA of t1 vars

needvarsT1 <-
  c(
    "needsmoneyt1",
    "likelylendmoneyt1",
    "angryt1",
    "satisfactiont1",
    "howsadt1",
    "howreasonablet1",
    "believehealtht1",
    # "daughterharmt1",
    "believeneedt1",
    "sisterbenefitt1",
    "trustrepayt1",
    "comfortablelendingt1",
    "closesistert1"
  )

cc <- complete.cases(d0[needvarsT1])
pcaT1 <- prcomp(d0[cc, needvarsT1], scale. = T)

d0$PC1t1 <- NA
d0$PC1t1[cc] <- pcaT1$x[,1]

# likelylendmoneyt2 = likelylendmoneyt2/100,
# needsmoneyt1 = needsmoneyt1/100,
# needsmoneyt2 = needsmoneyt2/100,

# pca_loadings_plot(m)

biplotT1 <- 
  autoplot(
    pcaT1, 
    loadings = T, 
    loadings.label = T, 
    data = d0[cc,], 
    alpha = 0.25
  ) +
  theme_bw()

autoplot(
  pcaT1, 
  loadings = T, 
  loadings.label = T, 
  data = d0[cc,], 
  colour = 'conflict', 
  frame.type = 'norm'
) +
  theme_bw()

autoplot(
  pcaT1, 
  loadings = T, 
  loadings.label = T, 
  data = d0[cc,], 
  colour = 'p_info', 
  frame.type = 'norm'
  ) +
  theme_bw()

# Effect of t1 conflict and private info on perceived need and PC1t1

mT1manipulation <- lm(-PC1t1 ~ conflict + p_info, d0)
Anova(mT1manipulation)

pT1a <- visreg(mT1manipulation, xvar = 'conflict', gg = T) + 
  scale_y_continuous(limits = c(-8, 5)) +
  labs(title = 'Conflict effect', x = '', y = 'PC1 (time 1)') +
  theme_bw()

pT1b <- visreg(mT1manipulation, xvar = 'p_info', gg = T) + 
  scale_y_continuous(limits = c(-8, 5)) +
  labs(title = 'Information effect', x = '', y = '') +
  theme_bw()

mT1interaction <- lm(-PC1t1 ~ conflict * p_info, d0[d0$p_info != 'Cheating',])
mT1interaction_anova <- Anova(mT1interaction, type = 3)

m <- lm(delta_needs_money ~ conflict + p_info, d0)
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

mT2pc1 <- lm(PC1t2all ~ PC1t1 + signal2, d0)
pT2pc1 <- visreg(mT2pc1, xvar = 'signal2', partial=F, gg = T, rug = F)
pT2pc1 <-
  pT2pc1 +
  # geom_hline(yintercept = mean(d0$comfortablelendingt1, na.rm=T), linetype = 'dotted') + 
  labs(
    title = "D. Signal effect on PC1",
    # subtitle = paste('N =', nobs(m)),
    x = '',
    y = "") +
  coord_flip() +  
  theme_bw() +
  theme(axis.title.y = element_blank(), axis.text.y=element_blank())
pT2pc1

# Main effects plots

pT2need <-
  pT2need +
  geom_hline(yintercept = 0, linetype = 'dotted') + 
  labs(
    title = "C. Change in perception of sister's need",
    # subtitle = paste('N =', nobs(m)),
    x = '', 
    y = "Change in perception of sister's need for money") +
  coord_flip() + 
  theme_bw()
pT2need

mT2lend <- lm(delta_lend ~ signal2 - 1, d0)
# mc <- names(sort(coef(m)))
# mc <- str_replace(mc, 'signal', '')
# d0$signal2 <- factor(d0$signal, levels = mc)
# m <- lm(delta_lend ~ signal2 - 1, d0)
pT2lend <- visreg(mT2lend, partial=F, gg = T, rug = F)

pT2lend <-
  pT2lend +
  geom_hline(yintercept = 0, linetype = 'dotted') + 
  labs(
    title = "B. Change in likelihood of lending money",
    # subtitle = paste('N =', nobs(m)),
    x = '', 
    y = "Change in likelihood of lending money") +
  coord_flip() +  
  theme_bw() +
  theme(axis.title.y = element_blank(), axis.text.y=element_blank())
pT2lend

mT2comfort <- lm(comfortablelendingt2 ~ comfortablelendingt1 + signal2 - 1, d0)
pT2comfort <- visreg(mT2comfort, xvar = 'signal2', partial=F, gg = T, rug = F)
pT2comfort <-
  pT2comfort +
  geom_hline(yintercept = mean(d0$comfortablelendingt1, na.rm=T), linetype = 'dotted') + 
  labs(
    title = "A. Amount of money comfortable lending",
    # subtitle = paste('N =', nobs(m)),
    x = '',
    y = "Amount comofortable lending in US dollars") +
  coord_flip() +  
  theme_bw()
pT2comfort

# Preregistered interaction models

interactplot <- function(f, trm, ylab, removeLegend = F, removeY = F){
  m <- lm(formula = f, d0)
  vdf <- visreg(m, xvar='signal2', by = trm, plot = F)
  
  trm <- sym(trm)
  
  p <- ggplot(vdf$fit, aes(signal2, visregFit, colour = !!trm)) + 
    geom_point(position = position_dodge(width = 0.3)) +
    geom_linerange(aes(ymin = visregLwr, ymax = visregUpr), position = position_dodge(width = 0.3)) +
    geom_hline(yintercept = 0, linetype = 'dotted') +
    labs(title = ylab, x = '', y = '') +
    coord_flip() +
    theme_bw()
  
  if (removeLegend) {
    p <- p + theme(legend.position = "none")
  }
  if (removeY){
    p <- p + theme(axis.title.y = element_blank(), axis.text.y=element_blank())
  }
  return(p)
}

# signal X p_info interactios
p_comfort_signal_pinfo <- 
  interactplot(comfortablelendingt2 ~ comfortablelendingt1 + signal2 * p_info - 1, 'p_info', '\nA. Amount comfortable lending at T2', removeLegend=T)
p_lend_signal_pinfo <- 
  interactplot(delta_lend ~ signal2 * p_info - 1, 'p_info', '\nB. Change in likelihood of lending money', removeY = T)
p_pc1_signal_pinfo <- 
  interactplot(PC1t2all ~ PC1t1 +signal2 * p_info - 1, 'p_info', '\nC. PC1 at T2', removeLegend = T)
p_money_signal_pinfo <- 
  interactplot(delta_needs_money ~ signal2 * p_info - 1, 'p_info', '\nD. Change in perceived need for money', removeY = T)

# (p_comfort_signal_pinfo + p_lend_signal_pinfo + scale_y_continuous(limits = c(-40, 20)))/(p_pc1_signal_pinfo + p_money_signal_pinfo + scale_y_continuous(limits = c(-40, 20)))

# signal X conflict interactios
p_comfort_signal_conflict <- 
  interactplot(comfortablelendingt2 ~ comfortablelendingt1 + signal2 * conflict - 1, 'conflict', '\nA. Amount comfortable lending at T2', removeLegend=T)
p_lend_signal_conflict <- 
  interactplot(delta_lend ~ signal2 * conflict - 1, 'conflict', '\nB. Change in likelihood of lending money', removeY = T)
p_pc1_signal_conflict <- 
  interactplot(PC1t2all ~ PC1t1 + signal2 * conflict - 1, 'conflict', '\nC. PC1 at T2', removeLegend = T)
p_money_signal_conflict <- 
  interactplot(delta_needs_money ~ signal2 * conflict - 1, 'conflict', '\nD. Change in perceived need for money', removeY = T)

# (p_comfort_signal_conflict + p_lend_signal_conflict + scale_y_continuous(limits = c(-40, 20)))/(p_pc1_signal_conflict + p_money_signal_conflict + scale_y_continuous(limits = c(-40, 20)))

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

# Retain only VerbalRequest, Crying, Depression
d2 <-
  d %>%
  dplyr::select(
    p_info,
    conflict,
    signal,
    needsmoneyt1,
    delta_needs_money,
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
      delta_needs_money < -1 ~ -1,
      delta_needs_money > 1 ~ 1,
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
    delta_needs_money,
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
      delta_needs_money < -1 ~ -1,
      delta_needs_money > 1 ~ 1,
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

ggplot(d2, aes(needsmoneyt1, delta_needs_money, colour = signal)) +
  geom_point() +
  geom_smooth(span = 2) +
  scale_color_discrete() +
  facet_wrap(~conflict) +
  theme_bw()

p_pc1_t1t2 <- 
  ggplot(d2, aes(-PC1t1, PC1t2, colour = signal)) +
  geom_point() +
  geom_smooth(span = 2) +
  scale_color_discrete() +
  facet_wrap(~conflict) +
  theme_bw()

# Mediation

model.y <- glm(delta_lend ~ needsmoneyt1 + delta_needs_money + signal, family = gaussian, data = d2)
model.m <- lm(delta_needs_money ~ needsmoneyt1 + signal, data = d2)

mediation_model <- mediate(model.m, model.y, treat = 'signal', mediator = 'delta_needs_money', boot = T)
plot(mediation_model)

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

m <- lm(PC1t2 ~ signal * conflict + p_info, data = d2) #pca after conditions filtered
summary(m)
Anova(m, type = 3)
plot(allEffects(m))
Plot_Exploratory <- visreg(m, xvar= "signal", by = "conflict", partial = F, rug = F, gg = T) + theme_bw() + labs(y = "PC1 Time 2", x = "")

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

signalmat <- as.matrix(d_mean_feel[-1])
rownames(signalmat) <- d_mean_feel$signal

heatmap(signalmat, scale = 'none')

## my attempt
distxy <- dist(signalmat) #gets rid of checked answers
hc <- hclust(distxy)
dend <- as.dendrogram(hc)
distxy2 <- dist(t(signalmat))
hc2 <- hclust(distxy2)
dend2 <- as.dendrogram(hc2)

grey_scale =c("#333333", "#5C5C5C", "#757575", "#8A8A8A", "#9B9B9B", "#AAAAAA", "#B8B8B8", "#C5C5C5", "#D0D0D0", "#DBDBDB", "#E6E6E6")
blue_red =c("#053061", "#2166AC", "#4393C3", "#92C5DE", "#D1E5F0", "#F7F7F7","#FDDBC7", "#F4A582", "#D6604D", "#B2182B", "#67001F")
yellow_red =c("#ffff00", "#ffea00", "#ffd400", "#ffbf00", "#ffaa00", "#ff9500", "#ff8000", "#ff6a00", "#ff5500", "#ff4000", "#ff2b00", "#ff1500", "#ff0000")
navajowhite_navy =c("#000080", "#151284", "#2b2587", "#40388b", "#554a8f", "#6a5d93", "#806f97", "#95819a", "#aa949e", "#bfa7a2", "#d4b9a5", "#eacba9", "#ffdead")
#gapmap(m = as.matrix(hclust), d_row= rev(dend), d_col=dend, col = grey_scale)
gapmap(m = signalmat, d_row= rev(dend), d_col=dend2,  mode = "quantitative", mapping="linear", col = yellow_red)

## end my attempt

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
emotions$PC1 <- m$x[,1]
emotions$PC2 <- m$x[,2]

plot(m)
pca_loadings_plot(m)

autoplot(
  m,
  loadings = T,
  loadings.label = T,
  data = emotions
) + theme_bw()

# Order signals by median of PC1
emotions$signal <- fct_reorder(emotions$signal, emotions$PC1)
autoplot(
  m,
  data = emotions,
  colour = 'signal',
  frame.type = 'norm'
) + facet_wrap(~signal) + theme_bw() + labs(title = 'Ordered by PC1')

ggplot(emotions, aes(signal, PC1)) + geom_boxplot() + geom_point() + theme_bw() + coord_flip()


# Order signals by median of PC2
emotions$signal <- fct_reorder(emotions$signal, emotions$PC2)
autoplot(
  m,
  data = emotions,
  colour = 'signal',
  frame.type = 'norm'
) + facet_wrap(~signal) + theme_bw() + labs(title = 'Ordered by PC2')

ggplot(emotions, aes(signal, PC2)) + geom_boxplot() + geom_point() + theme_bw() + coord_flip()


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

# Summary of correlation of all T1 outcome vars except anger

cm <- cm <- cor(d0[needvarsT1[-c(3,5)]], use = 'pairwise.complete.obs')
cmu <- cm[upper.tri(cm)]


# discussion explorations

# It is also a possibility that conflict might increase one’s suspicion of cheating in others, 
# however, because our measure for this is so closely correlated to our other pc1 variables.....
# something which is seen pre-signal with and without controlling for the level of information (c in analysis 2). 

c1 <- glm(believeneedt1 ~ conflict + p_info, data = d0)
summary(c1)
plot(allEffects(c1))

c2 <- glm(believeneedt1 ~ conflict, data = d0)
summary(c2)
plot(allEffects(c2))

# Although we did not ask if participants viewed the sister’s request to be manipulative directly, 
# those in the suicide attempt condition perceived the request as more indicative of an attempt to get 
# the money for other purposes than those who saw less costly signals.
bm <- glm(believeneedt2/100 ~ conflict * signal * p_info, family = binomial, data = d0)
summary(bm)
Anova(bm, type = 3)
plot(allEffects(bm))

#importance of percieved need on instrumental outcomes
# going for r-squards. Is this viable? Should anything else be included?
nm <- lm(likelylendmoneyt2 ~ needsmoneyt2 + conflict + p_info + signal, data = d0)
summary(nm)
plot(allEffects(nm))

nm2 <- lm(comfortablelendingt2 ~ needsmoneyt2, data = d0)
summary(nm2)

#then mention the effect of signal in prediciting needsmoney is small
nm3 <- lm(needsmoneyt2 ~ signal, data = d0)
summary(nm3)
plot(allEffects(nm3))


#testing for demographic effects at time on
d0$Sex <- as.factor(d0$Sex)
d0$Ed <- as.factor(d0$Ed)
d0$Siblings <- as.factor(d0$Siblings)
mdem <- lm(-PC1t1 ~ Sex * conflict + Sex * p_info + Sex * years_education + Sex * Age + Sex * Children + Sex * Siblings2, d0)
summary(mdem)
plot(allEffects(mdem))

# (p_comfort_signal_pinfo + p_lend_signal_pinfo + scale_y_continuous(limits = c(-40, 20)))/(p_pc1_signal_pinfo + p_money_signal_pinfo + scale_y_continuous(limits = c(-40, 20)))
p_ease <- interactplot(MC2.4_1 ~ signal2 * p_info - 1, 'p_info', '\nA. Reported ease of imagining (signal by information)', removeY = F)
p_ease

p_ease2 <- interactplot(MC2.4_1 ~ signal2 * conflict - 1, 'conflict', '\nB. Signal by conflict', removeY = F)
p_ease2

p_anger <- interactplot(angryt2 ~ angryt1 + signal2 * p_info - 1, 'p_info', '\nB. Ease of putting in scenario Reported ease of imagining (signal by conflict)', removeY = F)
p_anger

p_believeneed <- interactplot(believeneedt2 ~ believeneedt1 + signal2 * p_info - 1, 'p_info', '\nD. Ease of putting in scenario', removeY = F)
p_believeneed

# for analysis2
d0$signal <- factor(d0$signal, levels = c('Schizophrenia', 'Control', 'VerbalRequest', 'Crying', 'Anger', 'Depression', 'Depression&Suicidal', 'Suicide attempt'))
full_int_overview <- glm(delta_lend ~ signal * conflict * p_info, data = d0)
plot(allEffects(full_int_overview))


overviewa <- visreg(full_int_overview, xvar= "signal", by = "p_info", cond = list(conflict = "Conflict"), partial = F, rug = F, gg = T) +
  theme_bw() + labs(y = "PC1 Time 2", x = "", title = "A. Conflict") + coord_flip()

overviewb <- visreg(full_int_overview, xvar= "signal", by = "p_info", cond = list(conflict = "Support"), partial = F, rug = F, gg = T) +
  theme_bw() + labs(y = "PC1 Time 2", x = "", title = "B. Support") +coord_flip()


# Custom ggplot
vis_conflict <- visreg(full_int_overview, xvar= "signal", by = "p_info", cond = list(conflict = "Conflict"), plot = F)
vis_support <- visreg(full_int_overview, xvar= "signal", by = "p_info", cond = list(conflict = "Support"), plot = F)
vis_overview <- rbind(vis_conflict$fit, vis_support$fit)

p <- 
  ggplot(vis_overview, aes(signal, visregFit, colour=p_info)) + 
  geom_point(position = position_dodge(width = 0.3)) +
  geom_linerange(aes(ymin = visregLwr, ymax = visregUpr), position = position_dodge(width = 0.3)) +
  geom_hline(yintercept = 0, linetype = 'dotted') +
  facet_wrap(~conflict) +
  labs(title = 'Overview', x = '', y = '') +
  coord_flip() +
  theme_bw()
p
  
p <- 
  ggplot(vis_overview, aes(signal, visregFit, colour=p_info, shape=conflict)) + 
  geom_point(position = position_dodge(width = 0.7), size = 3) +
  geom_linerange(aes(ymin = visregLwr, ymax = visregUpr), position = position_dodge(width = 0.7), size = 1) +
  geom_hline(yintercept = 0, linetype = 'dotted') +
  labs(title = 'Overview', x = '', y = '\nLikely to lend money (time 2)') +
  coord_flip() +
  theme_bw(15)
p

  
#Plot_full_int
#full_int_overview2 <- glm(likelylendmoneyt2 ~ signal * conflict * p_info, data = d0)
#plot(allEffects(full_int_overview2))

# Upset plot

d0 %>%
  dplyr::select(signal, Angry:NoneOfAbove) %>% 
  mutate_if(is.numeric, as.integer) %>% 
  as.data.frame %>%
  upset(order.by = 'freq', nsets = 12)

# Control condition only
d0 %>%
  dplyr::select(signal, Angry:NoneOfAbove) %>% 
  dplyr::filter(signal == 'Control') %>% 
  mutate_if(is.numeric, as.integer) %>% 
  as.data.frame %>%
  upset(order.by = 'freq', nsets = 12, nintersect = 10)

d0 %>%
  dplyr::select(signal, Angry:NoneOfAbove) %>% 
  dplyr::filter(signal == 'Depression&Suicidal') %>% 
  mutate_if(is.numeric, as.integer) %>% 
  as.data.frame %>%
  upset(order.by = 'freq', nsets = 12, nintersect = 10)
