
# Setup environment

#libraries
library(tidyverse)
library(lubridate)
library(patchwork)
library(pastecs)
library(e1071)
library(lme4)
library(lmerTest)
library(performance)
library(see)
library(qqplotr)
library(sjPlot)
library(effectsize)

theme_lena <-
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(size = 18, hjust = .5),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    legend.title = element_text(size = 18), 
    legend.text = element_text(size = 16),
    legend.position = "bottom"
  )


################################
# STUDY 1 DATA
################################

VDemo <- read.csv("~/Dropbox/Research/Vanderbilt/BABIES/Data/Vanderbilt Data/Demographics/Filled6moDemo_20210318.csv")
VLENAL <- read.csv("~/Dropbox/Research/Vanderbilt/BABIES/Data/Vanderbilt Data/LENA Data/ConvInitData_Long.csv")

VLENALclean <- VLENAL %>%
  mutate(AIconvo = case_when(Label == "adultinit" ~ 1,
                               Label == "childinit" ~ 0))  %>%
  full_join(VDemo,
            by = "ID") %>%
  mutate(
    baby_dob = parse_date(baby_dob, '%m/%d/%y'),
    LENAage = (baby_dob %--% date_record) / months(1)) %>%
  filter(ID < 4000 & ID > 1000) %>%
  filter(LENAage < 8.0 & LENAage >= 5.0) %>%
  mutate(durationT = log(duration))

write.csv(VLENALclean, "~/Dropbox/Research/Vanderbilt/BABIES/Conversations/Analyses/VLENALclean.csv")

VanderbiltCombinedData <- 
  VLENALclean %>%
  group_by(ID,
           Label) %>%
  dplyr::summarize(date_record = first(date_record),
                   reclength = first(reclength),
                   startrec = first(startrec),
                   endrec = first(endrec),
                   LENAage = first(LENAage),
                   duration = mean(duration),
                   turnTaking = mean(turnTaking),
                   adultWordCnt = mean(adultWordCnt),
                   childUttCnt = mean(childUttCnt),
                   n = n()) %>%
  pivot_wider(names_from = Label, values_from = c(duration, 
                                                  turnTaking, 
                                                  adultWordCnt, 
                                                  childUttCnt,
                                                  n)) %>%
  mutate(n_convos = n_adultinit + n_childinit,
         convratio = n_adultinit / n_childinit,
         prop_adultinit = n_adultinit / n_convos,
         prop_childinit = n_childinit / n_convos) %>%
  arrange(ID) %>%
  left_join(VDemo, by = "ID")

write.csv(VanderbiltCombinedData, "~/Dropbox/Research/Vanderbilt/BABIES/Conversations/Analyses/VanderbiltCombinedData.csv")

################################
# STUDY 2 DATA
################################

SDemo <- read.csv("~/Dropbox/Research/Vanderbilt/BABIES/Data/Stanford Data/Scored/S_6m_Demographics.csv")
SLENAL <- read.csv("~/Dropbox/Research/Vanderbilt/BABIES/Data/Stanford Data/LENA Data/StanfordConvInitData_Long.csv")
SMCDI <- read.csv("~/Dropbox/Research/Vanderbilt/BABIES/Data/Stanford Data/Scored/mbcdi_scored_20200729.csv")

SLENALclean <- SLENAL %>%
  mutate(AIconvo = case_when(Label == "adultinit" ~ 1,
                             Label == "childinit" ~ 0)) %>%
  full_join(SDemo,
            by = "ID") %>%
  mutate(baby_dob = parse_date(baby_dob, '%Y-%m-%d'),
         LENAage = (baby_dob %--% date_record) / months(1)) %>%
  full_join(SMCDI, by = "ID") %>%
  filter(LENAage < 8.0 & LENAage >= 5.0) %>%
  filter(!is.na(mbcdi_understand_tot)) %>%
  filter(reclength == 57599.99) %>%
  mutate(durationT = log(duration))

write.csv(SLENALclean, "~/Dropbox/Research/Vanderbilt/BABIES/Conversations/Analyses/SLENALclean.csv")


StanfordCombinedData <- 
  SLENALclean %>%
  group_by(ID,
           Label) %>%
  dplyr::summarize(date_record = first(date_record),
                   reclength = first(reclength),
                   startrec = first(startrec),
                   endrec = first(endrec),
                   LENAage = first(LENAage),
                   duration = mean(duration),
                   turnTaking = mean(turnTaking),
                   adultWordCnt = mean(adultWordCnt),
                   childUttCnt = mean(childUttCnt),
                   n = n()) %>%
  pivot_wider(names_from = Label, values_from = c(duration, 
                                                  turnTaking, 
                                                  adultWordCnt, 
                                                  childUttCnt,
                                                  n)) %>%
  mutate(n_convos = n_adultinit + n_childinit,
         convratio = n_adultinit / n_childinit,
         prop_adultinit = n_adultinit / n_convos,
         prop_childinit = n_childinit / n_convos) %>%
  arrange(ID) %>%
  left_join(SDemo, by = "ID")  %>%
  left_join(SMCDI, by = "ID")

write.csv(StanfordCombinedData, "~/Dropbox/Research/Vanderbilt/BABIES/Conversations/Analyses/StanfordCombinedData.csv")


################################
# MERGE DATA
################################

CombinedData <- VanderbiltCombinedData %>%
  mutate(Site = "Vanderbilt") %>%
  bind_rows(StanfordCombinedData %>%
              mutate(Site = "Stanford") %>%
              rename(male = male.x))

CombineLENA <- VLENALclean %>%
  mutate(Site = "Vanderbilt") %>%
  select(ID,
         Site,
         durationT,
         turnTaking,
         adultWordCnt,
         Label) %>%
  bind_rows(SLENALclean %>%
              mutate(Site = "Stanford")%>%
              select(ID,
                     Site,
                     durationT,
                     turnTaking,
                     adultWordCnt,
                     Label)) %>%
  mutate(Stanford = case_when(Site == "Vanderbilt" ~ 0,
                              Site == "Stanford" ~ 1))

################################
# DEMOGRAPHIC INFORMATION
################################

stat.desc(VanderbiltCombinedData$LENAage) 
stat.desc(StanfordCombinedData$LENAage) 

BabyRace <- CombinedData %>%
  group_by(Site) %>%     
  count(Site, baby_race) %>%
  mutate(prop = prop.table(n))

BabyLatinx <- CombinedData %>%
  group_by(Site) %>%        
  count(Site, baby_latinx) %>%
  mutate(prop = prop.table(n))

Male <- CombinedData %>%
  group_by(Site) %>%        
  count(Site, male) %>%
  mutate(prop = prop.table(n))

Education <- CombinedData %>%
  group_by(Site) %>%     
  count(Site, education_txt) %>%
  mutate(prop = prop.table(n))

MaritalStatus <- CombinedData %>%
  group_by(Site) %>%        
  count(Site, marital_status_txt) %>%
  mutate(prop = prop.table(n))

EmployStatus <- CombinedData %>%
  group_by(Site) %>%       
  count(Site, employment_status_txt) %>%
  mutate(prop = prop.table(n))

Income <- CombinedData %>%
  group_by(Site) %>%        
  count(Site, annual_income_txt) %>%
  mutate(prop = prop.table(n))

ChildInHome <- CombinedData %>%
  group_by(Site) %>%        
  count(Site, ppl_in_home_allchild) %>%
  mutate(prop = prop.table(n))


################################
# STUDY 1 ANALYSES
################################

#Descriptive stats

V_N_desc <- VanderbiltCombinedData %>%
  pivot_longer(cols = c(n_adultinit,n_childinit,prop_adultinit,prop_childinit),
               names_to = c(".value", "Label"),
               names_sep = "_") %>%
  group_by(Label) %>%
  dplyr::summarize(n_mean = mean(n),
                   n_sd = sd(n),
                   n_min = min(n),
                   n_max = max(n),
                   prop_mean = mean(prop),
                   prop_sd = sd(prop),
                   prop_min = min(prop),
                   prop_max = max(prop)
  )
V_N_desc

V_Type_desc <- VLENALclean %>%
  group_by(Label) %>%
  dplyr::summarize(duration_mean = mean(duration),
                   duration_sd = sd(duration),
                   duration_min = min(duration),
                   duration_max = max(duration),
                   CT_mean = mean(turnTaking),
                   CT_sd = sd(turnTaking),
                   CT_min = min(turnTaking),
                   CT_max = max(turnTaking),
                   AWC_mean = mean(adultWordCnt),
                   AWC_sd = sd(adultWordCnt),
                   AWC_min = min(adultWordCnt),
                   AWC_max = max(adultWordCnt)
                   )
V_Type_desc

### PRIMARY ANALYSES ###

t.test(VanderbiltCombinedData$n_adultinit, VanderbiltCombinedData$n_childinit, paired = TRUE, alternative = "two.sided")
cohens_d(VanderbiltCombinedData$n_adultinit, VanderbiltCombinedData$n_childinit, paired = TRUE)

duration <- lmer(duration ~ AIconvo + (1 | ID), data = VLENALclean)
check_model(duration)
skewness(VLENALclean$duration)
skewness(VLENALclean$durationT)
durationT <- lmer(durationT ~ AIconvo + (1 | ID), data = VLENALclean)
check_model(durationT)

turnTakingP <- glmer(turnTaking ~ AIconvo + (1 | ID), data = VLENALclean, family = poisson(link = "log"))
check_model(turnTakingP)

adultWordCntP <- glmer(adultWordCnt ~ AIconvo + (1 | ID), data = VLENALclean, family = poisson(link = "log"))
check_model(adultWordCntP)

tab_model(durationT, turnTakingP, adultWordCntP)

icc(durationT)
icc(turnTakingP)
icc(adultWordCntP)


################################
# STUDY 2 ANALYSES
################################

#Descriptive stats

S_N_desc <- StanfordCombinedData  %>%
  pivot_longer(cols = c(n_adultinit,n_childinit,prop_adultinit,prop_childinit),
               names_to = c(".value", "Label"),
               names_sep = "_") %>%
  group_by(Label) %>%
  dplyr::summarize(n_mean = mean(n),
                   n_sd = sd(n),
                   n_min = min(n),
                   n_max = max(n),
                   prop_mean = mean(prop),
                   prop_sd = sd(prop),
                   prop_min = min(prop),
                   prop_max = max(prop)
  )
S_N_desc

S_Type_desc <- SLENALclean %>%
  group_by(Label) %>%
  dplyr::summarize(duration_mean = mean(duration),
                   duration_sd = sd(duration),
                   duration_min = min(duration),
                   duration_max = max(duration),
                   CT_mean = mean(turnTaking),
                   CT_sd = sd(turnTaking),
                   CT_min = min(turnTaking),
                   CT_max = max(turnTaking),
                   AWC_mean = mean(adultWordCnt),
                   AWC_sd = sd(adultWordCnt),
                   AWC_min = min(adultWordCnt),
                   AWC_max = max(adultWordCnt),
                   durationT_mean = mean(durationT),
                   durationT_sd = sd(durationT),
                   durationT_min = min(durationT),
                   durationT_max = max(durationT))
S_Type_desc

stat.desc(StanfordCombinedData$mbcdi_understand_tot)
stat.desc(StanfordCombinedData$mbcdi_says_tot)
stat.desc(StanfordCombinedData$mbcdi_says_percentile)

### PRIMARY ANALYSES ###

t.test(StanfordCombinedData$n_adultinit, StanfordCombinedData$n_childinit, paired = TRUE, alternative = "two.sided")
cohens_d(StanfordCombinedData$n_adultinit, StanfordCombinedData$n_childinit, paired = TRUE)

Sduration <- lmer(duration ~ AIconvo + (1 | ID), data = SLENALclean)
check_model(Sduration)
skewness(SLENALclean$duration)
skewness(SLENALclean$durationT)
SdurationT <- lmer(durationT ~ AIconvo + (1 | ID), data = SLENALclean)
check_model(SdurationT)
SturnTakingP <- glmer(turnTaking ~ AIconvo + (1 | ID), data = SLENALclean, family = poisson(link = "log"))
check_model(SturnTakingP)
SadultWordCntP <- glmer(adultWordCnt ~ AIconvo + (1 | ID), data = SLENALclean, family = poisson(link = "log"))
check_model(SadultWordCntP)

tab_model(SdurationT, SturnTakingP, SadultWordCntP)

icc(SdurationT)
icc(SturnTakingP)
icc(SadultWordCntP)

# Predicting later language

fit1aQP <- glm(mbcdi_says_tot ~ n_adultinit, data=StanfordCombinedData, family = quasipoisson)
check_model(fit1aQP)
fit1bQP <- glm(mbcdi_says_tot ~ n_childinit, data=StanfordCombinedData, family = quasipoisson)
check_model(fit1bQP)
fit1cQP <- glm(mbcdi_says_tot ~ n_adultinit + n_childinit, data=StanfordCombinedData, family = quasipoisson)
check_model(fit1cQP)
tab_model(fit1aQP,fit1bQP,fit1cQP)

fit2aQP <- glm(mbcdi_understand_tot ~ n_adultinit, data=StanfordCombinedData, family = quasipoisson)
check_model(fit2aQP)
fit2bQP <- glm(mbcdi_understand_tot ~ n_childinit, data=StanfordCombinedData, family = quasipoisson)
check_model(fit2bQP)
fit2cQP <- glm(mbcdi_understand_tot ~ n_adultinit + n_childinit, data=StanfordCombinedData, family = quasipoisson)
check_model(fit2cQP)
tab_model(fit2aQP,fit2bQP,fit2cQP)

FiltSCD <- StanfordCombinedData %>%
  filter(ID != 88)
fit1bQPf <- glm(mbcdi_says_tot ~ n_childinit, data=FiltSCD, family = quasipoisson)
check_model(fit1bQPf)
fit1cQPf <- glm(mbcdi_says_tot ~ n_adultinit + n_childinit, data=FiltSCD, family = quasipoisson)
check_model(fit1cQPf)
tab_model(fit1bQPf, fit1cQPf)


# Figures

# Figure 1: Box Plot

d_lg <- VLENALclean %>%
  mutate(site = "Study 1") %>%
  bind_rows(SLENALclean %>%
              mutate(site = "Study 2")) %>%
  select(
    site,
    AIconvo,
    duration,
    adultWordCnt,
    turnTaking,
  ) %>%
  pivot_longer(
    cols = duration:turnTaking,
    names_to = "metric"
  ) %>%
  mutate(
    metric = factor(
      metric,
      levels = c("duration", "adultWordCnt", "turnTaking"),
      labels = c("Duration", "Adult words", "Conversational turns")
    )
  )
d_lg %>%
  ggplot(aes(AIconvo, log(value), fill = site)) +
  geom_boxplot(outlier.size = .5, outlier.alpha = .5) +
  scale_y_continuous(breaks = seq.int(0, 8, 2)) +
  theme_lena +
  theme(
    axis.title.y = element_text(vjust = 2.5),
    axis.title.x = element_text(vjust = .05),
    axis.text.x = element_text(vjust = .1),
    legend.position = "none"
  ) +
  facet_grid(site~metric) +
  scale_fill_manual(values = c("white", "gray")) +
  labs(
    x = "Conversation type",
    y = "log(Value)",
    fill = NULL
  )
ggsave("~/Dropbox/Research/Vanderbilt/BABIES/Conversations/Analyses/coversation_features_boxplots.png",
  dpi = 300,
  height = 6,
  width = 9
)


# Plot regression model results onto raw data points

#Adult>Exp
AE <- StanfordCombinedData
AE$phatAE <- predict(fit1aQP, type="response")
AE$LoCI <- AE$phatAE - predict(fit1aQP, interval = "confidence", level = 0.95)
AE$HiCI <- AE$phatAE + predict(fit1aQP, interval = "confidence", level = 0.95)
AE <- AE[with(AE, order(n_adultinit)), ]
AdultExp <- ggplot(AE, aes(x = n_adultinit, y = phatAE)) +
  geom_point(aes(y = mbcdi_says_tot), alpha=.5, position=position_jitter(h=.2), color="grey26", fill="grey26", size=1.5) +
  geom_ribbon(aes(y = phatAE, ymin = LoCI, ymax = HiCI), alpha = .25) +
  geom_line(size = 1) +
  labs(x = "Adult Initiated Conversations", y = "Expected Expressive Vocabulary") +
  scale_x_continuous(limits = c(0,250)) +
  theme_lena +
  theme(axis.title.y = element_text(vjust = 2.5),
        axis.title.x = element_text(vjust = .05),
        axis.text.x = element_text(vjust = .1),
        legend.position = "none")

#Child>Exp
CE <- StanfordCombinedData
CE$phatCE <- predict(fit1bQP, type="response")
CE$LoCI <- CE$phatCE - predict(fit1bQP, interval = "confidence", level = 0.95)
CE$HiCI <- CE$phatCE + predict(fit1bQP, interval = "confidence", level = 0.95)
CE <- CE[with(CE, order(n_childinit)), ]
ChildExp<-ggplot(CE, aes(x = n_childinit, y = phatCE)) +
  geom_point(aes(y = mbcdi_says_tot), alpha=.5, position=position_jitter(h=.2), color="grey26", fill="grey26", size=1.5, shape=17) +
  geom_ribbon(aes(y = phatCE, ymin = LoCI, ymax = HiCI), alpha = .25) +
  geom_line(size = 1) +
  labs(x = "Infant Initiated Conversations", y = "Expected Expressive Vocabulary") +
  scale_x_continuous(limits = c(0,200)) +
  theme_lena +
  theme(axis.title.y = element_text(vjust = 2.5),
        axis.title.x = element_text(vjust = .05),
        axis.text.x = element_text(vjust = .1),
        legend.position = "none")

#Adult>Rec
AR <- StanfordCombinedData
AR$phatAR <- predict(fit2aQP, type="response")
AR$LoCI <- AR$phatAR - predict(fit2aQP, interval = "confidence", level = 0.95)
AR$HiCI <- AR$phatAR + predict(fit2aQP, interval = "confidence", level = 0.95)
AR <- AR[with(AR, order(n_adultinit)), ]
AdultRec <- ggplot(AR, aes(x = n_adultinit, y = phatAR)) +
  geom_point(aes(y = mbcdi_understand_tot), alpha=.5, position=position_jitter(h=.2), color="grey36", fill="grey36", size=1.5) +
  geom_ribbon(aes(y = phatAR, ymin = LoCI, ymax = HiCI), alpha = .25) +
  geom_line(size = 1) +
  labs(x = "Adult Initiated Conversations", y = "Expected Receptive Vocabulary") +
  scale_x_continuous(limits = c(0,250)) +
  theme_lena +
  theme(axis.title.y = element_text(vjust = 2.5),
        axis.title.x = element_text(vjust = .05),
        axis.text.x = element_text(vjust = .1),
        legend.position = "none")

#Child>Rec
CR <- StanfordCombinedData
CR$phatCR <- predict(fit2bQP, type="response")
CR$LoCI <- CR$phatCR - predict(fit2bQP, interval = "confidence", level = 0.95)
CR$HiCI <- CR$phatCR + predict(fit2bQP, interval = "confidence", level = 0.95)
CR <- CR[with(CR, order(n_childinit)), ]
ChildRec<-ggplot(CR, aes(x = n_childinit, y = phatCR)) +
  geom_point(aes(y = mbcdi_understand_tot), alpha=.5, position=position_jitter(h=.2), color="grey36", fill="grey36", size=1.5, shape=17) +
  geom_ribbon(aes(y = phatCR, ymin = LoCI, ymax = HiCI), alpha = .25) +
  geom_line(size = 1) +
  labs(x = "Infant Initiated Conversations", y = "Expected Receptive Vocabulary") +
  scale_x_continuous(limits = c(0,200)) +
  theme_lena +
  theme(axis.title.y = element_text(vjust = 2.5),
        axis.title.x = element_text(vjust = .05),
        axis.text.x = element_text(vjust = .1),
        legend.position = "none")

VocabPlots <- (AdultExp + ChildExp) / (AdultRec + ChildRec)
ggsave("~/Dropbox/Research/Vanderbilt/BABIES/Conversations/Analyses/VocabPlots.png",
       dpi = 300,
       height = 10)


################################
# SUPPLEMENT: Compare samples
################################

#### Conversation features

t.test(CombinedData$n_adultinit~CombinedData$Site, alternative = "two.sided")
cohens_d(n_adultinit ~ Site, data = CombinedData, paired = FALSE)

t.test(CombinedData$n_childinit~CombinedData$Site, alternative = "two.sided")
cohens_d(n_childinit ~ Site, data = CombinedData, paired = FALSE)

CompDur <- lmer(durationT ~ Stanford + (1|ID), data = CombineLENA)
CompTurn <- glmer(turnTaking ~ Stanford + (1|ID), data = CombineLENA, family = poisson(link = "log"))
CompAWC <- glmer(adultWordCnt ~ Stanford + (1|ID), data = CombineLENA, family = poisson(link = "log"))
tab_model(CompDur, CompTurn, CompAWC)

################################
# Variability in convo Ns
################################

NCI <- CombinedData %>%
  select(ID,
         Site,
         n_adultinit,
         n_childinit,
         n_convos,
         convratio,
         prop_adultinit,
         prop_childinit) %>%
  mutate(convratio = as.numeric(convratio)) %>%
  mutate(moreCI = if_else(convratio < 1,1,0),
         moreAI = if_else(convratio > 1,1,0))
stat.desc(CombinedData$prop_childinit) 
MoreCI <- NCI %>%
  group_by(Site) %>%
  count(Site,moreCI) %>%
  mutate(prop = prop.table(n))




