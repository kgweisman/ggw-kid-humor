########################################################### preliminaries #####

# --- PACKAGES & FUNCTIONS ----------------------------------------------------

# libraries
library(dplyr)
library(tidyr)
library(ggplot2)
# library(scatterplot3d)
library(lme4)
# library(psych)
# library(stats)
# library(scales)
# library(smacof)
# library(eba)
library(langcog)
library(arm)

# clear environment
rm(list=ls())

# clear graphics
dev.off()

# --- IMPORTING DATA ----------------------------------------------------------

# read in data: individual scores

# ... RUN01
dd = read.csv("./data/kid-run-01_2015-06-13_data_anonymized.csv")[-1] # get rid of column of obs numbers

# add median split for age
dd <- dd %>%
  mutate(ageCut = factor(cut(ageCalc, breaks = 2, labels = c("young", "old"))))

# --------> FILTERING ---------------------------------------------------------

# --------------->-> by no video/co--------------------------------------------

dd_coded = dd %>%
  filter(is.na(silliness) == F & is.na(sarcasm) == F)

dd_uncoded = dd %>%
  filter(is.na(silliness) == T | is.na(sarcasm) == T)

# set group of interest
# ... to coded:
dd <- dd_coded

# --------------->-> by ethnicity ---------------------------------------------

# dd_children_white = dd_children %>%
#   filter(ethnicity == "white")
# 
# dd_children_nonwhite = dd_children %>%
#   filter(ethnicity != "white" & 
#            ethnicity != "NA" & 
#            ethnicity != "other_prefNo")

# set group of interest
# ... to white:
# dd_children = dd_children_white

# # ... to nonwhite:
# dd_children = dd_children_nonwhite

# --------------->-> by age ---------------------------------------------------

dd_children_exact = dd_children %>%
  filter(ageCalc >= 4.5 & ageCalc <= 5.5)

# set group of interest
# ... to exact:
# dd_children = dd_children_exact

# --------------->-> exclude stapler trials -----------------------------------

dd_nostapler <- dd %>%
  filter(leftCharacter != "stapler" & rightCharacter != "stapler") %>%
  mutate(leftCharacter = factor(leftCharacter),
         rightCharacter = factor(rightCharacter))

# set group of interest
# ... to no stapler:
# dd = dd_nostapler

# --------------->-> exclude baby trials --------------------------------------

dd_nobaby <- dd %>%
  filter(leftCharacter != "baby" & rightCharacter != "baby") %>%
  mutate(leftCharacter = factor(leftCharacter),
         rightCharacter = factor(rightCharacter))

# set group of interest
# ... to no baby:
# dd = dd_nobaby

########################################################### summary stats #####

# --- DEMOGRAPHICS ------------------------------------------------------------

demo = dd %>% distinct(subid)

# total n
demo %>% summarise(n = length(subid))

# condition assignment
demo %>% group_by(sequence) %>% distinct(subid) %>% summarise(n = length(subid))

# gender
demo %>% count(gender)

# ethnicity
demo %>% count(ethnicity)

# age
demo %>% summarise(mean_age = mean(ageCalc, na.rm = T), sd_age = sd(ageCalc, na.rm = T))
qplot(demo$ageCalc)

######################################################## analysis & plots #####

# --- MAKE CHARACTER PAIRS DATAFRAME ------------------------------------------

d1 = dd %>% 
  filter(phase == "test") %>%
  mutate(
    pair = 
      ifelse(
        leftCharacter == "grownup" | 
          rightCharacter == "grownup",
        ifelse(
          rightCharacter == "kid" | 
            leftCharacter == "kid", 
          "grownup.kid",
          ifelse(
            rightCharacter == "baby" | 
              leftCharacter == "baby", 
            "grownup.baby",
            ifelse(
              rightCharacter == "dog" | 
                leftCharacter == "dog", 
              "grownup.dog",
              ifelse(
                rightCharacter == "bear" | 
                  leftCharacter == "bear", 
                "grownup.bear",
                ifelse(
                  rightCharacter == "bug" | 
                    leftCharacter == "bug", 
                  "grownup.bug",
                  ifelse(
                    rightCharacter == "robot" | 
                      leftCharacter == "robot", 
                    "grownup.robot",
                    ifelse(
                      rightCharacter == "computer" | 
                        leftCharacter == "computer", 
                      "grownup.computer",
                      ifelse(
                        rightCharacter == "car" | 
                          leftCharacter == "car", 
                        "grownup.car",
                        ifelse(
                          rightCharacter == "stapler" | 
                            leftCharacter == "stapler", 
                          "grownup.stapler",
                          NA))))))))),
        ifelse(
          leftCharacter == "kid" | 
            rightCharacter == "kid",
          ifelse(
            rightCharacter == "baby" | 
              leftCharacter == "baby", 
            "kid.baby",
            ifelse(
              rightCharacter == "dog" | 
                leftCharacter == "dog", 
              "kid.dog",
              ifelse(
                rightCharacter == "bear" | 
                  leftCharacter == "bear", 
                "kid.bear",
                ifelse(
                  rightCharacter == "bug" | 
                    leftCharacter == "bug", 
                  "kid.bug",
                  ifelse(
                    rightCharacter == "robot" | 
                      leftCharacter == "robot", 
                    "kid.robot",
                    ifelse(
                      rightCharacter == "computer" | 
                        leftCharacter == "computer", 
                      "kid.computer",
                      ifelse(
                        rightCharacter == "car" | 
                          leftCharacter == "car", 
                        "kid.car",
                        ifelse(
                          rightCharacter == "stapler" | 
                            leftCharacter == "stapler", 
                          "kid.stapler",
                          NA)))))))),
          ifelse(
            leftCharacter == "baby" | 
              rightCharacter == "baby",
            ifelse(
              rightCharacter == "dog" | 
                leftCharacter == "dog", 
              "baby.dog",
              ifelse(
                rightCharacter == "bear" | 
                  leftCharacter == "bear", 
                "baby.bear",
                ifelse(
                  rightCharacter == "bug" | 
                    leftCharacter == "bug", 
                  "baby.bug",
                  ifelse(
                    rightCharacter == "robot" | 
                      leftCharacter == "robot", 
                    "baby.robot",
                    ifelse(
                      rightCharacter == "computer" | 
                        leftCharacter == "computer", 
                      "baby.computer",
                      ifelse(
                        rightCharacter == "car" | 
                          leftCharacter == "car", 
                        "baby.car",
                        ifelse(
                          rightCharacter == "stapler" | 
                            leftCharacter == "stapler", 
                          "baby.stapler",
                          NA))))))),
            ifelse(
              leftCharacter == "dog" | 
                rightCharacter == "dog",
              ifelse(
                rightCharacter == "bear" | 
                  leftCharacter == "bear", 
                "dog.bear",
                ifelse(
                  rightCharacter == "bug" | 
                    leftCharacter == "bug", 
                  "dog.bug",
                  ifelse(
                    rightCharacter == "robot" | 
                      leftCharacter == "robot", 
                    "dog.robot",
                    ifelse(
                      rightCharacter == "computer" | 
                        leftCharacter == "computer", 
                      "dog.computer",
                      ifelse(
                        rightCharacter == "car" | 
                          leftCharacter == "car", 
                        "dog.car",
                        ifelse(
                          rightCharacter == "stapler" | 
                            leftCharacter == "stapler", 
                          "dog.stapler",
                          NA)))))),
              ifelse(
                leftCharacter == "bear" | 
                  rightCharacter == "bear",
                ifelse(
                  rightCharacter == "bug" | 
                    leftCharacter == "bug", 
                  "bear.bug",
                  ifelse(
                    rightCharacter == "robot" | 
                      leftCharacter == "robot", 
                    "bear.robot",
                    ifelse(
                      rightCharacter == "computer" | 
                        leftCharacter == "computer", 
                      "bear.computer",
                      ifelse(
                        rightCharacter == "car" | 
                          leftCharacter == "car", 
                        "bear.car",
                        ifelse(
                          rightCharacter == "stapler" | 
                            leftCharacter == "stapler", 
                          "bear.stapler",
                          NA))))),
                ifelse(
                  leftCharacter == "bug" | 
                    rightCharacter == "bug",
                  ifelse(
                    rightCharacter == "robot" | 
                      leftCharacter == "robot", 
                    "bug.robot",
                    ifelse(
                      rightCharacter == "computer" | 
                        leftCharacter == "computer", 
                      "bug.computer",
                      ifelse(
                        rightCharacter == "car" | 
                          leftCharacter == "car", 
                        "bug.car",
                        ifelse(
                          rightCharacter == "stapler" | 
                            leftCharacter == "stapler", 
                          "bug.stapler",
                          NA)))),
                  ifelse(
                    leftCharacter == "robot" | rightCharacter == "robot",
                    ifelse(
                      rightCharacter == "computer" | 
                        leftCharacter == "computer", 
                      "robot.computer",
                      ifelse(
                        rightCharacter == "car" | 
                          leftCharacter == "car", 
                        "robot.car",
                        ifelse(
                          rightCharacter == "stapler" | 
                            leftCharacter == "stapler", 
                          "robot.stapler",
                          NA))),
                    ifelse(
                      leftCharacter == "computer" | 
                        rightCharacter == "computer",
                      ifelse(
                        rightCharacter == "car" | 
                          leftCharacter == "car", 
                        "computer.car",
                        ifelse(
                          rightCharacter == "stapler" | 
                            leftCharacter == "stapler", 
                          "computer.stapler",
                          NA)),
                      ifelse(
                        leftCharacter == "car" | 
                          rightCharacter == "car",
                        ifelse(
                          rightCharacter == "stapler" | 
                            leftCharacter == "stapler", 
                          "car.stapler",
                          NA),
                        NA)))))))))) %>%
  mutate(pair = factor(pair,
                       levels = c("grownup.kid",
                                  "grownup.baby",
                                  "kid.baby",
                                  "grownup.dog",
                                  "kid.dog",
                                  "baby.dog",
                                  "grownup.bear",
                                  "kid.bear",
                                  "baby.bear",
                                  "grownup.bug",
                                  "kid.bug",
                                  "baby.bug",
                                  "grownup.robot",
                                  "kid.robot",
                                  "baby.robot",
                                  "grownup.computer",
                                  "kid.computer",
                                  "baby.computer",
                                  "grownup.car",
                                  "kid.car",
                                  "baby.car",
                                  "grownup.stapler",
                                  "kid.stapler",
                                  "baby.stapler",
                                  "dog.bear",
                                  "dog.bug",
                                  "bear.bug",
                                  "dog.robot",
                                  "bear.robot",
                                  "bug.robot",
                                  "dog.computer",
                                  "bear.computer",
                                  "bug.computer",
                                  "dog.car",
                                  "bear.car",
                                  "bug.car",
                                  "dog.stapler",
                                  "bear.stapler",
                                  "bug.stapler",
                                  "robot.computer",
                                  "robot.car",
                                  "computer.car",
                                  "robot.stapler",
                                  "computer.stapler",
                                  "car.stapler")),
         predicate = factor(predicate, 
                            levels = c("hunger", "feelings", "thinking")),
         responseNumFlip = 
           ifelse(
             substr(leftCharacter,1,3) == substr(pair,1,3),
             responseNum,
             -1 * responseNum),
         pairCat = 
           # check if there is a human
           ifelse(
             grepl("grownup", pair) == T | 
               grepl("kid", pair) == T |
               grepl("baby", pair) == T,
             # if there is, check if there are two humans
             ifelse(
               grepl("grownup.kid", pair) == T | 
                 grepl("grownup.baby", pair) == T |
                 grepl("kid.baby", pair) == T,
               # if there are, done
               "human.human",
               # if not, check if there is an animal
               ifelse(
                 grepl("dog", pair) == T | 
                   grepl("bear", pair) == T |
                   grepl("bug", pair) == T,
                 # if there is, done
                 "human.animal",
                 # if not, check if there is a tech
                 ifelse(
                   grepl("robot", pair) == T | 
                     grepl("computer", pair) == T |
                     grepl("car", pair) == T,
                   # if there is, done
                   "human.tech",
                   # if not, check if there is a stapler
                   ifelse(
                     grepl("stapler", pair) == T,
                     # if there is, done
                     "control",
                     # if not, error
                     NA)))),
             # if not, check if there is an animal
             ifelse(
               grepl("dog", pair) == T | 
                 grepl("bear", pair) == T |
                 grepl("bug", pair) == T,
               # if there is, check if there are two animals
               ifelse(
                 grepl("dog.bear", pair) == T | 
                   grepl("dog.bug", pair) == T |
                   grepl("bear.bug", pair) == T,
                 # if there are, done
                 "animal.animal",
                 # if not, check if there is a tech
                 ifelse(
                   grepl("robot", pair) == T | 
                     grepl("computer", pair) == T |
                     grepl("car", pair) == T,
                   # if there is, done
                   "animal.tech",
                   # if not, check if there is a stapler
                   ifelse(
                     grepl("stapler", pair) == T,
                     # if there is, done
                     "control",
                     # if not, error
                     NA))),
               # if not, check if there is a tech
               ifelse(
                 grepl("robot", pair) == T | 
                   grepl("computer", pair) == T |
                   grepl("car", pair) == T,
                 # if there is, check if there are two techs
                 ifelse(
                   grepl("robot.computer", pair) == T | 
                     grepl("robot.car", pair) == T |
                     grepl("computer.car", pair) == T,
                   # if there are, done
                   "tech.tech",
                   # if not, check if there is a stapler
                   ifelse(
                     grepl("stapler", pair) == T,
                     # if there is, done
                     "control",
                     # if not, error
                     NA)),
                 # if not, error
                 NA)))) %>%
  mutate(pairCat = factor(pairCat,
                          levels = c("human.human",
                                     "animal.animal",
                                     "tech.tech",
                                     "human.animal",
                                     "human.tech",
                                     "animal.tech",
                                     "control")),
         pairCatScore = ifelse(is.na(pairCat), NA,
                               ifelse(pairCat == "control", 3,
                                      ifelse(pairCat == "human.tech" |
                                               pairCat == "animal.tech",
                                             2,
                                             ifelse(pairCat == "human.animal", 1,
                                                    0)))))

# --- MAKE PROPORTIONS DATAFRAMES ---------------------------------------------

# by participant, by predicate

nTrials <- d1 %>%
  group_by(subid, ageCalc, predicate) %>%
  summarise(nTrials = length(subid))

sillinessTab <- d1 %>%
  count(subid, ageCalc, predicate, sillinessCat) %>%
  full_join(nTrials) %>%
  mutate(propSilliness = n/nTrials) %>%
  filter(sillinessCat != FALSE) %>%
  select(-sillinessCat, -n)

sarcasmTab <- d1 %>%
  count(subid, ageCalc, predicate, sarcasmCat) %>%
  full_join(nTrials) %>%
  mutate(propSarcasm = n/nTrials) %>%
  filter(sarcasmCat != FALSE) %>%
  select(-sarcasmCat, -n)

humorTab <- d1 %>%
  count(subid, ageCalc, predicate, humorCat) %>%
  full_join(nTrials) %>%
  mutate(propHumor = n/nTrials) %>%
  filter(humorCat != FALSE) %>%
  select(-humorCat, -n)

participantTab <- full_join(nTrials, sillinessTab) %>%
  full_join(sarcasmTab) %>%
  full_join(humorTab) %>%
  mutate(silliness = ifelse(is.na(propSilliness), 0, propSilliness),
         sarcasm = ifelse(is.na(propSarcasm), 0, propSarcasm),
         humor = ifelse(is.na(propHumor), 0, propHumor)) %>%
  select(-propSilliness, -propSarcasm, -propHumor) %>%
  gather(humorType, proportion, -subid, -ageCalc, -predicate, -nTrials) %>%
  arrange(subid, humorType, predicate)

participantTab

# bootstrap by predicate

sillinessPredicateBoot <- multi_boot(data = d1,
                            summary_function = "mean",
                            column = "sillinessCat",
                            summary_groups = "predicate",
                            statistics_functions = c("ci_lower", "mean", "ci_upper"),
                            statistics_groups = "predicate") %>%
  mutate(humorType = "silliness")

sarcasmPredicateBoot <- multi_boot(data = d1,
                            summary_function = "mean",
                            column = "sarcasmCat",
                            summary_groups = "predicate",
                            statistics_functions = c("ci_lower", "mean", "ci_upper"),
                            statistics_groups = "predicate") %>%
  mutate(humorType = "sarcasm")

humorPredicateBoot <- multi_boot(data = d1,
                          summary_function = "mean",
                          column = "humorCat",
                          summary_groups = "predicate",
                          statistics_functions = c("ci_lower", "mean", "ci_upper"),
                          statistics_groups = "predicate") %>%
  mutate(humorType = "humor")

predicateBoot <- full_join(sillinessPredicateBoot, sarcasmPredicateBoot) %>%
  full_join(humorPredicateBoot) %>%
  mutate(humorType = factor(humorType)) %>%
  select(predicate, humorType, ci_lower:ci_upper) %>%
  arrange(humorType, predicate)

predicateBoot

# bootstrap by pairCat

sillinessPairCatBoot <- multi_boot(data = d1,
                                     summary_function = "mean",
                                     column = "sillinessCat",
                                     summary_groups = "pairCat",
                                     statistics_functions = c("ci_lower", "mean", "ci_upper"),
                                     statistics_groups = "pairCat") %>%
  mutate(humorType = "silliness")

sarcasmPairCatBoot <- multi_boot(data = d1,
                                   summary_function = "mean",
                                   column = "sarcasmCat",
                                   summary_groups = "pairCat",
                                   statistics_functions = c("ci_lower", "mean", "ci_upper"),
                                   statistics_groups = "pairCat") %>%
  mutate(humorType = "sarcasm")

humorPairCatBoot <- multi_boot(data = d1,
                                 summary_function = "mean",
                                 column = "humorCat",
                                 summary_groups = "pairCat",
                                 statistics_functions = c("ci_lower", "mean", "ci_upper"),
                                 statistics_groups = "pairCat") %>%
  mutate(humorType = "humor")

pairCatBoot <- full_join(sillinessPairCatBoot, sarcasmPairCatBoot) %>%
  full_join(humorPairCatBoot) %>%
  mutate(humorType = factor(humorType)) %>%
  select(pairCat, humorType, ci_lower:ci_upper) %>%
  arrange(humorType, pairCat)

pairCatBoot

# bootstrap by predicate and pairCat

sillinessTwoWayBoot <- multi_boot(data = d1,
                                   summary_function = "mean",
                                   column = "sillinessCat",
                                   summary_groups = c("predicate", "pairCat"),
                                   statistics_functions = c("ci_lower", "mean", "ci_upper"),
                                   statistics_groups = c("predicate", "pairCat")) %>%
  mutate(humorType = "silliness")

sarcasmTwoWayBoot <- multi_boot(data = d1,
                                 summary_function = "mean",
                                 column = "sarcasmCat",
                                 summary_groups = c("predicate", "pairCat"),
                                 statistics_functions = c("ci_lower", "mean", "ci_upper"),
                                 statistics_groups = c("predicate", "pairCat")) %>%
  mutate(humorType = "sarcasm")

humorTwoWayBoot <- multi_boot(data = d1,
                               summary_function = "mean",
                               column = "humorCat",
                               summary_groups = c("predicate", "pairCat"),
                               statistics_functions = c("ci_lower", "mean", "ci_upper"),
                               statistics_groups = c("predicate", "pairCat")) %>%
  mutate(humorType = "humor")

twoWayBoot <- full_join(sillinessTwoWayBoot, sarcasmTwoWayBoot) %>%
  full_join(humorTwoWayBoot) %>%
  mutate(humorType = factor(humorType)) %>%
  select(pairCat, humorType, ci_lower:ci_upper) %>%
  arrange(humorType, pairCat)

twoWayBoot

# --- PLOTS -------------------------------------------------------------------

# by particpiant
# ... by ageCalc & predicate
qplot(x = ageCalc, y = proportion, data = participantTab %>% filter(humorType != "humor")) + 
  geom_smooth() +
  facet_wrap(humorType ~ predicate)

# overall
# ... by predicate alone
qplot(x = predicate, y = mean, data = predicateBoot %>% filter(humorType != "humor"), geom = "bar", stat = "identity") + 
  facet_wrap(~ humorType) + 
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper, width = .1)) +
  theme(text = element_text(size = 40)) +
  xlab("\npredicate") +
  ylab("proportion of trials\n")

# ... by pairCat alone
qplot(x = pairCat, y = mean, data = pairCatBoot %>% filter(humorType != "humor"), geom = "bar", stat = "identity") + 
  facet_wrap(~ humorType) + 
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper, width = .1))

# ... by predicate and pairCat
qplot(x = pairCat, y = mean, data = twoWayBoot %>% filter(humorType != "humor"), geom = "bar", stat = "identity") + 
  facet_grid(humorType ~ predicate) + 
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper, width = .1))

# ... by predicate and trialNum
# ... silliness
qplot(x = trialNum, y = silliness, data = d1, geom = "point", position = "jitter") + 
  facet_wrap(~ predicate) + 
  geom_smooth() +
  theme(text = element_text(size = 40)) +
  xlab("\ntrial number") +
  ylab("silliness\n")

# ... sarcasm
qplot(x = trialNum, y = sarcasm, data = d1, geom = "point", position = "jitter") + 
  facet_wrap(~ predicate) + 
  geom_smooth()

# --- CONTRASTS ---------------------------------------------------------------

contrasts(d1$pairCat) <- cbind(between.within = c(0,0,0,1,1,1,1),
                               w_inanimate.alive = c(-1,-1,2,0,0,0,0),
                               w_animal.human = c(-1,1,0,0,0,0),
                               b_inanimate.alive = c(0,0,0,-3,1,1,1),
                               b_i_control.tech = c(0,0,0,0,-1,-1,2),
                               b_i_t_human.animal = c(0,0,0,0,1,-1,0))
contrasts(d1$pairCat)

# CHOOSE ONE!
# # ...dummy coding
# contrasts(d1$predicate) <- cbind(hunger = c(1,0,0),
#                                  thinking = c(0,0,1))

# ...effect coding
contrasts(d1$predicate) <- cbind(effHunger = c(1,-1,0),
                                 effThinking = c(0,-1,1))

# # ...contrast coding
# contrasts(d1$predicate) <- cbind(lin = c(-1,0,1),
#                                  quad = c(-1,2,-1))

# --- MODELS ------------------------------------------------------------------

# silliness

# ... by predicate, age
silliness0 <- glmer(sillinessCat ~ predicate + (1 | subid), data = d1, family = "binomial")
summary(silliness0)

silliness1 <- glmer(sillinessCat ~ predicate + scale(ageCalc, scale = F) + (1 | subid), data = d1, family = "binomial")
summary(silliness1) 

silliness2 <- glmer(sillinessCat ~ predicate * scale(ageCalc, scale = F) + (1 | subid), data = d1, family = "binomial")
summary(silliness2)

anova(silliness0, silliness1, silliness2)

# ... by predicate, pairCat

silliness3 <- glmer(sillinessCat ~ pairCat + (1 | subid), data = d1, family = "binomial")
summary(silliness3)

# silliness4 <- glmer(sillinessCat ~ pairCat + predicate + (1 | subid), data = d1, family = "binomial")
# summary(silliness4) # failed to converge

# silliness5 <- glmer(sillinessCat ~ pairCat * predicate + (1 | subid), data = d1, family = "binomial")
# summary(silliness5) # failed to converge

# anova(silliness3, silliness4, silliness5)

# by predicate, trialNum

silliness6 <- glmer(sillinessCat ~ predicate + poly(trialNum, 1) + (1 |subid), data = d1, family = "binomial")
summary(silliness6)

silliness7 <- glmer(sillinessCat ~ predicate * poly(trialNum, 1) + (1 |subid), data = d1, family = "binomial")
summary(silliness7)

silliness8 <- glmer(sillinessCat ~ predicate * poly(trialNum, 2) + (1 |subid), data = d1, family = "binomial")
summary(silliness8)

# silliness9 <- glmer(sillinessCat ~ predicate * poly(trialNum, 2) + scale(ageCalc, scale = F) + (1 |subid), data = d1, family = "binomial")
# summary(silliness9) # failed to converge

# silliness10 <- glmer(sillinessCat ~ predicate * poly(trialNum, 2) * scale(ageCalc, scale = F) + (1 |subid), data = d1, family = "binomial")
# summary(silliness10) # failed to converge

anova(silliness0, silliness6, silliness7, silliness8)

# sarcasm

# ... by predicate, age
sarcasm0 <- glmer(sarcasmCat ~ predicate + (1 | subid), data = d1, family = "binomial")
summary(sarcasm0)

# sarcasm1 <- glmer(sarcasmCat ~ predicate + ageCalc + (1 | subid), data = d1, family = "binomial")
# summary(sarcasm1) # failed to converge 

# sarcasm2 <- glmer(sarcasmCat ~ predicate * ageCalc + (1 | subid), data = d1, family = "binomial")
# summary(sarcasm2) # failed to converge

# anova(sarcasm0, sarcasm1, sarcasm2)
# anova(sarcasm0, sarcasm2)

# ... by predicate, pairCat

# sarcasm3 <- glmer(sarcasmCat ~ pairCat + (1 | subid), data = d1, family = "binomial")
# summary(sarcasm3) # failed to converge
# 
# sarcasm4 <- glmer(sarcasmCat ~ pairCat + predicate + (1 | subid), data = d1, family = "binomial")
# summary(sarcasm4) # failed to converge
# 
# sarcasm5 <- glmer(sarcasmCat ~ pairCat * predicate + (1 | subid), data = d1, family = "binomial")
# summary(sarcasm5) # failed to converge
# 
# anova(sarcasm3, sarcasm4, sarcasm5)

# by predicate, trialNum

sarcasm6 <- glmer(sarcasmCat ~ predicate + poly(trialNum, 1) + (1 |subid), data = d1, family = "binomial")
summary(sarcasm6)

sarcasm7 <- glmer(sarcasmCat ~ predicate * poly(trialNum, 1) + (1 |subid), data = d1, family = "binomial")
summary(sarcasm7)

# sarcasm8 <- glmer(sarcasmCat ~ predicate * poly(trialNum, 2) + (1 |subid), data = d1, family = "binomial")
# summary(sarcasm8) # failed to converge

anova(sarcasm0, sarcasm6, sarcasm7, sarcasm8)

# humor

# ... by predicate, age
humor0 <- glmer(humorCat ~ predicate + (1 | subid), data = d1, family = "binomial")
summary(humor0)

humor1 <- glmer(humorCat ~ predicate + ageCalc + (1 | subid), data = d1, family = "binomial")
summary(humor1)

humor2 <- glmer(humorCat ~ predicate * ageCalc + (1 | subid), data = d1, family = "binomial")
summary(humor2)

anova(humor0, humor1, humor2)

# ... by predicate, pairCat

humor3 <- glmer(humorCat ~ pairCat + (1 | subid), data = d1, family = "binomial")
summary(humor3)

humor4 <- glmer(humorCat ~ pairCat + predicate + (1 | subid), data = d1, family = "binomial")
summary(humor4) # failed to converge

humor5 <- glmer(humorCat ~ pairCat * predicate + (1 | subid), data = d1, family = "binomial")
summary(humor5) # failed to converge

anova(humor3, humor4, humor5)

# by predicate, trialNum

humor6 <- glmer(humorCat ~ predicate + poly(trialNum, 1) + (1 |subid), data = d1, family = "binomial")
summary(humor6)

humor7 <- glmer(humorCat ~ predicate * poly(trialNum, 1) + (1 |subid), data = d1, family = "binomial")
summary(humor7)

humor8 <- glmer(humorCat ~ predicate * poly(trialNum, 2) + (1 |subid), data = d1, family = "binomial")
summary(humor8) # failed to converge

anova(humor0, humor6, humor7, humor8)
