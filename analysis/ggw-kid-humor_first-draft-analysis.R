########################################################### preliminaries #####

# --- PACKAGES & FUNCTIONS ----------------------------------------------------

# libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(scatterplot3d)
library(lme4)
library(psych)
# library(stats)
library(scales)
library(smacof)
library(eba)

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
                                     "control")))

# --- MAKE PROPORTIONS DATAFRAME ----------------------------------------------

nTrials <- d1 %>%
  group_by(subid) %>%
  summarise(nTrials = length(subid))

d2 <- d1 %>%
  count(subid, ageCalc, humorCat) %>%
  left_join(nTrials) %>%
  mutate(propJoke = n/nTrials) %>%
  filter(humorCat != "no") %>%
  select(-humorCat)

# --- PLOT JOKING -------------------------------------------------------------

# ... by ageCalc, predicate

qplot(x = ageCalc, y = humorCat, data = subset(d1, humorCat != "NA"), position = "jitter") +
  geom_smooth(method = "glm", family = "binomial")

qplot(x = ageCalc, y = propJoke, data = d2)

qplot(predicate, data = subset(d1, humorCat != "NA"), 
      geom = "bar", fill = relevel(humorCat, ref = "yes"))

qplot(predicate, data = subset(d1, humorCat != "NA"), 
      geom = "bar", fill = relevel(humorCat, ref = "yes")) +
  facet_grid(~ageCut)

qplot(predicate, data = subset(d1, humorCat != "NA"), 
      geom = "bar", fill = relevel(humorCat, ref = "yes")) +
  facet_grid(~pairCat)

# glmer
rm0 <- glmer(propJoke ~ ageCalc + (1 | subid), data = d2, family = "binomial",
          weights = nTrials)
summary(rm0)

rm1 <- glmer(humorCat ~ ageCalc + (1 | subid), data = subset(d1, ageCalc != "NA"), family = "binomial")
summary(rm1)

rm2 <- glmer(humorCat ~ predicate + (1 | subid), data = subset(d1, ageCalc != "NA"), family = "binomial")
summary(rm2)

rm3 <- glmer(humorCat ~ ageCalc + predicate + (1 | subid), data = subset(d1, ageCalc != "NA"), family = "binomial")
summary(rm3)

rm4 <- glmer(humorCat ~ ageCalc * predicate + (1 | subid), data = subset(d1, ageCalc != "NA"), family = "binomial")
summary(rm4)

anova(rm1, rm3, rm4)
anova(rm2, rm3, rm4)

rm5 <- glmer(humorCat ~ pairCat + (1 | subid), data = subset(d1, ageCalc != "NA"), family = "binomial")
summary(rm5)

rm6 <- glmer(humorCat ~ pairCat * predicate + (1 | subid), data = subset(d1, ageCalc != "NA"), family = "binomial")
summary(rm6)





# predict <- predict(r)
# 
# d3 <- d2 %>%
#   cbind(predict) %>%
#   mutate(logOdds = logit(propJoke))
# 
# qplot(x = ageCalc, y = predict, data = d3)
