library(psych)
install.packages("lm.beta")
library(lm.beta)
library(tidyverse)
library(gridExtra)
install.packages("car")
library(car)

#### ASSIGNMENT 1 ####

data_sample_1 = read.csv("https://tinyurl.com/ha-dataset1")

data_sample_1_sexasfactor = data_sample_1 %>% 
  mutate(sex = factor(sex))

data_sample_1_sexasfactor$sex

levels(data_sample_1_sexasfactor$sex)

view(data_sample_1_sexasfactor)

data_sample_1_sexasfactor %>% 
  summary()

data_sample_1_sexasfactor %>% 
  select(-ID, -sex) %>% 
  describe()

data_sample_1_sexasfactor_excl = data_sample_1_sexasfactor %>% 
  filter(age != 444, STAI_trait != 3.90, household_income != -3732)

view(data_sample_1_sexasfactor_excl)

data_sample_1_sexasfactor_excl %>% 
  summary()

data_sample_1_sexasfactor_excl %>% 
  select(-ID, -sex) %>% 
  describe()

data_sample_1_sexasfactor_excl %>% 
  ggplot() +
  aes(x = pain) +
  geom_histogram(bins = 50)

data_sample_1_sexasfactor_excl %>% 
  ggplot() +
  aes(x = STAI_trait) +
  geom_histogram(bins = 50)

data_sample_1_sexasfactor_excl %>% 
  ggplot() +
  aes(x = pain_cat) +
  geom_histogram(bins = 50)

data_sample_1_sexasfactor_excl %>% 
  ggplot() +
  aes(x = mindfulness) +
  geom_histogram(bins = 50)

data_sample_1_sexasfactor_excl %>% 
  ggplot() +
  aes(x = cortisol_serum) +
  geom_histogram(bins = 50)

data_sample_1_sexasfactor_excl %>% 
  ggplot() +
  aes(x = cortisol_saliva) +
  geom_histogram(bins = 50)

data_sample_1_sexasfactor_excl %>% 
  ggplot() +
  aes(x = STAI_trait, y = pain) +
  geom_point()

data_sample_1_sexasfactor_excl %>% 
  ggplot() +
  aes(x = pain_cat, y = pain) +
  geom_point()

data_sample_1_sexasfactor_excl %>% 
  ggplot() +
  aes(x = mindfulness, y = pain) +
  geom_point()

data_sample_1_sexasfactor_excl %>% 
  ggplot() +
  aes(x = cortisol_serum, y = pain) +
  geom_point()

data_sample_1_sexasfactor_excl %>% 
  ggplot() +
  aes(x = cortisol_saliva, y = pain) +
  geom_point()

model_1 = lm (pain ~ age + sex, data = data_sample_1_sexasfactor_excl)

model_2 = lm (pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva, data = data_sample_1_sexasfactor_excl)

CDplot_model_2 = model_2 %>% 
  plot(which = 4)

data_sample_1_sexasfactor_excl %>% 
  slice(112)

# Checking linear regression assumptions: #
  
describe(residuals(model_2))

model_2 %>% 
  residualPlots()

model_2 %>% 
  ncvTest()

model_2 %>% 
  vif()

model_2_new = lm (pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data_sample_1_sexasfactor_excl)

CDplot_model_2_new = model_2_new %>% 
  plot(which = 4)

describe(residuals(model_2_new))

model_2_new %>% 
  residualPlots()

model_2_new %>% 
  ncvTest()

model_2_new %>% 
  vif()

model_1

sm_model1 = summary(model_1)
sm_model1

AIC(model_1)

confint(model_1)

lm.beta(model_1)

summary(model_1)$adj.r.squared

model_2_new

sm_model2_new = summary(model_2_new)
sm_model2_new

AIC(model_2_new)

confint(model_2_new)

lm.beta(model_2_new)

summary(model_2_new)$adj.r.squared

anova(model_1, model_2_new)

#### ASSIGNMENT 2 ####

model_backward = lm (pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + weight + IQ + household_income, data = data_sample_1_sexasfactor_excl)

CDplot_model_backward = model_backward %>% 
  plot(which = 4)

describe(residuals(model_backward))

model_backward %>% 
  residualPlots()

model_backward %>% 
  ncvTest()

model_backward %>% 
  vif()

model_backward_run = step(model_backward, direction = "backward" )

model_backward_run

backward_model = lm (pain ~ age + sex + pain_cat + mindfulness + cortisol_serum + household_income, data = data_sample_1_sexasfactor_excl)

theory_based_model = lm (pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data_sample_1_sexasfactor_excl)

AIC(backward_model)

AIC(theory_based_model)

anova(backward_model, theory_based_model)

data_sample_2 = read.csv("https://tinyurl.com/ha-dataset2")

pred_theory_based_model = predict(theory_based_model, data_sample_2)

pred_backward_model = predict(backward_model, data_sample_2)

RSS_test_TBM = sum((data_sample_2[,"pain"] - pred_theory_based_model)^2)

RSS_test_BM = sum((data_sample_2[, "pain"] - pred_backward_model)^2)

RSS_test_TBM

RSS_test_BM

summary(backward_model)

confint(backward_model)

lm.beta(backward_model)

summary(model_backward)

AIC(model_backward)

#### ASSIGNMENT 3 ####

data_sample_3 = read.csv("https://tinyurl.com/ha-dataset3")

# Loading necessary packages and custom functions #


library(lme4)
library(influence.ME)
library(lattice)
library(cAIC4)
library(r2glmm)
library(MuMIn)
library(lmerTest)

stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object,"y"))
  sdx <- apply(getME(object,"X"), 2, sd)
  sc <- fixef(object)*sdx/sdy
  se.fixef <- coef(summary(object))[,"Std. Error"]
  se <- se.fixef*sdx/sdy
  return(data.frame(stdcoef=sc, stdse=se))
}

# Checking data #

view(data_sample_3)

data_sample_3 %>% 
  summary()

data_sample_3 %>% 
  describe()

# Correcting errors, excluding faulty observations, specifying categorical variables as factors #

data_sample_3_corr = data_sample_3 %>% 
  mutate(sex = replace(sex, sex=="femlae","female")) %>% 
  mutate(sex = factor(sex), hospital = factor(hospital), ID = factor(ID)) %>% 
  filter(household_income != -6994)

levels(data_sample_3_corr$hospital)

# Checking the data again, after the previous changes #

view(data_sample_3_corr)

data_sample_3_corr %>% 
  summary()

data_sample_3_corr %>% 
  select(-ID, -sex, -hospital) %>%
  describe()

model_hospital_intercept = lmer(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + (1 | hospital), data = data_sample_3_corr)

# Looking for influential outliers #

influence_obs_int = influence(model_hospital_intercept, obs = T)$alt.fixed

plot_influence_int = as_tibble(influence_obs_int) %>% 
  gather(colnames(influence_obs_int), value = coefficient, key = predictor)

plot_influence_int %>% 
  ggplot()+
  aes(x = 1, y = coefficient, group = predictor)+
  geom_violin()+
  geom_jitter(width = 0.2)+
  facet_wrap( ~ predictor, scales = "free")

influence_gr_int = influence(model_hospital_intercept, group = "hospital")$alt.fixed

plot_influence_int_gr = as_tibble(influence_gr_int) %>% 
  gather(colnames(influence_gr_int), value = coefficient, key = predictor)

plot_influence_int_gr %>% 
  ggplot()+
  aes(x = 1, y = coefficient, group = predictor)+
  geom_violin()+
  geom_jitter(width = 0.2)+
  facet_wrap( ~ predictor, scales = "free")

# Checking the assumptions for mixed linear regression models #

qqmath(model_hospital_intercept, id=0.05)

data_sample_3_corr_resid = data_sample_3_corr %>% 
  mutate(resid_int = residuals (model_hospital_intercept))

data_sample_3_corr_resid %>% 
  ggplot()+
  aes(sample = resid_int)+
  stat_qq()+
  stat_qq_line()+
  facet_wrap ( ~ hospital, scales = "free")

qqmath(ranef(model_hospital_intercept))

plot(model_hospital_intercept, arg = "pearson")

homosced_mod_int = lm (resid_int^2 ~ hospital, data = data_sample_3_corr_resid)
summary(homosced_mod_int)

pairs.panels(data_sample_3_corr[,c("age", "sex", "STAI_trait", "pain_cat", "mindfulness", "cortisol_serum")])

# Reporting results of model_hospital_intercept #

summary(model_hospital_intercept)

confint(model_hospital_intercept)

stdCoef.merMod(model_hospital_intercept)

r2beta(model_hospital_intercept, method = "nsj", data = data_sample_3_corr)

r.squaredGLMM(model_hospital_intercept)

data_sample_4 = read.csv("https://tinyurl.com/ha-dataset4")

View(data_sample_4)

pred_hospital_intercept_model = predict(model_hospital_intercept, data_sample_4, allow.new.levels = TRUE)

RSS_test_HIM = sum((data_sample_4[, "pain"] - pred_hospital_intercept_model)^2)

mean_HIM = lm(pain ~ 1, data = data_sample_4)

TSS_test_HIM = sum ((data_sample_4[, "pain"] - predict(mean_HIM))^2)

R2HIM_pred_data4 = 1 - (RSS_test_HIM/TSS_test_HIM)
R2HIM_pred_data4

# Building random slope model using only the strongest predictor of model_hospital_slope #

model_hospital_slope = lmer(pain ~ cortisol_serum + (cortisol_serum | hospital), data = data_sample_3_corr)

model_hospital_slope_opt = lmer(pain ~ cortisol_serum + (cortisol_serum | hospital), control = lmerControl(optimizer = "Nelder_Mead"), data = data_sample_3_corr)

# Visualizing the fitted regression lines of every level of the factor hospital in the model model_hospital_slope_opt separately #

data_sample_3_corr = data_sample_3_corr %>% 
  mutate(pred_hospital_slope = predict(model_hospital_slope_opt))

data_sample_3_corr %>% 
  ggplot() +
  aes(y = pain, x = cortisol_serum, group = hospital) +
  geom_point(aes(color = hospital), size = 3) +
  geom_line(aes(y = pred_hospital_slope, x = cortisol_serum)) +
  facet_wrap(~ hospital, ncol = 5)

dev.off()



