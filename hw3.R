library(tidyverse)
library(dplyr)
library(ggplot2)
library(reshape2)
library(ggpubr)
theme_set(theme_pubr())
library(manipulate)
library(patchwork)
library(infer)
library(broom)


labels_make = function(linear, x = 'Brain_Size_Species_Mean'){
  linear_res = linear[["coefficients"]]
  slope = linear_res[[x]]
  intercept = linear_res[['(Intercept)']]
  labels = paste("E[Y|X] =", toString(round(slope, digits = 3)), "X +",  
                 toString(round(intercept, digits = 3)))
  return(list(slope = slope, intercept = intercept, labels = labels))
}


cal_ci = function(means, std, n, conf = 0.975){
  error = qnorm(conf) * std / sqrt(n)
  return(c(means - error, means + error))
}


# challenge 1
# 1
df = as_tibble(read.csv("KamilarAndCooperData.csv"))
df_lm = df[c("WeaningAge_d", "Brain_Size_Species_Mean")] %>% drop_na()
linear = lm(WeaningAge_d ~ Brain_Size_Species_Mean, data = df_lm)
linear_res = labels_make(linear)

ggplot(data = df_lm, aes(x = Brain_Size_Species_Mean, y = WeaningAge_d)) + geom_point() + 
  geom_abline(intercept = linear_res[['intercept']], slope = linear_res[['slope']], col = 'blue') + 
  geom_text(x = 400, y = 1200, label = linear_res[['labels']], size = 4) +
  ggtitle("The fit model of weaning age ~ brain size")

log_linear = lm(log(WeaningAge_d) ~ log(Brain_Size_Species_Mean), data = df_lm)
log_linear_res = labels_make(log_linear, x = 'log(Brain_Size_Species_Mean)')

ggplot(data = df_lm, aes(x = log(Brain_Size_Species_Mean), y = log(WeaningAge_d))) + geom_point() + 
  geom_abline(intercept = log_linear_res[['intercept']], slope = log_linear_res[['slope']], col = 'blue') + 
  geom_text(x = 400, y = 6.5, label = log_linear_res[['labels']], size = 4) + 
  ggtitle("The fit model of log(weaning age) ~ log(brain size)")

for (i in names(summary(linear))){
  print(i)
  print(summary(linear)[[i]])
}

tidy(linear)[['statistic']]

summary(linear)[['coefficients']][[1]]

# 2

b = summary(linear)[['coefficients']][[2]]
b_log = summary(log_linear)[['coefficients']][[2]]
print(paste("regular model's estimated beta_1 is", toString(b), ", log model's estimated beta_1 is", toString(b_log)))


b_t = tidy(linear)
b_log_t = tidy(log_linear)

print(paste("t value of regular model of beta_1 is ", toString(b_t[['statistic']][[2]])))
print(paste("t value of log model of beta_1 is ", toString(b_log_t[['statistic']][[2]])))

print(paste("p value of regular model of beta_1 is ", toString(b_t[['p.value']][[2]])))
print(paste("p value of log model of beta_1 is ", toString(b_log_t[['p.value']][[2]])))

linear_ci = confint(linear, "Brain_Size_Species_Mean", level = 0.9)
log_linear_ci = confint(log_linear, "log(Brain_Size_Species_Mean)", level = 0.9)

print(paste("CI of beta_1 of regular model is ", toString(linear_ci)))
print(paste("CI of beta_1 of log model is ", toString(log_linear_ci)))

# 3
p = data.frame(df_lm[['Brain_Size_Species_Mean']])
names(p) = 'Brain_Size_Species_Mean'
pi = predict(linear, newdata = p, interval = 'prediction', level = 0.9)
pi = cbind(df_lm$Brain_Size_Species_Mean, data.frame(pi))
names(pi) = c('Brain_Size_Species_Mean', 'fit', 'lwr', 'upr')
df_pi = melt(pi, id = 'Brain_Size_Species_Mean')
ggplot(data = pi) + geom_point(aes(x = Brain_Size_Species_Mean, y = fit))
ggplot(data = df_pi) + geom_line(aes(x = Brain_Size_Species_Mean, y = value, color = variable)) + 
  geom_point(data = df_lm, aes(x = Brain_Size_Species_Mean, y = WeaningAge_d)) + 
  ggtitle("y ~ x linear regression with 90% prediction confidence interval")

p = data.frame(df_lm[['Brain_Size_Species_Mean']])
names(p) = 'Brain_Size_Species_Mean'
pi = predict(log_linear, newdata = p, interval = 'prediction', level = 0.9)
pi = cbind(log(p), data.frame(pi))
names(pi) = c('Brain_Size_Species_Mean', 'fit', 'lwr', 'upr')
df_pi = melt(pi, id = 'Brain_Size_Species_Mean')
ggplot(data = df_pi) + geom_line(aes(x = Brain_Size_Species_Mean, y = value, color = variable)) + 
  geom_point(data = df_lm, aes(x = log(Brain_Size_Species_Mean), y = log(WeaningAge_d))) + 
  ggtitle("log(y) ~ log(x) linear regression with 90% prediction confidence interval")
# 4
pe = predict(linear, newdata = data.frame(Brain_Size_Species_Mean = 750), 
                         interval = 'confidence', level = 0.9)
pe_log = predict(log_linear, newdata = data.frame(Brain_Size_Species_Mean = 750), 
                                  interval = 'confidence', level = 0.9)

pe_log

# challenge 2

# 1
linear = lm(log(MeanGroupSize) ~ log(Body_mass_female_mean), data = df)
linear[['coefficients']]

# 2
df_n = df[c('MeanGroupSize', 'Body_mass_female_mean')] %>% drop_na()
slope = list()
intercept = list()
for (i in 1:1000){
  df_d = sample_n(df_n, 33, replace = TRUE)
  linear_t = lm(log(MeanGroupSize) ~ log(Body_mass_female_mean), data = df_d)
  coef = linear_t[['coefficients']]
  intercept[[i]] = coef[['(Intercept)']]
  slope[[i]] = coef[['log(Body_mass_female_mean)']]
}
intercept = as.numeric(matrix(intercept))
slope = as.numeric(matrix(slope))
df_co = data.frame(intercept)
df_co[['slope']] = slope
a = ggplot(data = df_co, aes(x = slope)) + geom_histogram(bins = 30) + ggtitle("Slope histograme")
b = ggplot(data = df_co, aes(x = intercept)) + geom_histogram(bins = 30)+ ggtitle("intercept histograme")
ggarrange(a, b, labels = c('a', 'b'), ncol = 2, nrow = 1)

# 3
intercept_sd = sd(df_co[['intercept']])
slope_sd = sd(df_co[['slope']])
intercept_sd
slope_sd

# 4
intercept_ci = quantile(df_co[['intercept']], probs = c(0.025, 0.975))
slope_ci = quantile(df_co[['slope']], probs = c(0.025, 0.975))
intercept_ci
slope_ci

# 5
summary(linear)
linear_ci = confint(linear, level = 0.95)
linear_ci

# challenge 3

boot_lm_boot = function(d, model, conf.level = 0.95, reps = 1000){
  linear = lm(model, data = d)
  col_name = names(linear[['coefficients']])
  sum_linear = summary(linear)
  df_r = data.frame(sum_linear[['coefficients']][,c('Estimate', 'Std. Error')])
  df_co = data.frame(confint(linear, c(col_name), conf.level))
  df_r = merge(df_r, df_co, by = 0)
  names(df_r) = c("beta", "Estimate", "standard_error", "lower", "upper")
  coef = list()
  col_name = df_r$beta
  for (i in df_r$beta){
    coef[[i]] = list()
  }
  for (i in 1:reps){
    df_d = sample_n(d, 33, replace = TRUE)
    linear_t = lm(model, data = df_d)
    for (col in col_name){
      coef[[col]][[i]] = linear_t[['coefficients']][[col]]
    }
  }
  df_f = data.frame(as.numeric(matrix(coef[[col_name[[1]]]])))
  names(df_f) = col_name[[1]]
  for (i in 2:length(col_name)){
    df_f[[col_name[[i]]]] = as.numeric(matrix(coef[[col_name[[i]]]]))
  }
  means = list()
  std = list()
  lower = list()
  upper = list()
  for (i in 1:length(col_name)){
    means[[i]] = mean(df_f[[i]])
    std[[i]] = sd(df_f[[i]])
    lower[[i]] = quantile(df_f[[col_name[[i]]]], probs = (1-conf.level) / 2)[[1]]
    upper[[i]] = quantile(df_f[[col_name[[i]]]], probs = 1 - (1 - conf.level)/2)[[1]]
  }
  df_r[['estimate_sample']] = unlist(means)
  df_r[['standard_error_sample']] = unlist(std)
  df_r[['lower_sample']] = unlist(lower)
  df_r[['upper_sample']] = unlist(upper)
  # print(class(df_r[['estimate_sample']]))
  return(df_r)
}
res_1 = boot_lm(df, 'log(MeanGroupSize) ~ log(Body_mass_female_mean)')
res_2 = boot_lm(df, 'log(DayLength_km) ~ log(Body_mass_female_mean)')
res_3 = boot_lm(df, 'log(DayLength_km) ~ log(Body_mass_female_mean) + log(MeanGroupSize)')
df_drop = df[c('MeanGroupSize', 'Body_mass_female_mean')] %>%drop_na()
final = boot_lm(df_drop, 'log(MeanGroupSize) ~ log(Body_mass_female_mean)', reps = 10)
final = final[,c('beta', 'Estimate', 'estimate_sample', 'lower_sample', 'upper_sample')]
final[['reps']] = 10
for (i in 1:38){
  t = boot_lm(df_drop, 'log(MeanGroupSize) ~ log(Body_mass_female_mean)', reps = i * 5 + 10)
  t = t[,c('beta', 'Estimate', 'estimate_sample', 'lower_sample', 'upper_sample')]
  t[['reps']] = i * 5 + 10
  final = data.frame(rbind(final, t))
}
final = final %>% filter(beta == 'log(Body_mass_female_mean)')
final = final[, !names(final) %in% c('beta')]
final_m = melt(final, id = 'reps')
ggplot(data = final_m) + geom_line(aes(x = reps, y = value, color = variable))


slope = df %>% specify(as.formula('log(DayLength_km) ~ log(Body_mass_female_mean) + log(MeanGroupSize)')) %>%
  hypothesize(null = "independence") %>% generate(reps = 1000, type = "permute") %>% calculate(stat = 'slope')

slope = df %>% specify(formula = DayLength_km ~ Body_mass_female_mean) %>%
  hypothesize(null = "independence") %>% generate(reps = 1000, type = "permute") %>% calculate(stat = 'slope')
linear = lm(log(DayLength_km) ~ log(Body_mass_female_mean) + log(MeanGroupSize), data = df)
