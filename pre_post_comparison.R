library(glmmTMB)
library(ggplot2)
library(ggfortify)
library(lme4)
library(ggpubr)
########################### rate dfs ############################

# Fuse groom rates from Pre and Post Fusion
prefusion_subset_groom_rate_df$FusionPeriod <- "Pre"
postfusion_subset_groom_rate_df$FusionPeriod <- "Post"

groom_rate_df = bind_rows(
  prefusion_subset_groom_rate_df,
  postfusion_subset_groom_rate_df
)

combined_rates_long <- groom_rate_df %>%
  select(-FusionPeriod) %>%   # <-- remove old column
  pivot_longer(
    cols = c(prefusion_grooming_rate, postfusion_grooming_rate),
    names_to = "FusionPeriod",
    values_to = "GroomRate",
    values_drop_na = TRUE
  ) %>%
  mutate(
    FusionPeriod = ifelse(FusionPeriod == "prefusion_grooming_rate", "Pre", "Post"),
    FusionPeriod = factor(FusionPeriod, levels = c("Pre", "Post"))
  )

groom_rate_df <- combined_rates_long %>%
  group_by(Focal_ID) %>%
  filter(n_distinct(FusionPeriod) == 2) %>%
  ungroup()

# dyad rate pre/post fusion 
dyad_summary$FusionPeriod <- "Pre"
dyad_summary2$FusionPeriod <- "Post"

combined_dyad_rates = bind_rows(
  dyad_summary, 
  dyad_summary2
)

dyad_groom_rate_df<- combined_dyad_rates %>%
  group_by(Groomer, Recipient) %>%
  filter(n_distinct(FusionPeriod) == 2) %>%
  ungroup()

####################################################################
Y = groom_rate_df$GroomRate
X1 = groom_rate_df$FusionPeriod

#Just do model_X1 and the null 
model_X1   <- glm(Y ~ X1, data = groom_rate_df)
null = glm(GroomRate ~ 1, data = groom_rate_df)
  
models_list = list(
  "X1" = model_X1,
  "null" = null
)

aic_tab <- data.frame(
  Model = names(models_list),
  AIC = sapply(models_list, AIC)
)

aic_tab$DeltaAIC <- aic_tab$AIC - min(aic_tab$AIC)
aic_tab$Weight <- exp(-0.5 * aic_tab$DeltaAIC) /
  sum(exp(-0.5 * aic_tab$DeltaAIC))

aic_tab

ggpredict()

#################### Beta GLMM #####################
paired_model <- glmmTMB(
  GroomRate ~ FusionPeriod + (1 | Focal_ID),
  family = beta_family(link = "logit"),
  data = groom_rate_df
)

summary(paired_model)

#Results: Fusion did not have significant effect on overall rate of ID grooming 
#positive estimate: 0.04, p = 0.825 
#no evidence of change from pre to post fusion
#AIC = -21.7, BIC = -18.6 : standard errors are large

plot1 = ggplot(groom_rate_df, aes(x = FusionPeriod, y = GroomRate, group = Focal_ID)) +
  geom_line(aes(color = Focal_ID)) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(title = "Grooming Rates Pre vs Post Fusion", 
       x = "Subset Period",
       y = "Grooming Rate")

############### Paired non-parametric ###################

# Reshape to wide for paired test
groom_rates_wide <- groom_rate_df %>%
  pivot_wider(names_from = FusionPeriod, values_from = GroomRate)

# Wilcoxon paired test
wilcox.test(groom_rates_wide$Pre, groom_rates_wide$Post, paired = TRUE)

#################### anova? ####################

anova_model <- aov(GroomRate ~ FusionPeriod, data = groom_rate_df)
summary(anova_model)

ggplot(groom_rate_df, aes(x = FusionPeriod, y = GroomRate, fill = FusionPeriod)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.1, size = 2, color = "black") + # adds individual points
  theme_minimal() +
  labs(
    title = "Grooming Rate by Fusion Period",
    x = "Fusion Period",
    y = "Grooming Rate"
  )
ggboxplot(groom_rate_df, x = "FusionPeriod", y = "GroomRate", color = "FusionPeriod") +
  stat_compare_means(method = "anova")

