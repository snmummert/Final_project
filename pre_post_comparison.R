library(glmmTMB)
library(ggplot2)
library(ggfortify)
library(lme4)
library(ggpubr)
library(DHARMa)

#you must load in prefusion_subset and postfusion_subset first

################ Groom instance and Rate ###################

#### IGNORE THIS SECTION ####

# Fuse groom instance rates from Pre and Post Fusion
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

############ groom rate regression (IGNORE THIS FOR NOW) ######################
Y = groom_rate_df$GroomRate # response
X1 = groom_rate_df$FusionPeriod # sep fixed effect
X2 = groom_rate_df$Focal_ID #sep fixed effect

#interaction for rate
groomingrate_glm = glm(GroomRate ~ FusionPeriod * Focal_ID, data = groom_rate_df)

#GLM for Groom Instances
groommodel = glm(groom_scans ~ FusionPeriod * Focal_ID + offset(log(total_scans)),
                 data = groom_rate_df, family = "poisson")

summary(groommodel)
    

sim <- simulateResiduals(fittedModel = groomingrate_glm)
plot(sim)
testDispersion(sim)
testZeroInflation(sim)
testUniformity(sim)

model_1_glmm <- glmer(
  groom_scans ~ FusionPeriod + (1 | Focal_ID),
  data = groom_rate_df,
  family = poisson
)
summary(model_1_glmm)

# From this GLMM (fit by max), family poisson (counts), we see that 
# my grooming scans decrease in the post-fusio period, 
# with effect size at -0.392 (on the log scale), and highly 
# significant results, with p = 1 x 10e-9. The random intercept standard dev
# is 0.368, meaning that IDs differ in their pre-fusion baseline

#This is the better model because it accounts for pre/post measures
#and has enough degrees of freedom to estime properly

predictdata <- expand.grid(
  FusionPeriod = unique(groom_rate_df$FusionPeriod),
  Focal_ID = unique(groom_rate_df$Focal_ID)
)
predictdata$pred <- predict(model_1_glmm, newdata = predictdata, type = "response")

# Plot GLMM Predicted Grooming Across Fusion
ggplot() +
  geom_point(
    data = groom_rate_df,
    aes(FusionPeriod, groom_scans, color = Focal_ID),
    position = position_jitter(width = 0.1)
  ) +
  geom_line(
    data = predictdata,
    aes(FusionPeriod, pred, color = Focal_ID, group = Focal_ID),
    size = 1.2
  ) +
  theme_bw() +
  labs(y = "Grooming Scans", title = "GLMM Predicted Grooming Across Fusion")



#please ignore this section I am coming back to work on it after this class, 
#the focus of my final is the Dyad Groom Rate section!






############################# Dyad Groom Rate  ##############################
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

dyad_groom_rate_df$Dyad <- paste(dyad_groom_rate_df$Groomer,
                                 dyad_groom_rate_df$Recipient,
                                 sep = "_")

dyad_change_df <- dyad_groom_rate_df %>%
  select(Dyad, FusionPeriod, dyad_rates) %>%
  pivot_wider(
    names_from = FusionPeriod,
    values_from = dyad_rates
  ) %>%
  mutate(Change = Post - Pre)

# Model testing

#Simple linear model
lm_model <- lm(dyad_rates ~ FusionPeriod, data = dyad_groom_rate_df)

#ANOVA
anova_model <- aov(dyad_rates ~ FusionPeriod, data = dyad_groom_rate_df)

#Linear mixed model
lmm_model <- lmer(dyad_rates ~ FusionPeriod + (1 | Dyad), data = dyad_groom_rate_df, REML=FALSE)

#Beta GLMM
beta_model <- glmmTMB(dyad_rates ~ FusionPeriod + (1 | Dyad),
                      family = beta_family(link="logit"),
                      data = dyad_groom_rate_df)

#Test model fit #
model_names <- c("LM", "ANOVA", "LMM", "Beta GLMM")
aic_values <- c(AIC(lm_model),
                AIC(anova_model),
                AIC(lmm_model),
                AIC(beta_model))

loglik_values <- c(logLik(lm_model),
                   logLik(anova_model),
                   logLik(lmm_model),
                   logLik(beta_model))

model_fit <- data.frame(
  Model = model_names,
  AIC = round(aic_values, 2),
  LogLik = round(as.numeric(loglik_values), 2)
)
model_fit

#Beta GLMM is the best model for my data, modeling the dyad grooming
#rates as propostions (0-1) and accounting for repeated measures
#It also has the highest log-likelihood and the lowest AIC

#Plot showing model fit
dyad_groom_rate_df$predicted <- predict(beta_model, type = "response")

dyad_groom_rate_df$predicted <- predict(beta_model, type = "response")

ggplot(dyad_groom_rate_df, aes(x = predicted, 
                               y = dyad_rates, 
                               color = FusionPeriod)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_abline(intercept = 0, slope = 1, 
              linetype = "dashed", color = "orchid") +
  scale_color_manual(values = c("Pre" = "deeppink",
                                "Post" = "blue")) +
theme_classic(base_size = 14) +
  labs(
    x = "Predicted Grooming Rate",
    y = "Observed Grooming Rate",
    color = "Fusion Period",
    title = "Beta GLMM: Predicted vs Observed Dyadic Grooming Rates"
  )

# check assumptions
sim_res <- simulateResiduals(fittedModel = beta_model)

plotResiduals(sim_res)

testUniformity(sim_res) # cannot reject uniformity 

testDispersion(sim_res) # no concerns about dispersion/overdispered

testOutliers(sim_res) # no major outliers

#Model diagnostics performed using DHARMa revealed simulated residuals 
#were unifrm (KS test), dispersion was not significant, and no outliers. 
#This means assumptions were met and the Beta GLMM provides adequate fit.
#This also means that my lack of significance in Fusion is not to do with 
#poor model fit.

#what happened
ggplot(dyad_groom_rate_df, aes(x=FusionPeriod, y=dyad_rates, group=Dyad)) +
  geom_line(alpha=0.5) +
  geom_point(aes(color=FusionPeriod)) +
  theme_classic() +
  ylab("Dyadic Grooming Rate") +
  ggtitle("Pre- vs Post-Fusion Grooming by Dyad")

#change
summary(dyad_change_df$Change)
hist(dyad_change_df$Change)

ggplot(dyad_change_df, aes(x = Change)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.05, fill = "darkblue", color = "black", alpha = 0.6) +
  geom_density(color = "darkred", size = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "orchid") +
  xlab("Dyadic grooming change (Post - Pre)") +
  ylab("Density") +
  ggtitle()
  theme_classic()


summary(beta_model)
#interpretation of best model: 
#Accross comparable dyads, grooming rates did not change substantially
#from before fusion to after fusion. Change was approx 1.3, p = 0.884.
#Fusion did not cause a net increase or decrease in dyad grooming relationships
#accross the group (ave)....

#interpretation of Change
#...however, about half of the rates increase and half decreased
#This means that there was net change, just not towards one way or another.
#Could still indicate that there was some level of disruption.

#Still have work to do to test change in general. 

