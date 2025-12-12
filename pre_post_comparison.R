library(ggplot2)
library(lme4)
library(glmmTMB)
library(DHARMa)

#you must load in prefusion_subset and postfusion_subset first

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
summary(lm_model)

#Linear mixed model
lmm_model <- lmer(dyad_rates ~ FusionPeriod + (1 | Dyad), data = dyad_groom_rate_df, REML=FALSE)
summary(lm_model)

#Beta GLMM
beta_model <- glmmTMB(dyad_rates ~ FusionPeriod + (1 | Dyad),
                      family = beta_family(link="logit"),
                      data = dyad_groom_rate_df)
summary(beta_model)

#Test model fit #
model_names = c("LM", "LMM", "Beta GLMM")
aic_values = c(AIC(lm_model),
                AIC(lmm_model),
                AIC(beta_model))

loglik_values = c(logLik(lm_model),
                   logLik(lmm_model),
                   logLik(beta_model))

delta_aic = aic_values - min(aic_values)

model_fit <- data.frame(
  Model = model_names,
  AIC = round(aic_values, 2),
  LogLik = round(as.numeric(loglik_values), 2)
)
model_fit$Delta_AIC = round(delta_aic, 2)
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
res = simulateResiduals(fittedModel = beta_model)

plot(res)

testUniformity(res) #cannot reject uniformity 

testDispersion(res) # no concerns about dispersion/overdispered

testOutliers(res) # no major outliers

summary(beta_model)$varcor # Dyad as a random effect, SD = .081053

boxplot(dyad_rates ~ FusionPeriod, data=dyad_groom_rate_df,
        ylab="Dyad Grooming Rate", xlab="Fusion Period")


#Model diagnostics performed using DHARMa revealed simulated residuals 
#were unifrm (KS test), dispersion was not significant, and no outliers. 
#This means assumptions were met and the Beta GLMM provides adequate fit.
#This also means that my lack of significance in Fusion is not to do with 
#poor model fit.

#what happened
dyad_groom_rate_df$FusionPeriod <- factor(dyad_groom_rate_df$FusionPeriod, 
                                          levels = c("Pre", "Post"))

# Plot again
ggplot(dyad_groom_rate_df, aes(x = FusionPeriod, y = dyad_rates, group = Dyad)) +
  geom_line(alpha = 0.5) +
  geom_point(aes(color = FusionPeriod)) +
  scale_color_manual(values = c("Pre" = "darkorchid", "Post" = "orchid")) +
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
  theme_classic()

ggplot(dyad_change_df, aes(x = Dyad, y = Change)) +
  geom_point(size = 3, color = "darkblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "orchid") +
  theme_classic() +
  xlab("Dyad") +
  ylab("Dyadic grooming change (Post - Pre)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


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

