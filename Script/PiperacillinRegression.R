# ============================================================
# Piperacillin Steady-State Concentration vs Infusion Rate
# Linear Regression Analysis
# ============================================================

library(tidyverse)

# ---- Data Import ----
Piperacillin <- read_csv("https://raw.githubusercontent.com/dlonsdal/SGUL_PK_data/main/pip_ss.csv")

# ---- Exploratory Plot ----
p_scatter <- ggplot(Piperacillin,aes(infusion_rate, pip_ss))+
  geom_point()+
  stat_smooth(method='lm', colour='blue',se=F)+
  theme_bw()+
  labs(
    x='Infusion Rate (mg/hr)', 
    y='Steady-State Concentration (mg/L)',
    title = "Piperacillin Steady-State Concentration vs Infusion Rate") 

ggsave("pip_ss_vs_rate.pdf", p_scatter, width = 7, height = 5)


# ---- Correlation Analysis ----
cor_test <- cor.test(
  Piperacillin$infusion_rate, 
  Piperacillin$pip_ss, 
  method='pearson')

# ---- Linear Regression Model ----
Piperacillin_model <- lm(pip_ss~infusion_rate, data=Piperacillin)
summary(Piperacillin_model)

# ---- Mean Clearance ----
# Css = Rate / CL  →  slope ≈ 1 / CL
mean_clearance <- 1 / coef(Piperacillin_model)[2]
mean_clearance

# ---- Predict Infusion Rate for Mean Css = 16 mg/L ----
target_css <- 16

rate_mean_16 <- (target_css - coef(Piperacillin_model)[1]) / coef(Piperacillin_model)[2]
rate_mean_16

# ---- Infusion Rate to Ensure 90% Achieve Css ≥ 16 mg/L ----

Prediction_interval <- predict(Piperacillin_model, interval = 'prediction', level = 0.8)
Piperacillin <- cbind(Piperacillin,Prediction_interval)

Pip_16 <- data.frame(infusion_rate=seq(210,240,by=5)) # 16 mg/l falls between infusion rates of 210 to 240
infusion_interval <- predict(Piperacillin_model, newdata=Pip_16, interval = 'prediction', level = 0.8)
Pip_16 <- cbind(Pip_16,infusion_interval)

rate_90_target <- Pip_16 %>%
  filter(lwr >= 16) %>%
  slice(1)

rate_90_target






