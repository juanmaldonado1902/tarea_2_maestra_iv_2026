# =========================================================
# TAREA IV - versión simplificada
# Creada por Juan Pablo Maldonado Rojas
# =========================================================

library(tidyverse)
library(fixest)
library(gmm)

# ---------------------------------------------------------
# Cargar base (el if exists esta por un eror extraño que tuve durante la codificación de los data frame pero no es necesario)
# ---------------------------------------------------------
if (!exists("india_df", inherits = FALSE)) {
  india_df <- read_csv("../india_base_final.csv", show_col_types = FALSE)
}
moda <- function(x) names(sort(table(na.omit(x)), decreasing = TRUE))[1] # función con base en la generación de las predicciones

# ---------------------------------------------------------
# Limpiar base
# ---------------------------------------------------------
df0 <- india_df %>%
  mutate(
    across(c(Pole, gender, religion, caste, birthplace), as.factor)
  ) %>%
  filter(
    !is.na(total_expenditure),
    !is.na(treat),
    !is.na(distance),
    !is.na(gender),
    !is.na(age),
    !is.na(religion),
    !is.na(caste),
    !is.na(birthplace),
    !is.na(Pole)
  )

# =========================================================
# IV simple (creando la variable forcing, la cual tecnicamente no se necesitaría porque esta en la base de datos)
# De igual forma dado que no hay datos en el intervalo de 35-55 tampoco habría una necesidad de mayor especificación
# =========================================================
df_iv <- df0 %>%
  filter((distance >= 20 & distance <= 35) | (distance >= 45 & distance <= 60)) %>%
  mutate(
    forcing = as.integer(distance >= 20 & distance <= 35),
    experimental_condition = factor(ifelse(forcing == 1, "Forcing", "Control"),
                                    levels = c("Control", "Forcing"))
  )

# Modelos en donde tambien e desarrollan las predicciones para realizar las gráficas más adelante
fs_forcing <- feols(treat ~ forcing + gender + age + religion + caste + birthplace | Pole, data = df_iv)
fs_distance <- feols(treat ~ distance + gender + age + religion + caste + birthplace | Pole, data = df_iv)
rf_forcing <- feols(total_expenditure ~ forcing + gender + age + religion + caste + birthplace | Pole, data = df_iv)
rf_distance <- feols(total_expenditure ~ distance + gender + age + religion + caste + birthplace | Pole, data = df_iv)
iv_simple   <- feols(total_expenditure ~ gender + age + religion + caste + birthplace | Pole | treat ~ forcing, data = df_iv)

wald_iv <- coef(rf_forcing)["forcing"] / coef(fs_forcing)["forcing"]

# ---------------------------------------------------------
#  Esta parte del código realiza la figura 2 (sobre la cual después genero las predicciones para comparar forcing y distance)
# ---------------------------------------------------------
fig2_share <- df_iv %>%
  group_by(distance, experimental_condition) %>%
  summarise(share_legal = mean(treat), .groups = "drop")

fig2_iv <- ggplot() +
  geom_jitter(data = df_iv,
              aes(distance, treat, color = experimental_condition),
              width = 0.25, height = 0.02, alpha = 0.25, size = 2.5) +
  geom_point(data = fig2_share,
             aes(distance, share_legal, fill = experimental_condition),
             shape = 22, color = "darkred", size = 2.8, stroke = 0.4) +
  geom_vline(xintercept = 40, linetype = "dashed") +
  labs(x = "Distance From Pole", y = "Share With Legal Connection",
       color = "Experimental\nCondition", fill = "Experimental\nCondition") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "right", panel.grid.minor = element_blank())

ggsave("fig2_iv_replica.png", fig2_iv, width = 10, height = 6, dpi = 300)

# ---------------------------------------------------------
# Grid de predicción (con tibble genero la creación de valores y establezco otra vez a forcing desde cero)
# Mantengo las demás covariables constantes
# ---------------------------------------------------------
pred_grid <- tibble(distance = c(20:35, 45:60)) %>%
  mutate(
    forcing = as.integer(distance >= 20 & distance <= 35),
    experimental_condition = factor(ifelse(forcing == 1, "Forcing", "Control"),
                                    levels = c("Control", "Forcing")),
    gender = factor(moda(df_iv$gender), levels = levels(df_iv$gender)),
    age = mean(df_iv$age),
    religion = factor(moda(df_iv$religion), levels = levels(df_iv$religion)),
    caste = factor(moda(df_iv$caste), levels = levels(df_iv$caste)),
    birthplace = factor(moda(df_iv$birthplace), levels = levels(df_iv$birthplace)),
    Pole = factor(moda(df_iv$Pole), levels = levels(df_iv$Pole)),
    pred_forcing = predict(fs_forcing, newdata = cur_data()),
    pred_distance = predict(fs_distance, newdata = cur_data()),
    pred_rf_forcing = predict(rf_forcing, newdata = cur_data()),
    pred_rf_distance = predict(rf_distance, newdata = cur_data())
  )

# ---------------------------------------------------------
# Gráfica treat con predicciones ( versión para explicar diferencias entre uso de distance o forcing)
# ---------------------------------------------------------
fig_pred_treat <- ggplot() +
  geom_jitter(data = df_iv,
              aes(distance, treat, color = experimental_condition),
              width = 0.25, height = 0.02, alpha = 0.20, size = 2.2) +
  geom_step(data = pred_grid,
            aes(distance, pred_forcing, linetype = "Predicción con forcing"),
            linewidth = 1.1, color = "black") +
  geom_line(data = pred_grid,
            aes(distance, pred_distance, linetype = "Predicción con distance"),
            linewidth = 1.1, color = "darkred") +
  geom_vline(xintercept = 40, linetype = "dashed") +
  labs(x = "Distance From Pole", y = "Predicted / Observed Legal Connection",
       color = "Experimental\nCondition", linetype = "") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "right", panel.grid.minor = element_blank())

ggsave("fig_pred_treat_forcing_distance.png", fig_pred_treat, width = 10, height = 6, dpi = 300)

# ---------------------------------------------------------
# Gráfica gasto vs distance (aqui ocupo las predicciones que genere anteriormente)
# ---------------------------------------------------------
rf_share <- df_iv %>%
  group_by(distance, experimental_condition) %>%
  summarise(mean_expenditure = mean(total_expenditure), .groups = "drop")

fig_outcome_distance <- ggplot() +
  geom_point(data = df_iv,
             aes(distance, total_expenditure, color = experimental_condition),
             alpha = 0.18, size = 2) +
  geom_point(data = rf_share,
             aes(distance, mean_expenditure, fill = experimental_condition),
             shape = 22, color = "darkred", size = 2.8, stroke = 0.4) +
  geom_step(data = pred_grid,
            aes(distance, pred_rf_forcing, linetype = "Predicción RF con forcing"),
            linewidth = 1.1, color = "black") +
  geom_line(data = pred_grid,
            aes(distance, pred_rf_distance, linetype = "Predicción RF con distance"),
            linewidth = 1.1, color = "darkred") +
  geom_vline(xintercept = 40, linetype = "dashed") +
  labs(x = "Distance From Pole", y = "Total Expenditure",
       color = "Experimental\nCondition", fill = "Experimental\nCondition", linetype = "") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "right", panel.grid.minor = element_blank())

ggsave("fig_total_expenditure_distance.png", fig_outcome_distance, width = 10, height = 6, dpi = 300)

# =========================================================
# IV (con las variables múltiples)
# =========================================================
df_7 <- df0 %>%
  mutate(
    dist_group = case_when(
      distance <= 25 ~ "Z1_<=25",
      distance <= 40 ~ "Z2_25_40",
      distance < 55  ~ "Z3_40_55",
      TRUE ~ "Z4_>=55"
    ),
    dist_group = factor(dist_group, levels = c("Z1_<=25", "Z2_25_40", "Z3_40_55", "Z4_>=55"))
  )

fs_multi <- feols(treat ~ i(dist_group, ref = "Z4_>=55") + gender + age + religion + caste + birthplace | Pole, data = df_7)
iv_multi <- feols(total_expenditure ~ gender + age + religion + caste + birthplace | Pole | treat ~ i(dist_group, ref = "Z4_>=55"), data = df_7)

# =========================================================
# GMM
# =========================================================
X <- model.matrix(~ treat + gender + age + religion + caste + birthplace + Pole, data = df_7)
Z <- model.matrix(~ relevel(dist_group, ref = "Z4_>=55") + gender + age + religion + caste + birthplace + Pole, data = df_7)
y <- df_7$total_expenditure

g_iv <- function(theta, data) {
  u <- as.vector(data$y - data$X %*% theta)
  data$Z * u
}

gmm_7 <- gmm(g = g_iv, x = list(y = y, X = X, Z = Z), t0 = coef(lm(y ~ X - 1)), type = "twoStep")

nom_treat_gmm <- names(coef(gmm_7))[grep("treat", names(coef(gmm_7)))][1]
coef_gmm <- unname(coef(gmm_7)[nom_treat_gmm])
se_gmm   <- unname(sqrt(diag(vcov(gmm_7)))[nom_treat_gmm])
hansen_j <- specTest(gmm_7)

# =========================================================
# Comparación final (coloca los datos obtenidos en la tarea anterior)
# =========================================================
tabla_final <- tibble(
  estimador = c("RCT (Neyman)", "RCT estratificado", "IV simple (Wald/2SLS)", "IV múltiple (2SLS)", "GMM formal"),
  coeficiente = c(2386.057, 2370.165, coef(iv_simple)["fit_treat"], coef(iv_multi)["fit_treat"], coef_gmm),
  error_estandar = c(356.360, 350.918, se(iv_simple)["fit_treat"], se(iv_multi)["fit_treat"], se_gmm)
) %>%
  mutate(t = coeficiente / error_estandar)

print(tabla_final)

# =========================================================
#  Tablas y pruebas f
# =========================================================
etable(fs_forcing, fs_distance, rf_forcing, iv_simple, se = "hetero",
       headers = c("Primera etapa: forcing", "Primera etapa: distance", "Forma reducida", "2SLS"))

etable(fs_multi, iv_multi, se = "hetero",
       headers = c("Primera etapa múltiple", "2SLS múltiple"))

wald(fs_forcing, keep = "forcing", vcov = "hetero")
wald(fs_distance, keep = "distance", vcov = "hetero")
wald(fs_multi, keep = "dist_group", vcov = "hetero")
print(hansen_j)

