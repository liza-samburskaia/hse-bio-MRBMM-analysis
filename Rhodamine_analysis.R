library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)

# Длины волн (нм)
wavelengths_abs <- seq(400, 600, by = 5)
wavelengths_em <- seq(525, 750, by = 5)
wavelengths_excit <- seq(400, 575, by = 5)

rodamin_c1 <- c(
  0.1633, 0.1541, 0.1468, 0.1392, 0.1336, 0.1346, 0.147, 0.1682,
  0.2099, 0.2675, 0.3532, 0.487, 0.6215, 0.782, 0.9619, 1.2417,
  1.6031, 2.0862, 2.4052, 2.5316, 2.5417, 2.4794, 2.3927, 2.3334,
  2.3146, 2.3039, 2.3029, 2.2928, 2.1167, 1.5359, 0.9025, 0.5675,
  0.3329, 0.2274, 0.1658, 0.1295, 0.0976, 0.0755, 0.0611, 0.0494, 0.0426
)

rodamin_c2 <- c(
  0.0492, 0.0471, 0.0454, 0.0436, 0.0421, 0.0414, 0.0422, 0.0442,
  0.0486, 0.0552, 0.0636, 0.0764, 0.0888, 0.1055, 0.1272, 0.16,
  0.2027, 0.2564, 0.3048, 0.3437, 0.3746, 0.4107, 0.4782, 0.5901,
  0.6818, 0.7517, 0.7311, 0.6027, 0.4194, 0.2632, 0.1494, 0.0933,
  0.0584, 0.0438, 0.0366, 0.0332, 0.0313, 0.0303, 0.0298, 0.0294, 0.0292
)

emission <- c(
  312497760, 789217152, 1299039360, 1805981952, 2250044672, 2522004224, 2595747328,
  2480473344, 2233696768, 1919504384, 1600118784, 1323688832, 1103699456, 930701184,
  787147712, 661997376, 551331904, 453829728, 371655904, 300634272, 241779520, 192531280,
  152956784, 122127080, 98532752, 79891944, 64404352, 51671672, 41538748, 33639224, 27234974,
  21800036, 17160990, 13370818, 10462532, 8199413, 6586735, 5434462, 4537337, 3833968, 
  3230060, 2727544, 2245943, 1823543, 1454098, 1115483
)

excitation <- c(
  50406568, 45599092, 41126532, 37493672, 35214264, 35134212, 38263800, 46276436,
  60130036, 80791744, 108435112, 139776144, 175290400, 218357120, 278372928,
  357782912, 449078688, 535012096, 606292672, 666266368, 734583296, 819123328,
  922273408, 1027301440, 1116566400, 1164910208, 1146444288, 1047595328, 872974144,
  655074752, 437102176, 259079776, 137856592, 67185848, 29520906, 8009226
)

endpoint_abs <- matrix(c(
  2.3006, 2.3127, 2.3117,
  2.0772, 2.0804, 2.0535,
  1.3919, 1.355, 1.518,
  0.7452, 0.7561, 0.7161,
  0.3851, 0.3867, 0.3596,
  0.195, 0.2001, 0.1856,
  0.0963, 0.102, 0.097,
  0.0525, 0.0598, 0.0583,
  0.0512, 0.0403, 0.0383,
  0.0355, 0.0339, 0.0334,
  0.0318, 0.0315, 0.0311,
  0.032, 0.0308, 0.0302,
  0.0306, 0.0293, 0.0296,
  0.0318, 0.0277, 0.0281,
  0.0293, 0.0299, 0.0288,
  0.0284, 0.0406, 0.0271
), nrow = 16, byrow = TRUE)

endpoint_flour <- matrix(c(
  4996479488, 4601844736, 4919254016,
  5721676288, 5777144320, 6404230144,
  5318425088, 5592438784, 4355696640,
  4554345984, 4583084032, 4597155840,
  3003600128, 3096140032, 2912393984,
  1626013184, 1697158784, 1601740928,
  676368768, 713350848, 707998080,
  327369808, 357629024, 339102752,
  149449320, 169946376, 153159752,
  62340002, 74168784, 69059522,
  23875848, 24762515, 23191014,
  5197961, 7863547, 5734872,
  2107661, 2358214, 2239336,
  1193759, 1644168, 1315123,
  251951, 492342, 477149,
  107310, 128620, 155061
), nrow = 16, byrow = TRUE)

concentrations <- 200 / (2^(0:15))

ggplot() +
  geom_line(aes(x = wavelengths_abs, y = rodamin_c1, color = "200 мкМ"), linewidth = 1) +
  geom_point(aes(x = wavelengths_abs, y = rodamin_c1, color = "200 мкМ"), size = 2) +
  geom_line(aes(x = wavelengths_abs, y = rodamin_c2, color = "25 мкМ"), linewidth = 1) +
  geom_point(aes(x = wavelengths_abs, y = rodamin_c2, color = "25 мкМ"), size = 2) +
  scale_color_manual(values = c("200 мкМ" = "#440154", "25 мкМ" = "#FDE725")) +
  scale_y_continuous(breaks = seq(0, 2.6, by = 0.25)) +
  scale_x_continuous(breaks = seq(400, 600, by = 10)) +
  labs(
    x = "Длина волны, нм",
    y = "Оптическая плотность (A)",
    title = "Спектры поглощения раствора родамина при концентрациях 200 мкМ и 25 мкМ",
    color = "Концентрация"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ex_fixed <- sprintf("excitation fixed = 495 нм")
em_fixed <- sprintf("emission fixed = 600 нм")

ggplot() +
  geom_line(aes(x = wavelengths_em, y = emission, color = "Спектр эмиссии"), linewidth = 1) +
  geom_point(aes(x = wavelengths_em, y = emission, color = "Спектр эмиссии"), size = 2) +
  geom_line(aes(x = wavelengths_excit, y = excitation, color = "Спектр возбуждения"), linewidth = 1) +
  geom_point(aes(x = wavelengths_excit, y = excitation, color = "Спектр возбуждения"), size = 2) +
  annotate("text", x = 600, y = 1.5e+09, label = ex_fixed,
           hjust = 0, size = 5, fontface = "bold", color = "#440154") +
  annotate("text", x = 385, y = 8.5e+08, label = em_fixed,
           hjust = 0, size = 5, fontface = "bold", color = "#FDE725") +
  scale_color_manual(values = c("Спектр эмиссии" = "#440154", "Спектр возбуждения" = "#FDE725")) +
  scale_y_continuous(breaks = seq(0, 3e+09, by = 3e+08)) +
  scale_x_continuous(breaks = seq(400, 750, by = 25)) + 
  labs(
    x = "Длина волны, нм",
    y = "Интенсивность флуоресценции (RFU)",
    title = "Спектры возбуждения и эмиссии флуоресценции раствора родамина",
    color = " "
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

emission_norm <- emission / max(emission) * 100
excitation_norm <- excitation / max(excitation) * 100
absorbtion_norm <- rodamin_c2 / max(rodamin_c2) * 100

ggplot() +
  geom_line(aes(x = wavelengths_em, y = emission_norm, color = "Спектр эмиссии"), linewidth = 1) +
  geom_point(aes(x = wavelengths_em, y = emission_norm, color = "Спектр эмиссии"), size = 2) +
  geom_line(aes(x = wavelengths_excit, y = excitation_norm, color = "Спектр возбуждения"), linewidth = 1) +
  geom_point(aes(x = wavelengths_excit, y = excitation_norm, color = "Спектр возбуждения"), size = 2) +
  annotate("text", x = 600, y = 50, label = ex_fixed,
           hjust = 0, size = 4, fontface = "bold", color = "#440154") +
  annotate("text", x = 385, y = 38, label = em_fixed,
           hjust = 0, size = 4, fontface = "bold", color = "#FDE725") +
  scale_color_manual(values = c("Спектр эмиссии" = "#440154", "Спектр возбуждения" = "#FDE725")) +
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  scale_x_continuous(breaks = seq(400, 750, by = 25)) +
  labs(
    x = "Длина волны, нм",
    y = "Процент от максимума, %",
    title = "Нормированные спектры возбуждения и эмиссии флуоресценции раствора родамина",
    color = " "
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ggplot() +
  geom_line(aes(x = wavelengths_abs, y = absorbtion_norm, color = "Спектр поглощения"), linewidth = 1) +
  geom_point(aes(x = wavelengths_abs, y = absorbtion_norm, color = "Спектр поглощения"), size = 2) +
  geom_line(aes(x = wavelengths_excit, y = excitation_norm, color = "Спектр возбуждения"), linewidth = 1) +
  geom_point(aes(x = wavelengths_excit, y = excitation_norm, color = "Спектр возбуждения"), size = 2) +
  scale_color_manual(values = c("Спектр поглощения" = "#440154", "Спектр возбуждения" = "#FDE725")) +
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  scale_x_continuous(breaks = seq(400, 750, by = 25)) +
  labs(
    x = "Длина волны, нм",
    y = "Процент от максимума, %",
    title = "Нормированные спектры поглощения и возбуждения флуоресценции раствора родамина",
    color = " "
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

mean_abs <- rowMeans(endpoint_abs)
sd_abs <- apply(endpoint_abs, 1, sd)
n_abs <- ncol(endpoint_abs)
sem_abs <- sd_abs / sqrt(n_abs)
t_crit_abs <- qt(0.975, df = n_abs - 1)
ci_95_abs <- t_crit_abs * sem_abs

df_endpoint_abs <- data.frame(
  Concentration = concentrations,
  Mean_A = mean_abs,
  CI_95 = ci_95_abs
)

ggplot(df_endpoint_abs, aes(x = Concentration, y = Mean_A)) +
  geom_point(size = 3, color = "#21918C") +
  geom_errorbar(aes(ymin = Mean_A - CI_95, ymax = Mean_A + CI_95), 
                width = 5, color = "#21918C") +
  scale_y_continuous(breaks = seq(0, 2.5, by = 0.25)) +
  scale_x_continuous(breaks = seq(0, 200, by = 10)) +
  labs(x = "Концентрация родамина, мкМ", y = "Оптическая плотность (A)",
       title = "Зависимость поглощения от концентрации родамина при длине волны 525 нм") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

#ЛИНЕЙНАЯ АППРОКСИМАЦИЯ

# Берем диапазон 1,5625 <= C <= 50 мкМ
df_lin_abs <- df_endpoint_abs %>% filter(Concentration >= 1,5625 & Concentration <= 50)

# Линейная регрессия с весами (1/CI^2)
fit_abs <- lm(Mean_A ~ Concentration, data = df_lin_abs, weights = 1/(CI_95^2))

# Результаты
summary(fit_abs)

k_abs <- coef(fit_abs)[["Concentration"]]
b_abs <- coef(fit_abs)[["(Intercept)"]]
k_err_abs <- summary(fit_abs)$coefficients["Concentration", "Std. Error"]
b_err_abs <- summary(fit_abs)$coefficients["(Intercept)", "Std. Error"]
r_squared_abs <- summary(fit_abs)$r.squared

eq_abs <- sprintf("A = %.4f·C + %.4f\nR² = %.4f", k_abs, b_abs, r_squared_abs)

ggplot(df_endpoint_abs, aes(x = Concentration, y = Mean_A)) +
  geom_point(size = 3, color = "#21918C") +
  geom_errorbar(aes(ymin = Mean_A - CI_95, ymax = Mean_A + CI_95), 
                width = 5, color = "#21918C") +
  annotate("text", x = 40, y = 0.75, label = eq_abs,
           hjust = 0, size = 5, fontface = "bold", color = "#440154") +
  scale_y_continuous(breaks = seq(0, 2.5, by = 0.25)) +
  scale_x_continuous(breaks = seq(0, 200, by = 10)) +
  geom_smooth(data = df_lin_abs, method = "lm", se = FALSE, color = "#440154") +
  labs(x = "Концентрация родамина, мкМ", y = "Оптическая плотность (A)",
       title = "Калибровочная зависимость поглощения от концентрации родамина при 525 нм") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

epsilon_abs <- (k_abs / 0.4478) * 1e6
epsilon_abs_err <- (k_err_abs / 0.4478) * 1e6

cat("=== Расчёт коэффициента экстинкции (ε230) ===\n")
cat(sprintf("  Наклон k = %.4f ± %.4f A/мкМ\n", k_abs, k_err_abs))
cat(sprintf("  Свободный член b = %.4f ± %.4f\n", b_abs, b_err_abs))
cat(sprintf("  R^2 = %.4f\n", r_squared_abs))
cat(sprintf("  ε525 = %.2f ± %.2f M^-1 см^-1\n", epsilon_abs, epsilon_abs_err))

Cmin_abs <- 3 * b_err_abs / k_abs

cat(sprintf("Cmin = %.2f мкМ\n", Cmin_abs))

mask <- rowMeans(endpoint_abs) < 0.1
concentrations_filt <- 3.125 / (2^(0:9))
abs_filt <- endpoint_abs[mask, , drop = FALSE]

n_abs_filt <- ncol(abs_filt)
mean_abs_filt <- rowMeans(abs_filt)
sd_abs_filt <- apply(abs_filt, 1, sd)
sem_abs_filt <- sd_abs_filt / sqrt(n_abs_filt)
t_crit_abs_filt <- qt(0.975, df = n_abs_filt - 1)
ci_95_abs_filt <- t_crit_abs_filt * sem_abs_filt

df_abs_filt <- data.frame(
  Concentration = concentrations_filt,
  Mean_A = mean_abs_filt,
  CI_95 = ci_95_abs_filt
)

ggplot(df_abs_filt, aes(x = Concentration, y = Mean_A)) +
  geom_point(size = 3, color = "#21918C") +
  geom_errorbar(aes(ymin = Mean_A - CI_95, ymax = Mean_A + CI_95),
                width = 0.2, color = "#21918C") +
  scale_y_continuous(breaks = seq(0, 0.125, by = 0.01)) +
  scale_x_continuous(breaks = seq(0, 4, by = 0.25)) +
  labs(
    x = "Концентрация родамина 6G, мкМ",
    y = "Оптическая плотность (A)",
    title = "Зависимость поглощения от концентрации родамина 6G (A < 0.1)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

mean_flour <- rowMeans(endpoint_flour)
sd_flour <- apply(endpoint_flour, 1, sd)
n_flour <- ncol(endpoint_flour)
sem_flour <- sd_flour / sqrt(n_flour)
t_crit_flour <- qt(0.975, df = n_flour - 1)
ci_95_flour <- t_crit_flour * sem_flour

df_endpoint_flour <- data.frame(
  Concentration = concentrations,
  Mean_A = mean_flour,
  CI_95 = ci_95_flour
)

ggplot(df_endpoint_flour, aes(x = Concentration, y = Mean_A)) +
  geom_point(size = 3, color = "#21918C") +
  geom_errorbar(aes(ymin = Mean_A - CI_95, ymax = Mean_A + CI_95), 
                width = 5, color = "#21918C") +
  scale_y_continuous(breaks = seq(0, 7e+09, by = 5e+08)) +
  scale_x_continuous(breaks = seq(0, 200, by = 10)) +
  labs(x = "Концентрация родамина, мкМ", y = "Интенсивность флуоресценции (RFU)",
       title = "Зависимость флуоресценции от концентрации родамина при длине волны 555 нм") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

df_sorted <- df_endpoint_flour[order(df_endpoint_flour$Concentration), ]
df_10min <- head(df_sorted, 10)

ggplot(df_10min, aes(x = Concentration, y = Mean_A)) +
  geom_point(size = 3, color = "#21918C") +
  geom_errorbar(aes(ymin = Mean_A - CI_95, ymax = Mean_A + CI_95),
                width = 0.2, color = "#21918C") +
  scale_y_continuous(breaks = seq(0, 9e+08, by = 0.5e+08)) +
  scale_x_continuous(breaks = seq(0, 4, by = 0.25)) +
  labs(x = "Концентрация родамина 6G, мкМ", 
       y = "Интенсивность флуоресценции (RFU)",
       title = "Зависимость флуоресценции от концентрации (10 наименьших C)") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

flour_filt <- endpoint_flour[mask, , drop = FALSE]

n_flour_filt <- ncol(flour_filt)
mean_flour_filt <- rowMeans(flour_filt)
sd_flour_filt <- apply(flour_filt, 1, sd)
sem_flour_filt <- sd_flour_filt / sqrt(n_flour_filt)
t_crit_flour_filt <- qt(0.975, df = n_flour_filt - 1)
ci_95_flour_filt <- t_crit_flour_filt * sem_flour_filt

df_flour_filt <- data.frame(
  Concentration = concentrations_filt,
  Mean_F = mean_flour_filt,
  CI_95 = ci_95_flour_filt
)

ggplot(df_flour_filt, aes(x = Concentration, y = Mean_F)) +
  geom_point(size = 3, color = "#21918C") +
  geom_errorbar(aes(ymin = Mean_F - CI_95, ymax = Mean_F + CI_95),
                width = 0.2, color = "#21918C") +
  scale_y_continuous(breaks = seq(0, 9e+08, by = 0.5e+08)) +
  scale_x_continuous(breaks = seq(0, 4, by = 0.25)) +
  labs(
    x = "Концентрация родамина 6G, мкМ",
    y = "Интенсивность флуоресценции (RFU)",
    title = "Зависимость флуоресценции от концентрации родамина 6G (A < 0.1)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

#ЛИНЕЙНАЯ АППРОКСИМАЦИЯ

# Берем диапазон 0,049 <= C <= 3,125 мкМ
df_lin_flour_filt <- df_flour_filt %>% filter(Concentration >= 0.049 & Concentration <= 3.125)

# Линейная регрессия с весами (1/CI^2)
fit_flour <- lm(Mean_F ~ Concentration, data = df_lin_flour_filt, weights = 1/(CI_95^2))

# Результаты
summary(fit_flour)

k_flour <- coef(fit_flour)[["Concentration"]]
b_flour <- coef(fit_flour)[["(Intercept)"]]
k_err_flour <- summary(fit_flour)$coefficients["Concentration", "Std. Error"]
b_err_flour <- summary(fit_flour)$coefficients["(Intercept)", "Std. Error"]
r_squared_flour <- summary(fit_flour)$r.squared

eq_flour <- sprintf("A = %.2e·C + %.2e\nR² = %.4f", k_flour, b_flour, r_squared_flour)

ggplot(df_flour_filt, aes(x = Concentration, y = Mean_F)) +
  geom_point(size = 3, color = "#21918C") +
  geom_errorbar(aes(ymin = Mean_F - CI_95, ymax = Mean_F + CI_95), 
                width = 0.2, color = "#21918C") +
  annotate("text", x = -0.15, y = 3.0e+08, label = eq_flour,
           hjust = 0, size = 5, fontface = "bold", color = "#440154") +
  scale_y_continuous(breaks = seq(0, 9e+08, by = 0.5e+08)) +
  scale_x_continuous(breaks = seq(0, 4, by = 0.25)) +
  geom_smooth(data = df_lin_flour_filt, method = "lm", se = FALSE, color = "#440154") +
  labs(x = "Концентрация родамина, мкМ", y = "Интенсивность флуоресценции (RFU)",
       title = "Калибровочная зависимость флуоресценции от концентрации родамина при 555 нм") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

cat(sprintf("  Наклон k = %.2e ± %.2e A/мкМ\n", k_flour, k_err_flour))
cat(sprintf("  Свободный член b = %.2e ± %.2e\n", b_flour, b_err_flour))
cat(sprintf("  R^2 = %.4f\n", r_squared_flour))

Cmin_flour <- 3 * b_err_flour / k_flour

cat(sprintf("Cmin = %.4f мкМ\n", Cmin_flour))
