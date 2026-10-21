library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(stringr)

# Длины волн (нм)
wavelengths_abs <- seq(400, 800, by = 5)
wavelengths_emis <- seq(530, 750, by = 5)
wavelengths_exc <- seq(400, 615, by = 5)

abs = c(
  0.0687, 0.0673, 0.0667, 0.0662, 0.0654, 0.0632, 0.0633, 0.0627,
  0.0627, 0.0630, 0.0619, 0.0621, 0.0608, 0.0608, 0.0609, 0.0611,
  0.0624, 0.0601, 0.0598, 0.0595, 0.0598, 0.0601, 0.0605, 0.0603,
  0.0594, 0.0615, 0.0581, 0.0586, 0.0607, 0.0586, 0.0591, 0.0590,
  0.0594, 0.0593, 0.0592, 0.0583, 0.0581, 0.0586, 0.0583, 0.0589,
  0.0584, 0.0580, 0.0580, 0.0580, 0.0581, 0.0585, 0.0573, 0.0574,
  0.0576, 0.0575, 0.0577, 0.0574, 0.0569, 0.0571, 0.0569, 0.0571,
  0.0571, 0.0564, 0.0564, 0.0576, 0.0559, 0.0569, 0.0567, 0.0561,
  0.0548, 0.0546, 0.0550, 0.0547, 0.0550, 0.0547, 0.0543, 0.0540,
  0.0537, 0.0530, 0.0521, 0.0524, 0.0522, 0.0510, 0.0513, 0.0506,
  0.0502
)

em = c(
  8682, 9955, 11032, 13961, 17018, 22614, 27885, 31374,
  35378, 38181, 39248, 38068, 36144, 33171, 30392, 28544,
  24910, 21942, 19944, 16431, 14360, 12110, 10754, 8095,
  7100, 6756, 5495, 4622, 3894, 3373, 3187, 2560,
  2082, 1787, 1546, 1605, 1394, 1131, 1087, 1339,
  1438, 1289, 1458, 1265, 870
)

ex = c(
  1744, 0, 783, 1481, 2705, 2940, 3364, 3684,
  3576, 3575, 3363, 3959, 4187, 4322, 3911, 4476,
  5150, 6601, 7393, 9437, 11237, 11947, 14979, 17110,
  20483, 21950, 24300, 27572, 31956, 38130, 42546, 45654,
  43089, 36543, 26222, 17005, 11748, 7559, 5359, 3542,
  3457, 3580, 3373, 2552
)

endpoint_fluor <- matrix(c(
  144877, 186733, 154152,
  53327,  63847,  62800,
  31205,  30512,  30868,
  17188,  16485,  15785,
  11218,   8067,   7365,
  7365,   9469,   5963,
  8414,   3858,   6312,
  6313,   5962,   4559
), nrow = 8, ncol = 3, byrow = TRUE)

cells_count <- matrix(c(
  22543, 23420, 23505,
  9042,  9277,  9297,
  4812,  3631,  3889,
  2019,  1756,  1693,
  881,   896,   847,
  412,   339,   476,
  163,   119,   102,
  91,   101,    57
), nrow = 8, ncol = 3, byrow = TRUE)

ggplot() +
  geom_line(aes(x = wavelengths_abs, y = abs, color = "absorbtion"), linewidth = 1) +
  geom_point(aes(x = wavelengths_abs, y = abs, color = "absorbtion"), size = 2) +
  scale_color_manual(values = c("absorbtion" = "#440154")) +
  scale_y_continuous(breaks = seq(0, 0.07, by = 0.0025)) +
  scale_x_continuous(breaks = seq(400, 800, by = 25)) +
  #scale_y_continuous(breaks = seq(0, 2.6, by = 0.25)) +
  #scale_x_continuous(breaks = seq(400, 600, by = 10)) +
  labs(
    x = "Длина волны, нм",
    y = "Оптическая плотность (A)",
    title = "Спектр поглощения клеточной суспензии максимальной концентрации",
    color = " "
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

em_norm <- em / max(em) * 100
ex_norm <- ex / max(ex) * 100

ex_fix <- sprintf("excitation fixed = 500 нм")
em_fix <- sprintf("emission fixed = 640 нм")

ggplot() +
  geom_line(aes(x = wavelengths_emis, y = em_norm, color = "Спектр эмиссии"), linewidth = 1) +
  geom_point(aes(x = wavelengths_emis, y = em_norm, color = "Спектр эмиссии"), size = 2) +
  geom_line(aes(x = wavelengths_exc, y = ex_norm, color = "Спектр возбуждения"), linewidth = 1) +
  geom_point(aes(x = wavelengths_exc, y = ex_norm, color = "Спектр возбуждения"), size = 2) +
  annotate("text", x = 650, y = 60, label = ex_fix,
           hjust = 0, size = 5, fontface = "bold", color = "#440154") +
  annotate("text", x = 400, y = 48, label = em_fix,
           hjust = 0, size = 5, fontface = "bold", color = "#FDE725") +
  scale_color_manual(values = c("Спектр эмиссии" = "#440154", "Спектр возбуждения" = "#FDE725")) +
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  scale_x_continuous(breaks = seq(400, 750, by = 25)) +
  labs(
    x = "Длина волны, нм",
    y = "Процент от максимума, %",
    title = "Нормированные спектры возбуждения и эмиссии флуоресценции клеточной суспензии",
    color = " "
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

cells_per_well <- cells_count * 2

well_names <- paste(
  rep(paste0("row", 1:8), times = 3),
  rep(paste0("col", 1:3), each  = 8),
  sep = "_"
)

df_raw_lin <- data.frame(
  well         = well_names,
  fluorescence = as.vector(endpoint_fluor),
  cell_count   = as.vector(cells_per_well),
  stringsAsFactors = FALSE
)


# 2. Смотрим на график в ЛИНЕЙНЫХ координатах, чтобы глазами найти линейный участок.
# (Точки в начале будут прижаты к нулю, а в конце — взлетать вверх)
ggplot(df_raw_lin, aes(x = cell_count, y = fluorescence)) +
  geom_point(size = 3, color = "#21918C") +
  scale_y_continuous(breaks = seq(0, 200000, by = 25000)) +
  scale_x_continuous(breaks = seq(0, 50000, by = 5000)) +
  labs(
    x = "Количество клеток в лунке (Object Count × 2)",
    y = "Интенсивность флуоресценции, RFU",
    title = "Зависимость флуоресценции от количества клеток"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# 3. Задаем границы линейного участка (например, от 5000 клеток, как обсуждали)
# Вам нужно посмотреть на график из п.2 и выбрать порог.
# Предположим, линейный участок начинается с 5000 клеток:
linear_df <- subset(df_raw_lin, cell_count >= 50)

# 4. Строим регрессию (lm) для линейного участка
model <- lm(fluorescence ~ cell_count, data = linear_df)

# 5. Выводим результаты (коэффициенты, их погрешности, R^2)
summary(model)

# 6. Красивый график только для линейного участка с формулой
# Извлекаем коэффициенты и R^2 для подписи на графике
intercept <- coef(model)[1]
slope     <- coef(model)[2]
r_squared <- summary(model)$r.squared

# Формируем текст формулы
eq_label <- sprintf("y = %.3e * x + %.3e\nR² = %.4f", slope, intercept, r_squared)

ggplot(linear_df, aes(x = cell_count, y = fluorescence)) +
  geom_point(size = 3, color = "#21918C") +
  geom_smooth(method = "lm", se = FALSE, color = "#440154") +
  annotate("text", x = min(linear_df$cell_count) * 1.1, 
           y = max(linear_df$fluorescence) * 0.8, 
           label = eq_label, fontface = "bold", hjust = 0, size = 5, color = "#440154") +
  labs(
    x = "Количество клеток в лунке (Object Count × 2)",
    y = "Интенсивность флуоресценции, RFU",
    title = "Калибровочная зависимости флуоресценции от количества клеток"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Строим обычную линейную регрессию (без весов, т.к. нет ошибки для каждой точки)
fit_raw <- lm(fluorescence ~ cell_count, data = df_raw_lin)

# Извлекаем коэффициенты
k_raw     <- coef(fit_raw)[["cell_count"]]
b_raw     <- coef(fit_raw)[["(Intercept)"]]
k_err_raw <- summary(fit_raw)$coefficients["cell_count", "Std. Error"]
b_err_raw <- summary(fit_raw)$coefficients["(Intercept)", "Std. Error"]
r2_raw    <- summary(fit_raw)$r.squared

cat("--- ЭТАП 1 (Без усреднения) ---\n")
cat(sprintf("y = (%.3e ± %.3e) * x + (%.3e ± %.3e)\n", k_raw, k_err_raw, b_raw, b_err_raw))
cat(sprintf("R^2 = %.4f\n\n", r2_raw))

# ==========================================
# ЗАДАНИЯ 8, 9, 10: УСРЕДНЕНИЕ И РАСЧЕТ CI
# ==========================================
df_mean <- df_raw_lin %>%
  # Создаем колонку row_num, вытаскивая цифру из названия (например, из "row1_col2" получим "1")
  mutate(row_num = str_extract(well, "(?<=row)\\d+")) %>%
  
  # Группируем по номеру строки
  group_by(row_num) %>%
  summarise(
    # 8) Усредняем интенсивность флуоресценции
    mean_fluor = mean(fluorescence, na.rm = TRUE),
    
    # 10) Усредняем количество клеток (так как в повторах оно может чуть отличаться)
    mean_cells = mean(cell_count, na.rm = TRUE),
    
    # Считаем ошибку и CI
    sd_fluor = sd(fluorescence, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    # 9) Рассчитываем 95% доверительный интервал
    sem = sd_fluor / sqrt(n),
    t_crit = qt(0.975, df = n - 1),
    ci_95 = t_crit * sem
  )

# ==========================================
# ЗАДАНИЕ 11: ПОСТРОЕНИЕ ГРАФИКА
# ==========================================
# Обратите внимание: по оси X теперь mean_cells (среднее из df_mean)
ggplot(df_mean, aes(x = mean_cells, y = mean_fluor)) +
  geom_point(size = 3, color = "#21918C") +
  geom_errorbar(aes(ymin = mean_fluor - ci_95, ymax = mean_fluor + ci_95), 
                width = max(df_mean$mean_cells) * 0.02, # Ширина усов (2% от макс. значения X)
                color = "#21918C", alpha = 0.7) +
  labs(
    x = "Количество клеток в лунке (Object Count × 2)",
    y = "Интенсивность флуоресценции, RFU",
    title = "Усредненная зависимость флуоресценции от количества клеток"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# ==========================================
# ЗАДАНИЯ 12, 13: АППРОКСИМАЦИЯ ЛИНЕЙНОГО УЧАСТКА
# ==========================================
# Выбираем линейный участок (по вашему графику — это от 8000 клеток и выше)
# Если решите, что линейный участок начинается с 10000 — просто поменяйте цифру.
df_lin <- df_mean %>% filter(mean_cells >= 0)

# Строим регрессию с весами (1/CI^2)
fit_cells <- lm(mean_fluor ~ mean_cells, data = df_lin, weights = 1 / (ci_95^2 + 1e-5))

# Извлекаем результаты
k_cells     <- coef(fit_cells)[["mean_cells"]]
b_cells     <- coef(fit_cells)[["(Intercept)"]]
k_err_cells <- summary(fit_cells)$coefficients["mean_cells", "Std. Error"]
b_err_cells <- summary(fit_cells)$coefficients["(Intercept)", "Std. Error"]
r_squared   <- summary(fit_cells)$r.squared

# Выводим в консоль для отчета
cat(sprintf("Уравнение: y = (%.3e ± %.3e) * x + (%.3e ± %.3e)\n", 
            k_cells, k_err_cells, b_cells, b_err_cells))
cat(sprintf("R-squared: %.4f\n", r_squared))

# Строим финальный график только для линейного участка
eq_label <- sprintf("y = %.3e * x + %.3e\nR² = %.4f", k_cells, b_cells, r_squared)

ggplot(df_lin, aes(x = mean_cells, y = mean_fluor)) +
  geom_point(size = 3, color = "#21918C") +
  geom_errorbar(aes(ymin = mean_fluor - ci_95, ymax = mean_fluor + ci_95), 
                width = 1000, color = "#21918C", alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "#440154") +
  annotate("text", x = min(df_lin$mean_cells) * 1.05, 
           y = max(df_lin$mean_fluor) * 0.85, 
           label = eq_label, fontface = "bold", hjust = 0, size = 5, color = "#440154") +
  labs(
    x = "Количество клеток в лунке (Object Count × 2)",
    y = "Интенсивность флуоресценции, RFU",
    title = "Калибровочная зависимости флуоресценции от количества клеток (усредненные данные)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 1, face = "bold")
  )

C_min <- 3 * b_err_cells / k_cells

cat(sprintf("Cmin = 3 * %.3e / %.3e = %.1f клеток\n", b_err_cells, k_cells, C_min))
