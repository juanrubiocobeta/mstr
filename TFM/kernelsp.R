library(tidyquant)
sp500_data <- tq_get("^GSPC", 
                     get  = "stock.prices", 
                     from = "1980-01-01")
head(sp500_data)
rango_fechas <- range(sp500_data$date)
print(rango_fechas)

library(dplyr)
sp500_data <- sp500_data %>%
  arrange(date) %>%
  mutate(daily_return = (close / lag(close)) - 1)
head(sp500_data)

library(ggplot2)
sp500_plot <- na.omit(sp500_data)
sd_val <- sd(sp500_plot$daily_return)
ggplot(sp500_plot, aes(x = daily_return)) +
  geom_histogram(aes(y = after_stat(density)), bins = 100, fill = "#69b3a2", alpha = 0.6) +
  stat_function(fun = dnorm, args = list(mean = 0, sd = sd_val), color = "red", linetype = "dashed") +
  coord_cartesian(xlim = c(-0.08, 0.08)) +
  theme_minimal()

library(tseries)
resultado_jb <- jarque.bera.test(sp500_plot$daily_return)
print(resultado_jb)

library(moments)
asimetria <- skewness(sp500_plot$daily_return)
curtosis  <- kurtosis(sp500_plot$daily_return)
print(paste("Asimetría (debería ser 0):", round(asimetria, 4)))
print(paste("Curtosis (debería ser 3): ", round(curtosis, 4)))

last_plot() + 
  geom_density(color = "orange", linewidth = 0.8)









library(ggplot2)

mean_val <- mean(sp500_plot$daily_return, na.rm = TRUE)
sd_val   <- sd(sp500_plot$daily_return, na.rm = TRUE)

ggplot(sp500_plot, aes(x = daily_return)) +
  
  geom_histogram(aes(y = after_stat(density)), 
                 bins = 1000, 
                 fill = "#69b3a2", 
                 alpha = 0.6) +
  
  stat_function(fun = dnorm, 
                args = list(mean = mean_val, sd = sd_val), 
                color = "red", 
                linetype = "dashed",
                linewidth = 0.8) +
  
  geom_density(color = "orange", 
               linewidth = 0.8) +
  coord_cartesian(xlim = c(-0.07, -0.015), 
                  ylim = c(0, 8))

