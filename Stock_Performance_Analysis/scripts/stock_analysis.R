install.packages(c("quantmod", "dplyr", "ggplot2", "lubridate"))

library(quantmod)
library(dplyr)
library(ggplot2)
library(lubridate)

getSymbols("AAPL", src = "yahoo", from = Sys.Date() - 365, to = Sys.Date())
stock_data <- AAPL

stock_df <- data.frame(
  Date = index(stock_data),
  Open = as.numeric(Op(stock_data)),
  High = as.numeric(Hi(stock_data)),
  Low = as.numeric(Lo(stock_data)),
  Close = as.numeric(Cl(stock_data)),
  Volume = as.numeric(Vo(stock_data))
)

head(stock_df)
summary(stock_df)

# Add month column
stock_df <- stock_df %>%
  mutate(Month = floor_date(Date, "month"))

monthly_avg <- stock_df %>%
  group_by(Month) %>%
  summarise(Avg_Close = mean(Close))

ggplot(stock_df, aes(x = Date, y = Close)) +
  geom_line(linewidth = 1, color = "blue") +
  labs(title = "Apple (AAPL) Stock Closing Prices - Last 1 Year",
       x = "Date", y = "Closing Price (USD)") +
  theme_minimal()

ggsave("plots/aapl_closing_price.png", width = 10, height = 6, bg = 'white')

