install.packages(c("readxl"))
install.packages("data.table")
install.packages("openxlsx")
install.packages("ggrepel")
install.packages("pROC")
install.packages("viridis")
install.packages("tidyverse")
install.packages("caret")
install.packages("zoo")
install.packages("e1071")
install.packages("scales")

library(gridExtra)
library(ggrepel)
library(openxlsx)
library(data.table)
library(readxl)
library(scales)
library(tidyverse)
library(caret)
library(zoo)
library(viridis)
library(e1071)
library(pROC)
library(reshape2)


setwd("D:/Intro to Data Science/archive")

# List of file names
files <- c(
  "2014_Financial_Data.xlsx",
  "2015_Financial_Data.xlsx",
  "2016_Financial_Data.xlsx",
  "2017_Financial_Data.xlsx",
  "2018_Financial_Data.xlsx"
)


#To merge the files into one
consolidated_data <-
  dplyr::bind_rows(lapply(files, read_excel), .id = "Year")

head(consolidated_data)

View(consolidated_data)

# Mapping year names to numeric value
year_mapping <- c("2014", "2015", "2016", "2017", "2018")

consolidated_data <- consolidated_data %>%
  mutate(Year = year_mapping[as.numeric(Year)])

View(consolidated_data)

# Creating a new column 'Ticker_ID' with a unique identifier for each ticker

setDT(consolidated_data)[, Ticker_ID := rowid(Ticker)]


consolidated_data <- consolidated_data %>%
  mutate(Ticker_ID = dense_rank(Ticker)) %>%
  relocate(Ticker_ID, .after = Ticker)

head(consolidated_data)

write.xlsx(consolidated_data,
           "Cleaned File of Financial Data.xlsx",
           rowNames = FALSE)



#Cleaning the Data

#Removing duplicate companies in the same year
consolidated_data <-
  consolidated_data %>% distinct(Ticker, Year, .keep_all = TRUE)

# Remove columns with more than 20% missing values
threshold <- 0.2
cols_to_remove <-
  colnames(consolidated_data)[colSums(is.na(consolidated_data)) / nrow(consolidated_data) > threshold]
consolidated_data <-
  consolidated_data %>% select(-all_of(cols_to_remove))

# Report removed columns
num_cols_removed <- length(cols_to_remove)
print(paste("Removed columns:", paste(cols_to_remove, collapse = ", ")))
print(paste("Number of columns removed:", num_cols_removed))

#Filling the blanks with 0
consolidated_data[is.na(consolidated_data)] <- 0

View(consolidated_data)

# Renaming columns
original_colnames <- colnames(consolidated_data)
new_colnames <- gsub(" ", "_", original_colnames)
colnames(consolidated_data) <- new_colnames

num_renamed_columns <- length(new_colnames)

num_renamed_columns <- sum(original_colnames != new_colnames)

print(paste("Number of renamed columns:", num_renamed_columns))

write.xlsx(consolidated_data, "CF of Financial Data.xlsx", rowNames = FALSE)

#checking for repetitive companies
company_counts_per_year <- consolidated_data %>%
  group_by(Year, Ticker) %>%
  summarize(Count = n(), .groups = 'drop')

# Identify repetitive companies for each year
repetitive_companies <- company_counts_per_year %>%
  filter(Count > 1)

# Print repetitive companies, if any
if (nrow(repetitive_companies) > 0) {
  print("Repetitive companies found in each year:")
  print(repetitive_companies)
} else {
  print("No repetitive companies found in any year.")
}

# number of unique sectors
unique_sectors <- unique(consolidated_data$Sector)
num_unique_sectors <- length(unique_sectors)


# number of unique companies
unique_companies <- unique(consolidated_data$Ticker)
num_unique_companies <- length(unique_companies)

# missing values in each column
missing_values <- colSums(is.na(consolidated_data))


cat("Number of Unique Sectors:", num_unique_sectors, "\n")
cat("Number of Unique Companies:", num_unique_companies, "\n")

cat("\nNumber of Missing Values in Each Column:\n")
print(missing_values)


# Getting the count of number of companies in each sector
companies_per_sector <- consolidated_data %>%
  group_by(Sector) %>%
  summarize(Num_Companies = n_distinct(Ticker))


ggplot(companies_per_sector,
       aes(
         x = reorder(Sector, Num_Companies),
         y = Num_Companies,
         fill = Sector
       )) +
  geom_bar(stat = "identity",
           color = "white",
           size = 0.5) +
  labs(title = "Number of Companies in Each Sector",
       x = "Sector",
       y = "Number of Companies") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

#Analysis by particular sector

selected_sector_data <- consolidated_data %>%
  filter(Sector %in% c("Financial Services"))

# Creating new data frame with selected sectors
sector_df <- selected_sector_data

print("New data frame with only 'Financial Services' sector:")
print(sector_df)
View(sector_df)

#Sector - Financial Services - for future reference ease

financial_services_data <- sector_df %>%
  filter(Sector == "Financial Services")

financial_services_data$Year <-
  as.numeric(financial_services_data$Year)


# Calculate moving average for each year
window_size <- 3
financial_services_data <- financial_services_data %>%
  arrange(Ticker, Year) %>%
  group_by(Year) %>%
  mutate(Moving_Avg = zoo::rollmean(
    Revenue,
    k = window_size,
    fill = NA,
    align = "right"
  )) %>%
  ungroup()

# Check for missing values in Moving_Avg
missing_values <- financial_services_data %>%
  filter(is.na(Moving_Avg)) %>%
  select(Ticker, Year, Moving_Avg)

print("Rows with missing values in Moving_Avg:")
print(missing_values)
View(financial_services_data)

#importing for better check
write.xlsx(financial_services_data,
           "financial_services.xlsx",
           rowNames = FALSE)

# Calculating the overall moving average for all companies
overall_moving_avg <- financial_services_data %>%
  group_by(Year) %>%
  summarise(Overall_Moving_Avg = mean(Moving_Avg, na.rm = TRUE))


print(overall_moving_avg)



# Finding top companies across all years based on Market Cap
yearly_max_market_cap <- financial_services_data %>%
  group_by(Year, Ticker) %>%
  summarize(Max_Market_Cap = max(Market_Cap, na.rm = TRUE),
            .groups = 'drop')

yearly_ranks <- yearly_max_market_cap %>%
  group_by(Year) %>%
  mutate(Rank = rank(-Max_Market_Cap, ties.method = "min")) %>%
  filter(Rank <= 10)

common_companies <- yearly_ranks %>%
  group_by(Ticker) %>%
  summarize(Count = n(), .groups = 'drop') %>%
  filter(Count == length(unique(financial_services_data$Year))) %>%
  pull(Ticker)

common_financial_services_data <- financial_services_data %>%
  filter(Ticker %in% common_companies)

View(common_financial_services_data)


#Rank companies based on Market Cap for each year

ranked_data <- common_financial_services_data %>%
  arrange(Year, desc(Market_Cap)) %>%
  group_by(Year) %>%
  mutate(Rank = row_number())

print(ranked_data)
View(ranked_data)

ggplot(ranked_data, aes(
  x = Ticker,
  y = Rank,
  color = Market_Cap / 1000000
)) +
  geom_point(size = 3) +
  labs(
    title = "Ranked Data based on Market Cap",
    x = "Ticker",
    y = "Rank",
    color = "Market Cap (in thousands)"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap( ~ Year, scales = "free") +
  scale_color_continuous(labels = scales::comma_format(scale = 1e-3, accuracy = 1),
                         name = "Market Cap (in MM)") +
  
  
  
  #Plotting overall moving average of all companies with top 10 companies
  ggplot() +
  geom_line(
    data = common_financial_services_data,
    aes(
      x = Year,
      y = Revenue / 1000000,
      group = Ticker,
      color = Ticker
    ),
    size = 1
  ) +
  geom_line(
    data = overall_moving_avg,
    aes(x = Year, y = Overall_Moving_Avg / 1000000),
    linetype = "dotted",
    color = "red",
    size = 1.5
  ) +
  labs(
    title = "Financial Services Sector: Revenue and Overall Moving Average",
    subtitle = "Comparison of Individual Company Revenues and Sector Average",
    x = "Year",
    y = "Revenue (Millions)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10)
  ) +
  scale_y_continuous(labels = comma) +
  scale_color_viridis_d() +
  guides(color = guide_legend(title = "Company"))




#Liquidity ratios - Current and Quick Ratio

current_ratio_benchmark <- c(1, 1.5)
quick_ratio_benchmark <- 1

# Line plot for Current Ratio
plot_current_ratio <-
  ggplot(common_financial_services_data,
         aes(x = Year, y = currentRatio, color = Ticker)) +
  geom_line(size = 1, linetype = "solid") +
  labs(
    title = "Current Ratio",
    x = "Year",
    y = "Current Ratio",
    color = "Company"
  ) +
  scale_color_viridis_d() +
  geom_hline(yintercept = current_ratio_benchmark,
             linetype = "dotted",
             color = "red") +
  theme_minimal() +
  theme(
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      size = 1
    ),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Line plot for Quick Ratio
plot_quick_ratio <-
  ggplot(common_financial_services_data,
         aes(x = Year, y = quickRatio, color = Ticker)) +
  geom_line(size = 1, linetype = "dashed") +
  labs(
    title = "Quick Ratio",
    x = "Year",
    y = "Quick Ratio",
    color = "Company"
  ) +
  scale_color_viridis_d() +
  geom_hline(yintercept = quick_ratio_benchmark,
             linetype = "dotted",
             color = "red") +
  theme_minimal() +
  theme(
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      size = 1
    ),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

gridExtra::grid.arrange(plot_current_ratio, plot_quick_ratio, ncol = 2)



# Profitability Ratios - Profit margin, ROE and ROA

common_financial_services_data <-
  transform(common_financial_services_data,
            returnOnAssets = Net_Income / Total_assets)

# benchmark values
net_profit_margin_benchmark <- c(0.10, 0.35)
roe_benchmark <- c(0.15, 0.20)
roa_benchmark <- 0.05

# Created a theme for clean look and feasibility
custom_theme <- theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.major = element_line(color = "grey", size = 0.5),
    panel.grid.minor = element_line(color = "lightgrey", size = 0.25),
    panel.background = element_blank(),
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      size = 1
    ),
    axis.line = element_line(color = "black"),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1,
      size = 7
    ),
    axis.text.y = element_text(size = 8),
    axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
    plot.title = element_text(hjust = 0.5),
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 8)
  )

# Line plot for Net Profit Margin
plot_net_profit_margin <-
  ggplot(common_financial_services_data,
         aes(x = Year, y = netProfitMargin, color = Ticker)) +
  geom_line(size = 1, linetype = "solid") +
  labs(
    title = "Net Profit Margin",
    x = NULL,
    y = "Net Profit Margin",
    color = "Company"
  ) +
  scale_color_viridis_d() +
  geom_hline(
    yintercept = net_profit_margin_benchmark,
    linetype = "dotted",
    color = "red",
    alpha = 1
  ) +
  custom_theme +
  guides(color = guide_legend(override.aes = list(alpha = 1), title = NULL))

# Line plot for (ROE)
plot_roe <-
  ggplot(common_financial_services_data,
         aes(x = Year, y = returnOnEquity, color = Ticker)) +
  geom_line(size = 1, linetype = "solid") +
  labs(
    title = "Return on Equity (ROE)",
    x = NULL,
    y = "ROE",
    color = "Company"
  ) +
  scale_color_viridis_d() +
  geom_hline(
    yintercept = roe_benchmark,
    linetype = "dotted",
    color = "red",
    alpha = 1
  ) +
  custom_theme +
  guides(color = guide_legend(override.aes = list(alpha = 1), title = NULL))

# Line plot for (ROA)
plot_roa <-
  ggplot(common_financial_services_data,
         aes(x = Year, y = returnOnAssets, color = Ticker)) +
  geom_line(size = 1, linetype = "solid") +
  labs(
    title = "Return on Assets (ROA)",
    x = "Year",
    y = "ROA",
    color = "Company"
  ) +
  scale_color_viridis_d() +
  geom_hline(
    yintercept = roa_benchmark,
    linetype = "dotted",
    color = "red",
    alpha = 1
  ) +
  custom_theme +
  guides(color = guide_legend(override.aes = list(alpha = 1), title = NULL))

gridExtra::grid.arrange(plot_net_profit_margin, plot_roe, plot_roa, ncol = 3)



#Market Base Ratio - PE and EPS
PE_benchmark <- c(20, 25)
EPS_benchmark <- 1

#Used same approached as above

bold_theme <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      text = element_text(face = "bold"),
      axis.text.x = element_text(
        angle = 45,
        hjust = 1,
        size = 8
      ),
      axis.text.y = element_text(size = 8),
      axis.line = element_line(color = "black"),
      panel.grid.major = element_line(color = "grey", size = 0.5),
      panel.grid.minor = element_line(color = "lightgrey", size = 0.25),
      legend.text = element_text(size = 6),
      legend.title = element_text(size = 8)
    )
}

# Line for (PE)
plot_PE <-
  ggplot(common_financial_services_data,
         aes(x = Year, y = PE_ratio, color = Ticker)) +
  geom_line(size = 1, linetype = "solid") +
  labs(
    title = "Price To Earnings",
    x = "Year",
    y = "Price To Earnings",
    color = "Company"
  ) +
  scale_color_viridis_d() +
  geom_hline(yintercept = PE_benchmark,
             linetype = "dotted",
             color = "red") +
  bold_theme() +
  scale_y_log10()

# Line plot for (EPS)
plot_EPS <-
  ggplot(common_financial_services_data,
         aes(x = Year, y = EPS, color = Ticker)) +
  geom_line(size = 1, linetype = "solid") +
  labs(
    title = "Earning Per Share",
    x = "Year",
    y = "Earning Per Share",
    color = "Company"
  ) +
  scale_color_viridis_d() +
  geom_hline(yintercept = EPS_benchmark,
             linetype = "dotted",
             color = "red") +
  bold_theme()

grid.arrange(plot_PE, plot_EPS, ncol = 2)


# Leverage Ratios

#Calculating ratios
calculate_ratios <- function(data) {
  data %>% mutate(
    Debt_To_Equity = Total_debt / Total_shareholders_equity,
    EV_EBITDA = Market_Cap / EBITDA,
    Debt_Services_Coverage_Ratio = Operating_Income / Total_current_liabilities
  )
}

common_financial_services_data <-
  calculate_ratios(common_financial_services_data)


common_financial_services_data_1 <-
  common_financial_services_data %>%
  select(Year,
         Ticker,
         Debt_To_Equity,
         EV_EBITDA,
         Debt_Services_Coverage_Ratio) %>%
  pivot_longer(
    cols = c(Debt_To_Equity, EV_EBITDA, Debt_Services_Coverage_Ratio),
    names_to = "Ratio_Type",
    values_to = "Ratio_Value"
  )

benchmarks <- data.frame(
  Ratio_Type = c(
    "Debt_To_Equity",
    "EV_EBITDA",
    "Debt_Services_Coverage_Ratio"
  ),
  Benchmark_Value = c(1.5, 7.5, 1.5)
)

# Line plot for leverage ratio
ggplot(common_financial_services_data_1,
       aes(x = Year, y = Ratio_Value, color = Ticker)) +
  geom_line(size = 1) +
  geom_hline(
    data = benchmarks,
    aes(yintercept = Benchmark_Value, linetype = Ratio_Type),
    color = "red",
    size = 0.5
  ) +
  scale_color_viridis_d() +
  labs(
    title = "Financial Services Sector: Debt To Equity, EV/EBITDA, and Debt Service Coverage Ratio",
    x = "Year",
    y = "Ratio Value",
    color = "Company"
  ) +
  facet_wrap( ~ Ratio_Type, scales = "free_y", nrow = 1) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      size = 1
    ),
    legend.position = "bottom"
  )







#Logistic Regression

#Correlation matrix for checking suitability

# Selecting the independent variables from the dataset for correlation matrix
independent_data <- sector_df[, c(
  "currentRatio",
  "quickRatio",
  "returnOnAssets",
  "netProfitMargin",
  "returnOnEquity",
  "PE_ratio",
  "EPS",
  "Debt_to_Assets",
  "cashRatio"
)]

# Calculating the correlation matrix
correlation_matrix <- cor(independent_data, use = "complete.obs") 

print(correlation_matrix)

# Plot for correlation matrix

clarified_correlation_matrix <- melt(correlation_matrix)
ggplot(data = melted_correlation_matrix, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x = "Variables", y = "Variables", fill = "Correlation")



sector_df$returnOnAssets <-
  sector_df$Net_Income / sector_df$Total_assets

# Replace Inf and NaN with NA

sector_df <- sector_df %>%
  mutate(across(
    c(
      currentRatio,
      quickRatio,
      returnOnAssets,
      netProfitMargin,
      returnOnEquity,
      PE_ratio,
      EPS,
      Debt_to_Assets,
      cashRatio
    ),
    ~ ifelse(is.infinite(.) | is.nan(.), NA, .)
  ))

# Split Data (70-30)

set.seed(123)
index <- createDataPartition(sector_df$Class, p = 0.7, list = FALSE)
trainData <- sector_df[index,]
testData <- sector_df[-index,]

trainData$returnOnAssets[is.na(trainData$returnOnAssets)] <-
  mean(trainData$returnOnAssets, na.rm = TRUE)


# Imputation to testData
testData$returnOnAssets[is.na(testData$returnOnAssets)] <-
  mean(trainData$returnOnAssets, na.rm = TRUE)

# independent variables for the model
independent_variables <-
  c(
    "currentRatio",
    "quickRatio",
    "returnOnAssets",
    "netProfitMargin",
    "returnOnEquity",
    "PE_ratio",
    "EPS",
    "Debt_to_Assets",
    "cashRatio"
  )


formula <-
  as.formula(paste("Class ~", paste(independent_variables, collapse = " + ")))

# Fit the logistic regression model using the training data
regression_model <-
  glm(formula, data = trainData, family = binomial())

# Summary of the model
summary(regression_model)


# Prediction on test data for confusion matrix
predicted_probs <-
  predict(regression_model, newdata = testData, type = "response")
predicted_class <- ifelse(predicted_probs > 0.5, 1, 0)

#confusion matrix
conf_mat <-
  table(Predicted = predicted_class, Actual = testData$Class)
print(conf_mat)
# Calculate accuracy
accuracy <- sum(diag(conf_mat)) / sum(conf_mat)
print(paste("Accuracy:", accuracy))


