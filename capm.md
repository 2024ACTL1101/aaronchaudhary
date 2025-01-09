
# CAPM Analysis

## Introduction

In this assignment, you will explore the foundational concepts of the Capital Asset Pricing Model (CAPM) using historical data for AMD and the S&P 500 index. This exercise is designed to provide a hands-on approach to understanding how these models are used in financial analysis to assess investment risks and returns.

## Background

The CAPM provides a framework to understand the relationship between systematic risk and expected return, especially for stocks. This model is critical for determining the theoretically appropriate required rate of return of an asset, assisting in decisions about adding assets to a diversified portfolio.

## Objectives

1. **Load and Prepare Data:** Import and prepare historical price data for AMD and the S&P 500 to ensure it is ready for detailed analysis.
2. **CAPM Implementation:** Focus will be placed on applying the CAPM to examine the relationship between AMD's stock performance and the overall market as represented by the S&P 500.
3. **Beta Estimation and Analysis:** Calculate the beta of AMD, which measures its volatility relative to the market, providing insights into its systematic risk.
4. **Results Interpretation:** Analyze the outcomes of the CAPM application, discussing the implications of AMD's beta in terms of investment risk and potential returns.

## Instructions

### Step 1: Data Loading

- We are using the `quantmod` package to directly load financial data from Yahoo Finance without the need to manually download and read from a CSV file.
- `quantmod` stands for "Quantitative Financial Modelling Framework". It was developed to aid the quantitative trader in the development, testing, and deployment of statistically based trading models.
- Make sure to install the `quantmod` package by running `install.packages("quantmod")` in the R console before proceeding.

```r
# Set start and end dates
start_date <- as.Date("2019-05-20")
end_date <- as.Date("2024-05-20")

# Load data for AMD, S&P 500, and the 1-month T-Bill (DTB4WK)
amd_data <- getSymbols("AMD", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
gspc_data <- getSymbols("^GSPC", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
rf_data <- getSymbols("DTB4WK", src = "FRED", from = start_date, to = end_date, auto.assign = FALSE)

# Convert Adjusted Closing Prices and DTB4WK to data frames
amd_df <- data.frame(Date = index(amd_data), AMD = as.numeric(Cl(amd_data)))
gspc_df <- data.frame(Date = index(gspc_data), GSPC = as.numeric(Cl(gspc_data)))
# Accessing the first column of rf_data
rf_df <- data.frame(Date = index(rf_data), RF = as.numeric(rf_data[,1]))

# Merge the AMD, GSPC, and RF data frames on the Date column
df <- merge(amd_df, gspc_df, by = "Date")
df <- merge(df, rf_df, by = "Date")
```

#### Data Processing 
```r
colSums(is.na(df))
# Fill N/A RF data
df <- df %>%
  fill(RF, .direction = "down") 
```

### Step 2: CAPM Analysis

The Capital Asset Pricing Model (CAPM) is a financial model that describes the relationship between systematic risk and expected return for assets, particularly stocks. It is widely used to determine a theoretically appropriate required rate of return of an asset, to make decisions about adding assets to a well-diversified portfolio.

#### The CAPM Formula
The formula for CAPM is given by:

$$
E(R_i) = R_f + \beta_i (E(R_m) - R_f)
$$

Where:

- $E(R_i)$ is the expected return on the capital asset,
- $R_f$ is the risk-free rate,
- $\beta_i$ is the beta of the security, which represents the systematic risk of the security,
- $E(R_m)$ is the expected return of the market.



#### CAPM Model Daily Estimation

- **Calculate Returns**: First, we calculate the daily returns for AMD and the S&P 500 from their adjusted closing prices. This should be done by dividing the difference in prices between two consecutive days by the price at the beginning of the period.
  
$$
\text{Daily Return} = \frac{\text{Today's Price} - \text{Previous Trading Day's Price}}{\text{Previous Trading Day's Price}}
$$

```r
# To ensure clarity and readability, we utilise the pipe operator '%>%' to pass the data frame 'df' to any of the following functions. 

df <- df %>%
  
# We then utilise the 'mutate' function to add the columns of 'AMD_Daily_Return' and 'GSPC_Daily_Return' to the 'df' data frame. We calculate the daily returns of AMD through the formula: AMD - lag(AMD)) / lag(AMD), whereby the 'lag' function enables us to use the previous day's closing price. A similar calculation is then utilised to determine the daily returns of the S&P 500.
  
  mutate(AMD_Daily_Return = (AMD - lag(AMD)) / lag(AMD), 
         GSPC_Daily_Return = (GSPC - lag(GSPC)) / lag(GSPC))

```

- **Calculate Risk-Free Rate**: Calculate the daily risk-free rate by conversion of annual risk-free Rate. This conversion accounts for the compounding effect over the days of the year and is calculated using the formula:
  
$$
\text{Daily Risk-Free Rate} = \left(1 + \frac{\text{Annual Rate}}{100}\right)^{\frac{1}{360}} - 1
$$

```r
# The pipe operator '%>%' enables us to pass the data frame 'df' to all following functions.

df <- df %>%
  
# The 'mutate' function is then utilised to add the column of 'Daily_Risk_Free_Rate' to the 'df' data frame. We calculate the daily risk-free rate through the formula: (1 + RF/100)^(1/360) - 1. 
  
  mutate(Daily_Risk_Free_Rate = (1 + RF/100)^(1/360) - 1)
```


- **Calculate Excess Returns**: Compute the excess returns for AMD and the S&P 500 by subtracting the daily risk-free rate from their respective returns.

```r
# Once, again, we use the pipe operator '%>%' to pass the data frame 'df' to all following functions.

df <- df %>%

# We then utilise the 'mutate' function to add the columns of 'AMD_Excess_Return' and 'GSPC_Excess_Return' to the 'df' data frame. We calculate the excess returns of AMD through the formula: AMD_Daily_Return - Daily_Risk_Free_Rate. A similar calculation is then used to determine the excess returns of the S&P 500. 
  
  mutate(AMD_Excess_Return = AMD_Daily_Return - Daily_Risk_Free_Rate,
         GSPC_Excess_Return = GSPC_Daily_Return - Daily_Risk_Free_Rate)
```


- **Perform Regression Analysis**: Using linear regression, we estimate the beta (\(\beta\)) of AMD relative to the S&P 500. Here, the dependent variable is the excess return of AMD, and the independent variable is the excess return of the S&P 500. Beta measures the sensitivity of the stock's returns to fluctuations in the market.

```r
# We utilise the 'lm' function to perform a linear regression analysis, whereby 'AMD_Excess_Return' is the dependent variable and 'GSPC_Excess_Return' is the independent variable. This result is then stored under the variable name 'linear_model'. 

linear_model <- lm(AMD_Excess_Return ~ GSPC_Excess_Return, data = df)

# Next, we use the 'coef' function to estimate and extract coefficients for the linear regression model. These results are then stored under the variable name 'coefficients'. 

coefficients <- coef(linear_model)

# We then print a summary of our linear regression model. The summary provides us with coefficients of statistics such as the standard-error, t-value, p-value and R-squared, enabling us to ascertain the validity of our model. 

summary(linear_model)

# Finally, we extract and print the coefficient associated with the variable 'GSPC_Excess_Return' and label this result as 'beta'. 

beta <- coefficients["GSPC_Excess_Return"]
cat("Beta:", beta, "\n")

```


#### Interpretation

What is your \(\beta\)? Is AMD more volatile or less volatile than the market?

**Answer:** Through my linear regression model, the \(\beta\) of AMD, relative to the S&P500, was recorded as 1.569999. A \(\beta\) value greater than 1 indicates that the price of a stock experiences larger variability as opposed to the market. In the context of my given \(\beta\) value, this means that AMD's returns are expected to fluctuate 1.569999 times more than market returns. This implies that AMD carries greater investment risk, however, also presents the opportunity for heightened profits. Thereby, AMD is more volatile than the market.


#### Plotting the CAPM Line
Plot the scatter plot of AMD vs. S&P 500 excess returns and add the CAPM regression line.

```r
# In order to plot the scatter plot of the excess returns between AMD and the S&P 500, we shall specify the aesthetic mappings of the plot. By taking data from the 'df' data frame, we assign the x-axis to the 'GSPC_Excess_Return' column and the y-axis to the 'AMD_Excess_Return' column. In addition to this, we set 'na.rm = TRUE' to ensure that rows containing 'NA' values are ignored.  

ggplot(df, aes(x = GSPC_Excess_Return, y = AMD_Excess_Return), na.rm = TRUE) +
  
# Using the 'geom_point' function, we add add points to create our scatter plot. By setting alpha as 0.4, we decrease the opacity of the points to 40%, enabling for clearer visualisation of any overlapping points. 
  
  geom_point(alpha = 0.4, na.rm = TRUE) + 
  
# Next, we utilise the 'geom_smooth' function, and set the method to 'lm', to add a linear regression line (CAPM regression line) to our plot.  
  
  geom_smooth(method = "lm", colour = "blue", na.rm = TRUE) + 
  
# We then add a title for our plot, as well as labels to the x and y axis. 
  
  labs(title = "Excess Returns of AMD vs. S&P 500", 
       x = "Excess Return of S&P 500",
       y = "Excess Return of AMD") + 
  
# Finally, we apply a minimalist theme to the plot, using the 'theme_minimal' function.
  
  theme_minimal()
```

### Step 3: Predictions Interval
Suppose the current risk-free rate is 5.0%, and the annual expected return for the S&P 500 is 13.3%. Determine a 90% prediction interval for AMD's annual expected return.



**Answer:**

```r
# First, we start by initialising the variables 'Current_Risk_Free_Rate' and 'Annual_Expected_Return', which will be utilised in further calculations. 

Current_Risk_Free_Rate <- 0.05
Annual_Expected_Return <- 0.133

# Next, we convert our annual risk free rate to the daily risk free rate through the formula: (1 + Current_Risk_Free_Rate)^(1/252) - 1. We then label this result as 'daily_rf'. (We use 252, given that there are 252 trading days in a year).

daily_rf <- (1 + Current_Risk_Free_Rate)^(1/252) - 1 

# Using our newly adjusted daily risk free rate, we calculate the annual expected return through the formula: Annual_Expected_Return/sqrt(252) - daily_rf. This result is then stored under the variable name 'xf'. 

xf <- Annual_Expected_Return/sqrt(252) - daily_rf 

# Next, we determine the average of the excess returns for the S&P500, using the 'mean' function. In addition to this, we set 'na.rm = TRUE', to ensure that 'NA' values are ignored. We then store this result as 'x_bar'.

x_bar <- mean(df$GSPC_Excess_Return, na.rm = TRUE)

# We then calculate the sum of the squared differences between each of the excess returns and the mean excess return, by using the 'sum' function. Once again, we set 'na.rm = TRUE', to ensure that 'NA' values are ignored. This result is then saved under the variable name 'x_difference_squared_sum'.  

x_difference_squared_sum <- sum((df$GSPC_Excess_Return - x_bar)^2, na.rm = TRUE)

# In order to calculate the annual standard error of the forecast, we extract the 'standard_error' from the summary, and calculate the daily standard error, 'sf', through the formula: standard_error * sqrt(1 + (1/nrow(df)) + (xf - x_bar)^2 / x_difference_squared_sum). We then convert this to the annual standard error through the formula: sf * sqrt(252). 

standard_error <- summary(linear_model)$sigma 
sf <- standard_error * sqrt(1 + (1/nrow(df)) + (xf - x_bar)^2 / x_difference_squared_sum)
annual_sf <- sf * sqrt(252)

# Using the CAPM formula, we calculate the expected returns for AMD via the formula: Current_Risk_Free_Rate + beta*(Annual_Expected_Return - Current_Risk_Free_Rate. 

AMD_Expected_Return <- Current_Risk_Free_Rate + beta*(Annual_Expected_Return - Current_Risk_Free_Rate)

# For a prediction level of 90%, we must account for the two tails of distribution. That is, since 1 - 0.9 = 0.1 and 0.1 / 2 = 0.5, each tail is to contain 5% of the distribution. Thereby, we determine the critical t-value by specifying 0.95 for the 'qt' function, which will account for the upper tail of the distribution. 
t_critical <- qt(0.95, df = nrow(df) - 2)

# The error margin is then calculated through the formula: t_critical * annual_sf. We determine the lower and upper bounds for AMD's annual expected return by appropriately subtracting and adding the 'error_margin' to the variable 'AMD_Expected_Return'.

error_margin <- t_critical * annual_sf
lower_bound <- AMD_Expected_Return - error_margin
upper_bound <- AMD_Expected_Return + error_margin

# Finally, we print out results, showing the lower and upper bounds, as well as the 90% prediction interval.

cat("Lower bound:", lower_bound, "\n")
cat("Upper bound:", upper_bound, "\n")
cat("90% Prediction Interval: [",lower_bound,",",upper_bound,"]\n")
```
