
## Algorithmic Trading Strategy

## Introduction

In this assignment, you will develop an algorithmic trading strategy by incorporating financial metrics to evaluate its profitability. This exercise simulates a real-world scenario where you, as part of a financial technology team, need to present an improved version of a trading algorithm that not only executes trades but also calculates and reports on the financial performance of those trades.

## Background

Following a successful presentation to the Board of Directors, you have been tasked by the Trading Strategies Team to modify your trading algorithm. This modification should include tracking the costs and proceeds of trades to facilitate a deeper evaluation of the algorithm’s profitability, including calculating the Return on Investment (ROI).

After meeting with the Trading Strategies Team, you were asked to include costs, proceeds, and return on investments metrics to assess the profitability of your trading algorithm.

## Objectives

1. **Load and Prepare Data:** Open and run the starter code to create a DataFrame with stock closing data.

2. **Implement Trading Algorithm:** Create a simple trading algorithm based on daily price changes.

3. **Customize Trading Period:** Choose your entry and exit dates.

4. **Report Financial Performance:** Analyze and report the total profit or loss (P/L) and the ROI of the trading strategy.

5. **Implement a Trading Strategy:** Implement a trading strategy and analyze the total updated P/L and ROI. 

6. **Discussion:** Summarise your finding.


## Instructions

### Step 1: Data Loading

Start by running the provided code cells in the "Data Loading" section to generate a DataFrame containing AMD stock closing data. This will serve as the basis for your trading decisions. First, create a data frame named `amd_df` with the given closing prices and corresponding dates. 

```r
# Load data from CSV file
amd_df <- read.csv("AMD.csv")
# Convert the date column to Date type and Adjusted Close as numeric
amd_df$date <- as.Date(amd_df$Date)
amd_df$close <- as.numeric(amd_df$Adj.Close)
amd_df <- amd_df[, c("date", "close")]
```

#### Plotting the Data
Plot the closing prices over time to visualize the price movement.
```r
plot(amd_df$date, amd_df$close,'l')
```

### Step 2: Trading Algorithm
Implement the trading algorithm as per the instructions. You should initialize necessary variables, and loop through the dataframe to execute trades based on the set conditions.

- Initialize Columns: Start by ensuring dataframe has columns 'trade_type', 'costs_proceeds' and 'accumulated_shares'.
- Change the algorithm by modifying the loop to include the cost and proceeds metrics for buys of 100 shares. Make sure that the algorithm checks the following conditions and executes the strategy for each one:
  - If the previous price = 0, set 'trade_type' to 'buy', and set the 'costs_proceeds' column to the current share price multiplied by a `share_size` value of 100. Make sure to take the negative value of the expression so that the cost reflects money leaving an account. Finally, make sure to add the bought shares to an `accumulated_shares` variable.
  - Otherwise, if the price of the current day is less than that of the previous day, set the 'trade_type' to 'buy'. Set the 'costs_proceeds' to the current share price multiplied by a `share_size` value of 100.
  - You will not modify the algorithm for instances where the current day’s price is greater than the previous day’s price or when it is equal to the previous day’s price.
  - If this is the last day of trading, set the 'trade_type' to 'sell'. In this case, also set the 'costs_proceeds' column to the total number in the `accumulated_shares` variable multiplied by the price of the last day.



```r

# In order to maintain readability and avoid repetition of the code, a function with the name 'original_algorithm' and the argument 'amd_df' is first introduced. This function will enable us to use the code found in Step 2 in subsequent steps of the task.

original_algorithm <- function(amd_df) {
  
# Next, we initialize the three new columns of 'trade_type', 'cost_proceeds' and 'accumulated shares' to the 'amd_df' data frame. The 'trade_type' column will be be used to record the type of trade (buy, hold or sell), the 'cost_proceeds' column will serve to record the expenditures and returns of the trades and the 'accumulated shares' column will function to keep track of the total number of shares held at any given time. Initially, the 'trade_type' and 'cost_proceeds' columns are assigned 'NA', as no trades have yet occurred and hence these values cannot be defined, whilst the 'accumulated shares' column is assigned '0' to indicate that no shares are held in the beginning.
  
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA
amd_df$accumulated_shares <- 0

# Similarly, we will initialize the variables of 'previous_price', 'share_size' and 'accumulated_shares' which will be used for trading logic and calculations. The 'previous_price' variable will be used to compare the current day's price to decide whether to hold or buy shares, and is initially assigned '0', given that the first day will have no previous price to compare against. The 'share_size' variable, assigned the value of '100', will be used by the algorithm to purchase exactly one hundred shares whenever it does decide to make the trade type of 'buy'. Finally, the 'accumulated_shares' variable will keep track of the total number of shares held by the algorithm and is initially assigned '0' as no shares are held in the beginning. 

previous_price <- 0
share_size <- 100
accumulated_shares <- 0

# Next, a 'for' loop is initialized to ensure that the algorithm is executed for all rows, starting from row 1, of the 'amd_df' data frame.  

for (i in 1:nrow(amd_df)) {

# Additionally, we establish that for the current row 'i' the closing price is to be stored in 'present_price'. 
  
  present_price <- amd_df$close[i]  
  
# First the algorithm will check that the variable 'previous_price' is equal to 0. If this condition is satisfied, the algorithm will 'buy' the chosen number of shares (100) on the first day, and this transaction will be recorded in the the first row of the 'trade_type' column. Following this, the expenditure of buying these shares will be calculated through the formula '-present_price*share_size', before being recorded in the first row of the 'cost_proceeds' column. The algorithm will then update the accumulated number of shares held, adding the number of shares bought to the number of shares held initially by applying the formula 'accumulated_shares+share_size'. 
  
  if (previous_price == 0) {
    amd_df$trade_type[i] <- "buy"
    amd_df$costs_proceeds[i] <- -present_price*share_size
    accumulated_shares <- accumulated_shares+share_size
  }
    
# For the all days excluding the first and last days of the period, the algorithm is set to check if the present day's closing price is less than the previous day's closing price. If this condition is true, the algorithm will proceed to buy one hundred shares and record this purchase in the appropriate row of the 'trade_type' column. Similarly to the first day, the algorithm will calculate the cost of purchasing these shares through the formula '-present_price*share_size', and update the accumulated shares held through the formula 'accumulated_shares+share_size'. These values will be recorded in the 'cost_proceeds' and 'accumulated_shares' columns respectively.  
    
  else if (present_price < previous_price) {
    amd_df$trade_type[i] <- "buy"
    amd_df$costs_proceeds[i] <- -present_price*share_size
    accumulated_shares <- accumulated_shares+share_size
  }
    
# If all previous conditions are not satisfied, the algorithm will 'hold' onto the shares and record this in the appropriate row of the 'trade_type' column. Since the number of shares held remains unchanged, the respective row of the 'cost_proceeds' column is assigned 'NA' whenever a 'hold' is encountered. 
    
  else {
    amd_df$trade_type[i] <- "hold"
    amd_df$costs_proceeds[i] <- NA
  }
  
 # If the current row 'i' is equal to the total number of rows present in the 'amd_df' data frame, the algorithm will 'sell' all of the shares held and record this transaction in the last row of 'trade_type' column. The proceeds from selling these shares will then be calculated through the formula 'present_price*accumulated_shares' and presented in the last row of the 'cost_proceeds' column. Given that all shares will now have been sold, 'accumulated_shares' is assigned the value '0'.  
  
  if (i == nrow(amd_df)) {
    amd_df$trade_type[i] <- "sell"
    amd_df$costs_proceeds[i] <- present_price*accumulated_shares
    accumulated_shares <- 0 
  }
  
# We assign 'present_price' to 'previous_price' to ensure that the 'previous_price' variable holds the present day's closing price at the end of current iteration. This is executed as the algorithm will compare the updated 'previous_price' with the new 'present_price' in the following iteration, to determine whether to buy, hold or sell shares. Additionally, we update the 'accumulated_shares' column of the data frame with the value of the 'accumulated_shares' variable at the end of each iteration, enabling us to keep track of shares held at any given time. 
  
  previous_price <- present_price
  amd_df$accumulated_shares[i] <- accumulated_shares
}

# Finally, we return the modified 'amd_df' data frame as the output of the function. This will enable us to call upon the 'original_algorithm' in later steps. 

  return(amd_df)
}

```


### Step 3: Customize Trading Period
- Define a trading period you wanted in the past five years 
```r
# In order to customize the trading period, we must first assign a specific date to the variable 'start_date' and a specific date to the variable 'end_date'.

start_date <- as.Date('2022-11-21')
end_date <- as.Date('2023-11-21')

# Next, we shall filter the 'amd_df' data frame such that it only includes rows of data where the 'date' falls within 2022-11-21 and 2023-11-21. This is achieved by accounting for all dates larger than or equal to the 'start_date' variable but less than or equal to the 'end_date' variable. We shall then name this filtered data frame 'chosenperiod_df'.

chosenperiod_df <- amd_df[amd_df$date >= start_date & amd_df$date <= end_date,]

# Following this, we apply the 'original_algorithm' function that we established in Step 2 to the filtered 'chosenperiod_df' data frame. We then store this altered information into a newly introduced data frame named 'newamd_df', so that it can be utilized for later steps. 

newamd_df <- original_algorithm(chosenperiod_df)
```


### Step 4: Run Your Algorithm and Analyze Results
After running your algorithm, check if the trades were executed as expected. Calculate the total profit or loss and ROI from the trades.

- Total Profit/Loss Calculation: Calculate the total profit or loss from your trades. This should be the sum of all entries in the 'costs_proceeds' column of your dataframe. This column records the financial impact of each trade, reflecting money spent on buys as negative values and money gained from sells as positive values.
- Invested Capital: Calculate the total capital invested. This is equal to the sum of the 'costs_proceeds' values for all 'buy' transactions. Since these entries are negative (representing money spent), you should take the negative sum of these values to reflect the total amount invested.
- ROI Formula: $$\text{ROI} = \left( \frac{\text{Total Profit or Loss}}{\text{Total Capital Invested}} \right) \times 100$$

```r
# To execute the total profit/loss calculation, the algorithm is set to 'omit' rows which contain 'NA' values, before taking the sum of the remaining values in the 'cost_proceeds' column of the 'newamd_df' data frame.  

total_profit_loss <- sum(na.omit(newamd_df$costs_proceeds))

# Next, to calculate the total capital invested, the algorithm will select only the values within the 'cost_proceeds' column where the 'trade_type' is recorded as 'buy'. Additionally, the algorithm will exclude any 'NA' values present within the 'cost_proceeds' column. The negative sum of all selected values is then taken, to represent the quantity of the capital invested. 

invested_capital <--sum(na.omit(newamd_df$costs_proceeds[newamd_df$trade_type == "buy"]))

# In order to determine the return on investment (ROI), the algorithm will divide the 'total_profit_loss' by the 'invested_capital', before multiplying this result by 100.

return_on_investment <- (total_profit_loss/invested_capital)*100

# Finally, we concatenate and present our results. Each variable is labelled and printed on a new line to ensure readability and clarity. 

cat("Total Profit/Loss:", total_profit_loss, "\nInvested Capital:", invested_capital, "\nReturn on Investment (ROI):", return_on_investment, "%\n")
```

### Step 5: Profit-Taking Strategy or Stop-Loss Mechanisum (Choose 1)
- Option 1: Implement a profit-taking strategy that you sell half of your holdings if the price has increased by a certain percentage (e.g., 20%) from the average purchase price.
- Option 2: Implement a stop-loss mechanism in the trading strategy that you sell half of your holdings if the stock falls by a certain percentage (e.g., 20%) from the average purchase price. You don't need to buy 100 stocks on the days that the stop-loss mechanism is triggered.


```r
# *** CHOSEN OPTION: OPTION 1 ***

# Similarly to Step 2, we start by first initializing the three columns of 'trade_type', 'cost_proceeds' and 'accumulated_shares' to the 'newamd_df' data frame. The 'trade_type' column will be used to record the type of trade executed (buy, hold or sell), the 'cost_proceeds' column will function to store the expenditures and returns of the trades and the 'accumulated_shares' column will serve to keep track of the number of shares held at any given time. Once again, the 'trade_type' and 'cost_proceeds' columns are initially assigned 'NA', as no trades have yet occurred and hence these values cannot be defined, whilst the 'accumulated shares' column is assigned '0' to indicate that no shares are held at the start. 

newamd_df$trade_type <- NA
newamd_df$costs_proceeds <- NA
newamd_df$accumulated_shares <- 0

# Next, we will add two new columns named 'cumulative_investment' and 'avg_price' to the "newamd_df' data frame. The 'cumulative_investment' column will be used to record the total amount of money invested and returned over time, whilst the 'avg_price' column will function to keep track of the running average purchase price respective to each date. The 'avg_price' column will enable the algorithm to determine when the price has increased by 10% from the average purchase price; that is, it will allow the algorithm to decide when to 'sell' half of its holdings. Both columns are initially assigned '0', as money is yet to be invested and purchase prices are yet to be detailed. 

newamd_df$cumulative_investment <- 0
newamd_df$avg_price <- 0

# We shall also initialize the variable 'share_size' and assign it a value of '100'. This will be used for calculations when the algorithm decides to 'buy' shares. 

share_size <- 100

# Additionally, a 'for' loop is initialized to ensure that the algorithm is executed for all rows, starting from row 1 of the 'newamd_df' data frame.   

for (i in 1:nrow(newamd_df)) {
  
# The algorithm will first check that the current row 'i' is the first row in the data frame. If this condition is satisfied, the algorithm will 'buy' the chosen number of shares (100), calculate the expenditure of purchasing these shares through the formula '-newamd_df$close[i]*share_size' and update the accumulated shares held to reflect the number of shares bought on the first day. Furthermore, the algorithm will determine the initial total investment through the formula 'newamd_df$close[i]*share_size' and set the closing price of the first day as the average price, since only a single purchase has been made thus far. These values will all be recorded in the first rows of their respective columns.  
  
  if (i == 1) {
    newamd_df$trade_type[i] <- "buy"
    newamd_df$costs_proceeds[i] <- -newamd_df$close[i]*share_size
    newamd_df$accumulated_shares[i] <- share_size
    newamd_df$cumulative_investment[i] <- newamd_df$close[i]*share_size
    newamd_df$avg_price[i] <- newamd_df$close[i]
  } 
  
# The algorithm will then check if the current row 'i' is neither the first nor the last row of the data frame. It then checks if the closing price of the present day 'i' is less than the closing price of the previous day 'i-1'. If these conditions are satisfied, the algorithm will make a purchase of one hundred shares, calculate the cost of this purchase through the formula '-newamd_df$close[i]*share_size', and update the total number of shares held by adding the 'share_size' to the accumulated shares reported on the previous 'i-1'th day. Additionally, the algorithm will determine the total investment made through the formula 'newamd_df$cumulative_investment[i-1]+newamd_df$close[i]*share_size' and continually update the running average price for each row by dividing the 'accumulated_investment' by the 'accumulated_shares'. These values will all be recorded in the appropriate rows of their respective columns. 
  
  else if (i != 1 && i != nrow(newamd_df)) {
    if (newamd_df$close[i] < newamd_df$close[i-1]) {
      newamd_df$trade_type[i] <- "buy"
      newamd_df$costs_proceeds[i] <- -newamd_df$close[i]*share_size
      newamd_df$accumulated_shares[i] <- newamd_df$accumulated_shares[i-1]+share_size
      newamd_df$cumulative_investment[i] <- newamd_df$cumulative_investment[i-1]+newamd_df$close[i]*share_size
      newamd_df$avg_price[i] <- newamd_df$cumulative_investment[i]/newamd_df$accumulated_shares[i]
    } 
    
# Next, the algorithm will check if the closing price of the present day 'i' is 10% larger than the average price taken on the 'i-1'th day. If this condition is true, the algorithm will 'sell' half of its shares, reflected through the formula 'newamd_df$close[i]*(newamd_df$accumulated_shares[i-1]/2)'. Correspondingly, the number of accumulated shares will be halved and hence the cumulative investment made will be modeled through the formula 'newamd_df$cumulative_investment[i-1]-(newamd_df$avg_price[i-1]* (newamd_df$accumulated_shares[i-1]/2))'. As a result of these changes to 'cumulative_investment' and 'accumulated_shares', the average price will remain unaltered when shares are sold, since 'avg_price' is given by the formula 'newamd_df$cumulative_investment[i]/newamd_df$accumulated_shares[i]'. Once more, these values will all be recorded in the appropriate rows of their respective columns.
   
    else if (newamd_df$close[i] >= 1.10*newamd_df$avg_price[i-1]) {
      newamd_df$trade_type[i] <- "sell"
      newamd_df$costs_proceeds[i] <- newamd_df$close[i]*(newamd_df$accumulated_shares[i-1]/2)
      newamd_df$accumulated_shares[i] <- newamd_df$accumulated_shares[i-1]/2
      newamd_df$cumulative_investment[i] <- newamd_df$cumulative_investment[i-1]-(newamd_df$avg_price[i-1]* (newamd_df$accumulated_shares[i-1]/2))
      newamd_df$avg_price[i] <- newamd_df$cumulative_investment[i]/newamd_df$accumulated_shares[i]
    } 

# If all previous conditions have not been met, the trade type for the current row 'i' will be set as 'hold'. Since shares will be held, the 'cost_proceeds' column will be assigned an 'NA' value to indicate that there are no costs or proceeds associated with this action. As the accumulated shares will be carried forward from the 'i-1'th to the 'i'th day, the 'accumulated_shares' column is set to display the value of the previous row. In similar fashion, the 'cumulative_investment' and 'avg_price' columns will also present values derived from data detailed on the 'i-1'th day. 
    
    else {
      newamd_df$trade_type[i] <- "hold"
      newamd_df$costs_proceeds[i] <- NA
      newamd_df$accumulated_shares[i] <- newamd_df$accumulated_shares[i-1]
      newamd_df$cumulative_investment[i] <- newamd_df$cumulative_investment[i-1]
      newamd_df$avg_price[i] <- newamd_df$avg_price[i-1]
    }
  
#  Finally, the algorithm will check if the current row 'i' is the last row of the 'newamd_df' data frame. If this is the case, the algorithm is set to 'sell' all shares held on the last day. As a result, the last row of the 'cost_proceeds' column will be given by 'newamd_df$close[i]*newamd_df$accumulated_shares[i-1]', whilst the last row of the 'accumulated_shares' column will be assigned the value of '0', indicating that there remains no more shares. Given all shares are now sold, the last row of the 'cumulative_investment' column will be modeled by 'newamd_df$cumulative_investment[i-1]-newamd_df$close[i]*newamd_df$accumulated_shares[i-1]' and the 'avg_price' column will reflect data detailed on the 'i-1'th day, as the running average purchasing price will remain unaltered. 
    
  } else if (i == nrow(newamd_df)) {
    newamd_df$trade_type[i] <- "sell"
    newamd_df$costs_proceeds[i] <- newamd_df$close[i]*newamd_df$accumulated_shares[i-1]
    newamd_df$accumulated_shares[i] <- 0
    newamd_df$cumulative_investment[i] <- newamd_df$cumulative_investment[i-1]-newamd_df$close[i]*newamd_df$accumulated_shares[i-1]
    newamd_df$avg_price[i] <- newamd_df$avg_price[i-1]
  }
}
```


### Step 6: Summarize Your Findings
- Did your P/L and ROI improve over your chosen period?
- Relate your results to a relevant market event and explain why these outcomes may have occurred.


```r
# *** This code is the exact same as the code found in Step 4, however, calculates the new values of total  profit/loss, invested capital and return on investment when the profit taking strategy is implemented ***

# To execute the total profit/loss calculation, the algorithm is set to 'omit' rows which contain 'NA' values, before taking the sum of the remaining values in the 'cost_proceeds' column of the 'newamd_df' data frame.  

total_profit_loss <- sum(na.omit(newamd_df$costs_proceeds))

# Next, to calculate the total capital invested, the algorithm will select only the values within the 'cost_proceeds' column where the 'trade_type' is recorded as 'buy'. Additionally, the algorithm will exclude any 'NA' values present within the 'cost_proceeds' column. The negative sum of all selected values is then taken, to represent the quantity of capital invested. 

invested_capital <--sum(na.omit(newamd_df$costs_proceeds[newamd_df$trade_type == "buy"]))

# In order to determine the return on investment (ROI), the algorithm will divide the 'total_profit_loss' by the 'invested_capital', before multiplying this result by 100.

return_on_investment <- (total_profit_loss/invested_capital)*100

# Finally, we concatenate and present our results. Each variable is labelled and printed on a new line to ensure readability and clarity. 

cat("Total Profit/Loss:", total_profit_loss, "\nInvested Capital:", invested_capital, "\nReturn on Investment (ROI):", return_on_investment, "%\n")
```

Discussion: 

Analysis of the closing AMD stock price from 2022-11-21 to 2023-11-21, reveals that the stock experienced periods of relatively short but sharp increases, subsequently followed by marginal declines, between the end of 2022 and the middle of 2023. Whilst the closing stock price peaked at a value of $127.33 per share, before falling and displaying stagnation in the latter half of 2023, the initial steep increases to stock prices across late 2022 and the first half of 2023 translated to an overall growth of AMD stock between 2022-11-21 to 2023-11-21.

This growth can be largely attributed to the launch of the AMD Radeon RX 7000 series, AMD's high-performance graphics processing units (GPUs) released on the 13th of December, 2022. As opposed to its predecessors and competitors, AMD's new series of GPUs were significantly more advanced at enhancing performance and energy efficiency, due to the incorporation of the newly improved RDNA 3 Architecture. Additionally, this series of GPUs offered numerous advanced features, including the incorporation of the redeveloped 'Infinity Cache' system which offered one of the fastest cache technologies available for its time. Coupled with prices comparatively lower than competitors, the AMD Radeon RX 7000 series appealed heavily to both professional and gaming markets alike, and received a largely positive reception when it was launched. Ultimately, this strengthened AMD's position in the market and enabled AMD to rival the leading companies in the industry, boosting investor confidence and leading to rapid, sharp increases in AMD stock prices. 

Through the use of the simple trading algorithm, as outlined in Step 2, the profit obtained over the chosen period, 2022-11-21 to 2023-11-21, was $309367.1, whilst the return on investment across this time frame was 26.75489%. However, with the implementation of the profit-taking strategy, as detailed in Step 5, both the obtained profit and return on investment decreased across this period, to $142220.9 and 12.29964% respectively. This decrease in profit and return on investment is primarily as a result of the aforementioned periods of short and sharp increases to the AMD stock price, experienced between late 2022 and the first half of 2023. These steep rises exceeded the 10% threshold as outlined within the profit-taking strategy, resulting in the algorithm to sell half of its shares prematurely, prior to when a peak was met. Consequently, this not only reduced the potential return obtained for each share sold, but also decreased the profits received when the peak was finally met, since the halving mechanism would have decreased the shares held before this heightened stock price was achieved. As opposed to this, the simple trading algorithm outlined in Step 2 was able to by-pass the volatility of the AMD stock prices and only sell outright at the end of the period, when the price of shares was relatively high. 

Thereby, whilst the stock price of AMD saw an overall increase between 2022-11-21 to 2023-11-21, the profit-taking strategy was not optimized in achieving improved values for profit and return on investment.

