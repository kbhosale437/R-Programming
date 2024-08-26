library(readxl)
library(dplyr)
library(ggplot2)
library(triangle)
# Load data from Excel
data <- read_excel("/Users/karanbhosale/Downloads/Case4_distParameters.xlsx")

monte_carlo_simulation <- function(data, n_simulations = 10000, discount_rate = 0.1) {
  results <- list()
  
  for (i in 1:nrow(data)) {
    npvs <- numeric(n_simulations)
    payback_periods <- numeric(n_simulations)
    rois <- numeric(n_simulations)
    
    for (j in 1:n_simulations) {
      # Simulate initial investment
      initial_investment <- rtriangle(1, data$`Initial Investment (min)`[i],
                                      data$`Initial Investment (most likely)`[i],
                                      data$`Initial Investment (max)`[i])
      
      # Monthly simulations
      net_cash_flows <- numeric(36) # Simulate for 3 years
      for (month in 1:36) {
        monthly_costs <- rnorm(1, data$`Monthly Operating Cost (mean)`[i],
                               data$`Monthly Operating Cost (std)`[i])
        monthly_traffic <- rpois(1, data$`Monthly Foot Traffic (mean)`[i])
        avg_spend <- rlnorm(1, meanlog = log(data$`Average Spend per Customer (mean)`[i]) - 0.5 * data$`Average Spend per Customer (std)`[i]^2,
                            sdlog = data$`Average Spend per Customer (std)`[i])
        
        # Calculate revenue and net cash flow
        monthly_revenue <- monthly_traffic * avg_spend
        net_cash_flows[month] <- monthly_revenue - monthly_costs
      }
      
      # Calculate NPV
      npvs[j] <- sum(net_cash_flows / (1 + discount_rate/12)^(1:36))
      cumulative_cash_flow <- cumsum(net_cash_flows)
      payback <- which(cumulative_cash_flow + initial_investment >= 0)
      payback_periods[j] <- ifelse(length(payback) > 0, payback[1], NA)
      
      # Calculate ROI
      rois[j] <- (cumulative_cash_flow[36] - initial_investment) / initial_investment
    }
    
    results[[i]] <- data.frame(Location = data$Location[i],
                               Average_NPV = mean(npvs, na.rm = TRUE),
                               Median_Payback_Period = median(payback_periods, na.rm = TRUE),
                               Average_ROI = mean(rois, na.rm = TRUE))
  }
  
  return(do.call(rbind, results))
}

results <- monte_carlo_simulation(data)
print(results)

ggplot(results, aes(x = Location, y = Average_NPV)) +geom_col() +
labs(title = "Average NPV by Location", y = "Average NPV", x = "Location")
