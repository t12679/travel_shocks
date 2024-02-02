
#read in data

library(readxl)
#file_path <- "/Users/ry/Desktop/SOCI RA/Data_cleaned_for_continents.xlsx"
#data <- read_excel(file_path)
data_cleaned_wo_ven <- read_csv("Documents/Shocks_To_Travel_Flow_Study/impact.of.trump.on.travel.inflows.project/data_cleaned_wo_ven.csv")


#DID analysis for country level data

library(readxl)
#you can assign  a file path name to an object
getwd()
file_path<-"Documents/Shocks_To_Travel_Flow_Study/impact.of.trump.on.travel.inflows.project/data_cleaned.xls"
#file_path <- "/Users/ry/Desktop/SOCI RA/data_cleaned.xls"
#this ojbect you can read into a data frame by usig the read_excel funtion from readxl
data <- read_excel(file_path)


install.packages("writeexl")

# package ‘writeexl’ is not available for this version of R
# 
# A version of this package for your version of R might be available elsewhere,
# see the ideas at
# https://cran.r-project.org/doc/manuals/r-patched/R-admin.html#Installing-packages

#install.packages("xlsx")

library(writexl)
library(zoo)
#create a new varaible called time by applying the function as.yearmon which transforms
#this variable into the class representin monthly data, 
#paste the text from the column year and then the text from the column month and separate them by a -

#CREATE NEW VARIABLES FOR ANALYSIS:
data$time <- as.yearmon(paste(data$year, data$month, sep = "-"))
#subset the data into those rows that have the yearmon between 2015-11 and 2017-09
#note that I will need to include my analysis of percentage change even before this because I 
#need the data from 2014
data <- data[data$time >= as.yearmon("2015-11") & data$time <= as.yearmon("2017-09"), ]
#factorize and move teh data in the Source column into a column called country
data$country <- as.factor(data$Source)
#log the column called count_citizens and put it in new column called lcount_citizens
data$lcount_citizens <- log(data$count_citizens)
#create three new columns in data set called did1, did2, and did3 which resectively
#are the product of the data in column treat1 and post1, and so on
data$did1 <- data$treat1 * data$post1
data$did2 <- data$treat2 * data$post2
data$did3 <- data$treat3 * data$post3




#percentage change graphs:

#create a function that plots each country
library(dplyr)
plot_country = function(country_name, 
                        variable,
                        main_title, 
                        y_axis_label,
                        y_elected,
                        y_ban,
                        y_court) {
  #here the ingredinets of the function are country_name, variable (outcome variable), main_title, y_axis label, y_elected, y_ban)
  # Prep the month labels
  Time <- c("2014-11-01", "2014-12-01", "2015-01-01", "2015-02-01", 
            "2015-03-01", "2015-04-01", "2015-05-01", "2015-06-01", 
            "2015-07-01", "2015-08-01", "2015-09-01", "2015-10-01", 
            "2015-11-01", "2015-12-01", "2016-01-01", "2016-02-01", 
            "2016-03-01", "2016-04-01", "2016-05-01", "2016-06-01", 
            "2016-07-01", "2016-08-01", "2016-09-01", "2016-10-01", 
            "2016-11-01", "2016-12-01", "2017-01-01", "2017-02-01", 
            "2017-03-01", "2017-04-01", "2017-05-01", "2017-06-01", 
            "2017-07-01", "2017-08-01", "2017-09-01")
  
  # Select Canada, extract percent change
  HML = tr_country %>% 
    filter(country == country_name) %>% 
    pull(!! variable)
  #what does pull(!! variable doing?)
  # Build a dataframe contraining two columns: 1 with the time strings, 1 with the % change
  dat <- data.frame(Time=as.POSIXct(Time), HML)[13:35, ]
  #create an object called dat which is a data frame containing the values of Time with the HML containing rows 13 to 35 for all columns)
  
  # Take every third month as an axis tick
  tick_set = dat$Time[seq(1, length(dat$Time), 3)]
  ?substr
  #labels=substr 
  ?str
  # Plot the data (xaxt="n" removes the x axis, axis function adds a new x axis tick set )
  plot(HML ~ Time, type="l", col="black",  
       lwd=1, data=dat, xaxt="n",
       main = main_title,
       ylab = y_axis_label)
  axis(side = 1, 
       at=tick_set, 
       labels=substr(as.character(tick_set), 1, 7), #assigns a substring of character that goes from date 1 to 7
       cex.axis = 0.8)#the number of tick marks
  abline(v = dat$Time[13], col="red", lty=2)#lty assigns different type of line types
  abline(v = dat$Time[16], col="red", lty=1)
  abline(v = dat$Time[20], col="red", lty=3)
  abline(h = 0, col = "grey") #puts in a horizontal line
  text(x = dat$Time[13], y = y_elected, labels = "Trump Elected", pos = 2, cex = 0.7)
  text(x = dat$Time[16], y = y_ban, labels = "Travel Ban", pos = 4, cex = 0.7)
  text(x = dat$Time[20], y = y_court, labels = "Court Approves", pos = 4, cex = 0.7)
}

plot_country("Canada", "percent.change", 
             "Annual % Change in Entries, Canada",
             "% Change",
             0.10, -0.15, 0.10)


plot_country("Mexico", "percent.change", 
             "Figure 6: Annual % Change in Entries, Mexico",
             "% Change",
             0.11, 0.11, 0.10)


plot_country("Argentina", "percent.change", 
             "Figure 7: Annual % Change in Entries, Argentina",
             "% Change",
             0.11, 0.11, 0.10)


plot_country("Brazil", "percent.change", 
             "Annual % Change in Entries, Brazil",
             "% Change",
             -0.05, -0.05, -0.05)

plot_country("Colombia", "percent.change", 
             "Annual % Change in Entries, Colombia",
             "% Change",
             0.11, 0.11, 0.10)

plot_country("Australia", "percent.change", 
             "Annual % Change in Entries, Australia",
             "% Change",
             0.05, 0.05, 0.08)

plot_country("France", "percent.change", 
             "Annual % Change in Entries, France",
             "% Change",
             0.11, 0.11, 0.10)

plot_country("Germany", "percent.change", 
             "Annual % Change in Entries, Germany",
             "% Change",
             0.11, 0.11, 0.10)

plot_country("Ireland", "percent.change", 
             "Annual % Change in Entries, Ireland",
             "% Change",
             0.11, 0.11, 0.10)

plot_country("Italy", "percent.change", 
             "Annual % Change in Entries, Italy",
             "% Change",
             0.11, 0.11, 0.10)

plot_country("Netherlands", "percent.change", 
             "Annual % Change in Entries, Netherlands",
             "% Change",
             0.11, 0.11, 0.10)


plot_country("Spain", "percent.change", 
             "Annual % Change in Entries, Spain",
             "% Change",
             0.11, 0.11, 0.10)

plot_country("Sweden", "percent.change", 
             "Annual % Change in Entries, Sweden",
             "% Change",
             0.11, 0.11, 0.10)

plot_country("Switzerland", "percent.change", 
             "Annual % Change in Entries, Switzerland",
             "% Change",
             0.11, 0.11, 0.10)


plot_country("United Kingdom", "percent.change", 
             "Annual % Change in Entries, United Kingdom",
             "% Change",
             0.11, 0.15, 0.10)


plot_country("Ireland", "percent.change", 
             "Annual % Change in Entries, Ireland",
             "% Change",
             0.11, 0.15, 0.10)

plot_country("India", "percent.change", 
             "Annual % Change in Entries, India",
             "% Change",
             0.11, 0.11, 0.10)

plot_country("Japan", "percent.change", 
             "Annual % Change in Entries, Japan",
             "% Change",
             0.05, 0.05, 0.05)

plot_country("South Korea", "percent.change", 
             "Annual % Change in Entries, South Korea",
             "% Change",
             0.11, 0.11, 0.10)


plot_country("PRC (Excluding HK)", "percent.change", 
             "Annual % Change in Entries, PRC (Excluding HK)",
             "% Change",
             0.11, 0.11, 0.10)

plot_country("Russia", "percent.change", 
             "Annual % Change in Entries, Russia",
             "% Change",
             0.09, 0.09, 0.07)


plot_country("Taiwan", "percent.change", 
             "Annual % Change in Entries, Taiwan",
             "% Change",
             0.11, 0.11, 0.10)

plot_country("Venuezuela", "percent.change", 
             "Annual % Change in Entries, Taiwan",
             "% Change",
             0.11, 0.11, 0.10)






#here I create a function that will plot each continent 
```{r}
library(dplyr)
plot_continent = function(continent_name, 
                          variable,
                          main_title, 
                          y_axis_label,
                          y_elected,
                          y_ban,
                          y_court) {
  #here the ingredinets of the function are country_name, variable (outcome variable), main_title, y_axis label, y_elected, y_ban)
  # Prep the month labels
  Time <- c("2014-11-01", "2014-12-02", "2015-01-01", "2015-02-01", 
            "2015-03-01", "2015-04-01", "2015-05-01", "2015-06-01", 
            "2015-07-01", "2015-08-01", "2015-09-01", "2015-10-01", 
            "2015-11-01", "2015-12-01", "2016-01-01", "2016-02-01", 
            "2016-03-01", "2016-04-01", "2016-05-01", "2016-06-01", 
            "2016-07-01", "2016-08-01", "2016-09-01", "2016-10-01", 
            "2016-11-01", "2016-12-01", "2017-01-01", "2017-02-01", 
            "2017-03-01", "2017-04-01", "2017-05-01", "2017-06-01", 
            "2017-07-01", "2017-08-01", "2017-09-01")
  
  # Select a continent, extract percent change
  HML = tr_continent %>% 
    filter(continent == continent_name) %>% 
    pull(!! variable)
  #what does pull(!! variable doing?)
  # Build a dataframe contraining two columns: 1 with the time strings, 1 with the % change
  dat <- data.frame(Time=as.POSIXct(Time), HML)[13:35, ]
  #create an object called dat which is a data frame containing the values of Time with the HML containing rows 13 to 35 for all columns)
  
  # Take every third month as an axis tick
  tick_set = dat$Time[seq(1, length(dat$Time), 3)]
  ?substr
  #labels=substr 
  ?str
  # Plot the data (xaxt="n" removes the x axis, axis function adds a new x axis tick set )
  plot(HML ~ Time, type="l", col="black", 
       lwd=1, data=dat, xaxt="n",
       main = main_title,
       ylab = y_axis_label)
  axis(side = 1, 
       at=tick_set, 
       labels=substr(as.character(tick_set), 1, 7), #assigns a substring of character that goes from date 1 to 7
       cex.axis = 0.8)#the number of tick marks
  abline(v = dat$Time[13], col="red", lty=2)#lty assigns different type of line types
  abline(v = dat$Time[16], col="red", lty=1)
  abline(v = dat$Time[20], col="red", lty=3)
  abline(h = 0, col = "grey") #puts in a horizontal line
  text(x = dat$Time[13], y = y_elected, labels = "Trump Elected", pos = 2, cex = 0.7)
  text(x = dat$Time[16], y = y_ban, labels = "Travel Ban", pos = 4, cex = 0.7)
  text(x = dat$Time[20], y = y_court, labels = "Court Approves", pos = 4, cex = 0.7)
}

```

#how do I smooth the lines in the graph below?
#one person suggested using Loess or make a running average but I think the running average will reduce hte variation

```{r}

plot_continent("All_Arrivals", "percent.change", 
               "Figure 1: Annual % Change in Entries, All Arrivals",
               "% change",
               -0.10, -0.10, -0.05)


plot_continent("Europe", "percent.change", 
               "Figure 4: Annual % Change in Entries, Europe",
               "% change",
               0.11, 0.11, 0.05)

plot_continent("Latin America (Excluding Mexico)", "percent.change", 
               "Figure 5: Annual % Change in Entries, Latin America",
               "% change",
               0.05, 0.05, 0.05)

plot_continent("Asia", "percent.change", 
               "Annual % Change in Entries, Asia",
               "% change",
               0.08, 0.05, 0.05)

plot_continent("Middle East", "percent.change", 
               "Figure 2: Annual % Change in Entries, Middle East",
               "% change",
               -0.05, -0.05, 0.05)

plot_continent("Africa", "percent.change", 
               "Figure 3: Annual % Change in Entries, Africa",
               "% change",
               0.10, 0.10, 0.05)

plot_continent("Oceania", "percent.change", 
               "Annual % Change in Entries, Oceania",
               "% change",
               -0.05, -0.05, -0.05)


Data_for_Time_Money_Spent <- read_excel("Documents/Dissertation_/Data_for_Time_Money_Spent.xlsx")
Data_for_Time_Money_Spent$time_spent<-as.numeric(Data_for_Time_Money_Spent$time_spent)
mean(na.omit(Data_for_Time_Money_Spent$time_spent))
sd(na.omit(Data_for_Time_Money_Spent$time_spent))
min(na.omit(Data_for_Time_Money_Spent$time_spent))
max(na.omit(Data_for_Time_Money_Spent$time_spent))
Data_for_Time_Money_Spent$money_spent<-as.numeric(Data_for_Time_Money_Spent$money_spent)
mean(na.omit(Data_for_Time_Money_Spent$money_spent))
sd(na.omit(Data_for_Time_Money_Spent$money_spent))
min(na.omit(Data_for_Time_Money_Spent$money_spent))
max(na.omit(Data_for_Time_Money_Spent$money_spent))
hist(na.omit(Data_for_Time_Money_Spent$time_spent))
(na.omit(Data_for_Time_Money_Spent$money_spent))
################################################
#######Standard DID model (OLS regression)
#for continent level data
########
################################################
####did1#####

results <- data.frame()

for (c in continents) {
  continent_data <- filter(data, Source == c)
  
  if (nrow(continent_data) > 0) {
    # Fit the Poisson regression model with clustered standard errors
    model_continent <- lm(lcount_citizens ~ did1 + treat1 + post1, data = continent_data)
    cluster_var <- interaction(continent_data$time, continent_data$country)
    vcov_cluster <- vcovCL(model_continent, cluster = cluster_var)
    result_continent <- coeftest(model_continent, vcov. = vcov_cluster)
    
    result_tidy <- tidy(result_continent)
    did1_coef <- result_tidy$estimate[result_tidy$term == "did1"]
    did1_se <- result_tidy$std.error[result_tidy$term == "did1"]
    did1_pval <- result_tidy$p.value[result_tidy$term == "did1"]
    
    # Create a data frame with the country name and coefficient values
    result <- data.frame(Continent = c, Coefficient = did1_coef, SE = did1_se, PValue = did1_pval)
    
    # Append the result to the overall results data frame
    results <- rbind(results, result)
  }
}

library(ggplot2)

plot <- ggplot(results, aes(x = Continent, y = Coefficient, color = Continent)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Coefficient - 1.96 * SE, ymax = Coefficient + 1.96 * SE), width = 0.2) +
  labs(x = "Continent", y = "Treatment Effect") +
  ggtitle("Impact of Trump's Election on the Count of Cross-Border Entries by Continents") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Adjust x-axis text angle for better visibility
print(plot)

####did2#####
results_2 <- data.frame()

for (c in continents) {
  continent_data <- filter(data, Source == c)
  
  if (nrow(continent_data) > 0) {
    # Fit the Poisson regression model with clustered standard errors
    model_continent <- lm(lcount_citizens ~ did2 + treat2 + post2, data = continent_data)
    cluster_var <- interaction(continent_data$time, continent_data$country)
    vcov_cluster <- vcovCL(model_continent, cluster = cluster_var)
    result_continent <- coeftest(model_continent, vcov. = vcov_cluster)
    
    result_tidy <- tidy(result_continent)
    did2_coef <- result_tidy$estimate[result_tidy$term == "did2"]
    did2_se <- result_tidy$std.error[result_tidy$term == "did2"]
    did2_pval <- result_tidy$p.value[result_tidy$term == "did2"]
    
    # Create a data frame with the country name and coefficient values
    result_2 <- data.frame(Continent = c, Coefficient = did2_coef, SE = did2_se, PValue = did2_pval)
    
    # Append the result to the overall results data frame
    results_2 <- rbind(results_2, result_2)
  }
}

library(ggplot2)

plot <- ggplot(results_2, aes(x = Continent, y = Coefficient, color = Continent)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Coefficient - 1.96 * SE, ymax = Coefficient + 1.96 * SE), width = 0.2) +
  labs(x = "Continent", y = "Treatment Effect") +
  ggtitle("Figure 12: Impact of Travel Ban on Cross-Border Entries by Continents") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Adjust x-axis text angle for better visibility
print(plot)

####did3#####
results_3 <- data.frame()

for (c in continents) {
  continent_data <- filter(data, Source == c)
  
  if (nrow(continent_data) > 0) {
    # Fit the Poisson regression model with clustered standard errors
    model_continent <- lm(lcount_citizens ~ did3 + treat3 + post3, data = continent_data)
    cluster_var <- interaction(continent_data$time, continent_data$country)
    vcov_cluster <- vcovCL(model_continent, cluster = cluster_var)
    result_continent <- coeftest(model_continent, vcov. = vcov_cluster)
    
    result_tidy <- tidy(result_continent)
    did3_coef <- result_tidy$estimate[result_tidy$term == "did3"]
    did3_se <- result_tidy$std.error[result_tidy$term == "did3"]
    did3_pval <- result_tidy$p.value[result_tidy$term == "did3"]
    
    # Create a data frame with the country name and coefficient values
    result_3 <- data.frame(Continent = c, Coefficient = did3_coef, SE = did3_se, PValue = did3_pval)
    
    # Append the result to the overall results data frame
    results_3 <- rbind(results_3, result_3)
  }
}

library(ggplot2)

plot <- ggplot(results_3, aes(x = Continent, y = Coefficient, color = Continent)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Coefficient - 1.96 * SE, ymax = Coefficient + 1.96 * SE), width = 0.2) +
  labs(x = "Continent", y = "Treatment Effect") +
  ggtitle("Figure 14: Impact of Court Approval on Cross-Border Entries by Continents") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Adjust x-axis text angle for better visibility
print(plot)

all_results <- bind_rows(results, results_2, results_3)
# Export the merged results to an Excel file
write.xlsx(all_results, file = "continent_Standard_ols.xlsx", sheetName = "Results")

##########################################################
########DID model (OLS regression)#########
########################################################
##did1
results <- data.frame()

for (c in continents) {
  continent_data <- filter(data, Source == c)
  
  if (nrow(continent_data) > 0) {
    continent_time_dummies <- model.matrix(~ as.factor(continent_data$time) - 1)
    # Fit the Poisson regression model with clustered standard errors
    model_continent <- lm(lcount_citizens ~ did1 + treat1 + as.factor(month), data = continent_data)
    vcov_cluster <- vcovCL(model_continent, cluster =~time)
    result_continent <- coeftest(model_continent, vcov. = vcov_cluster)
    
    result_tidy <- tidy(result_continent)
    did1_coef <- result_tidy$estimate[result_tidy$term == "did1"]
    did1_se <- result_tidy$std.error[result_tidy$term == "did1"]
    did1_pval <- result_tidy$p.value[result_tidy$term == "did1"]
    
    # Create a data frame with the country name and coefficient values
    result <- data.frame(Continent = c, Coefficient = did1_coef, SE = did1_se, PValue = did1_pval)
    
    # Append the result to the overall results data frame
    results <- rbind(results, result)
  }
}

library(ggplot2)

plot <- ggplot(results, aes(x = Continent, y = Coefficient, color = Continent)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Coefficient - 1.96 * SE, ymax = Coefficient + 1.96 * SE), width = 0.2) +
  labs(x = "Continent", y = "Treatment Effect") +
  ggtitle("Figure 10: Impact of Trump's Election on Cross-Border Entries by Continents") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Adjust x-axis text angle for better visibility
print(plot)


#trial of interacting 
results <- data.frame()

for (c in continents) {
  continent_data <- filter(data, Source == c)
  
  if (nrow(continent_data) > 0) {
    continent_time_dummies <- model.matrix(~ as.factor(continent_data$time) - 1)
    # Fit the Poisson regression model with clustered standard errors
    model_continent <- lm(lcount_citizens ~ did1 + treat1 + did1*as.factor(month), data = continent_data)
    vcov_cluster <- vcovCL(model_continent, cluster =~time)
    result_continent <- coeftest(model_continent, vcov. = vcov_cluster)
    
    result_tidy <- tidy(result_continent)
    did1_coef <- result_tidy$estimate[result_tidy$term == "did1"]
    did1_se <- result_tidy$std.error[result_tidy$term == "did1"]
    did1_pval <- result_tidy$p.value[result_tidy$term == "did1"]
    
    # Create a data frame with the country name and coefficient values
    result <- data.frame(Continent = c, Coefficient = did1_coef, SE = did1_se, PValue = did1_pval)
    
    # Append the result to the overall results data frame
    results <- rbind(results, result)
  }
}

library(ggplot2)
plot <- ggplot(results, aes(x = Continent, y = Coefficient, color = Continent)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Coefficient - 1.96 * SE, ymax = Coefficient + 1.96 * SE), width = 0.2) +
  labs(x = "Continent", y = "Treatment Effect") +
  ggtitle("Figure 10a: Impact of Trump's Election on Cross-Border Entries by Continents") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Adjust x-axis text angle for better visibility
print(plot)





#did2
results_2 <- data.frame()

for (c in continents) {
  continent_data <- filter(data, Source == c)
  
  if (nrow(continent_data) > 0) {
    continent_time_dummies <- model.matrix(~ as.factor(continent_data$time) - 1)
    # Fit the Poisson regression model with clustered standard errors
    model_continent <- lm(lcount_citizens ~ did2 + treat2 + continent_time_dummies, data = continent_data)
    vcov_cluster <- vcovCL(model_continent, cluster =~time)
    result_continent <- coeftest(model_continent, vcov. = vcov_cluster)
    
    result_tidy <- tidy(result_continent)
    did2_coef <- result_tidy$estimate[result_tidy$term == "did2"]
    did2_se <- result_tidy$std.error[result_tidy$term == "did2"]
    did2_pval <- result_tidy$p.value[result_tidy$term == "did2"]
    
    # Create a data frame with the country name and coefficient values
    result_2 <- data.frame(Continent = c, Coefficient = did2_coef, SE = did2_se, PValue = did2_pval)
    
    # Append the result to the overall results data frame
    results_2 <- rbind(results_2, result_2)
  }
}

library(ggplot2)

plot <- ggplot(results_2, aes(x = Continent, y = Coefficient, color = Continent)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Coefficient - 1.96 * SE, ymax = Coefficient + 1.96 * SE), width = 0.2) +
  labs(x = "Continent", y = "Treatment Effect") +
  ggtitle("Impact of Travel Ban on the Count of Cross-Border Entries by Continents") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Adjust x-axis text angle for better visibility
print(plot)

#did3
results_3 <- data.frame()

for (c in continents) {
  continent_data <- filter(data, Source == c)
  
  if (nrow(continent_data) > 0) {
    continent_time_dummies <- model.matrix(~ as.factor(continent_data$time) - 1)
    # Fit the Poisson regression model with clustered standard errors
    model_continent <- lm(lcount_citizens ~ did3 + treat3 + continent_time_dummies, data = continent_data)
    vcov_cluster <- vcovCL(model_continent, cluster =~time)
    result_continent <- coeftest(model_continent, vcov. = vcov_cluster)
    
    result_tidy <- tidy(result_continent)
    did3_coef <- result_tidy$estimate[result_tidy$term == "did3"]
    did3_se <- result_tidy$std.error[result_tidy$term == "did3"]
    did3_pval <- result_tidy$p.value[result_tidy$term == "did3"]
    
    # Create a data frame with the country name and coefficient values
    result_3 <- data.frame(Continent = c, Coefficient = did3_coef, SE = did3_se, PValue = did3_pval)
    
    # Append the result to the overall results data frame
    results_3 <- rbind(results_3, result_3)
  }
}

library(ggplot2)

plot <- ggplot(results_3, aes(x = Continent, y = Coefficient, color = Continent)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Coefficient - 1.96 * SE, ymax = Coefficient + 1.96 * SE), width = 0.2) +
  labs(x = "Continent", y = "Treatment Effect") +
  ggtitle("Impact of Court Approval on the Count of Cross-Border Entries by Continents") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Adjust x-axis text angle for better visibility
print(plot)

all_results <- bind_rows(results, results_2, results_3)
library(openxlsx)
# Export the merged results to an Excel file
write.xlsx(all_results, file = "continent_2WFE_Ols.xlsx", sheetName = "Results")




############################################
###Standard DID model (OLS regression)#######
#for country level data
#############################################
#do as before but with ols regression
model_ols1 <- lm(lcount_citizens ~ did1 + treat1 + post1+as.factor(month),
                 data = data)
#interaction computes a factor which represents the interaction of the given factors. The result of interaction is always unordered.
cluster_var <- interaction(data$time, data$country)
#Estimation of one-way and multi-way clustered covariance matrices using an object-oriented approach.
vcov_cluster <- vcovCL(model_ols1, cluster = cluster_var)
resulta <-coeftest(model_ols1, vcov. = vcov_cluster)
resulta_df <- tidy(resulta)

model_ols2 <- lm(lcount_citizens ~ did2 + treat2 + post2+as.factor(month),
                 data = data)
cluster_var <- interaction(data$time, data$country)
vcov_cluster <- vcovCL(model_ols2, cluster = cluster_var)
resultb <-coeftest(model_ols2, vcov. = vcov_cluster)
resultb_df <- tidy(resultb)

model_ols3 <- lm(lcount_citizens ~ did3 + treat3 + post3+as.factor(month),
                 data = data)
cluster_var <- interaction(data$time, data$country)
vcov_cluster <- vcovCL(model_ols3, cluster = cluster_var)
resultc <-coeftest(model_ols3, vcov. = vcov_cluster)
resultc_df <- tidy(resultc)

#check for heterogeneous effect across visa requirement
model_visa_1_1 <- lm(lcount_citizens ~ did1 + treat1 + post1+as.factor(month),
                     data = subset(data, Visa == 1))
# Create the cluster variable for visa = 1
cluster_var_1_1 <- interaction(subset(data, Visa == 1)$time, subset(data, Visa == 1)$country)
# Compute clustered standard errors
vcov_cluster_1_1 <- vcovCL(model_visa_1_1, cluster = cluster_var_1_1)
resultd <- coeftest(model_visa_1_1, vcov. = vcov_cluster_1_1)
resultd_df <- tidy(resultd)

model_visa_0_1 <- lm(lcount_citizens ~ did1 + treat1 + post1+as.factor(month),
                     data = subset(data, Visa == 0))
cluster_var_0_1 <- interaction(subset(data, Visa == 0)$time, subset(data, Visa == 0)$country)
vcov_cluster_0_1 <- vcovCL(model_visa_0_1, cluster = cluster_var_0_1)
resulte <- coeftest(model_visa_0_1, vcov. = vcov_cluster_0_1)
resulte_df <- tidy(resulte)


#For did2
model_visa_1_2 <- lm(lcount_citizens ~ did2 + treat2 + post2+as.factor(month),
                     data = subset(data, Visa == 1))
cluster_var_1_2 <- interaction(subset(data, Visa == 1)$time, subset(data, Visa == 1)$country)
vcov_cluster_1_2 <- vcovCL(model_visa_1_2, cluster = cluster_var_1_2)
resultf <- coeftest(model_visa_1_2, vcov. = vcov_cluster_1_2)
resultf_df <- tidy(resultf)

model_visa_0_2 <- lm(lcount_citizens ~ did2 + treat2 + post2+as.factor(month),
                     data = subset(data, Visa == 0))
cluster_var_0_2 <- interaction(subset(data, Visa == 0)$time, subset(data, Visa == 0)$country)
vcov_cluster_0_2 <- vcovCL(model_visa_0_2, cluster = cluster_var_0_2)
resultg <- coeftest(model_visa_0_2, vcov. = vcov_cluster_0_2)
resultg_df <- tidy(resultg)
#For did3
model_visa_1_3 <- lm(lcount_citizens ~ did3 + treat3 + post3+as.factor(month),
                     data = subset(data, Visa == 1))
cluster_var_1_3 <- interaction(subset(data, Visa == 1)$time, subset(data, Visa == 1)$country)
vcov_cluster_1_3 <- vcovCL(model_visa_1_3, cluster = cluster_var_1_3)
resulth <- coeftest(model_visa_1_3, vcov. = vcov_cluster_1_3)
resulth_df <- tidy(resulth)

model_visa_0_3 <- lm(lcount_citizens ~ did3 + treat3 + post3+as.factor(month),,
                     data = subset(data, Visa == 0))
cluster_var_0_3 <- interaction(subset(data, Visa == 0)$time, subset(data, Visa == 0)$country)
vcov_cluster_0_3 <- vcovCL(model_visa_0_3, cluster = cluster_var_0_3)
resulti <- coeftest(model_visa_0_3, vcov. = vcov_cluster_0_3)
resulti_df <- tidy(resulti)

OLS_combined_results <- data.frame()
OLS_combined_results <- rbind(OLS_combined_results, resulta_df)
OLS_combined_results <- rbind(OLS_combined_results, resultb_df)
OLS_combined_results <- rbind(OLS_combined_results, resultc_df)
OLS_combined_results <- rbind(OLS_combined_results, resultd_df)
OLS_combined_results <- rbind(OLS_combined_results, resulte_df)
OLS_combined_results <- rbind(OLS_combined_results, resultf_df)
OLS_combined_results <- rbind(OLS_combined_results, resultg_df)
OLS_combined_results <- rbind(OLS_combined_results, resulth_df)
OLS_combined_results <- rbind(OLS_combined_results, resulti_df)

write.xlsx(OLS_combined_results, file = "OLS_combined_results.xlsx", sheetName = "Results")

data$Country<-data$Source
results <- data.frame()

for (c in countries) {
  # Filter the data for the current country
  country_data <- filter(data, Source == c)
  
  if (nrow(country_data) > 0) {
    # Fit the OLS regression model with clustered standard errors
    model_country <- lm(lcount_citizens ~ did1 + treat1 + post1+as.factor(month), data = country_data)
    cluster_var <- interaction(country_data$time, country_data$countries)
    vcov_cluster <- vcovCL(model_country, cluster = cluster_var)
    result_country <- coeftest(model_country, vcov. = vcov_cluster)
    
    result_tidy <- tidy(result_country)
    did1_coef <- result_tidy$estimate[result_tidy$term == "did1"]
    did1_se <- result_tidy$std.error[result_tidy$term == "did1"]
    did1_pval <- result_tidy$p.value[result_tidy$term == "did1"]
    
    # Create a data frame with the country name and coefficient values
    result <- data.frame(Country = c, Coefficient = did1_coef, SE = did1_se, PValue = did1_pval)
    
    # Append the result to the overall results data frame
    results <- rbind(results, result)
  }
}

library(ggplot2)

plot <- ggplot(results, aes(x = Country, y = Coefficient, color = Country)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Coefficient - 1.96 * SE, ymax = Coefficient + 1.96 * SE), width = 0.2) +
  labs(x = "Country", y = "Treatment Effect") +
  ggtitle("Figure 11: Impact of Trump Election on Entries of Foreign Nationals Into the US") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Adjust x-axis text angle for better visibility

# Display the plot
print(plot)

results_2 <- data.frame()
for (c in countries) {
  # Filter the data for the current country
  country_data <- filter(data, Source == c)
  
  if (nrow(country_data) > 0) {
    # Fit the Poisson regression model with clustered standard errors
    model_country <- glm(count_citizens ~ did2 + treat2 + post2, data = country_data, family = quasipoisson)
    cluster_var <- interaction(country_data$time, country_data$country)
    vcov_cluster <- vcovCL(model_country, cluster = cluster_var)
    result_country <- coeftest(model_country, vcov. = vcov_cluster)
    
    result_tidy <- tidy(result_country)
    did2_coef <- result_tidy$estimate[result_tidy$term == "did2"]
    did2_se <- result_tidy$std.error[result_tidy$term == "did2"]
    did2_pval <- result_tidy$p.value[result_tidy$term == "did2"]
    
    # Create a data frame with the country name and coefficient values
    result_2 <- data.frame(Country = c, Coefficient = did2_coef, SE = did2_se, PValue = did2_pval)
    
    # Append the result to the overall results data frame
    results_2 <- rbind(results_2, result_2)
  }
}

plot <- ggplot(results_2, aes(x = Country, y = Coefficient, color = Country)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Coefficient - 1.96 * SE, ymax = Coefficient + 1.96 * SE), width = 0.2) +
  labs(x = "Country", y = "Treatment Effect") +
  ggtitle("Figure 13: Impact of Ban Proposal on Entries of Foreign Nationals Into the US") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Adjust x-axis text angle for better visibility

# Display the plot
print(plot)

results_3 <- data.frame()
for (c in countries) {
  # Filter the data for the current country
  country_data <- filter(data, Source == c)
  
  if (nrow(country_data) > 0) {
    # Fit the Poisson regression model with clustered standard errors
    model_country <- glm(count_citizens ~ did3 + treat3 + post3, data = country_data, family = quasipoisson)
    cluster_var <- interaction(country_data$time, country_data$country)
    vcov_cluster <- vcovCL(model_country, cluster = cluster_var)
    result_country <- coeftest(model_country, vcov. = vcov_cluster)
    
    result_tidy <- tidy(result_country)
    did3_coef <- result_tidy$estimate[result_tidy$term == "did3"]
    did3_se <- result_tidy$std.error[result_tidy$term == "did3"]
    did3_pval <- result_tidy$p.value[result_tidy$term == "did3"]
    
    # Create a data frame with the country name and coefficient values
    result_3 <- data.frame(Country = c, Coefficient = did3_coef, SE = did3_se, PValue = did3_pval)
    
    # Append the result to the overall results data frame
    results_3 <- rbind(results_3, result_3)
  }
}

plot <- ggplot(results_3, aes(x = Country, y = Coefficient, color = Country)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Coefficient - 1.96 * SE, ymax = Coefficient + 1.96 * SE), width = 0.2) +
  labs(x = "Country", y = "Treatment Effect") +
  ggtitle("Figure 15: Impact of Court Ban on Entries of Foreign Nationals Into the US") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Adjust x-axis text angle for better visibility
print(plot)


#merge the results for did1 did2 did3 
all_results <- bind_rows(results, results_2, results_3)

# Export the merged results to an Excel file
write.xlsx(all_results, file = "country_standard_OLS.xlsx", sheetName = "Results")




#placebo analysis:




#creating a new vector for the first permutation: 

# i create a vector with the election indicator 
vector = tr_country$election.treat
vector
#now create a column in tr_country called p1 where you store the vector
tr_country$p1<-vector

#25-35 are always 1
#permutation 1: 
# so, I am going to change all of the ones that are 25, into 0
# And, after this, I am going to change all of the ones that are 26 to 1 so the election happened in December. 
tr_country$p1[tr_country$month_total==25]=0
tr_country$p1[tr_country$month_total==26] =1
#creates the vector for the second permutation (p2) by replicating the last 
tr_country$p2<-tr_country$p1
#replaces 26 for 0
#I am going to change all of the ones that are 26 to 1 so the election happened in January 2017 
tr_country$p2[tr_country$month_total==27] =0

#creates the vector for the third permutation (p3)  
tr_country$p3<-tr_country$p2
#replaces 26 for 0 
tr_country$p3[tr_country$month_total==28] =0

#creates the vector for the 4th permutation (p4)
tr_country$p4<-tr_country$p3
#replaces 26 for 0 
tr_country$p4[tr_country$month_total==29] =0

#creates the vector for the 5th permutation (p5)
tr_country$p5<-tr_country$p4
#replaces 26 for 0 
tr_country$p5[tr_country$month_total==30] =0

#creates the vector for the 6th permutation (p6)
tr_country$p6<-tr_country$p5
#replaces 26 for 0 
tr_country$p6[tr_country$month_total==31] =0

#creates the vector for the 7th permutation (p7)
tr_country$p7<-tr_country$p6
#replaces 26 for 0 
tr_country$p7[tr_country$month_total==31] =0

#creates the vector for the 8th permutation (p8)
tr_country$p8<-tr_country$p7
#replaces 26 for 0 
tr_country$p8[tr_country$month_total==32] =0


#creates the vector for the 8th permutation (p8)
tr_country$p8<-tr_country$p7
#replaces 26 for 0 
tr_country$p8[tr_country$month_total==33] =0


#Now, I am running regressions with these permutated variables.  We can techincally even have more permutations 

#(we can have another set of permutations that 'begin the treatment in the period before')
#the issues is that we do not have enough dta for previous months (for the months FE)
# but if we had the data this is what we would do. 
# so this is on average, what it would look like if we moved the tx. 

#regressions with the outcome...: 
#election regular no perm: (non sig when on counts or log?)
#1 is percent change #2 is logcounts and #3 is counts. 
# note only the % change is sig. 
Election1 =lm(log(count_residents)~as.factor(election.treat)+as.factor(month), data=tr_country)
summary(Election1)
Election2 =lm(log(count_residents)~as.factor(election.treat)+as.factor(month), data=tr_country)
summary(Election2)
Election3 =lm(log(count_residents)~as.factor(election.treat)+as.factor(month), data=tr_country)
summary(Election3)

# for starters permutation, i will use % change as the outcome: 
#now we create regression results from the placebo treatments of election happening in december 2017, january 2018, february 2018, march 2018 and so on
election_p1=lm(log(count_residents)~as.factor(p1)+as.factor(month), data=tr_country)
summary(election_p1)

#and continue

election_p2=lm(logcount~as.factor(p2)+as.factor(month), data=tr_country)
summary(election_p2)
election_p3=lm(logcount~as.factor(p3)+as.factor(month), data=tr_country)
summary(election_p3)
election_p4=lm(logcount~as.factor(p4)+as.factor(month), data=tr_country)
summary(election_p4)
election_p5=lm(logcount~as.factor(p5)+as.factor(month), data=tr_country)
summary(election_p5)
election_p6=lm(logcount~as.factor(p6)+as.factor(month), data=tr_country)
summary(election_p6)
election_p7=lm(logcount~as.factor(p7)+as.factor(month), data=tr_country)
summary(election_p7)
election_p8=lm(logcount~as.factor(p8)+as.factor(month), data=tr_country)
summary(election_p8)






tr_country$p1<-as.factor(tr_country$p1)
tr_country$p2<-as.factor(tr_country$p2)
tr_country$p3<-as.factor(tr_country$p3)
tr_country$p4<-as.factor(tr_country$p4)
tr_country$p5<-as.factor(tr_country$p5)
tr_country$p6<-as.factor(tr_country$p6)
tr_country$p7<-as.factor(tr_country$p6)

# for starters permutation, i will use count_residents  as the outcome: 
election_p1=lm(log(count_residents)~p1+as.factor(month), data=tr_country)
summary(election_p1)
election_p2=lm(log(count_residents)~p2+as.factor(month), data=tr_country)
summary(election_p2)
election_p3=lm(log(count_residents)~p3+as.factor(month), data=tr_country)
summary(election_p3)
election_p4=lm(log(count_residents)~p4+as.factor(month), data=tr_country)
summary(election_p4)
election_p5=lm(log(count_residents)~p5+as.factor(month), data=tr_country)
summary(election_p5)
election_p6=lm(log(count_residents)~p6+as.factor(month), data=tr_country)
summary(election_p6)
election_p7=lm(log(count_residents)~p7+as.factor(month), data=tr_country)
summary(election_p7)


#in any case, below, I graph the different 9 starting times for the election with the outomce of % change

#crreate the model frames for the graphs: 
mp1 <- data.frame(Variable = rownames(summary(election_p1)$coef),
                  change = summary(election_p1)$coef[2,1 ],
                  SE = summary(election_p1)$coef[2,2],
                  model_name = "election in Dec 2016")

mp1=mp1[2,]

mp2 <- data.frame(Variable = rownames(summary(election_p2)$coef),
                  change = summary(election_p2)$coef[2,1 ],
                  SE = summary(election_p2)$coef[2,2],
                  model_name = "election in Jan 2017")

mp2=mp2[2,] 
mp3 <- data.frame(Variable = rownames(summary(election_p3)$coef),
                  change = summary(election_p3)$coef[2,1 ],
                  SE = summary(election_p3)$coef[2,2],
                  model_name = "election in Feb 2017")

mp3=mp3[2,] 

mp4 <- data.frame(Variable = rownames(summary(election_p4)$coef),
                  change = summary(election_p4)$coef[2,1 ],
                  SE = summary(election_p4)$coef[2,2],
                  model_name = "election in March 2017")

mp4=mp4[2,]

mp5 <- data.frame(Variable = rownames(summary(election_p5)$coef),
                  change = summary(election_p5)$coef[2,1 ],
                  SE = summary(election_p5)$coef[2,2],
                  model_name = "election in April 2017")

mp5=mp5[2,]
mp6 <- data.frame(Variable = rownames(summary(election_p6)$coef),
                  change = summary(election_p6)$coef[2,1 ],
                  SE = summary(election_p6)$coef[2,2],
                  model_name = "election in May 2017")

mp6=mp6[2,] 
mp7<- data.frame(Variable = rownames(summary(election_p7)$coef),
                 change = summary(election_p7)$coef[2,1 ],
                 SE = summary(election_p7)$coef[2,2],
                 model_name = "election in June 2017")
mp7=mp7[2,]



#so the graph dta frames are done: 
# Combine theSE_rc data.Frame_rcs
permutation_model_election<- data.frame(rbind(mp1, mp2, mp3, mp4, mp5, mp6, mp7)) # etc.

# Specify the width of your confidence intervals
interval1 <- -qnorm((1-0.9)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier

# Plot
b <- ggplot(permutation_model_election , aes(colour = model_name))
b <- b + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)
b <- b + geom_linerange(aes(x = Variable, ymin = change - SE*interval1,
                            ymax = change + SE *interval1),
                        lwd = 1, position = position_dodge(width = 1/2))
b <- b + geom_pointrange(aes(x = Variable , y = change , ymin = change  - SE *interval2,
                             ymax = change  + SE *interval2),
                         lwd = 1/2, position = position_dodge(width = 1/2),
                         shape = 21, fill = "WHITE")
b <- b + coord_flip() + theme_bw()
b <- b + ggtitle("Figure A9: Association between change in visitors & election happening later")
print(b)  # The trick to theSE_rc is position_dodge().


#Now, I will do the same thing for the ban: 
#step 1: change the month of the ban: 
# i create a vector with the election indicator 
vector1b = tr_country$ban.treat
vector1b
tr_country$p1b<-vector1b

#25-35 are always 1
#permutation 1: 
# so, I am going to change all of the ones that are 28, into 0
tr_country$p1b[tr_country$month_total==28] =0

#creates the vector fr the second permutation (p2)
tr_country$p2b<-tr_country$p1b
#replaces 26 for 0 
tr_country$p2b[tr_country$month_total==29] =0

#creates the vector for the third permutation (p3)  
tr_country$p3b<-tr_country$p2b
#replaces 26 for 0 
tr_country$p3b[tr_country$month_total==30] =0

#creates the vector for the 4th permutation (p4)
tr_country$p4b<-tr_country$p3b
#replaces 26 for 0 
tr_country$p4b[tr_country$month_total==31] =0

#creates the vector for the 5th permutation (p5)
tr_country$p5b<-tr_country$p4b
#replaces 26 for 0 
tr_country$p5b[tr_country$month_total==32] =0

#creates the vector for the 6th permutation (p6)
tr_country$p6b<-tr_country$p5b
#replaces 26 for 0 
tr_country$p6b[tr_country$month_total==33] =0

#creates the vector for the 7th permutation (p7)
tr_country$p7b<-tr_country$p6b
#replaces 26 for 0 
tr_country$p7b[tr_country$month_total==34] =0

#creates the vector for the 8th permutation (p8)
tr_country$p8b<-tr_country$p7b
#replaces 26 for 0 
tr_country$p8b[tr_country$month_total==35] =0

###########

#Now, I am running regressions  with these new vectors of different starting points of the ban.
#I convert each month variablre into a factor variable ahead of time so it does not show
#up as factor in my graph
tr_country$p1b<-as.factor(tr_country$p1b)
tr_country$p2b<-as.factor(tr_country$p2b)
tr_country$p3b<-as.factor(tr_country$p3b)
tr_country$p4b<-as.factor(tr_country$p4b)
tr_country$p5b<-as.factor(tr_country$p5b)
tr_country$p6b<-as.factor(tr_country$p6b)
tr_country$p7b<-as.factor(tr_country$p6b)
# for starters permutation, i will use % change as the outcome: 
ban_p1=lm(log(count_residents)~p1b+as.factor(month), data=tr_country)
summary(ban_p1)
ban_p2=lm(log(count_residents)~p2b+as.factor(month), data=tr_country)
summary(ban_p2)
ban_p3=lm(log(count_residents)~p3b+as.factor(month), data=tr_country)
summary(ban_p3)
ban_p4=lm(log(count_residents)~p4b+as.factor(month), data=tr_country)
summary(ban_p4)
ban_p5=lm(log(count_residents)~p5b+as.factor(month), data=tr_country)
summary(ban_p5)
ban_p6=lm(log(count_residents)~p6b+as.factor(month), data=tr_country)
summary(ban_p6)
ban_p7=lm(log(count_residents)~p7b+as.factor(month), data=tr_country)
summary(ban_p7)

#BAN: #crreate the model frames for the graphs: 
bp1 <- data.frame(Variable = rownames(summary(ban_p1)$coef),
                  change = summary(ban_p1)$coef[2,1 ],
                  SE_b = summary(ban_p1)$coef[2,2],
                  model_name = "March 2017")

bp1=bp1[2,]

bp2 <- data.frame(Variable = rownames(summary(ban_p2)$coef),
                  change = summary(ban_p2)$coef[2,1 ],
                  SE_b = summary(ban_p2)$coef[2,2],
                  model_name = "April 2017")

bp2=bp2[2,] 
bp3 <- data.frame(Variable = rownames(summary(ban_p3)$coef),
                  change = summary(ban_p3)$coef[2,1 ],
                  SE_b = summary(ban_p3)$coef[2,2],
                  model_name = "May 2017")

bp3=bp3[2,] 

bp4 <- data.frame(Variable = rownames(summary(ban_p4)$coef),
                  change = summary(ban_p4)$coef[2,1 ],
                  SE_b = summary(ban_p4)$coef[2,2],
                  model_name = "June 2017")

bp4=bp4[2,]

bp5 <- data.frame(Variable = rownames(summary(ban_p5)$coef),
                  change = summary(ban_p5)$coef[2,1 ],
                  SE_b = summary(ban_p5)$coef[2,2],
                  model_name = "July 2017")

bp5=bp5[2,]
bp6 <- data.frame(Variable = rownames(summary(ban_p6)$coef),
                  change = summary(ban_p6)$coef[2,1 ],
                  SE_b = summary(ban_p6)$coef[2,2],
                  model_name = "Aug 2017")

bp6=bp6[2,] 

#so the graph dta frames are done: 
# Combine theSE_b_rc data.Frame_rcs
permutation_model<- data.frame(rbind(bp1, bp2, bp3, bp4, bp5, bp6)) # etc.

# Specify the width of your confidence intervals
interval1 <- -qnorm((1-0.9)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier

# Plot
b <- ggplot(permutation_model , aes(colour = model_name))
b <- b + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)
b <- b + geom_linerange(aes(x = Variable, ymin = change - SE_b*interval1,
                            ymax = change + SE_b *interval1),
                        lwd = 1, position = position_dodge(width = 1/2))
b <- b + geom_pointrange(aes(x = Variable , y = change , ymin = change  - SE_b *interval2,
                             ymax = change  + SE_b *interval2),
                         lwd = 1/2, position = position_dodge(width = 1/2),
                         shape = 21, fill = "WHITE")
b <- b + coord_flip() + theme_bw()
b <- b + ggtitle("Figure A10: Association between change in visitors & ban happening later")
print(b)  # The trick to theSE_b_rc is position_dodge().
#how can I remove the annoying labels. I tried to insert this into theme but it did not work?):
#     (panel.grid = element_blank(),
# axis.title = element_blank(),
# axis.text.y = element_blank())
#how do you change_in_travelers the name of the permutation model


