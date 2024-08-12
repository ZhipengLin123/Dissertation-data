
# 加载readxl包
library(readxl)
library(dplyr)

# 读取Excel文件并保存为dat
dat <- read_excel("/Users/chrislin/Desktop/Dissertation/data/data analysis/pub-market.xlsx")

# 查看数据的前几行以确认读取成功
head(dat)

#回归分析
# 1. Median Price (Euro) as the independent variable
model_median_price <- lm(`pub-Count` ~ `Median Price(EURO)`, data = dat)
summary(model_median_price)

# 2. Mean Sale Price (EURO) as the independent variable
model_mean_sale_price <- lm(`pub-Count` ~ `Mean Sale Price(EURO)`, data = dat)
summary(model_mean_sale_price)

# 3. Value of Sales (Euro Million) as the independent variable
model_value_sales <- lm(`pub-Count` ~ `Value of Sales(Euro Million)`, data = dat)
summary(model_value_sales)

# 4. Volume of Sales as the independent variable
model_volume_sales <- lm(`pub-Count` ~ `Volume of Sales`, data = dat)
summary(model_volume_sales)

# 5. Volume of Sales &  Mean Sale Price (EURO) as the independent variable

model_volume_sales_and_Mean_Sale_Price <- lm(`pub-Count` ~ `Volume of Sales`+`Median Price(EURO)`, data = dat)
summary(model_volume_sales_and_Mean_Sale_Price)




# 读取Excel文件并保存为dat
dat2 <- read_excel("/Users/chrislin/Desktop/Dissertation/data/data analysis/pub-market (3months later).xlsx")


#三个月后数据回归分析
# 1. Median Price (Euro) as the independent variable
model_median_price2 <- lm(`Median Price(EURO)` ~ `pub-Count`, data = dat2)
summary(model_median_price2)

# 2. Mean Sale Price (EURO) as the independent variable
model_mean_sale_price2 <- lm(`Mean Sale Price(EURO)` ~ `pub-Count` , data = dat2)
summary(model_mean_sale_price2)

# 3. Value of Sales (Euro Million) as the independent variable
model_value_sales2 <- lm(`Value of Sales(Euro Million)` ~ `pub-Count`, data = dat2)
summary(model_value_sales2)

# 4. Volume of Sales as the independent variable
model_volume_sales2 <- lm(`Volume of Sales` ~ `pub-Count`, data = dat2)
summary(model_volume_sales2)










