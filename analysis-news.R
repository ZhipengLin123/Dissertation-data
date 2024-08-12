#一 导入
install.packages("readxl")
install.packages("wordcloud2")
install.packages("factoextra")
"factoextra"
# 加载readxl包
library(readxl)

# 读取Excel文件并保存为dat
dat <- read_excel("/Users/chrislin/Desktop/Dissertation/data/news（08-07）/total.xlsx")

# 查看数据的前几行以确认读取成功
head(dat)

#二 统计画图

#（1）Dublin相关新闻数量波动情况，
# 载入必要的包
library(dplyr)
library(ggplot2)

# 假设你的pubdate2列是以字符形式保存的日期，我们先将其转换为日期格式
dat$pubdate2 <- as.Date(dat$pubdate, format="%b %d, %Y") # 请根据实际日期格式调整

# 提取年份和季度信息，并进行分组统计
dat_summary_quarter <- dat %>%
  mutate(YearQuarter = paste(format(pubdate2, "%Y"), "Q", quarters(pubdate2), sep="")) %>%
  group_by(YearQuarter) %>%
  summarise(Count = n())

# 绘制按季度分组的折线图，并设置x轴显示部分标签
ggplot(dat_summary_quarter, aes(x = YearQuarter, y = Count, group = 1)) +
  geom_line(color = "blue") +
  labs(title = "Quarterly Publication Count",
       x = "Quarter",
       y = "Count of Publications") +
  theme_minimal() +
  scale_x_discrete(breaks = dat_summary_quarter$YearQuarter[seq(1, nrow(dat_summary_quarter), by = 2)]) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#（2）关注该话题的新闻类型：opinion/news+时间（折线图，饼图）


# 加载必要的包
library(dplyr)
library(ggplot2)

# 创建一个新的列，将类型进行整合
dat <- dat %>%
  mutate(documentType_simplified = case_when(
    grepl("Editorial|Commentary|Opinions", documentType, ignore.case = TRUE) ~ "Editorial/Opinions",
    grepl("News|NEWSPAPER", documentType, ignore.case = TRUE) ~ "News",
    TRUE ~ "Other"
  ))

# 按年份季度和简化后的documentType进行分组统计
dat_summary_simplified <- dat %>%
  mutate(YearQuarter = paste(format(pubdate2, "%Y"), "Q", quarters(pubdate2), sep="")) %>%
  group_by(YearQuarter, documentType_simplified) %>%
  summarise(Count = n()) %>%
  ungroup()

# 绘制整合后的折线图
ggplot(dat_summary_simplified, aes(x = YearQuarter, y = Count, color = documentType_simplified, group = documentType_simplified)) +
  geom_line(size = 1) +
  labs(title = "Quarterly Publication Count by Simplified Document Type",
       x = "Quarter",
       y = "Count of Publications",
       color = "Document Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks = dat_summary_simplified$YearQuarter[seq(1, nrow(dat_summary_simplified), by = 2)])
#饼图
# 加载必要的包
library(dplyr)
library(ggplot2)

# 创建一个新的列，将类型进行整合
dat <- dat %>%
  mutate(documentType_simplified = case_when(
    grepl("Editorial|Commentary|Opinions", documentType, ignore.case = TRUE) ~ "Editorial/Opinions",
    grepl("News|NEWSPAPER", documentType, ignore.case = TRUE) ~ "News",
    TRUE ~ "Other"
  ))

# 计算不同documentType_simplified的总数和占比
dat_pie_simplified <- dat %>%
  group_by(documentType_simplified) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# 绘制简化后的饼图
ggplot(dat_pie_simplified, aes(x = "", y = Percentage, fill = documentType_simplified)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Distribution of Simplified Document Types",
       fill = "Document Type") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()) +
  geom_text(aes(label = ifelse(Percentage > 5, paste0(round(Percentage, 1), "%"), "")), 
            position = position_stack(vjust = 0.5))


#（3）最关注该话题的作者+时间 
#3.1（饼图——》折线图）
# 加载必要的包
library(dplyr)
library(tidyr)

# 清理Authors栏，拆分多个作者并统计
dat_cleaned <- dat %>%
  filter(!is.na(Authors)) %>%
  mutate(Authors = strsplit(Authors, ",")) %>%  # 拆分多个作者，假设使用逗号分隔
  unnest(Authors) %>%
  mutate(Authors = trimws(Authors))  # 去掉前后的空格

# 统计每个作者的出现次数，并按次数从高到低排序
authors_summary <- dat_cleaned %>%
  group_by(Authors) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

# 显示结果表格
authors_summary

#3.2 top9浮点图
# 加载必要的包
library(dplyr)
library(ggplot2)
library(tidyr)
library(zoo)  # 载入zoo包以使用as.yearmon函数

# 清理Authors栏，拆分多个作者并统计
dat_cleaned <- dat %>%
  filter(!is.na(Authors)) %>%
  mutate(Authors = strsplit(Authors, ",")) %>%
  unnest(Authors) %>%
  mutate(Authors = trimws(Authors))

# 统计每个作者的出现次数，并按次数从高到低排序
authors_summary <- dat_cleaned %>%
  group_by(Authors) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

# 提取前10名（除去Anonymous）
top_authors <- head(authors_summary %>% filter(Authors != "Anonymous"), 10)$Authors

# 解析pubdate2列为年份月份格式，并提取年份
yearly_data <- dat_cleaned %>%
  filter(Authors %in% top_authors) %>%
  mutate(Year = format(as.yearmon(pubdate2, "%Y-%m"), "%Y")) %>%
  group_by(Year, Authors) %>%
  summarise(Count = n()) %>%
  ungroup()

# 绘制散点图并连接相同颜色的点
ggplot(yearly_data, aes(x = Year, y = Count, color = Authors, group = Authors)) +
  geom_point(size = 3) +  # 使用geom_point绘制散点图
  geom_line(linewidth = 1) +  # 使用geom_line连接相同颜色的点
  labs(title = "Publication Trends of Top 9 Authors (Excluding Anonymous)",
       x = "Year",
       y = "Number of Publications",
       color = "Authors") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#3.3 top9 作者话题统计
# 加载必要的包
library(dplyr)
library(tidyr)

# 清理Authors栏，拆分多个作者并统计
dat_cleaned <- dat %>%
  filter(!is.na(Authors)) %>%
  mutate(Authors = strsplit(Authors, ",")) %>%
  unnest(Authors) %>%
  mutate(Authors = trimws(Authors))

# 统计每个作者的出现次数，并按次数从高到低排序
authors_summary <- dat_cleaned %>%
  group_by(Authors) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

# 提取前9名（除去Anonymous）
top_authors <- head(authors_summary %>% filter(Authors != "Anonymous"), 10)$Authors

# 清理subjectTerms栏，拆分多个主题
subject_terms_cleaned <- dat_cleaned %>%
  filter(Authors %in% top_authors) %>%
  filter(!is.na(subjectTerms)) %>%
  mutate(subjectTerms = strsplit(subjectTerms, ",")) %>%
  unnest(subjectTerms) %>%
  mutate(subjectTerms = trimws(subjectTerms))

# 统计前9名作者关注的主题
author_subject_summary <- subject_terms_cleaned %>%
  group_by(Authors, subjectTerms) %>%
  summarise(Count = n()) %>%
  arrange(Authors, desc(Count))

# 查看前9名作者分别关注的前几个主题
top_author_subjects <- author_subject_summary %>%
  group_by(Authors) %>%
  top_n(5, wt = Count)  # 这里提取前5个主题，你可以调整这个数字

# 显示结果
print(top_author_subjects)


##############
#（4）头版数量
#############

# 加载必要的包
library(dplyr)
library(ggplot2)
library(zoo)

# 假设pubdate列的日期格式为 "Jun 19, 2021"
# 将pubdate列转换为日期格式，并提取季度信息
dat <- dat %>%
  mutate(pubdate2 = as.Date(pubdate, format="%b %d, %Y")) %>%  # 转换为日期格式
  mutate(Quarter = as.yearqtr(pubdate2))  # 提取季度信息



# 统计每个季度中startPage为1的次数
quarterly_count <- dat %>%
  filter(!is.na(startPage)) %>%
  group_by(Quarter) %>%
  summarise(Count = sum(startPage == 1, na.rm = TRUE))

# 如果某个季度没有出现1，计为0
all_quarters <- data.frame(Quarter = seq(min(quarterly_count$Quarter), max(quarterly_count$Quarter), by = 0.25))
quarterly_count <- full_join(all_quarters, quarterly_count, by = "Quarter") %>%
  mutate(Count = ifelse(is.na(Count), 0, Count))


# 假设你已经有了quarterly_count数据框
# 首先确保Quarter列是因子，并且按照时间顺序排列
quarterly_count <- quarterly_count %>%
  mutate(Quarter = factor(Quarter, levels = unique(Quarter)))

# 绘制浮点图并修改颜色
ggplot(quarterly_count, aes(x = Quarter, y = Count)) +
  geom_point(size = 3, color = "steelblue") +  # 修改颜色为steelblue
  labs(title = "Occurrences of startPage = 1 by Quarter",
       x = "Quarter",
       y = "Number of Occurrences") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks = quarterly_count$Quarter[seq(1, nrow(quarterly_count), by = 4)]) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), # 标题居中且加粗
        axis.title.x = element_text(face = "bold"),            # x轴标题加粗
        axis.title.y = element_text(face = "bold"),            # y轴标题加粗
        panel.grid.major = element_line(color = "lightgray"),  # 设置主网格线颜色
        panel.grid.minor = element_blank(),                    # 去除次网格线
        panel.background = element_rect(fill = "white"))       # 背景为白色



##############
#（5）文本分析（词云）
#############、
# 加载必要的包
library(dplyr)
library(tidyr)
library(tm)            # 用于文本挖掘
library(wordcloud2)    # 用于绘制词云

# 将subjectTerms列拆分成单个词语，并去除多余的空格
terms <- dat %>%
  filter(!is.na(subjectTerms)) %>%
  mutate(subjectTerms = strsplit(subjectTerms, ",")) %>%
  unnest(subjectTerms) %>%
  mutate(subjectTerms = trimws(subjectTerms))  # 去除前后的空格

# 排除无关关键词
irrelevant_words <- c("Ireland", "prices","Housing","Housing prices","Prices","ECONOMICS","Houses")
terms_filtered <- terms %>%
  filter(!subjectTerms %in% irrelevant_words)

# 创建词频表
term_freq <- terms_filtered %>%
  count(subjectTerms, sort = TRUE)

# 查看高频次词语（前10个）
top_terms <- head(term_freq, 10)
print(top_terms)

# 绘制词云
wordcloud2(term_freq, size = 1, color = "random-light", backgroundColor = "white")

#分时间段词云 2020前，2020-2021，2021后
# 加载必要的包
# 加载必要的包
library(dplyr)
library(tidyr)
library(tm)
library(wordcloud2)
library(lubridate)  # 用于处理日期
library(grid)       # 用于添加标题
library(ggplot2)    # 用于添加标题

# 假设pubdate2是日期格式，如果不是，需要先转换
dat <- dat %>%
  mutate(pubdate2 = as.Date(pubdate, format="%b %d, %Y"))

# 去除无关词汇的列表
irrelevant_words <- c("Ireland", "prices", "Housing", "Housing prices", "Prices", "ECONOMICS", "Houses")

# 1. 分割数据 - 2020年之前
dat_before_2020 <- dat %>%
  filter(pubdate2 < as.Date("2020-01-01"))

# 2. 分割数据 - 2020-2021年
dat_2020_2021 <- dat %>%
  filter(pubdate2 >= as.Date("2020-01-01") & pubdate2 <= as.Date("2021-12-31"))

# 3. 分割数据 - 2021年之后
dat_after_2021 <- dat %>%
  filter(pubdate2 > as.Date("2021-12-31"))

# 定义一个函数来清理数据并生成词频表
generate_term_freq <- function(data, output_file) {
  terms <- data %>%
    filter(!is.na(subjectTerms)) %>%
    mutate(subjectTerms = strsplit(subjectTerms, ",")) %>%
    unnest(subjectTerms) %>%
    mutate(subjectTerms = trimws(subjectTerms)) %>%
    filter(!subjectTerms %in% irrelevant_words) %>%
    count(subjectTerms, sort = TRUE)
  
  # 保存高频词表到CSV文件
  write.csv(terms, file = output_file, row.names = FALSE)
  
  return(terms)
}

# 生成词频表并保存为CSV文件
term_freq_before_2020 <- generate_term_freq(dat_before_2020, "term_freq_before_2020.csv")
term_freq_2020_2021 <- generate_term_freq(dat_2020_2021, "term_freq_2020_2021.csv")
term_freq_after_2021 <- generate_term_freq(dat_after_2021, "term_freq_after_2021.csv")


# 生成词云
wordcloud2(term_freq_before_2020, size = 1, color = "random-light", backgroundColor = "white")
wordcloud2(term_freq_2020_2021, size = 1, color = "random-light", backgroundColor = "white")
wordcloud2(term_freq_after_2021, size = 1, color = "random-light", backgroundColor = "white")



##############
#（6）月份数量表格
#############、
# 加载必要的包
library(dplyr)
library(lubridate)

# 假设pubdate列是日期格式，如果不是，需要先转换
dat <- dat %>%
  mutate(pubdate2 = as.Date(pubdate, format="%b %d, %Y"))

# 提取月份信息，并按月份统计记录数量
monthly_count <- dat %>%
  mutate(Month = format(pubdate2, "%Y-%m")) %>%  # 提取年-月信息
  group_by(Month) %>%
  summarise(Count = n()) %>%
  arrange(Month)  # 按月份排序

# 显示结果表格
print(monthly_count)
# 将结果表格保存为CSV文件
write.csv(monthly_count, "monthly_count.csv", row.names = TRUE)

getwd()
