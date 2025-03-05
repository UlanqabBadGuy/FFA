# This is a sample R script for assignment 1.

# 安装并加载所需的包
install.packages(c("mice", "VIM", "naniar", "DescTools"))
install.packages("ggplot2")
install.packages("reshape2")
install.packages("RColorBrewer")
install.packages("corrplot")
install.packages("caret")
install.packages("pheatmap")

library(corrplot)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(caret)
library(pheatmap)
# 加载包
library(mice)    # 多重插补
library(VIM)     # 缺失值可视化
library(naniar)  # 缺失值分析
library(DescTools) # Winsorize处理
library(dplyr)   # 数据操作


# Load the data
data = read.csv("A1_data.csv")

# 2.探索数据集结构
# print("structure of dataset")
# str(data)
# print("summary statistics")
# summary(data)
print("proportion of isFraud's class")
table(data$isFraud)/length(data$isFraud)


# 3.单变量分析
# 绘制isFraud的分布情况（柱状图）
ggplot(data, aes(x = as.factor(isFraud))) +
  geom_bar(width = 0.3, fill = "cyan", color = "cyan") +  # 使用颜色填充和边框
  theme_minimal() +                              # 极简主题
  labs(title = "Distribution of isFraud", x = "Category", y = "Count") +
  theme(plot.title = element_text(hjust = 0.5, size = 16),  # 标题居中，设置字体大小
        axis.text.x = element_text(size = 12),     # x轴字体大小
        axis.text.y = element_text(size = 12)) +   # y轴字体大小
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5)  # 在柱子上显示数量







# 检查整体缺失情况
cat("Total missing values:", sum(is.na(data)), "\n")

# 每列的缺失值数量
missing_counts <- colSums(is.na(data))
print(missing_counts)

# 查看缺失值比例
# 计算每列缺失值的数量

# 计算每列缺失值的比例
missing_percentage <- missing_counts / nrow(data) * 100

# 将缺失值比例按降序排序
sorted_missing_percentage <- sort(missing_percentage, decreasing = TRUE)

# 查看每列缺失值的比例
sorted_missing_percentage


# 设置阈值（例如，删除缺失值比例超过30%的列）
threshold <- 42.881

# 获取缺失值比例小于或等于阈值的列
columns_to_keep <- names(missing_percentage[missing_percentage < threshold])

# 创建一个新的数据集，删除缺失值比例大于阈值的列
data_after_removeal <- data[columns_to_keep]

# 使用data_after_removeal继续操作
new_missing_counts <- colSums(is.na(data_after_removeal))
print(new_missing_counts)
new_sorted_missing_col <- sort(new_missing_counts, decreasing = TRUE)
new_sorted_missing_col

# 我们需要处理的列是真正有缺失值的列
missing_cols <- names(new_missing_counts[new_missing_counts > 0])
data_need_to_be_porcessed <- data_after_removeal[, missing_cols]

sort(colSums(is.na(data_need_to_be_porcessed))/ nrow(data_need_to_be_porcessed) * 100, decreasing = TRUE)
threshold_5 <- colSums(is.na(data_need_to_be_porcessed))/ nrow(data_need_to_be_porcessed) * 100

# 检验缺失值比例小于5%的列的缺失情况是否为完全随机缺失（MCAR）
# 如果是的，则直接删除对应行数据,如果缺失值的比例很低（通常小于5%），
# 删除缺失值对数据集的影响可能较小，可作为一种简单且快速的方法。
# 如果不是另做考虑
temp_data <- data_after_removeal[,names(threshold_5[threshold_5 < 5])]

aggr_plot <- aggr(temp_data, col = c('navyblue', 'red'), 
                  numbers = TRUE, sortVars = TRUE, 
                  labels = names(temp_data), cex.axis = .8, 
                  gap = 1, ylab = c("Missing data", "Pattern"))


# 我们发现除了V310,V311和V312，其他的数据基本符合完全随机缺失（MCAR），但由于其缺失值比例很小，因此同样采取直接删除的操作。
# 因此在后面的操作中，我们需要过滤掉除了这三列之外的有缺失值的记录
# 那么对于（id_13  id_05  id_06）前面比例大于5%的列，我们需要用mice进行多重插补
# 需要知道id_13  id_05  id_06与其他属性的相关性

temp_data_1 <- temp_data[!is.na(temp_data$V310) & !is.na(temp_data$V311) & !is.na(temp_data$V312),]
# 此时temp_data_1代表去掉了V310,V311和V312中有缺失值的行
# 类似的，我们要对removal的数据集进行处理了
# 第一步，删除了V310,V311和V312中有缺失值的行
step1 <- data_after_removeal[!is.na(data_after_removeal$V310) & !is.na(data_after_removeal$V311) & !is.na(data_after_removeal$V312),]

# C5 C9全是0，没什么作用，如果一列属性的值几乎全是0，那么这个特征的变异性几乎为零，
# 实际上它对分析和模型的影响可能非常有限，甚至可能引入噪音。因此，移除这类特征通常是合理的，
step1 <- step1 %>%
  select(-C5, -C9)

# 第二步，mice插值

########################## 对数值型变量的研究
numeric_data <- select_if(step1, is.numeric)  # 提取出数值型变量

# 将数值型变量的数据转化为长格式
melted_numeric_data <- melt(numeric_data)  # 生成的列统称value

ggplot(melted_numeric_data, aes(x = value)) +
  geom_histogram(fill = "cyan", color = "cyan", bins = 30) +
  facet_wrap(~ variable, scales = "free") +  # 每个变量一个图，坐标轴独立
  theme_minimal() +
  labs(title = "Distribution of Numeric Variables", 
       x = "Values", 
       y = "Frequency") +
  theme(
    plot.title = element_text(hjust = 0.1),  # 调整标题位置
    axis.text.x = element_text(size = 8),    # 调整横坐标文本大小
    axis.text.y = element_text(size = 8)     # 调整纵坐标文本大小
  )



##### 检测数值型变量间的相关系数
# 计算数值型变量的相关性
cor_matrix_numeric <- cor(numeric_data, use = "complete.obs")
# 将相关性矩阵转化为长格式
corr1 <- melt(cor_matrix_numeric) # 按两两匹配方式排出缺失值的影响
# corrplot(corr1)
# title(main = "Correlation Coefficient of Numeric Variables", line = -3, adj = 0.55)  # 给热力图添加一个标题
# 绘制热图
ggplot(corr1, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.text.y = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Coefficient of Numeric Variables", x = "Variables", y = "Variables")


########################## 对分类型变量的研究
category_data <- step1 %>% select_if(~ !is.numeric(.)) # 提取出所有类别型变量

# 将分类型变量的数据转化为长格式
melted_category_data <- melt(category_data, id.vars = NULL, na.rm = FALSE)  # 不指定 id.vars，将所有列视为度量变量，同时忽略缺失值


# 绘制所有分类变量的分布条形图，使用 facet_wrap() 将它们整合在一张图中
ggplot(melted_category_data, aes(x = value)) +
  geom_bar(fill = "cyan", color = "black") +
  facet_wrap(~ variable, scales = "free", ncol = 5) +  # 每个变量一个图，坐标轴独立
  theme_minimal() +
  labs(title = "Distribution of Discrete Variables", x = "Category", y = "Count") +
  theme(plot.title = element_text(hjust = 0.1),
        axis.text.x = element_text(size = 7),   # 调整横坐标文本大小
        axis.text.y = element_text(size = 7) 
  )   # 调整纵坐标文本大小





# 选择数值型变量
# numeric_data <- step1[sapply(step1, is.numeric)]
# 检测分类变量（唯一值少于阈值，如10）
# categorical_data <- data %>%
#   select_if(~ n_distinct(.) <= 100 & is.character(.) | is.factor(.))

# 计算数值型变量的相关性
# cor_matrix <- cor(numeric_data, use = "complete.obs")
# 将相关性矩阵转化为长格式
# cor_melted <- melt(cor_matrix)

# 绘制热图
# ggplot(cor_melted, aes(Var1, Var2, fill = value)) +
#   geom_tile() +
#   scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.text.y = element_text(angle = 45, hjust = 1)) +
#   labs(title = "Correlation Heatmap", x = "Variables", y = "Variables")

# numeric_cols <- names(numeric_data)
# categorical_cols <- names(categorical_data)

# step1$categorical_cols <- as.factor(step1$categorical_cols)
# step1$numeric_cols <- as.numeric(step1$numeric_cols)

############################################# 数值型变量分析
# 检查每一列的类型
sapply(numeric_data, class)
# 检查数值型变量的缺失值情况
sort(colSums(is.na(numeric_data)),decreasing = TRUE)
# 检查无穷值（Inf）和缺失值（NA）
sum(is.na(numeric_data))  # 统计缺失值的数量

sum(is.infinite(unlist(numeric_data)))  # 统计无穷值的数量

# 将数值变量的无穷值转为NA
step1 <- step1 %>%
  mutate(across(where(is.numeric), ~ replace(., is.infinite(.), NA)))

# 将字符型变量的空字符串转为NA
# step1 <- step1 %>%
#   mutate(across(where(is.character), ~ na_if(., "")))


# 数值型变量的缺失值列包含 
# id_13                id_05                id_06 
# id_20                id_19                id_17 
# id_02                id_11                card2 
# card5                   D1                 V313 
# V314                card3        TransactionID 


# id系列的缺失值我们发现很多都是一起全部缺失的

# 运行 MICE 插补（默认方法为 "pmm"：预测均值匹配）
# 对id进行mice插补
selected_columns_idstep1 <- step1[, c("id_13", "id_05", "id_06", "id_20")]
# 转换成因子形式，以顺利进行mice
selected_columns_idstep1$id_13 <- factor(selected_columns_idstep1$id_13)
selected_columns_idstep1$id_05 <- factor(selected_columns_idstep1$id_05)
selected_columns_idstep1$id_06 <- factor(selected_columns_idstep1$id_06)
selected_columns_idstep1$id_20 <- factor(selected_columns_idstep1$id_20)
#selected_columns_idstep1$id_19 <- factor(selected_columns_idstep1$id_19)
#selected_columns_idstep1$id_17 <- factor(selected_columns_idstep1$id_17)
#selected_columns_idstep1$id_02 <- factor(selected_columns_idstep1$id_02)
#selected_columns_idstep1$id_11 <- factor(selected_columns_idstep1$id_11)
# 使用 MICE 进行插补
# 这里使用 m = 5 表示生成5个插补数据集，根据不同数据类型选用不同的method参数
mice_idstep1 <- mice(selected_columns_idstep1, m = 5, method = c('pmm', 'pmm', 'pmm', 'pmm'), seed = 123)
# 查看插补结果的摘要
summary(mice_idstep1)
# 提取插补后的完整数据集,并插回原数据集
completed_idstep1 <- complete(mice_idstep1)
step1[, c("id_13", "id_05", "id_06", "id_20")] <- completed_idstep1

# 再从因子型变量转换为字符型变量
A1_data2$id_13 <- as.integer(step1$id_13)
A1_data2$id_05 <- as.integer(step1$id_05)
A1_data2$id_06 <- as.integer(step1$id_06)
A1_data2$id_20 <- as.integer(step1$id_20)
#A1_data2$id_19 <- as.integer(step1$id_19)
#A1_data2$id_17 <- as.integer(step1$id_17)
#A1_data2$id_02 <- as.integer(step1$id_02)
#A1_data2$id_11 <- as.numeric(step1$id_11)

##########################################################



##############################################分类型变量
# 检查每一列的类型
sapply(category_data, class)
# 检查分类型变量的缺失值情况
sort(colSums(is.na(category_data)),decreasing = TRUE)
# 检查无穷值（Inf）和缺失值（NA）
sum(is.na(category_data))  # 统计缺失值的数量

sum(is.infinite(unlist(category_data)))  # 统计无穷值的数量

# 将字符型变量的空字符串转为NA
step1 <- step1 %>%
  mutate(across(where(is.character), ~ na_if(., "")))

# 分类型变量的缺失值列包含 
# id_35         id_36         id_37         id_38

##########################################################

##############################################进行一个相关性分析，来决定谁与谁来插值





# Exploratory Data Analysis

# 1. Distinguish Attributes
str(data)
summary(data)
...


# 2. Univariate Analysis
ggplot(...)
hist(...)
...


# 3. Bi-/Multi-variate Analysis
ggplot(data, aes(...)) + geom_bar()
corrplot(...)
......


