# This is a sample R script for assignment 1.

# 安装必要的包（如果尚未安装）
install.packages(c("mice", "VIM", "naniar", "DescTools"))
# 安装并加载pheatmap
if (!requireNamespace("pheatmap", quietly = TRUE)) {
  install.packages("pheatmap")
}

# 安装并加载所需的包
install.packages("ggplot2")
install.packages("reshape2")
install.packages("RColorBrewer")
install.packages("corrplot")

library(corrplot)
library(ggplot2)
library(reshape2)
library(RColorBrewer)

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
print("structure of dataset")
str(A1_data)
print("summary statistics")
summary(A1_data)
print("proportion of isFraud's class")
table(A1_data$isFraud)/length(A1_data$isFraud)


# 3.单变量分析
# 绘制isFraud的分布情况（柱状图）
ggplot(A1_data, aes(x = as.factor(isFraud))) +
  geom_bar(width = 0.3, fill = "cyan", color = "cyan") +  # 使用颜色填充和边框
  theme_minimal() +                              # 极简主题
  labs(title = "Distribution of isFraud", x = "Category", y = "Count") +
  theme(plot.title = element_text(hjust = 0.5, size = 16),  # 标题居中，设置字体大小
        axis.text.x = element_text(size = 12),     # x轴字体大小
        axis.text.y = element_text(size = 12)) +   # y轴字体大小
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5)  # 在柱子上显示数量


########################## 对数值型变量的研究
numeric_data <- select_if(A1_data, is.numeric)  # 提取出数值型变量

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
    axis.text.x = element_text(size = 6),    # 调整横坐标文本大小
    axis.text.y = element_text(size = 6)     # 调整纵坐标文本大小
  )




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
data_after_removeal <- data[, columns_to_keep]

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
# 假设要移除的列名是 col1 和 col2
step1 <- step1 %>%
  select(-C5, -C9)

# 第二步，mice插值
# 选择数值型变量
numeric_data <- step1[sapply(step1, is.numeric)]
# 检测分类变量（唯一值少于阈值，如10）
categorical_data <- data %>%
  select_if(~ n_distinct(.) <= 100 & is.character(.) | is.factor(.))

# 计算数值型变量的相关性
cor_matrix <- cor(numeric_data, use = "complete.obs")
# 将相关性矩阵转化为长格式
cor_melted <- melt(cor_matrix)

# 绘制热图
ggplot(cor_melted, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.text.y = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Heatmap", x = "Variables", y = "Variables")

numeric_cols <- names(numeric_data)
categorical_cols <- names(categorical_data)

step1$categorical_cols <- as.factor(step1$categorical_cols)
step1$numeric_cols <- as.numeric(step1$numeric_cols)

# 检查每一列的类型
sapply(numeric_data, class)
# 检查无穷值（Inf）和缺失值（NA）
sum(is.na(numeric_data))  # 统计缺失值的数量

sum(is.infinite(numeric_data))  # 统计无穷值的数量

# 将无穷值替换为NA
numeric_data[is.infinite(numeric_data)] <- NA

# 进行PCA
pca <- prcomp(numeric_data, center = TRUE, scale. = TRUE)
pca_data <- pca$x  # 取主成分
# 使用主成分进行插补
step2 <- mice(pca_data, m = 5, maxit = 5, method = 'pmm', seed = 500)

# 根据关系热力图我们可以看到某些数据之间的关系如何
# 缺失值列包含 
# Variable   Count
# id_20 0.03415
# id_19 0.03374
# id_17 0.03337
# id_02 0.02276
# id_11 0.02203
# id_35 0.02196
# id_36 0.02196
# id_37 0.02196
# id_38 0.02196
# card2 0.00688
# card5 0.00660
# D1 0.00155
# V313 0.00155
# V314 0.00155
# card3 0.00115



step2 <- mice(step1, m = 5, method = 'pmm', maxit = 5, seed = 500)


target_vars <- c("id_13", "id_05", "id_06")


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


