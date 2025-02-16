# This is a sample R script for assignment 1.

# 安装必要的包（如果尚未安装）
install.packages(c("mice", "VIM", "naniar", "DescTools"))
# 安装并加载pheatmap
if (!requireNamespace("pheatmap", quietly = TRUE)) {
  install.packages("pheatmap")
}

library(pheatmap)
# 加载包
library(mice)    # 多重插补
library(VIM)     # 缺失值可视化
library(naniar)  # 缺失值分析
library(DescTools) # Winsorize处理
library(dplyr)   # 数据操作


# Load the data
data = read.csv("A1_data.csv")

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


