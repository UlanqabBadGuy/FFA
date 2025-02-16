# Report for Assignment 1

+++

Author: Jiahui Han 3036381357

+++

## Data Cleaning and Preparation Process

### Data Cleaning

首先，我们需要找到数据集中每一列的缺失值。我们使用==colSums(is.na(data))==确认所有列的缺失值个数，随后计算每一列的缺失值的百分比。根据缺失值占比大小使用==sort==函数进行降序排列。

从Figure 2中我们不难看出，addr2的缺失值占比是42.881%，是一个重要的分界线，因为下一个属性是id_13，它的缺失值占比仅为11.528%。而在属性addr2之前的所有属性，它们的该项均大于42.881%。实际上，当缺失值占比如此之大时，该属性对结果的预测贡献度很小。而且，会增加分析的难度，甚至如果因为填充不合适的值，导致预测结果的错误率增加。因此，我们以42.881%为阈值，移除上述属性，仅保留缺失值较少的属性。经此操作，101列数据中的40列被移除。

其次，剩下的列仍有缺失值存在。我们将针对不同缺失值的属性和特征进行不同的插补值操作。





# Appendix

Figure 1: The number of missing value of each columns.

![image-20250207162917351](C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20250207162917351.png)

Figure 2: The sorted result based on missing value percentage.

![image-20250207163410296](C:\Users\Administrator\AppData\Roaming\Typora\typora-user-images\image-20250207163410296.png)