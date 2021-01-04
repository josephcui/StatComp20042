# Introduction
这是中国科学技术大学研究生课程“统计计算”的大作业，独自完成一个R包(using Rcpp)，其中包含了日常的作业和对机器学习基础算法的简单复现。

# Install
如果要build vignettes时间会很久，因为日常作业部分的代码耗时较长(大概半小时)。
```{r}
# you can set build_vignettes to TRUE to install the full content of my package,
# which will cost about half an hour.
devtools::install_github("josephcui/StatComp20042",build_vignettes = FALSE)
```
# How to write a R package
There is some useful links:
[Book:R Packages](https://r-pkgs.org/index.html),
[Rcpp for everyone](https://teuder.github.io/rcpp4everyone_en/)
