# HKRbook

## Introduction to Statistics

The book **[Introduction to Statistics](https://link.springer.com/book/10.1007/978-3-319-17704-5)** by W. Härdle, S.Klinke and B. Rönz has been published in 2015 by Springer Verlag (paper/pdf/epub).

It covers all the topics found in introductory descriptive statistics courses, including simple linear regression and time series analysis, the fundamentals of inferential statistics (probability theory, random sampling and estimation theory), and inferential statistics itself (confidence intervals, testing).

# Interactive examples from the book

The interactive examples in the book could be reached via web link of the form `https://u.hu-berlin.de/men_xxxx`. In the package
`HKRbook` these links has been replaced by functions `men_xxxx()`.

```
# install the package once from CRAN
# install.packages("HKRbook")
library("HKRbook")
men_asso()  # calls the Shiny app behind "https://u.hu-berlin.de/men_asso"
# Additionally you may use your data sets, for details see ?men_asso
men_asso(Titanic)
```
However, we streamlined some apps since they are more or less duplicates from each other.



| R function    |  Parameters   |  Content                                                    |  Book link (`https://u.hu-berlin.de/`) |
| ------------- | ------------- | ----------------------------------------------------------- | -------------------------------------- |
| men_asso()    |  data set(s)  |  Association of categorical data                            |  `men_asso`, `men_tab2`                |
| men_bin()     |  parameters   |  Binomial distribution                                      |  `men_bin`                             |
| men_ci1()     |  data set(s)  |  Confidence interval for the mean                           |  `men_ci1`                             |
| men_ci2()     |  data set(s)  |  Confidence interval for the difference of two means        |  `men_ci2`                             |
| men_cilen()   |  --           |  Necessary sample size for confidence interval              |  `men_cilen`                           | 
| men_cipi()    |  data set(s)  |  Confidence interval for the proportion                     |  `men_cipi`                            |
| men_cisig()   |  data set(s)  |  Confidence interval for the variance                       |  `men_cisig`                           |
| men_corr()    |  data set(s)  |  Scatterplots and correlation                               |  `men_corr`, `men_plot`                |
| men_die()     |  --           |  Die rolling sisters (Bayes theorem)                        |  `men_die`                             |
| men_dot()     |  data set(s)  |  Dot plot/strip chart                                       |  `men_dot1`, `men_dot2`                |
| men_exp()     |  parameters   |  Exponential distribution                                   |  `men_exp`                             |
| men_hall()    |  --           |  Monty Hall problem                                         |  `men_hall`                            |
| men_hist()    |  data set(s)  |  Histogram                                                  |  `men_hist`                            |
| men_hyp()     |  parameters   |  Hypergeometric distribution                                |  `men_hyp`                             |
| men_norm()    |  parameters   |  Normal distribution                                        |  `men_norm`                            |
| men_parn()    |  parameter    |  Distribution of sample parameters of a numerical variable  |                                        |
| men_poi()     |  parameter    |  Poisson distribution                                       |  `men_poi`                             |
| men_rank()    |  data set(s)  |  Rank correlation of ordered variables                      |  `men_rank`                            |
| men_regr()    |  data set(s)  |  Simple linear regression                                   |  `men_regr`                            |
| men_tab()     |  data set(s)  |  Frequencies table/Crosstable                               |                                        |
| men_terr()    |  data set(s)  |  Test of mean with type I and II error                      |  `men_terr`                            |
| men_time()    |  time series  |  Classical time series analysis                             |  `men_time1`, `men_time2`, `men_time3` |
| men_tmu1()    |  data set(s)  |  Test for mean                                              |  `men_tmu1`                            |
| men_tmu2()    |  data set(s)  |  Test for mean difference                                   |  `men_tmu2`                            |
| men_tprop()   |  data set(s)  |  Binomial test                                              |  `men_tprop`                           |
| men_vis()     |  data set(s)  |  Visualizations of a numeric variable                       |  `men_vis`                             |

