HW3 Gapminder Exploration Using Package(dplyr)
================

Pre-Work
--------

#### Load packages

Load the necessary package (assuming you have installed them)

``` r
library(dplyr)
library(gapminder)
library(ggplot2)
library(tidyr)
```

1. Get the maximum and minimum of GDP per capita for all continents.
--------------------------------------------------------------------

`{r results='asis', echo=FALSE} tbl <- gapminder %>% group_by(continent) %>% summarize(minGDP = round(min(gdpPercap), digit = 2), maxGDP = round(max(gdpPercap), digit = 2)) knitr::kable(tbl)}`

tbl %&gt;% ggplot(aes(x= continent, y= gdpPercap, col= continent)) + geom\_boxplot() \`\`\`

2. Look at the spread of GDP per capita within the continents.
--------------------------------------------------------------

``` r
gapminder %>%
  group_by(continent) %>%
  summarise(meanGDP = round(mean(gdpPercap), digit = 2),
            sdGDP = round(sqrt(var(gdpPercap)), digit =2)) %>%
  knitr::kable()
```

| continent |   meanGDP|     sdGDP|
|:----------|---------:|---------:|
| Africa    |   2193.75|   2827.93|
| Americas  |   7136.11|   6396.76|
| Asia      |   7902.15|  14045.37|
| Europe    |  14469.48|   9355.21|
| Oceania   |  18621.61|   6358.98|

3. Compute a trimmed mean of life expectancy for different years. Or a weighted mean, weighting by population. Just try something other than the plain vanilla mean.
--------------------------------------------------------------------------------------------------------------------------------------------------------------------

``` r
# 10% Trimmed Mean: Remove the top and bottom 10% (#14) of data
gapminder %>%
  group_by(year) %>%
  mutate(trimed_mean = mean(lifeExp, trim = .1)) %>%
  distinct(year, trimed_mean) %>%
  knitr::kable()
```

|  year|  trimed\_mean|
|-----:|-------------:|
|  1952|      48.57668|
|  1957|      51.26888|
|  1962|      53.58075|
|  1967|      55.86538|
|  1972|      58.01444|
|  1977|      60.10206|
|  1982|      62.11694|
|  1987|      63.92106|
|  1992|      65.18519|
|  1997|      66.01736|
|  2002|      66.71641|
|  2007|      68.11489|

``` r
# Weighted mean
gapminder %>%
  group_by(year) %>%
  mutate(weighted_lifeE = weighted.mean(lifeExp, pop)) %>%
  select(year, weighted_lifeE) %>%
  distinct(year, weighted_lifeE) %>%
  knitr::kable()
```

|  year|  weighted\_lifeE|
|-----:|----------------:|
|  1952|         48.94424|
|  1957|         52.12189|
|  1962|         52.32438|
|  1967|         56.98431|
|  1972|         59.51478|
|  1977|         61.23726|
|  1982|         62.88176|
|  1987|         64.41635|
|  1992|         65.64590|
|  1997|         66.84934|
|  2002|         67.83904|
|  2007|         68.91909|

How is life expectancy changing over time on different continents?
------------------------------------------------------------------

``` r
#BenchMark: median of lifeExp
benchmark <- 
  gapminder 

gapminder %>%
  group_by(continent) 
```

    ## # A tibble: 1,704 x 6
    ## # Groups:   continent [5]
    ##        country continent  year lifeExp      pop gdpPercap
    ##         <fctr>    <fctr> <int>   <dbl>    <int>     <dbl>
    ##  1 Afghanistan      Asia  1952  28.801  8425333  779.4453
    ##  2 Afghanistan      Asia  1957  30.332  9240934  820.8530
    ##  3 Afghanistan      Asia  1962  31.997 10267083  853.1007
    ##  4 Afghanistan      Asia  1967  34.020 11537966  836.1971
    ##  5 Afghanistan      Asia  1972  36.088 13079460  739.9811
    ##  6 Afghanistan      Asia  1977  38.438 14880372  786.1134
    ##  7 Afghanistan      Asia  1982  39.854 12881816  978.0114
    ##  8 Afghanistan      Asia  1987  40.822 13867957  852.3959
    ##  9 Afghanistan      Asia  1992  41.674 16317921  649.3414
    ## 10 Afghanistan      Asia  1997  41.763 22227415  635.3414
    ## # ... with 1,694 more rows

Report the absolute and/or relative abundance of countries with low life expectancy over time by continent: Compute some measure of worldwide life expectancy – you decide – a mean or median or some other quantile or perhaps your current age. Then determine how many countries on each continent have a life expectancy less than this benchmark, for each year.

Find countries with interesting stories. Open-ended and, therefore, hard. Promising but unsuccessful attempts are encouraged. This will generate interesting questions to follow up on in class.

Make up your own! Between the dplyr coverage in class and the list above, I think you get the idea.
