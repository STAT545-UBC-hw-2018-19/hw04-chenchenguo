hw-04
================
Guochenchen
October 7th, 2018

Homework 04: Tidy data and joins
================================

Data Reshaping Prompts(And relationship to aggregation)
-------------------------------------------------------

``` r
# Firstly load all libraries
suppressPackageStartupMessages(library(gapminder))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(knitr))
suppressPackageStartupMessages(library(kableExtra))
```

### Make a tibble with one row per year and columns for life expectancy for two or more countries

``` r
data1 <- gapminder %>% 
  filter(country %in% c('China', 'Japan', 'Canada')) %>% 
  select(year, country, lifeExp)

knitr::kable(head(data1, 20)) %>% 
  kable_styling(bootstrap_options = "bordered", latex_options = "basic", full_width = F)
```

<table class="table table-bordered" style="width: auto !important; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:right;">
year
</th>
<th style="text-align:left;">
country
</th>
<th style="text-align:right;">
lifeExp
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
1952
</td>
<td style="text-align:left;">
Canada
</td>
<td style="text-align:right;">
68.75000
</td>
</tr>
<tr>
<td style="text-align:right;">
1957
</td>
<td style="text-align:left;">
Canada
</td>
<td style="text-align:right;">
69.96000
</td>
</tr>
<tr>
<td style="text-align:right;">
1962
</td>
<td style="text-align:left;">
Canada
</td>
<td style="text-align:right;">
71.30000
</td>
</tr>
<tr>
<td style="text-align:right;">
1967
</td>
<td style="text-align:left;">
Canada
</td>
<td style="text-align:right;">
72.13000
</td>
</tr>
<tr>
<td style="text-align:right;">
1972
</td>
<td style="text-align:left;">
Canada
</td>
<td style="text-align:right;">
72.88000
</td>
</tr>
<tr>
<td style="text-align:right;">
1977
</td>
<td style="text-align:left;">
Canada
</td>
<td style="text-align:right;">
74.21000
</td>
</tr>
<tr>
<td style="text-align:right;">
1982
</td>
<td style="text-align:left;">
Canada
</td>
<td style="text-align:right;">
75.76000
</td>
</tr>
<tr>
<td style="text-align:right;">
1987
</td>
<td style="text-align:left;">
Canada
</td>
<td style="text-align:right;">
76.86000
</td>
</tr>
<tr>
<td style="text-align:right;">
1992
</td>
<td style="text-align:left;">
Canada
</td>
<td style="text-align:right;">
77.95000
</td>
</tr>
<tr>
<td style="text-align:right;">
1997
</td>
<td style="text-align:left;">
Canada
</td>
<td style="text-align:right;">
78.61000
</td>
</tr>
<tr>
<td style="text-align:right;">
2002
</td>
<td style="text-align:left;">
Canada
</td>
<td style="text-align:right;">
79.77000
</td>
</tr>
<tr>
<td style="text-align:right;">
2007
</td>
<td style="text-align:left;">
Canada
</td>
<td style="text-align:right;">
80.65300
</td>
</tr>
<tr>
<td style="text-align:right;">
1952
</td>
<td style="text-align:left;">
China
</td>
<td style="text-align:right;">
44.00000
</td>
</tr>
<tr>
<td style="text-align:right;">
1957
</td>
<td style="text-align:left;">
China
</td>
<td style="text-align:right;">
50.54896
</td>
</tr>
<tr>
<td style="text-align:right;">
1962
</td>
<td style="text-align:left;">
China
</td>
<td style="text-align:right;">
44.50136
</td>
</tr>
<tr>
<td style="text-align:right;">
1967
</td>
<td style="text-align:left;">
China
</td>
<td style="text-align:right;">
58.38112
</td>
</tr>
<tr>
<td style="text-align:right;">
1972
</td>
<td style="text-align:left;">
China
</td>
<td style="text-align:right;">
63.11888
</td>
</tr>
<tr>
<td style="text-align:right;">
1977
</td>
<td style="text-align:left;">
China
</td>
<td style="text-align:right;">
63.96736
</td>
</tr>
<tr>
<td style="text-align:right;">
1982
</td>
<td style="text-align:left;">
China
</td>
<td style="text-align:right;">
65.52500
</td>
</tr>
<tr>
<td style="text-align:right;">
1987
</td>
<td style="text-align:left;">
China
</td>
<td style="text-align:right;">
67.27400
</td>
</tr>
</tbody>
</table>
``` r
# To further explore this table
data2 <- spread(data1, key = "country", value = "lifeExp") %>% 
  rename(lifeexp_Canada = Canada, lifeexp_China = China, lifeexp_Japan = Japan)

knitr::kable(data2) %>% 
  kable_styling(bootstrap_options = "bordered", latex_options = "basic", full_width = F)
```

<table class="table table-bordered" style="width: auto !important; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:right;">
year
</th>
<th style="text-align:right;">
lifeexp\_Canada
</th>
<th style="text-align:right;">
lifeexp\_China
</th>
<th style="text-align:right;">
lifeexp\_Japan
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
1952
</td>
<td style="text-align:right;">
68.750
</td>
<td style="text-align:right;">
44.00000
</td>
<td style="text-align:right;">
63.030
</td>
</tr>
<tr>
<td style="text-align:right;">
1957
</td>
<td style="text-align:right;">
69.960
</td>
<td style="text-align:right;">
50.54896
</td>
<td style="text-align:right;">
65.500
</td>
</tr>
<tr>
<td style="text-align:right;">
1962
</td>
<td style="text-align:right;">
71.300
</td>
<td style="text-align:right;">
44.50136
</td>
<td style="text-align:right;">
68.730
</td>
</tr>
<tr>
<td style="text-align:right;">
1967
</td>
<td style="text-align:right;">
72.130
</td>
<td style="text-align:right;">
58.38112
</td>
<td style="text-align:right;">
71.430
</td>
</tr>
<tr>
<td style="text-align:right;">
1972
</td>
<td style="text-align:right;">
72.880
</td>
<td style="text-align:right;">
63.11888
</td>
<td style="text-align:right;">
73.420
</td>
</tr>
<tr>
<td style="text-align:right;">
1977
</td>
<td style="text-align:right;">
74.210
</td>
<td style="text-align:right;">
63.96736
</td>
<td style="text-align:right;">
75.380
</td>
</tr>
<tr>
<td style="text-align:right;">
1982
</td>
<td style="text-align:right;">
75.760
</td>
<td style="text-align:right;">
65.52500
</td>
<td style="text-align:right;">
77.110
</td>
</tr>
<tr>
<td style="text-align:right;">
1987
</td>
<td style="text-align:right;">
76.860
</td>
<td style="text-align:right;">
67.27400
</td>
<td style="text-align:right;">
78.670
</td>
</tr>
<tr>
<td style="text-align:right;">
1992
</td>
<td style="text-align:right;">
77.950
</td>
<td style="text-align:right;">
68.69000
</td>
<td style="text-align:right;">
79.360
</td>
</tr>
<tr>
<td style="text-align:right;">
1997
</td>
<td style="text-align:right;">
78.610
</td>
<td style="text-align:right;">
70.42600
</td>
<td style="text-align:right;">
80.690
</td>
</tr>
<tr>
<td style="text-align:right;">
2002
</td>
<td style="text-align:right;">
79.770
</td>
<td style="text-align:right;">
72.02800
</td>
<td style="text-align:right;">
82.000
</td>
</tr>
<tr>
<td style="text-align:right;">
2007
</td>
<td style="text-align:right;">
80.653
</td>
<td style="text-align:right;">
72.96100
</td>
<td style="text-align:right;">
82.603
</td>
</tr>
</tbody>
</table>
``` r
#To scatter plot this data further indicate the comparison between lifeexp of these three countries
ggplot(data1, aes(year, lifeExp)) +
  geom_point(aes(color = country))+
  scale_x_continuous(limits = c(1952, 2007), breaks = seq(1952, 2007, 5))+
  xlab("Year")+ ylab("Life expectancy") + ggtitle("Life expectancy of Canada, China and Japan")
```

![](hw_04_ChenchenGuo_files/figure-markdown_github/unnamed-chunk-2-1.png)

### Compute life expectancy for all possible combinations of content and year. Reshape that to have one row per year and one variable for each continent.

``` r
# Here to compute the maximum lifeExpectancy for all continents each year
data3 <- gapminder %>% 
  group_by(continent, year) %>% 
  summarise(MaxLifeExp = max(lifeExp))

data4 <- spread(data3, key = "continent", value = "MaxLifeExp")

knitr::kable(data4) %>% 
  kable_styling(bootstrap_options = "bordered", latex_options = "basic", full_width = F)
```

<table class="table table-bordered" style="width: auto !important; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:right;">
year
</th>
<th style="text-align:right;">
Africa
</th>
<th style="text-align:right;">
Americas
</th>
<th style="text-align:right;">
Asia
</th>
<th style="text-align:right;">
Europe
</th>
<th style="text-align:right;">
Oceania
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
1952
</td>
<td style="text-align:right;">
52.724
</td>
<td style="text-align:right;">
68.750
</td>
<td style="text-align:right;">
65.390
</td>
<td style="text-align:right;">
72.670
</td>
<td style="text-align:right;">
69.390
</td>
</tr>
<tr>
<td style="text-align:right;">
1957
</td>
<td style="text-align:right;">
58.089
</td>
<td style="text-align:right;">
69.960
</td>
<td style="text-align:right;">
67.840
</td>
<td style="text-align:right;">
73.470
</td>
<td style="text-align:right;">
70.330
</td>
</tr>
<tr>
<td style="text-align:right;">
1962
</td>
<td style="text-align:right;">
60.246
</td>
<td style="text-align:right;">
71.300
</td>
<td style="text-align:right;">
69.390
</td>
<td style="text-align:right;">
73.680
</td>
<td style="text-align:right;">
71.240
</td>
</tr>
<tr>
<td style="text-align:right;">
1967
</td>
<td style="text-align:right;">
61.557
</td>
<td style="text-align:right;">
72.130
</td>
<td style="text-align:right;">
71.430
</td>
<td style="text-align:right;">
74.160
</td>
<td style="text-align:right;">
71.520
</td>
</tr>
<tr>
<td style="text-align:right;">
1972
</td>
<td style="text-align:right;">
64.274
</td>
<td style="text-align:right;">
72.880
</td>
<td style="text-align:right;">
73.420
</td>
<td style="text-align:right;">
74.720
</td>
<td style="text-align:right;">
71.930
</td>
</tr>
<tr>
<td style="text-align:right;">
1977
</td>
<td style="text-align:right;">
67.064
</td>
<td style="text-align:right;">
74.210
</td>
<td style="text-align:right;">
75.380
</td>
<td style="text-align:right;">
76.110
</td>
<td style="text-align:right;">
73.490
</td>
</tr>
<tr>
<td style="text-align:right;">
1982
</td>
<td style="text-align:right;">
69.885
</td>
<td style="text-align:right;">
75.760
</td>
<td style="text-align:right;">
77.110
</td>
<td style="text-align:right;">
76.990
</td>
<td style="text-align:right;">
74.740
</td>
</tr>
<tr>
<td style="text-align:right;">
1987
</td>
<td style="text-align:right;">
71.913
</td>
<td style="text-align:right;">
76.860
</td>
<td style="text-align:right;">
78.670
</td>
<td style="text-align:right;">
77.410
</td>
<td style="text-align:right;">
76.320
</td>
</tr>
<tr>
<td style="text-align:right;">
1992
</td>
<td style="text-align:right;">
73.615
</td>
<td style="text-align:right;">
77.950
</td>
<td style="text-align:right;">
79.360
</td>
<td style="text-align:right;">
78.770
</td>
<td style="text-align:right;">
77.560
</td>
</tr>
<tr>
<td style="text-align:right;">
1997
</td>
<td style="text-align:right;">
74.772
</td>
<td style="text-align:right;">
78.610
</td>
<td style="text-align:right;">
80.690
</td>
<td style="text-align:right;">
79.390
</td>
<td style="text-align:right;">
78.830
</td>
</tr>
<tr>
<td style="text-align:right;">
2002
</td>
<td style="text-align:right;">
75.744
</td>
<td style="text-align:right;">
79.770
</td>
<td style="text-align:right;">
82.000
</td>
<td style="text-align:right;">
80.620
</td>
<td style="text-align:right;">
80.370
</td>
</tr>
<tr>
<td style="text-align:right;">
2007
</td>
<td style="text-align:right;">
76.442
</td>
<td style="text-align:right;">
80.653
</td>
<td style="text-align:right;">
82.603
</td>
<td style="text-align:right;">
81.757
</td>
<td style="text-align:right;">
81.235
</td>
</tr>
</tbody>
</table>
``` r
# alter the x, y of table 
data5 <- spread(data3, key = "year", value = "MaxLifeExp")

knitr::kable(data5) %>% 
  kable_styling(bootstrap_options = "bordered", latex_options = "basic", full_width = F)
```

<table class="table table-bordered" style="width: auto !important; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
continent
</th>
<th style="text-align:right;">
1952
</th>
<th style="text-align:right;">
1957
</th>
<th style="text-align:right;">
1962
</th>
<th style="text-align:right;">
1967
</th>
<th style="text-align:right;">
1972
</th>
<th style="text-align:right;">
1977
</th>
<th style="text-align:right;">
1982
</th>
<th style="text-align:right;">
1987
</th>
<th style="text-align:right;">
1992
</th>
<th style="text-align:right;">
1997
</th>
<th style="text-align:right;">
2002
</th>
<th style="text-align:right;">
2007
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Africa
</td>
<td style="text-align:right;">
52.724
</td>
<td style="text-align:right;">
58.089
</td>
<td style="text-align:right;">
60.246
</td>
<td style="text-align:right;">
61.557
</td>
<td style="text-align:right;">
64.274
</td>
<td style="text-align:right;">
67.064
</td>
<td style="text-align:right;">
69.885
</td>
<td style="text-align:right;">
71.913
</td>
<td style="text-align:right;">
73.615
</td>
<td style="text-align:right;">
74.772
</td>
<td style="text-align:right;">
75.744
</td>
<td style="text-align:right;">
76.442
</td>
</tr>
<tr>
<td style="text-align:left;">
Americas
</td>
<td style="text-align:right;">
68.750
</td>
<td style="text-align:right;">
69.960
</td>
<td style="text-align:right;">
71.300
</td>
<td style="text-align:right;">
72.130
</td>
<td style="text-align:right;">
72.880
</td>
<td style="text-align:right;">
74.210
</td>
<td style="text-align:right;">
75.760
</td>
<td style="text-align:right;">
76.860
</td>
<td style="text-align:right;">
77.950
</td>
<td style="text-align:right;">
78.610
</td>
<td style="text-align:right;">
79.770
</td>
<td style="text-align:right;">
80.653
</td>
</tr>
<tr>
<td style="text-align:left;">
Asia
</td>
<td style="text-align:right;">
65.390
</td>
<td style="text-align:right;">
67.840
</td>
<td style="text-align:right;">
69.390
</td>
<td style="text-align:right;">
71.430
</td>
<td style="text-align:right;">
73.420
</td>
<td style="text-align:right;">
75.380
</td>
<td style="text-align:right;">
77.110
</td>
<td style="text-align:right;">
78.670
</td>
<td style="text-align:right;">
79.360
</td>
<td style="text-align:right;">
80.690
</td>
<td style="text-align:right;">
82.000
</td>
<td style="text-align:right;">
82.603
</td>
</tr>
<tr>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
72.670
</td>
<td style="text-align:right;">
73.470
</td>
<td style="text-align:right;">
73.680
</td>
<td style="text-align:right;">
74.160
</td>
<td style="text-align:right;">
74.720
</td>
<td style="text-align:right;">
76.110
</td>
<td style="text-align:right;">
76.990
</td>
<td style="text-align:right;">
77.410
</td>
<td style="text-align:right;">
78.770
</td>
<td style="text-align:right;">
79.390
</td>
<td style="text-align:right;">
80.620
</td>
<td style="text-align:right;">
81.757
</td>
</tr>
<tr>
<td style="text-align:left;">
Oceania
</td>
<td style="text-align:right;">
69.390
</td>
<td style="text-align:right;">
70.330
</td>
<td style="text-align:right;">
71.240
</td>
<td style="text-align:right;">
71.520
</td>
<td style="text-align:right;">
71.930
</td>
<td style="text-align:right;">
73.490
</td>
<td style="text-align:right;">
74.740
</td>
<td style="text-align:right;">
76.320
</td>
<td style="text-align:right;">
77.560
</td>
<td style="text-align:right;">
78.830
</td>
<td style="text-align:right;">
80.370
</td>
<td style="text-align:right;">
81.235
</td>
</tr>
</tbody>
</table>
### Reshape the table to have one row per year or per year \* continent combination

###
