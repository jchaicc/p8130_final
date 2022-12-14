data
================
2022-12-14

``` r
body_df=read_excel("data/body_density_data.xlsx") %>%
  janitor::clean_names() %>%
  select(-bodyfat_brozek,-body_density)
```

``` r
# all are continuous data

sum = function(variable){
  tibble(
    mean = mean(variable),
    sd = sd(variable),
    median = median(variable),
    maximum = max(variable),
    minimum = min(variable),
    IQR = IQR(variable)
  )
}

map(body_df, sum) %>% 
  bind_rows() %>% 
  mutate(variable = names(body_df)) %>% 
  select(variable,everything()) %>% 
  filter(variable!="id") %>%
  knitr::kable(digits = 2, 
               caption = "Descriptive statistics of continuous variables")
```

| variable     |   mean |    sd | median | maximum | minimum |   IQR |
|:-------------|-------:|------:|-------:|--------:|--------:|------:|
| bodyfat_siri |  19.15 |  8.37 |  19.20 |   47.50 |     0.0 | 12.83 |
| age          |  44.88 | 12.60 |  43.00 |   81.00 |    22.0 | 18.25 |
| weight       | 178.92 | 29.39 | 176.50 |  363.15 |   118.5 | 38.00 |
| height       |  70.31 |  2.61 |  70.00 |   77.75 |    64.0 |  4.00 |
| neck         |  37.99 |  2.43 |  38.00 |   51.20 |    31.1 |  3.02 |
| chest        | 100.82 |  8.43 |  99.65 |  136.20 |    79.3 | 11.02 |
| abdomen      |  92.56 | 10.78 |  90.95 |  148.10 |    69.4 | 14.75 |
| hip          |  99.90 |  7.16 |  99.30 |  147.70 |    85.0 |  8.03 |
| thigh        |  59.41 |  5.25 |  59.00 |   87.30 |    47.2 |  6.35 |
| knee         |  38.59 |  2.41 |  38.50 |   49.10 |    33.0 |  2.95 |
| ankle        |  23.10 |  1.69 |  22.80 |   33.90 |    19.1 |  2.00 |
| bicep        |  32.27 |  3.02 |  32.05 |   45.00 |    24.8 |  4.12 |
| forearm      |  28.66 |  2.02 |  28.70 |   34.90 |    21.0 |  2.70 |
| wrist        |  18.23 |  0.93 |  18.30 |   21.40 |    15.8 |  1.20 |

Descriptive statistics of continuous variables

``` r
# check collinearity
body_df %>% 
select(-id, -bodyfat_siri) %>% 
cor() ## Correlation coefficient
```

    ##                 age      weight     height      neck     chest   abdomen
    ## age      1.00000000 -0.01274609 -0.2452123 0.1135052 0.1764497 0.2304094
    ## weight  -0.01274609  1.00000000  0.4868880 0.8307162 0.8941905 0.8879949
    ## height  -0.24521233  0.48688800  1.0000000 0.3211409 0.2268286 0.1897662
    ## neck     0.11350519  0.83071622  0.3211409 1.0000000 0.7848350 0.7540774
    ## chest    0.17644968  0.89419052  0.2268286 0.7848350 1.0000000 0.9158277
    ## abdomen  0.23040942  0.88799494  0.1897662 0.7540774 0.9158277 1.0000000
    ## hip     -0.05033212  0.94088412  0.3721060 0.7349579 0.8294199 0.8740662
    ## thigh   -0.20009576  0.86869354  0.3385576 0.6956973 0.7298586 0.7666239
    ## knee     0.01751569  0.85316739  0.5005005 0.6724050 0.7194964 0.7371789
    ## ankle   -0.10505810  0.61368542  0.3931315 0.4778924 0.4829879 0.4532227
    ## bicep   -0.04116212  0.80041593  0.3185075 0.7311459 0.7279075 0.6849827
    ## forearm -0.08505555  0.63030143  0.3220273 0.6236603 0.5801727 0.5033161
    ## wrist    0.21353062  0.72977489  0.3977796 0.7448264 0.6601623 0.6198324
    ##                 hip      thigh       knee      ankle       bicep     forearm
    ## age     -0.05033212 -0.2000958 0.01751569 -0.1050581 -0.04116212 -0.08505555
    ## weight   0.94088412  0.8686935 0.85316739  0.6136854  0.80041593  0.63030143
    ## height   0.37210602  0.3385576 0.50050052  0.3931315  0.31850749  0.32202734
    ## neck     0.73495788  0.6956973 0.67240498  0.4778924  0.73114592  0.62366027
    ## chest    0.82941992  0.7298586 0.71949640  0.4829879  0.72790748  0.58017273
    ## abdomen  0.87406618  0.7666239 0.73717888  0.4532227  0.68498272  0.50331609
    ## hip      1.00000000  0.8964098 0.82347262  0.5583868  0.73927252  0.54501412
    ## thigh    0.89640979  1.0000000 0.79917030  0.5397971  0.76147745  0.56684218
    ## knee     0.82347262  0.7991703 1.00000000  0.6116082  0.67870883  0.55589819
    ## ankle    0.55838682  0.5397971 0.61160820  1.0000000  0.48485454  0.41904999
    ## bicep    0.73927252  0.7614774 0.67870883  0.4848545  1.00000000  0.67825513
    ## forearm  0.54501412  0.5668422 0.55589819  0.4190500  0.67825513  1.00000000
    ## wrist    0.63008954  0.5586848 0.66450729  0.5661946  0.63212642  0.58558825
    ##             wrist
    ## age     0.2135306
    ## weight  0.7297749
    ## height  0.3977796
    ## neck    0.7448264
    ## chest   0.6601623
    ## abdomen 0.6198324
    ## hip     0.6300895
    ## thigh   0.5586848
    ## knee    0.6645073
    ## ankle   0.5661946
    ## bicep   0.6321264
    ## forearm 0.5855883
    ## wrist   1.0000000

``` r
corrplot(cor(body_df), type = "upper", diag = FALSE)
```

![](data_files/figure-gfm/correlation-1.png)<!-- -->

``` r
boxplot(body_df$bodyfat_siri,main='Percent body fat using Siri’s equation')
```

![](data_files/figure-gfm/check%20outcome%20variable-1.png)<!-- -->

``` r
body_df%>% select(-id,-bodyfat_siri)%>%
  funModeling::plot_num()
```

    ## Warning: The `<scale>` argument of `guides()` cannot be `FALSE`. Use "none" instead as
    ## of ggplot2 3.3.4.
    ## ℹ The deprecated feature was likely used in the funModeling package.
    ##   Please report the issue at <]8;;https://github.com/pablo14/funModeling/issueshttps://github.com/pablo14/funModeling/issues]8;;>.

![](data_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->
