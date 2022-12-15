data
================
2022-12-14

``` r
body_df=read_excel("data/body_density_data.xlsx") %>%
  janitor::clean_names() %>%
  select(-bodyfat_brozek,-body_density,-id)
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

table=map(body_df, sum) %>% 
  bind_rows() %>% 
  mutate(variable = names(body_df)) %>% 
  select(variable,everything()) %>% 
  filter(variable!="id") %>%
  knitr::kable(digits = 2, 
               caption = "Descriptive statistics of continuous variables") 
```

``` r
# check multicollinearity
body_corr=
body_df %>% 
cor() 
corr=corrplot(cor(body_corr), 
         method = "color", 
         type = "upper",
         addCoef.col = "black", 
         number.cex = 0.6,
         diag = FALSE) 
```

![](data_files/figure-gfm/correlation-1.png)<!-- -->

``` r
#Raw VIF
mult.fit <- lm(bodyfat_siri ~ .,data = body_df)
vif_val <- vif(mult.fit)
name <- str_to_title (names(vif_val))
vifdf <- 
  tibble(variable = name, vif = vif_val) %>%
  mutate(variable = fct_reorder(variable,vif_val))
ggplot(data = vifdf,aes(y = variable, x = vif_val)) +
  geom_col() +
  geom_vline(xintercept = 5,colour="red",linetype = "longdash") +
 theme_bw()
```

![](data_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
raw_fit=lm(bodyfat_siri~.,data=body_df)
summary(raw_fit)
```

    ## 
    ## Call:
    ## lm(formula = bodyfat_siri ~ ., data = body_df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -11.1966  -2.8824  -0.1111   3.1901   9.9979 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -21.35323   22.18616  -0.962  0.33680    
    ## age           0.06457    0.03219   2.006  0.04601 *  
    ## weight       -0.09638    0.06185  -1.558  0.12047    
    ## height       -0.04394    0.17870  -0.246  0.80599    
    ## neck         -0.47547    0.23557  -2.018  0.04467 *  
    ## chest        -0.01718    0.10322  -0.166  0.86792    
    ## abdomen       0.95500    0.09016  10.592  < 2e-16 ***
    ## hip          -0.18859    0.14479  -1.302  0.19401    
    ## thigh         0.24835    0.14617   1.699  0.09061 .  
    ## knee          0.01395    0.24775   0.056  0.95516    
    ## ankle         0.17788    0.22262   0.799  0.42505    
    ## bicep         0.18230    0.17250   1.057  0.29166    
    ## forearm       0.45574    0.19930   2.287  0.02309 *  
    ## wrist        -1.65450    0.53316  -3.103  0.00215 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.309 on 238 degrees of freedom
    ## Multiple R-squared:  0.7486, Adjusted R-squared:  0.7348 
    ## F-statistic:  54.5 on 13 and 238 DF,  p-value: < 2.2e-16

``` r
bodyvf_data=body_df %>%
select(-weight,-hip,-chest,-thigh) 

mult.fit2 <- lm(bodyfat_siri ~ .,data = bodyvf_data)
vif_val <- vif(mult.fit2)
name <- str_to_title (names(vif_val))
vifdf <- 
  tibble(variable = name, vif = vif_val) %>%
  mutate(variable = fct_reorder(variable,vif_val))
ggplot(data = vifdf,aes(y = variable, x = vif_val)) +
  geom_col() +
  geom_vline(xintercept = 5,colour="red",linetype = "longdash") +
 theme_bw()
```

![](data_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
boxplot(body_df$bodyfat_siri,main='Percent body fat using Siri’s equation')
```

![](data_files/figure-gfm/check%20outcome%20variable-1.png)<!-- -->

``` r
body_df%>% select(-bodyfat_siri)%>%
  funModeling::plot_num()
```

    ## Warning: The `<scale>` argument of `guides()` cannot be `FALSE`. Use "none" instead as
    ## of ggplot2 3.3.4.
    ## ℹ The deprecated feature was likely used in the funModeling package.
    ##   Please report the issue at <]8;;https://github.com/pablo14/funModeling/issueshttps://github.com/pablo14/funModeling/issues]8;;>.

![](data_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->
