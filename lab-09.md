Lab 09 - Grading the professor, Pt. 1
================
Zi Li
3/24/2025

## Load Packages and Data

``` r
library(tidyverse) 
library(broom)
library(openintro)

?evals
```

## Exercise 1

``` r
ggplot(evals, aes(x = score)) +
  geom_histogram(binwidth = 0.1, fill = "skyblue", color = "black")
```

![](lab-09_files/figure-gfm/exercise%20part1_code-1.png)<!-- -->

``` r
# Slight Left-Skewed. The majority of students' ratings were centered between ～ 3.5 - 5. The rating of ~ 4.2 - 5 was given by the largest number. I think this is to be expected, as I rated my professor between 4 or 5 as an undergraduate.

summary(evals$score)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   2.300   3.800   4.300   4.175   4.600   5.000

``` r
# Visualize and describe the relationship between score and the variable bty_avg, a professor’s average beauty rating.
ggplot(evals, aes(x = bty_avg, y = score)) +
  geom_point()
```

![](lab-09_files/figure-gfm/exercise%20part1_code-2.png)<!-- -->

``` r
# this plot⬆️ is kinda messy. 

ggplot(evals, aes(x = bty_avg, y = score)) +
  geom_jitter(width = 0.2, height = 0.1)
```

![](lab-09_files/figure-gfm/exercise%20part1_code-3.png)<!-- -->

``` r
# Now we see that the high beauty average does get some high ratings, but not a lot. Most of the points are still centered between beatuy average of 3 - 5 with a score of 3.5 - 5. 

# geom_jitter give us a better view of the overlapping data points; the initial plot is misleading because a lot of plots overlap on each others, make it harder to see a trend. 
```

## Exercise 2

``` r
m_bty <- lm(score ~ bty_avg, data = evals)
summary(m_bty)
```

    ## 
    ## Call:
    ## lm(formula = score ~ bty_avg, data = evals)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.9246 -0.3690  0.1420  0.3977  0.9309 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  3.88034    0.07614   50.96  < 2e-16 ***
    ## bty_avg      0.06664    0.01629    4.09 5.08e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5348 on 461 degrees of freedom
    ## Multiple R-squared:  0.03502,    Adjusted R-squared:  0.03293 
    ## F-statistic: 16.73 on 1 and 461 DF,  p-value: 5.083e-05

``` r
# score(hat)=3.88034 + 0.06664 x bty_avg

# The regression model predicting evaluation score from beauty rating was statistically significant, F(1, 461) = 16.73, p < .001. The model explained 3.5% of the variance in scores. The slope (b = 0.067) indicates that each additional point in beauty rating is associated with a 0.067-point increase in evaluation score.

# note, even though beauty significantly predicts evaluation scores, the effect size is small — most of the variation is explained by other factors. (thats what I thought)

ggplot(evals, aes(x = bty_avg, y = score)) +
  geom_point()
```

![](lab-09_files/figure-gfm/exercise%20Part2_code-1.png)<!-- -->

``` r
# remove.packages("tidyverse");install.packages("tidyverse")
# ⬆️. I got a error message saying: Error in base::nchar(wide_chars$test, type = "width") : promise already under evaluation: recursive default argument reference or earlier problems?
# after couple trouble shot, some smart people on internet said, remove tidyverse and reinstall, it would work. it did!!

ggplot(evals, aes(x = bty_avg, y = score)) +
  geom_jitter(width = 0.2, height = 0.1) +
  geom_smooth(aes(x = bty_avg, y = score), method = "lm", color = "orange", se = FALSE) 
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](lab-09_files/figure-gfm/exercise%20Part2_code-2.png)<!-- -->

``` r
# the plot is not what I thought, it feels a little sketchy... But it can still be interpreted. 
# For every 1-point increase in average beauty rating, the professor’s evaluation score increases by 0.067 points on average.

# And the intercept (3.88) represents the predicted score when bty_avg = 0.
# Mathematically, the expected score would be 3.88 if a professor had a beauty rating of zero. but it doesn't make sense here, no one got the beauty rating that low (not in my data tho).  

# with R-squared = 0.03502; it indicated that about 3.5% of the variability in professor evaluation scores is explained by the professor's average beauty rating.
# The remaining 96.5% of the variation is due to other factors not captured by this model. 
```

## Exercises 3

``` r
m_gen <- lm(score ~ gender, data = evals)
summary(m_gen)
```

    ## 
    ## Call:
    ## lm(formula = score ~ gender, data = evals)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.83433 -0.36357  0.06567  0.40718  0.90718 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  4.09282    0.03867 105.852  < 2e-16 ***
    ## gendermale   0.14151    0.05082   2.784  0.00558 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5399 on 461 degrees of freedom
    ## Multiple R-squared:  0.01654,    Adjusted R-squared:  0.01441 
    ## F-statistic: 7.753 on 1 and 461 DF,  p-value: 0.005583

``` r
levels(evals$gender)
```

    ## [1] "female" "male"

``` r
# Male: score(hat)= 4.09282 + 0.14151 × 1=4.23433
# Female: score(hat)= 4.09282 + 0.14151 × 0=4.09282

m_rank <- lm(score ~ rank, data = evals)
summary(m_rank)
```

    ## 
    ## Call:
    ## lm(formula = score ~ rank, data = evals)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.8546 -0.3391  0.1157  0.4305  0.8609 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       4.28431    0.05365  79.853   <2e-16 ***
    ## ranktenure track -0.12968    0.07482  -1.733   0.0837 .  
    ## ranktenured      -0.14518    0.06355  -2.284   0.0228 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5419 on 460 degrees of freedom
    ## Multiple R-squared:  0.01163,    Adjusted R-squared:  0.007332 
    ## F-statistic: 2.706 on 2 and 460 DF,  p-value: 0.06786

``` r
levels(evals$rank)
```

    ## [1] "teaching"     "tenure track" "tenured"

``` r
# score(hat)= 4.28431 − 0.12968 × tenure track − 0.14518 × tenured

# The intercept is the predicted average evaluation score for teaching faculty (with an average score of 4.28).
# Tenure-track professors score 0.13 points lower on average than teaching faculty, but this difference is not significant yet approaching (p = 0.084).
# Tenured professors score 0.15 points lower on average than teaching faculty.This difference is significant (p = 0.023).

# About 1.16% of the variance in evaluation scores is explained by rank.

evals <- evals %>%
  mutate(rank_relevel = fct_relevel(rank, "tenure track"))

m_rank_relevel <- lm(score ~ rank_relevel, data = evals)
summary(m_rank_relevel) # Seriously, things get a little complicated once you done a lot modeling; trying to wrangling my head around this..
```

    ## 
    ## Call:
    ## lm(formula = score ~ rank_relevel, data = evals)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.8546 -0.3391  0.1157  0.4305  0.8609 
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)           4.15463    0.05214  79.680   <2e-16 ***
    ## rank_relevelteaching  0.12968    0.07482   1.733   0.0837 .  
    ## rank_releveltenured  -0.01550    0.06228  -0.249   0.8036    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5419 on 460 degrees of freedom
    ## Multiple R-squared:  0.01163,    Adjusted R-squared:  0.007332 
    ## F-statistic: 2.706 on 2 and 460 DF,  p-value: 0.06786

``` r
# score(hat) = 4.15463 + 0.12968(teaching) − 0.01550(tenured)

# The expected evaluation score for tenure track faculty is 4.15.

# Teaching faculty score 0.13 points higher than tenure track faculty, but this difference is only approaching significance and yet not significant (p= 0.084).
# Tenured professors score slightly lower (- 0.016 points), and this difference is not significant (p= 0.804).

# 1.16% of the variability in evaluation scores is explained by releveled professor rank.
```

## Exercises 3- continued

``` r
evals <- evals %>%
  mutate(tenure_eligible = if_else(rank == "teaching", "no", "yes"))

m_tenure_eligible <- lm(score ~ tenure_eligible, data = evals)
summary(m_tenure_eligible)
```

    ## 
    ## Call:
    ## lm(formula = score ~ tenure_eligible, data = evals)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.8438 -0.3438  0.1157  0.4360  0.8562 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          4.2843     0.0536  79.934   <2e-16 ***
    ## tenure_eligibleyes  -0.1406     0.0607  -2.315    0.021 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5413 on 461 degrees of freedom
    ## Multiple R-squared:  0.0115, Adjusted R-squared:  0.009352 
    ## F-statistic: 5.361 on 1 and 461 DF,  p-value: 0.02103

``` r
# score(hat)= 4.2843 − 0.1406 × tenure_eligible

# teaching faculty (Professors not eligible for tenure) have an average predicted evaluation score of 4.28.
# tenure-track or tenured professors receive evaluation scores of 0.14 points lower on average than teaching faculty. This result is significant with p = 0.021.

# About 1.15% of the variance in evaluation scores is explained by tenure eligibility.
# Being tenure-eligible vs. teaching faclty explains 1.15% difference in scores/ tenure eligibility has 1.15% yet significant effect on evaluation ratings.
```
