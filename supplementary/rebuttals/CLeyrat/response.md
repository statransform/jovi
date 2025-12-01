---
title: "Responses to our reviewer"
output: github_document
---


**Important:** @CLeyrat @xxx @Lonni 
We have realized that our previous response to @CLeyrat (Comment 18: "More fundamentally, the authors should clarify how the simulation plan with a latent variable relates to real-life data generation mechanisms") was not correct. After further investigation, we have made an important decision to revise our simulation methodology. Below, we explain the issue.

Our latent-variable approach, which generates data by applying a monotonic transformation to a normal distribution, works as intended for log-normal data, and it also remains valid for ordinal outcomes, where we rely on thresholding a latent normal variable. However, this approach fails for the other distributions used in our simulations: Exponential, Cauchy, Binomial, and Poisson.

In these cases, whenever an effect is present (e.g., $a_1 > 0$ or $a_2 > 0$), the transformed distributions no longer follow the intended theoretical distributions. For instance, under the "Cauchy" condition, the generated data become asymmetric, meaning the data are not truly Cauchy-distributed. Although the Type I error and power results we reported are correct for the data we generated, they unfortunately pertain to distributions that we can neither clearly characterize nor theoretically justify.

Given this, and also acknowledging that the latent-modeling approach caused confusion and raised legitimate concerns, we have decided to replace it with a standard simulation framework based on Generalized Linear Models, using the appropriate link functions as described in Equation 4. The only exception concerns ordinal (Likert-like) data, where our original latent-threshold method remains suitable and will be retained.

Because we must now rerun a large number of simulations and substantially revise our methodology and results sections, we anticipate needing additional time. Since all designs in the main article are balanced, we plan to use ANOVA (via R’s aov() function) instead of linear mixed-effects models, as this greatly improves computational efficiency.

@CLeyrat: please let us know if you have any objections to this change.

We sincerely apologize for not identifying these issues earlier, and we thank the reviewer for prompting this important correction.




We thank our reviewer for her time and constructive comments. We provide detailed clarifications below and address each point in turn. We would **greatly appreciate a direct discussion within this thread** to ensure that all points of doubt are adequately addressed.

A brief summary of some key points:

1. **Comparability of methods**. Our article clearly distinguishes between scenarios where interpretation issues due to different definitions of the null hypothesis arise, and those where they do NOT. For most of the scenarios we test, the Type I error rates and power are perfectly comparable across methods---both for main and interaction effects. Comparability issues arise when testing interactions in the presence of parallel main effects (also [in this scenario (Fig. 29)](https://www.journalovi.org/2024-tsandilas-ranktransforms/#fig-unequal-main-1) and [in Appendix: Fig. 16-19](https://www.journalovi.org/2024-tsandilas-ranktransforms/appendix.html#removable-main)), but the article explicitly identifies and discusses these scenarios and is explicit about the null hypothesis we test.

2. **LMER models**. For the scenarios we test, using *lmer* leads to identical (or nearly identical) results to ANOVA. Any potential issues with mixed models have not affected our results.

3. **Categorical variables**. We treat independent variables as categorical, not numerical. Our coding approach is compatible with dummy coding but offers additional control over effect magnitudes.  

4. **Data transformations**. The monotonic transformations we apply do not induce selection bias and do not violate any assumptions of random sampling.

We respond below to all individual reviewer comments (numbered from 1 to 28 for easy reference). Regarding presentation suggestions, we note that our reviewer's disciplinary background differs from ours. We kindly ask that the discussion focus primarily on the validity of our research methods and conclusions rather than presentation preferences.
 
**Note on terminology:** Our reviewer uses the term *estimand*, whereas we refer to population parameters of interest. We prefer our terminology for conceptual clarity and consistency with the terminology most commonly used in disciplines closer to ours.

### "Overall weakness"
**Comment 1.** *"Nevertheless, what is presented as bias or error (for ART) in the manuscript might be explained by something more fundamental: a discrepancy between the estimands the authors wish to target, and the estimands actually targeted with each estimator. Although the authors clearly state that the methods they investigate test different null hypotheses, the simulation study may not fully capture this. My main concern is that comparing these methods is like comparing apples and oranges, but with the aim to show they all look like apples ..."* 

[Section 4](https://www.journalovi.org/2024-tsandilas-ranktransforms/#interpretation) already extensively discusses these points. We have also clarified them through our [our responses to Prof. Higgins](https://github.com/journalovi/2024-tsandilas-ranktransforms/issues/2#issuecomment-2528141692), where we present additional tests to explain why our comparisons are correct. 

We repeat that unless we explicitly mention interpretational issues, the Type I error rates and power results we report are identical regardless of whether one tests means, medians, or full distributional shape. This holds for the vast majority of our results (e.g., Figures 15–17, 19–24, 30–34). In all these cases, Type I errors are evaluated for identical populations, meaning all population parameters (means, medians, etc.) are equal across levels---both in the latent and the transformed space. Hence, the null hypothesis definition does not affect these comparisons.

We agree that there are situations where interpretation issues arise, but the article is very clear about when this happens. See captions of Figures 25-27, and 29, as well as the related discussion in these paragraphs. As we further discuss later, we are explicit about which null hypothesis we test for these results. 

### Specific comments
**Comment 2.** *"... the current structure of the manuscript could be improved ... This is particularly important because the illustrative example comes before any explanation of the methods ... the illustrative example is the least convincing section.*
 
Our choice to begin with an illustrative example is motivated by closely related work. Higgins et al. (1990) demonstrated the breakdown of rank transformations for interactions using a concrete data example. Elkin et al. (2021) likewise used a specific running example to show how ART fails to handle contrasts, and Lüpsen (2016) illustrated ART’s problems under discrete distributions with a concrete example as well.

Naturally, the examples in all these works are deliberately chosen. Their purpose is to illustrate the problem and provide intuition for readers---not to serve as a systematic proof. It is the role of Monte Carlo experiments to evaluate such issues systematically over a large number of iterations.

**Comment 3.** *"Was this dataset selected at random (as it should be) or selected to illustrate the point? If it is the latter, it may not be completely honest: based on this example, it may only be fair to argue that ART may be more sensitive to outliers."*

The example was chosen to *illustrate* (so we call it *illustrative*) ART’s erratic behavior. 

Our results clearly demonstrate that ART inflates Type I errors. However, in many cases, it still produces results that are numerically close to the ground truth. For this reason, we do not see how randomly drawing a dataset would add meaningful insight.

It is also important to note that ART’s *p*-values in our example are orders of magnitude smaller than those returned by other methods---indicating an erratic behavior that is clearly not normal. We do not present this example as a proof, and we would not have included it without first confirming this problematic pattern through systematic simulations.

Even if one interprets these results as showing that ART is more sensitive to "outliers" than other methods, this is already troubling, since previous literature advocating ART explicitly claims the opposite.

**Comment 4.** *"Given the small sample size, I am not confident in commenting on observed differences between p-values."*  

We used a small sample to keep the example simple and easily reproducible. We also chose $n=12$ because it is a very common sample size in HCI experiments following a within-participants design (although we know that such a sample size is considered insufficient in other research domains).

Larger samples do not reduce *p*-value variability (ART actually struggles even more with larger samples). Nonetheless, if preferred, we can provide an example with a larger sample, e.g., see ["A challenge for Prof. Higgins"](https://github.com/journalovi/2024-tsandilas-ranktransforms/issues/2#issuecomment-2564006376), where we discuss results for a sample of $n=1000$.

### General comment about the mixed models and the R implementation

**Comment 5:** *"The authors used a random-effect model as an estimator across methods. Was the correlation between pairs of measures for a participant assumed identical no matters the order of the repetitions?"*

We did not model any order effect; our simulation design includes none.

**Comment 6:** *"Also, by default, the lmer package does not provide p-values for main effects. How were the p-values calculated? (Wald test with REML, LRT with ML, etc.). In particular, the authors provide one p-value per variable (even when there are more than two levels), so it is important to clarify how this overall p-value is obtained for categorical variables, given that they are not provided by default."*

As our reviewer remarks, our simulation designs are balanced (with the exception of [these results in the Appendix](https://www.journalovi.org/2024-tsandilas-ranktransforms/appendix.html#missing)), so Type I, Type II, and Type III tests give the same results (see  [Kuznetsova et al., 2017](https://www.jstatsoft.org/article/view/v082i13)). We verified that results are identical (or nearly so) across all these methods:

1. Conducting ANOVA with R's *aov()* function, where *p*-values are then obtained using R's *summary()* function. Example: 
`mpar <- aov(Time ~ Difficulty*Technique + Error(Participant), data=df)`
`summary(mpar)` 
2. Using *lmer* (fit by REML) and then R's *anova()* function to obtain *p*-values (Type III tests). This is the method we use for our all designs with a subject random effect. Example:
`mpar <- lmer(Time ~ Difficulty*Technique + (1|Participant), data=df)`
`anova(mpar)`
3. Using *lmer* (fit by REML) and then R's *car::Anova()* function to obtain *p*-values (Type II Wald F tests). Example:
`mpar <- lmer(Time ~ Difficulty*Technique + (1|Participant), data=df)`
`car::Anova(mpar, type=2, test.statistic = "F")`

We also obtain practically identical results, whether we use a Kenward-Roger (default for *art()* but more computationally expensive as [Kuznetsova et al., 2017](https://www.jstatsoft.org/article/view/v082i13) report) or a Satterthwaite (default for *anova()*) approximation for calculating the *F* test.

For example, the figures below reproduce [Figure 15](https://www.journalovi.org/2024-tsandilas-ranktransforms/#fig-ratio-main) (Type I error rates for ratio scales) and [Figure 19](https://www.journalovi.org/2024-tsandilas-ranktransforms/#fig-ordinal-main) (Type I error rates for ordinal scales) when $n=10$ (smallest sample size we test), using repeated measures ANOVA: 
![](https://notes.inria.fr/uploads/upload_7b6bd501ee3d7f2c12e7a733ad2ee615.png)

![](https://notes.inria.fr/uploads/upload_963e517256cba4fc9b19ec6fa416b78e.png)


Trends are identical with the ones of the original figures (where we use LMER). We can provide additional results upon request. 


**Comment 7:**  *"My concern after reading the simulation plan is that the main effects were included as numeric variables ..."* 

We treat all experimental factors as categorical, not numeric. All factor levels in our simulated data are strings that take the form "A1", "A2", ... or "B1", "B2", ..., and we use R's *factor()* function before analysis. Our coding method (sum-to-zero contrasts) (explained [here](https://www.journalovi.org/2024-tsandilas-ranktransforms/#statistical-modeling)) ensures symmetry and a grand mean of $\mu$, while allowing controlled manipulation of effect sizes via parameters like $a_1$, $a_2$, and $a_{12}$.

See examples of distributions [(Figure 14)](https://www.journalovi.org/2024-tsandilas-ranktransforms/#fig-effects) we generate with this approach for two, three, or four **categorical** levels.

**Comment 8:** *"... thus making the unrealistic assumption of a linear effect of the experimentation conditions on the outcome. This assumption is not needed, and the different levels of the fixed effects can be included as dummy variables.  In particular, the authors state that both ART and ANOVA require the linearity assumption, but as far as I understand, the variables considered in this paper are all categorical (e.g. easy, medium, hard), included as dummy variables, and therefore no linearity assumption is required."*  

All linear regression and ANOVA models are linear regardless of how categorical variables are encoded. Assumptions about a linear relationship between independent variables and responses do not disappear when using dummy coding.

For example, consider the first factor $X_1$ in Equation 9. Suppose it has three levels. Using dummy coding, the term $x_{1i}$ can be expressed as:

$$x_{1i} = \beta_1​ + \beta_2 d_2 + \beta_3 d_3$$

where $d_2$, and $d_3$ are the dummy variables for the three levels of the categorical variable. Our coding simply enforces desirable constraints (zero-centered effects) for $\beta_1$, $\beta_2$, and $\beta_3$. But linearity is intrinsic to the model, not to the coding scheme. 

We would appreciate clarification from the reviewer: how would the linearity assumption disappear when using dummy coding?
 
**Comment 9:** 
*"Finally, mixed-models can lead to incorrect type I error rates in small samples. Did the authors use any small sample correction?"*

No correction was used. Type I error rates are stable across sample sizes in our simulations, and as our results show, Type I error rates are not affected by small sample sizes (down to $n=10$). ART’s problems, in contrast, worsen with larger $n$ (and this is unrelated to the models we use).

### Illustrative example: in addition to my previous comments

**Comment 10:** 
*"'We observe that ART exaggerates both the effect of Technique and its interaction with Difficulty' : this is incorrect, the p-values quantify the strength of evidence, not the magnitude of the effect."*

Our statement refers to the partial $\eta^2$ values shown in the preceding table, not to *p*-values. As we discuss later, such effect size estimates are commonly reported by authors who make use of ART.

**Comment 11:** 
*"Figure 1, y-axis label: should it be Time (rather than median time) unless each dot is a median across other parameters."* 

For the first plot, we aggregate over difficulty levels but not for the second one. We will correct the mistake.

**Comment 12:** 
*"Perceived efficiency: despite having in theory 5 levels, in the sample it is almost binary and any method for continuous or discrete data should be prohibited. This example is too extreme and as a consequence may be quite damaging overall. The authors should only compare the performance of methods that are realistic for the setting."* 

The data are not binary, though responses cluster at higher scale points. Such skewed ordinal data are common in practice---precisely where nonparametric methods like ART are typically recommended. Demonstrating ART's instability here is therefore important and relevant.

Furthermore, as we discuss [here](https://www.journalovi.org/2024-tsandilas-ranktransforms/#analyzing-likert-type-data), there is ongoing debate regarding the use of parametric techniques with Likert data, thus we therefore disagree with the term "prohibited." 

**Comment 13:** 
*"Table 1 (p-values): it would be useful to report the regression coefficient and CI from the mixed models (even though their interpretation differ), along with the p-values, and be consistent with the number of decimal places."*

Below, we show the 95% CIs of the regression coefficients for the LOG method only: 

![](https://notes.inria.fr/uploads/upload_ac1cc1b4dddb9e437b143a5ddc355d13.png)

We are concerned that presenting all of these numbers (and for all methods) would only confuse readers. Note that ART produces three separate models, each corresponding to a different alignment: the first factor (*Difficulty*), the second factor (*Technique*), and their interaction. It is unclear which set of coefficients should be reported, or how such information could be meaningfully interpreted.

However, we can replace the mixed-model analysis with a repeated-measures ANOVA (which yields **identical results**)---if this addresses the reviewer's concern. See [ANOVA analysis](https://www.journalovi.org/2024-tsandilas-ranktransforms/supplementary/examples-case-studies/1-Introduction/illustrative-example-aov.html) vs. [LMER](https://www.journalovi.org/2024-tsandilas-ranktransforms/supplementary/examples-case-studies/1-Introduction/illustrative-example.html). 

### Interactions
**Comment 14:** *"In the paper, the methods apply different transformations, thus making the coefficients (and inferences) for the interaction parameters not comparable."* 

A [full subsection in our article](https://www.journalovi.org/2024-tsandilas-ranktransforms/#interpreting-interaction-effects) is dedicated to this issue. An [additional subsection](https://www.journalovi.org/2024-tsandilas-ranktransforms/#disagreement-on-the-interpretation-of-effects-in-glms) discusses the debate within the context of GLMs, where we explicitly state when the interpretation of interaction is ambiguous (and thus comparisons are problematic) and **when it is not**. Also see [our older response to Matthew Kay](https://github.com/journalovi/2024-tsandilas-ranktransforms/issues/2#issuecomment-2627759256). 

We repeat that when either $a_1 = 0$ or $a_2 = 0$ (see Equation 4), there is no ambiguity in the interpretation of Type I errors for the interaction---no matter which method we use, no matter whether we test means or medians, either on the latent or the observed variable. If $a_{12} = 0$, no interaction should appear under any monotonic transformation---so comparing Type I errors across methods (see Figures 17, 20, 21, 24, 31) is valid.  

*Note on power:* As discussed in the article, transformations to discrete distributions are monotonic but not strictly monotonic. Thus, an effect in the latent variable may vanish after transformation. This issue affects all methods equally, so power comparisons remain valid.

**Comment 15:** *"Instead of debating whether one is better than the other, the authors should instead explain when one type of interaction may be more relevant than the other for the research question of interest. Thinking about the different experimental settings, when do we care about interaction, and which type of interaction is relevant?"*

This is precisely what we do for the scenarios in which interpretational issues arise--namely, when the main effects of all interacting factors are non-zero: 

- Please see our discussion on [the interpretation of effects in GLMs](https://www.journalovi.org/2024-tsandilas-ranktransforms/#disagreement-on-the-interpretation-of-effects-in-glms).

- Please also see our results on [interactions under parallel main effects](https://www.journalovi.org/2024-tsandilas-ranktransforms/#interactions) and the related discussion in this section. Note that the captions of Figures 25, 26, 27 explicitly state that "Type I errors are defined based on the null hypothesis $a_{12} = 0$. A different definition may lead to different results." 

### Simulation study
**Comment 16:** *"The authors should also consider whether the simulations on ordinal scales are important here. Many statisticians would argue for the use of appropriate regression models for this type of outcomes (extensions of logistic regression) ...*" 

Our article includes a dedicated [discussion on ordinal data](https://www.journalovi.org/2024-tsandilas-ranktransforms/#analyzing-likert-type-data), where we explicitly advocate for the use of appropriate regressions models. However, these methods also present several practical challenges, and, as we discuss extensively, there is no consensus on whether ANOVAs and linear parametric models can be safely applied to ordinal data. 

Moreover, in practice, researchers often use nonparametric methods far more frequently than ordinal regression models. As detailed in [this section](https://www.journalovi.org/2024-tsandilas-ranktransforms/#ART-use), ART is very commonly used for exactly this type of data. 

We invite our reviewer to consider:

1. [This decision tree](https://github.com/valentin-schwind/statistics-decision-tree/?tab=readme-ov-file), which provides methodological recommendations for HCI researchers. For discrete ordinal data and multiple independent variables, it explicitly recommends ART---ordered probit models are not even mentioned. 

2. A simple query to ChatGPT: *"I want to conduct a multifactorial statistical analysis where responses follow an ordinal scale of 5 levels. Which statistical methods could I use?"* ART appears among the five recommended methods ([see here](https://chatgpt.com/share/6910d909-ae20-8008-bb05-8935f1040404)), despite the fact that no empirical studies have demonstrated that ART is valid for such data.

Our results provide clear evidence that ART is **inappropriate** for ordinal data and poses **greater risks** than commonly used parametric methods. We therefore believe it is both relevant and necessary to present these findings.

**Comment 17:** *"Similarly, it is unlikely statisticians pick methods for continuous data for count (Poisson distribution) or binomial distributions. This applies to the re-analyses as well. It may be useful to reduce the amount of examples and scenarios but focus on the key differences between methods."* 

Rank transformations (and the nonparametric statistical tests based on them) are in fact very commonly used for both continuous and discrete data. While this may not be typical practice in our reviewer's field, it is widespread in many others. Importantly, researchers ignore that the alignment step in ART implicitly assumes continuous distributions.

It is true that trained statisticians may not typically choose such methods. However, in many scientific domains, data analyses are performed by non-statisticians (ourselves included) and frequently by researchers with limited statistical training. Our work aims precisely to evaluate how these widely used methods behave under realistic conditions encountered in such applied research contexts.

**Comment 18:** *"More fundamentally, the authors should clarify how the simulation plan with a latent variable relates to real-life data generation mechanisms."* 

Our simulation plan produces a broad range of distributions that are commonly used as benchmarks in the related literature. A justification for the various ratio scales employed is provided [here](https://www.journalovi.org/2024-tsandilas-ranktransforms/#ratio). For ordinal scales, we used appropriate ordinal models, following approaches such as those described by Liddell and Kruschke (2018). 

As we [also explain here](https://github.com/journalovi/2024-tsandilas-ranktransforms/issues/2#issuecomment-2638043122), the great advantage of our methodology lies in its flexibility: it enables the generation of data for any distribution using the same underlying structure, by changing only a single line of code. This also allows us to:

- present results consistently across different distributions by varying the same parameters;
- define a clear ground truth for effect size estimates (see below); and
- systematically study scenarios where interaction interpretation issues arise---defining the null hypothesis with respect to the parameters of the latent linear model, as is standard in regression analyses.

We also provide [supplementary code and results](https://github.com/journalovi/2024-tsandilas-ranktransforms/tree/main/supplementary/rebuttals/kay) using a more traditional simulation methodology, where data are drawn directly from the target distributions (e.g., using the *rlnorm()* function for log-normal samples). The results are fully consistent with those obtained via our latent-variable approach. However, controlling parameters becomes cumbersome with this approach, and presenting results coherently across multiple distributions is considerably more difficult. 

We empahsize that our methodology has many strenghts and was chosen after considering several alternative approaches. 

**Comment 19:** *"If the true population distribution is normal, but the observed distribution follows a different distribution, it seems to immediately imply a selection bias and a violation of the assumption of random sampling."*

This is incorrect: there is neither selection bias nor any violation of random sampling. If we draw a random sample of the latent variable $Y$ and then apply a monotonic transform $g$, the transformed observations $g(Y_i)$ are an independent and identically distributed sample from $g(Y)$. 

Monotonic transformations preserve the rank order and quantiles of the distribution. Specifically, if $q_k$ denotes the $k^{th}$ quantile of $Y$, then $g(q_k)$ is the $k^{th}$ quantile of $g(Y)$. 

**Comment 20:** *"Either the null hypothesis relates to the population but the assumptions for all the methods are violated ... Therefore, my concern is that the design of the simulation study implicitly favours some methods over others.*" 

This is not the case. Suppose we have two identical distributions $Y_1 = Y_2$. This implies that their population parameters---means, medians, and so on---are also identical. Because monotonic transformations preserve equality, it follows that $g(Y_1) = g(Y_2)$, and the corresponding means, medians, etc. of $g(Y_1)$ and $g(Y_2)$ are again the same. 

If a method rejects the null hypothesis under these conditions, it constitutes a Type I error, regardless of how the method defines the null hypothesis (e.g., in terms of means, medians, or another parameter).

As we noted earlier, our article explicitly clarifies when and why issues related to the definition of the null hypothesis become relevant.

**Comment 21:** *"What may be helpful would be to write down the different possible null hypotheses for the main effects (e.g. No difference in means, No difference in mean log, No difference of location shift …) and for the interaction (e.g. no multiplicative interaction), and compare the statistical performances of each method."* 

See our response above.

Note that in the case of parallel main effects, where the definition of the null hypothesis for interaction becomes ambiguous, we explicitly state that the reported Type I error rates are defined with respect to the null hypothesis $a_{12}=0$. As we further discuss at the end of this section: *"It is unclear how to reliably control the null hypothesis at the level of the response scale when both main effects are varied."*  

If our reviewer is aware of any method for nonlinear models that allows defining null hypotheses for interactions (for example, based on means) directly at the level of the response scale and comparing methods under this definition, we would be very interested to learn more.

**Comment 22:** *"How come the type I error rate for ART is inflated as compared to the other methods, but the power is similar?"*

We explain in our article: *"Because there is a tradeoff between Type I and Type II errors, high power can simply be the result of a high Type I error rate. Since parallel effects can inflate errors (see our previous results), we focus here on single effects, both main or interaction effects."*

To clarify further: for continuous heavy-tailed distributions (e.g., log-normal and exponential), ART's Type I error inflation for one factor (e.g., $X_1$) is only noticeable when there is also an effect on the other factor (e.g., $X_2$). In the power results we present, the effect on the second factor is zero. This choice avoids confounding Type I and Type II errors. If the effect on $X_2$ were increased, ART would appear more powerful, but such a comparison would be unfair. 

For discrete distributions, however, ART inflates Type I error rates even when all effects are null. This explains why ART's power may appear higher for small effects (due to increased Type I errors) but deteriorates relative to other methods as effects grow larger---because the influence of Type I errors diminishes in these scenarios.

**Comment 23:** *"DGM: the authors should explain why the variables x1 and x2 were not generated using dummy indicators."*

See responses to Comments 7-8.

 **Comment 24:** *"It would be interesting to see, for each mixed model, whether the residuals and the random effects follow a normal distribution."*

We are not entirely sure we understand the purpose of this suggestion. Could the reviewer clarify what insight they expect from examining the residuals and random effects distributions?

 **Comment 25:** *"Were there any convergence issues with the mixed models?"*

Our models are relatively simple, and we did not encounter any convergence issues throughout the simulations. Convergence problems can arise in discrete data when all values for a particular factor level coincide, especially with a small number of levels; however, we have excluded such rare cases from our results.

 **Comment 26:** *"Did the empirical standard deviation of the regression coefficients across simulations correspond to the model-based standard errors?*" 

We did not perform checks on the regression coefficients and are unsure what specific comparison the reviewer intends with this suggestion.

 **Comment 27:** *"In the presence of heteroscedasticity, sandwich standard errors may be used. Did the authors consider it?"*

Our experiments focus on comparing the robustness of different methods without any additional corrections. In reviewing previous studies using ART, we observed that several authors justify its use as a way to handle heteroscedasticity---even though, as we noted earlier, these authors ignore the method's underlying assumptions.

While we agree that appropriate methods exist to address heteroscedasticity, evaluating them was beyond the scope of our study. However, we have conducted additional tests that we do not report in the article---for example, for single factor designs ([see Appendix](https://www.journalovi.org/2024-tsandilas-ranktransforms/appendix.html#nonparametric-tests)), showing that combining the inverse normal transformation (INT) with sphericity corrections (for within-subjects designs) leads to better results than the Friedman test, which otherwise was shown to be more robust under heteroscedasticity (suffering from lower power though).

 **Comment 28:** *"Can the eta squared be directly compared across methods? Given that the total variance changes based on the transformation, it may impact the percentage explained by each of the variables."* 

This is an excellent question. We fully agree that (partial) eta squared is not preserved under transformations, whether monotonic or not. We will clarify this point in a revised version.
 
It is also important to note that when researchers use ART, they typically report standardized effect sizes, following [Matthew Kay's vignette on effect sizes with ART](https://cran.r-project.org/web/packages/ARTool/vignettes/art-effect-size.html#partial-eta-squared). This raises a broader question: how should standardized effect sizes obtained from rank transformations be interpreted? Are they meaningful?

Our approach, using a latent variable, addresses this question by providing a baseline (ground truth) for comparison (see Figures 35–40). In many scenarios, we observe that the estimates from rank-based methods are close to this baseline, indicating that they can provide useful information in those cases.

However, significant discrepancies occur in certain situations, mainly due to two types of information loss (as discussed in the article):
 
 1. Rank transformations fail to capture large effects. As we explain: *"Consider, for example, two sets of numbers $A= \{1, 2, 3\}$ and $B= \{4, 5, 6\}$. The mean difference between them is $\overline{B} - \overline{A} = 3$, whether calculated from the row values or their ranks. However, if the set $B$ becomes $\{9, 10, 11\}$, the mean difference of raw values increases to $8$, while the mean difference in ranks remains $3$. As effects become larger, this problem becomes more apparent."*  

2. Discretization in ordinal scales leads to information loss. All methods tend to underestimate effect sizes, though some methods still perform better than others.

A natural question arises: why is our ground truth appropriate? The answer is that it represents the effect size estimates of the **ideal** generalized linear regression model, which fully normalizes the data and supports inference at the level of the latent linear model parameters. If the data are log-normal, our baseline represents the effect sizes estimates of the linear model after applying a log transformation. If the data are binomial, it represents the estimates of the ideal logistic regression model. If the data are ordinal, it represents the estimates of the ideal ordinal regression model, and so on.