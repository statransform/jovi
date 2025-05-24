I came across the decision tree for inferential statistics through a post shared by colleagues. I truly appreciate your effort to make statistics more accessible to HCI researchers. However, I have several concerns regarding some of the terminology and specific guidelines presented in the figure. I hope my feedback will encourage the authors to revise the figure and include necessary flags and warnings.

#### Parametric vs. Nonparametric Data 
The terms and “parametric” or “nonparametric” refer to the statistical methods used to analyze the data --- they are not inherent characteristics of the collected data. If when data violate the assumptions of ANOVA or linear regression, one may still be able to use alternative parametric methods to analyze the data, such as to use generalized linear models. 

I want emphasize this point for another reason. By classifying the data as parametric or nonparametric, we automatically make a "prescription" of which statistical method is valid and which is not, which is generally a bad practice. For example, see [Vellman and Wilkinson](https://www.cs.uic.edu/~wilkinson/Publications/stevens.pdf) for a detailed argumentation about why data typologies can be misleading if they are not appropriately used. In my lectures, I used to point to [Baguley's](https://www.amazon.com/Serious-Stat-advanced-statistics-behavioral/dp/0230577180) recommendations regarding the criteria one needs to consider for choosing among statistical methods:

*"An alternative approach – that advocated here – is to consider a range of factors of data that impact on the statistical model you are considering. These factors include whether data are discrete or continuous, but other factors, such as the probability distribution being assumed, the size of the sample and what the model is being used for, are also important."* (Page 6). 

Unfortunately, it is not always easy to prescribe a set of rules for making decisions. Personally, I often use simulations to test the robustness of different methods when model assumptions are violated.   

*(Minor comment: Integer ratings, counts, and Likert items make discrete values --- why are they characterized as continuous here?)*

#### The Aligned Rank Transform 
For multi-factor designs and as long as data are classified as "nonparametric" (e.g., for counts, single Likert items, and skewed distributions that don't pass a normality test), the decision tree naturally leads to the use of the Aligned Rank Transform (ART). Unfortunately, as we show in our [JoVI submission](https://www.journalovi.org/2024-tsandilas-ranktransforms), ART is actually inappropriate for exactly this type of data. 

Yes, this may sound counterintuitive, but we found that ART's alignment method only works under very strict linearity assumptions and also fails under discrete data (see, for example, [this real example](https://www.journalovi.org/2024-tsandilas-ranktransforms/supplementary/examples-case-studies/6-Case-studies/Rosso-et-al-23/Rosso-et-al-23.html)). Interestingly, using regular ANOVA with such data is actually safer than using ART. Check out [our recommendations](https://www.journalovi.org/2024-tsandilas-ranktransforms/#recommendations) for more details (we hope to further improve them in an updated version). 

However, I should note that the article is still under review and ART's authors have not yet acknowledged these problems. You can follow the ongoing open discussions [here](https://github.com/journalovi/2024-tsandilas-ranktransforms/issues), where we provide additional explanation about why and how ART fails. 

#### Meaning of 95% Confidence Intervals
The heuristics given in the figure about the interpretation of confidence intervals is not correct. Please, see [Cumming and Finch](https://pubmed.ncbi.nlm.nih.gov/18991332/) for an explanation. In the general case, the correct approach is to construct the confidence interval of the difference and check whether this includes zero. 

Also notice that null hypothesis significance testing does not allow for rejecting H1. That said, one could specify a minimal difference of interest $\delta$ and reject H1 if the confidence interval of the difference resides below this $\delta$ (which may require a large sample size). But this is not what we read on the image. 

#### Interaction Effects
I am not familiar with the terms *exponential* and *antagonistic* interaction. In [our submission](https://www.journalovi.org/2024-tsandilas-ranktransforms/#interpreting-interactions), however, we make a distinction between *removable* and *non-removable* interactions, following the analysis of [Loftus](https://link.springer.com/article/10.3758/BF03197461).