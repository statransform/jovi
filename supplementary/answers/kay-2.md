@mjskay Please, check this simplified code I have uploaded in [this supplementary folder](https://github.com/journalovi/2024-tsandilas-ranktransforms/tree/main/supplementary/rebuttals/kay).

@mjskay Yes, you are right! I oversimplified the code and did not think about adding random intercepts for participants (I call them subjects as they could be non-human subjects). I [updated the code](https://github.com/journalovi/2024-tsandilas-ranktransforms/tree/main/supplementary/rebuttals/kay).

I now include two types of tests: 
1. For within-subjects designs, where I now add a random subject-level effects. I use a different method for each type of data. You can use your own strategy.
2. For between-subjects designs (see files with *-independent* in their name), where I do not add subject-level effects and omit the subject error term from the model. These simulations are much faster and thus easier to quickly test. 

My result files under the ``log/`` folders have been updated as well. 

Let me know if this is enough for you.

**Some remarks on ART and discrete data.** @wobbrock I understand Higgin's argument but unfortunately, it's not valid. ANOVA's robustness comes from the fact that it makes inferences about means, which are aggregations over a larger number of values. Even if the original values are discrete and bounded, it is well known that the distributions of the means are asymptotically normal, and in practice, good results are often achieved even with small sample sizes.

However, ART's problems are unrelated to this. With distributions that have a small number of discrete values, ART's error does not decrease as the number of observations increases. Quite the opposite—it grows even more! Lüpsen has explained the problem (see Section 3 [here]((https://www.uni-koeln.de/~a0032/statistik/texte/ART-discrete.pdf))):

*"There is an explanation for the increase of the type I error rate when the number of distinct values gets smaller or the sample size larger: due to the subtraction of the other effects - a linear combination of the means - from the observed values even tiny differences between the means lead to large differences in the ranking [...] So tiny mean differences result in larger differences between the mean ranks and therefore in significant results. And for larger sample sizes this effect is multiplied."*

We have conducted numerous tests (not all of which are included in our submission), and they overwhelmingly confirm this problem. Even on discrete scales with up to 21 levels (such as those found in a NASA-TXL index), ART's error can become considerably large as the sample size increases.

@casiez We should actually add a graph in a revised version of our article to show how ART's error rate grows with *n* when data are discrete. 


@wobbrock I am not sure if continuing this discussion with Prof. Higgins is productive. The paper he cites demonstrates something we explicitly show (e.g., see Fig. 5, Fig. 13, Fig. 17, Fig. 21, etc.) and discuss extensively. We never dispute this result. Yes, RNK (and to a lesser extent INT) distorts interactions in the presence of parallel main effects—we state this repeatedly in the paper. However, we also caution researchers that unless a well-suited parametric model is used to describe the underlying data, inferring interactions in the presence of parallel effects is generally problematic.

This, of course, has nothing to do with the examples in Fig. 1 and Fig. 2, where parallel effects are not present. Again, it remains unclear what Prof. Higgins means by a "wrong model." We know exactly which model we used to generate the data for these examples.

Regarding discrete data, I find his argument difficult to follow. We are not attempting to impose Likert data onto the linear model he describes. We clearly distinguish between the latent variable, which follows this model, and the observed ordinal variable. We then use the effects on this latent variable as the ground truth for evaluating all these methods (e.g., see [Liddell and Kruschke, 2018](https://www.sciencedirect.com/science/article/pii/S0022103117307746?via%3Dihub)).
