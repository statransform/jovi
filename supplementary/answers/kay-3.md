We will ask our reviewers to carefully read our full article. It extensively discusses interpretation issues associated with different scales (*how many previous studies in the literature that we cite have identified these issues, and how many researchers in HCI were aware of them?*). However, we have also made a clear effort to specify when these issues arise and when they do not. We are more than willing to revise our article to further clarify these points, and constructive suggestions are always welcome! But throwing all these cases into the same bucket may not help the discussion.

### "the scales aren't equivalent"
The example of Liddell and Kruschke presents a very problematic scenario (I further discuss it later), where variances are unequal, and the changing of scale transforms parameters in a *non-monotonic way.* 

Please, see our [Section 3. Dealing with the interpretation of main effects](https://www.journalovi.org/2024-tsandilas-ranktransforms/#what-is-the-null-hypothesis-of-interest): 

*"To partly avoid these interpretation issues, we focus on effects that apply monotonic transformations to population distributions. This also ensures a monotonic relationship between different measures of central tendency such as medians and means (with the exception of the Cauchy distribution, where the mean is undefined)."*

Please, see our results ([Section 5. Type I errors under unequal variances](https://www.journalovi.org/2024-tsandilas-ranktransforms/#type-i-errors-under-unequal-variances)), where we emphasize:

*"Main effects. We begin by investigating how the four methods detect main effects on $X_1$. The interpretation of results in this scenario is challenging due to differences in the original populations — although they share the same means (and medians), their variances differ. Depending on the null hypothesis of interest, conclusions may vary. Complicating matters further, non-linear transformations ... "*

Our readers will notice that in [Figure 23](https://www.journalovi.org/2024-tsandilas-ranktransforms/#fig-unequal-main-1), the axis label is *Positives (%)*, not Type I error rates. Please, also refer to our [previous posts](https://github.com/journalovi/2024-tsandilas-ranktransforms/issues/2#issuecomment-2507452242) on other cases where the scale affects the interpretation of the results, with our own recommendations for revisions. 

In contrast, the rates in [Figure 24](https://www.journalovi.org/2024-tsandilas-ranktransforms/#fig-unequal-main-2) and Figure 25 **are Type I error rates, regardless of the scale.** The same holds for the results in Figures 10, 11, 15, 16, 18, 19, 20, etc. (as well as the numerous examples we presented in our previous posts). Please, @mjskay, we need an honest answer to this question: Aren't these rates Type I error rates regardless of which scale we use to define the null hypothesis? What is the source of the doubt here? If there are any doubts, let's examine them in more detail rather than spreading confusion.

### Limitations of ordinal latent models
We are aware of the limitation of these models, and our paper does not claim that *"we should always be using the latent scale"* as @mjskay claims. See our recommendations about [ordinal data](https://www.journalovi.org/2024-tsandilas-ranktransforms/#analyzing-likert-type-data):  

*"In conclusion, while ANOVA seems to be a valid method for hypothesis testing even based on the analysis of individual Likert-item responses, we identified situations that require special attention. Unfortunately, nonparametric approaches may not necessarily work well in these situations..."*

We don't say "don't use ANOVA" (but of course we say "don't use ART!"). 

Also: 
*"These models [ordered probit models] offer numerous advantages, including their ability to support interval estimation and prediction in addition to null hypothesis testing. However, researchers are required to acquaint themselves with concepts of generalized mixed-effects models or Bayesian inference in order to use them correctly. We hope that our examples in the paper and the supplementary materials can inspire the community to give these methods more thoughtful consideration."* 

Would @mjskay disagree with these statements? We would be happy to revise the recommendations based on the feedback of our reviewers. However, we feel that it is unfortunate to use the limitations of these models (our any other models) to discredit our efforts in searching for better alternative methods, while also spreading the idea that *"oh, look all models fail, so if ART fails under some circumstances is normal."* This ignores the fact that ART is widely used in practice as a fits-all method and that it severely fails in clear situations for which it was believed to work. 

Also, please, check our discussion and examples in [Section 3 -- Interaction interpretation issues](https://www.journalovi.org/2024-tsandilas-ranktransforms/#interaction-interpretation-problems). People are not aware of these issues, and I will insist that latent modeling helps to (at least) better understand them. See the example of the [perceived performance on the figure](https://www.journalovi.org/2024-tsandilas-ranktransforms/#fig-interactions). Isn't latent modeling here a better approach for understanding such phenomena? HCI (adn not only) papers are full of such "significant interactions" that mean absolutely nothing. See Fig.6 and Table 2 in [this paper](https://ieeexplore.ieee.org/document/10274140). Are all these statistically significant interactions meaningful? (besides the fact that all analyses were conducted with ART).    

### A final comment on the example of Liddell and Kruschke
Although we agree with many of the points of @mjskay, this specific example does not demonstrate that parametric ANOVA is necessarily a good choice. It happens to result in a more intuitive decision in this particular case, but it also fails to model bi-modal distributions. 

The real issue here is the presence of a hidden effect that the modeling method does not capture. For instance, the first film may contain political content, leading to polarized opinions, while the second might be a simple comedy with no political implications. However, such problems are not unique to ordinal scales or these types of models. Yes, if the chosen model fails to accurately capture the underlying processes and effects, the conclusions may also fail. But I don’t see how this relates to the methodology we use in the paper.

Finally, notice that @mjskay used the full information of histograms to make a decision. In fact, my own experience suggests that making statistical inferences with such ordinal scales that represent preferences is rarely useful. Plotting the detailed frequencies and visually inspecting the results is often the best and most informative approach.


# Folow-up

This is fair enough—I understand. However, I would accept this criticism only if our approach consisted solely of taking real data and comparing the results of different methods, as we do in Section 6 - Case Studies. Yes, this approach provides a weak argument since there is no known ground truth, and many things can go wrong. In simulations, however, we have full control over our populations, so we do know the ground truth.

For Likert data, we could have simply used an ad hoc method to generate integer values, like Lüpsen (2017), and the conclusions would change very little. Instead, we chose to use an existing ordinal model because previous work suggests that it produces more realistic data while also allowing for more systematic control of effects. 

I want to emphasize that the great advantage of our methodology is its flexibility. We can generate data for any distribution using the exact same approach by simply changing a single line of code. This also allows us to present results consistently across different distributions by varying the same parameters. Initially, we started with simulations similar to those I uploaded for @mjskay. But for each distribution, one would need to devise a different way to control effects, making it difficult to present results in a coherent and systematic manner. Notice that Lüpsen (2018) conducted a large number of simulations, but understanding his results is quite challenging due to such issues.

