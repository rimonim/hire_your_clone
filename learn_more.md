## Where did the data come from?

Data on names of bosses and employees were obtained from [The Official Board](www.theofficialboard.com), which maintains detailed corporate organizational charts for tens of thousands of companies worldwide. The analysis on this site reflects an analysis of 11,702 employee/boss pairs across 779 US companies. Because these data are mostly restricted to high-level executives, the results may or may not be valid for lower-level positions.

## Are my odds the same as the probability that I will get hired?

No. Since the data analyzed here only include people who *were* hired, and not people who were rejected, no conclusions can be made about your true probability of being accepted when you apply to a job.

Also, employees were not always hired by their current direct boss. Sometimes bosses are hired after their employees, and sometimes hiring is managed by other people in the organizational structure.

For these reasons, this analysis can make claims only about **relative** odds that employee/boss pairings will exist. The reasons that such imbalances appear are left for the reader to guess.

## How do you calculate odds?

Odds are calculated using machine learning (specifically - Bayesian multilevel logistic regression) to predict whether employee-boss pairs are real or drawn randomly from the available bosses and employees in the dataset. To learn more about the statistical methodology at work here, [read the blog post](https://rimonim.github.io/projects/hire_your_clone.html).

## Why are Western European and African American in the Same Ethnic Category?

Since the raw data used here did not include gender or ethnicity for each employee and boss, these were inferred from their names using existing machine learning models. [The model used here to predict ethnic background](https://www3.cs.stonybrook.edu/~skiena/lydia/names.pdf) considers African American names to be "British", probably because many African Americans have names of British origin (as do many white Americans). 

## About Me

My name is Louis. To learn more about me, and to see other projects I've done, visit [my website](https://rimonim.github.io).
