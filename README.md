# A comprehensive approach to analyzing environmental data with non-detects

Non-detects—measurements below a detection limit—are ubiquitous in
environmental science and engineering. They are frequently replaced with
a constant, but this biases estimates of means, regression slopes, and
correlation coefficients. Omitting non-detects is even worse and has led
to serious errors. Simple alternatives are available: rank-based
statistics, maximum likelihood estimation, and re-purposed survival
analysis routines. But many environmental datasets do not fit neatly
into the confines of these methods—it is often necessary to account for
hierarchy (e.g., measurements nested within lakes), sampling strategy
(e.g., measurements collected as time series), heterogeneity (e.g.,
site-dependent variance), and measurement error. Bayesian methods offer
the flexibility to do this; incorporating non-detects is also easy and
does not bias model parameter estimates as substitution does. Here we
discuss Bayesian implementations of common bivariate and multivariate
statistical methods with relevance to environmental science. We use a
dataset comprising time series of Ag, As, Cd, Ce, Co, Sb, Ti, U, and V
concentrations in municipal biosolids that includes many non-detects.
The models can be reproduced and extended to new problems using the data
and code that accompany this paper.

# Introduction

Non-detects—measurements recorded as less than a detection or reporting
limit—are ubiquitous in environmental science and engineering. In the
statistical literature, they are known as left-censored observations. A
popular method of representing them in statistical routines is to
replace them with one-half, or some other fraction, of the detection
limit. But while common, this strategy can severely bias estimates based
on the data. Worse still is omission—leaving out non-detects has led to
serious and well-documented errors.<sup>1</sup>

For basic tasks like comparing two groups or estimating a mean, linear
regression slope, or correlation coefficient, there are simple
alternatives to substitution and omission. These include rank-based
methods, maximum likelihood estimation, and re-purposed survival
analysis routines.<sup>1</sup> But many environmental datasets require
more complex models that account for hierarchy, sampling strategy,
heterogeneity, and measurement error. For instance, measurements may be
collected across multiple lakes with different characteristics or they
may be recorded as time series (i.e., serially dependent data). The
standard toolbox for censored data analysis does not always accommodate
these features.

Bayesian methods excel in this context—since the sampling techniques
they rely on provide a near-universal approach to parameter estimation,
they can be very flexible. In particular, it is straightforward to
account for non-detects in almost any model. Here, we provide examples
of common statistical models in environmental science and engineering
whose Bayesian versions can easily accommodate non-detects. They are
reproducible via the code and data that accompany this paper.

# Materials and methods

## Data collection

We fit models to a dataset comprising concentrations of Ag, As, Cd, Ce,
Co, Sb, Ti, U, and V in municipal biosolids. Biosolids samples were
collected from the clarifiers of three wastewater treatment facilities
in 125 mL polypropylene bottles. Samples were autoclaved, desiccated by
baking at 105°C for approximately 60 hours, digested according to EPA
Method 3050B,<sup>2</sup> diluted serially, and quantified according to
Standard Method 3125.<sup>3</sup> A summary of the data is available in
Table 1.

<table style="width:100%;">
<caption><strong>Table 1.</strong> A summary of element concentrations
in biosolids samples collected at three wastewater treatment
facilities.</caption>
<colgroup>
<col style="width: 7%" />
<col style="width: 22%" />
<col style="width: 29%" />
<col style="width: 29%" />
<col style="width: 10%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Element</th>
<th style="text-align: right;">Median (µg g<sup>-1</sup>)</th>
<th style="text-align: right;">Lower quartile (µg g<sup>-1</sup>)</th>
<th style="text-align: right;">Upper quartile (µg g<sup>-1</sup>)</th>
<th style="text-align: right;">% censored</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Ag</td>
<td style="text-align: right;">0.8</td>
<td style="text-align: right;">0.6</td>
<td style="text-align: right;">1.1</td>
<td style="text-align: right;">6.4</td>
</tr>
<tr class="even">
<td style="text-align: left;">As</td>
<td style="text-align: right;">4.9</td>
<td style="text-align: right;">3.7</td>
<td style="text-align: right;">6.8</td>
<td style="text-align: right;">1.7</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Cd</td>
<td style="text-align: right;">0.7</td>
<td style="text-align: right;">0.5</td>
<td style="text-align: right;">1.0</td>
<td style="text-align: right;">6.4</td>
</tr>
<tr class="even">
<td style="text-align: left;">Ce</td>
<td style="text-align: right;">12.2</td>
<td style="text-align: right;">8.4</td>
<td style="text-align: right;">22.2</td>
<td style="text-align: right;">1.2</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Co</td>
<td style="text-align: right;">2.3</td>
<td style="text-align: right;">1.6</td>
<td style="text-align: right;">3.1</td>
<td style="text-align: right;">1.7</td>
</tr>
<tr class="even">
<td style="text-align: left;">Sb</td>
<td style="text-align: right;">0.2</td>
<td style="text-align: right;">0.1</td>
<td style="text-align: right;">0.2</td>
<td style="text-align: right;">77.5</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Ti</td>
<td style="text-align: right;">30.1</td>
<td style="text-align: right;">17.3</td>
<td style="text-align: right;">47.1</td>
<td style="text-align: right;">1.2</td>
</tr>
<tr class="even">
<td style="text-align: left;">U</td>
<td style="text-align: right;">0.7</td>
<td style="text-align: right;">0.5</td>
<td style="text-align: right;">1.0</td>
<td style="text-align: right;">9.2</td>
</tr>
<tr class="odd">
<td style="text-align: left;">V</td>
<td style="text-align: right;">4.4</td>
<td style="text-align: right;">3.2</td>
<td style="text-align: right;">5.9</td>
<td style="text-align: right;">1.7</td>
</tr>
</tbody>
</table>

**Table 1.** A summary of element concentrations in biosolids samples
collected at three wastewater treatment facilities.

## Data analysis

The data and code necessary to reproduce the main results from the paper
are available at
<https://github.com/bentrueman/censored-env-data-analysis>; several
functions used to fit the models in Stan are available in a separate R
package.<sup>4</sup> We used R version 4.3.3 throughout,<sup>5</sup>
along with a collection of contributed packages.<sup>6–12</sup>

# Results and discussion

## Bayesian modeling

Bayesian inference entails fitting a probability model to data, then
summarizing it as the joint distribution of the model parameters,
*θ*.<sup>13</sup> The model starts as a prior, *P*(*θ*), quantifying the
plausibility of all possible parameter values. The prior reflects
background knowledge and practical considerations.<sup>14</sup>

The data, *x*, are used to update the prior via Bayes’ theorem. It
relates the posterior or updated joint parameter distribution,
*P*(*θ*|*x*), with the prior and the likelihood. The likelihood,
*P*(*x*|*θ*), quantifies the compatibility of the data with the proposed
model.

In practice, model fitting follows these basic steps:

1.  Assign a probability distribution to the data.
2.  Choose a model for each of its parameters.
3.  Choose a distribution of prior probability for each parameter.
4.  Iterate the following steps to obtain a sample from the posterior:
    1.  Propose values for all parameters.
    2.  Quantify their plausibility without reference to the data (via
        the prior distributions).
    3.  Quantify the plausibility of each data point given the assumed
        data distribution and the proposed parameter values (i.e., the
        likelihood).
    4.  Obtain the relative, or unnormalized, posterior probability as
        the product of the likelihood and the prior (i.e., Bayes’
        theorem).

Iterating over steps 4 (a–d) may require searching a high-dimensional
parameter space, which is often accomplished via the Hamiltonian Monte
Carlo algorithm.<sup>14</sup> Fortunately, software packages make this
straightforward: the R package *brms*<sup>7</sup>, for instance, fits a
huge variety of Bayesian regression models—including censored data
models—with a standard and familiar syntax. Further customization is
possible using Stan,<sup>15</sup> a programming platform for Bayesian
statistics written in C++.

## Substitution biases parameter estimates

Replacing non-detects with a constant can bias parameter estimates
substantially. We show this using a small simulation study that compares
substitution at one-half the detection limit with a parameter estimation
strategy that relies on the cumulative distribution function.

When the dependent variable in a linear regression model includes
left-censored observations, one method of accounting for them is to
construct the likelihood for every censored observation using the
appropriate cumulative distribution function in place of the probability
density function. Here, the cumulative distribution function quantifies
the probability that a data point is less than the detection limit—that
is, the compatibility of a non-detect with the proposed model. The
likelihood, then, becomes
*P*(*x*|*θ*) = *F*(*x*<sub>*observed*</sub>|*θ*)*G*(*x*<sub>*censored*</sub>|*θ*),
where *F* and *G* are the probability density and cumulative
distribution functions, respectively.

We simulated from a simple linear regression model,
*y*<sub>*i*</sub> ∼ *N*(*μ*<sub>*i*</sub> = 3 + 0.15*x*<sub>*i*</sub>, *σ* = 0.5),
where the dependent variable was partially censored—here, *N* represents
the normal distribution with mean *μ*<sub>*i*</sub> and standard
deviation *σ*. We fit a censored regression using *brms* where the
simulated non-detects were modeled using the normal cumulative
distribution function. We compared it to a naive model where the
non-detects were replaced with one-half the detection limit.

Specifically, the *i* simulated observations, *y*<sub>*i*</sub>, were
modeled as follows. Except for the special handling of left-censored
observations
(*y*<sub>*i*</sub>|*censored*<sub>*i*</sub> = 1), the
naive model was identical.

$$
(1)~~
\begin{align}
\text{likelihood:} \\
y\_i | censored\_i=0 \sim N(\mu\_i, \sigma)~~\[observed\] \\
y\_i | censored\_i=1 \sim N\text{-}CDF(\mu\_i, \sigma)~~\[left \text{-} censored\] \\
\\
\text{model for }\mu\text{:} \\
\mu\_i = \beta\_0 + \beta\_1 x\_i \\
\\
\text{priors:} \\
\beta\_j \sim T(\mu\_\beta = 0, \sigma\_\beta = 2.5, \nu\_\beta = 3), \text{ for } \textit{j} = 0,1 \\
\sigma \sim T(\mu\_\sigma = 0, \sigma\_\sigma = 2.5, \nu\_\sigma = 3) \\
\end{align}
$$

In equation (1), *censored*<sub>*i*</sub> is a binary
variable (0 = *observed*,
1 = *left*-*censored*), and *N*-*C**D**F* is the
normal cumulative distribution function (i.e., *P*(*X* ≤ *x*), the
probability that a random variable *X* is less than or equal to some
value *x*).<sup>14,15</sup> The parameters *β*<sub>0</sub> and
*β*<sub>1</sub> define the linear model for *μ*<sub>*i*</sub>, and *T*
denotes the t distribution with degrees of freedom—which controls
probability density in the tails—parameterized by *ν*.

<figure>
<img
src="https://github.com/bentrueman/censored-env-data-analysis/blob/main/figures/figure-simulation.png"
alt="Figure 1. (a) One iteration of the linear regression simulation. The model that accounts for left-censoring via the cumulative distribution function recovered the true model parameters well, whereas the naive model that used substitution at one-half the detection limit was biased. (b) The same pattern was evident across the entire simulation: the censored regression model recovered the true parameters well and the naive model was biased." />
<figcaption aria-hidden="true"><strong>Figure 1. (a)</strong> One
iteration of the linear regression simulation. The model that accounts
for left-censoring via the cumulative distribution function recovered
the true model parameters well, whereas the naive model that used
substitution at one-half the detection limit was biased.
<strong>(b)</strong> The same pattern was evident across the entire
simulation: the censored regression model recovered the true parameters
well and the naive model was biased.</figcaption>
</figure>

<br> The censored regression model recovered the true parameter values
much more accurately than the naive model (Figure 1). That is, the
censored model yielded 95% credible intervals on the intercept, slope,
and residual standard deviation that included the true parameter values
96, 98, and 97% of the time, respectively. The naive model yielded
intervals that included the true values just 2, 14, and 1% of the time.

## Accounting for non-detects in a more complex model

The same strategy can be incorporated into more complex models: here, we
use a dataset of metals concentrations in municipal biosolids to
demonstrate fitting a smoothing spline. This type of model might be used
to compare time series while accounting for variation over time in the
mean. It was fitted using *bgamcar1*,<sup>4</sup> an extension of *brms*
that accommodates continuous-time autoregression—accounting for the
dependence of one observation on the previous one in unequally spaced
time series.<sup>16,17</sup> Titanium concentrations at three wastewater
treatment facilities (Sites 1–3) were modeled as follows:

$$
(2)~~
\begin{align}
\text{likelihood}: \\
log(\[Ti\]\_t) | censored\_t = 0 \sim T(\mu\_t,\sigma,\nu)~~\[observed\] \\ 
log(\[Ti\]\_t) | censored\_t = 1 \sim T\text{-}CDF(\mu\_t,\sigma,\nu)~~\[left \text{-} censored\] \\
\\
\text{model for }\mu\_t\text{:} \\
\mu\_t = \alpha + \beta\_{site} X\_{site} + f(t) + \phi^s r\_{t-s} \\
r\_{t-s} = log(\[Ti\]\_{t-s}) - \alpha - f(t-s) \\
\\
\text{priors}: \\
\sigma \sim Half \text{-} T(\mu\_\sigma=0, \sigma\_\sigma=2.5, \nu\_\sigma=3) \\
\beta\_{site} \sim T(\mu\_{\beta} = 0, \sigma\_{\beta} = 2.5, \nu\_{\beta} = 3) \\
\nu \sim Gamma(\mu\_\nu=2,\alpha\_\nu=0.1) \\
\phi \sim N(\mu\_\phi=0.5, \sigma\_\phi=0.5) \\
\alpha \sim T(\mu\_\alpha=0, \sigma\_\alpha=2.5, \nu\_\alpha=3)
\end{align}
$$

where, in addition to the symbols defined above, *Half* − *T*
represents the positive-valued t distribution and *G**a**m**m**a* the
gamma distribution, parameterized by mean *μ* and shape parameter *α*.
The linear model *β*<sub>*site*</sub>*X*<sub>*site*</sub>
estimates a separate intercept for each site, where
*X*<sub>*site*</sub> is the design matrix and
*β*<sub>*site*</sub> the coefficients. The autocorrelation
coefficient, *ϕ*<sup>*s*</sup>, and the residual at time *t* − *s*,
*r*<sub>*t* − *s*</sub>, define the dependence of each observation on
the previous one, where *s* is the spacing between adjacent
observations.<sup>16,17</sup>

The term *f*(*t*) is a smooth spline function that captures nonlinear
variation in the mean with time. It takes the following form:

$$
(3)~~
\begin{align}
f(t) = X\_{spline}\beta\_{spline} + Zb \\
\\
\text{priors on spline parameters}: \\
\beta\_{spline} \sim T(\mu\_{\beta} = 0, \sigma\_{\beta} = 2.5, \nu\_{\beta} = 3)~~\[unpenalized\] \\
b \sim N(0, \sigma\_b)~~\[penalized\] \\
\sigma\_b \sim  Half \text{-} T(\mu\_{\sigma\_b} = 0, \sigma\_{\sigma\_b} = 2.5, \nu\_{\sigma\_b} = 3) \\
\end{align}
$$

where *Z* and *X*<sub>*spline*</sub> are matrices representing
the penalized and unpenalized basis functions, while
*β*<sub>*spline*</sub> and *b* represent the corresponding
spline coefficient vectors.<sup>18</sup>

<figure>
<img
src="https://github.com/bentrueman/censored-env-data-analysis/blob/main/figures/figure-censored-response.png"
alt="Figure 2. Titanium concentration time series representing biosolids collected at three locations (light lines). Model predictions are superimposed in bold, the shaded regions represent a 95% credible interval on the posterior mean, and non-detects are shown as vertical dashed lines extending to the detection limit. A single value beyond the plot limits is annotated." />
<figcaption aria-hidden="true"><strong>Figure 2.</strong> Titanium
concentration time series representing biosolids collected at three
locations (light lines). Model predictions are superimposed in bold, the
shaded regions represent a 95% credible interval on the posterior mean,
and non-detects are shown as vertical dashed lines extending to the
detection limit. A single value beyond the plot limits is
annotated.</figcaption>
</figure>

<br> Geometric mean titanium concentrations varied in a quasi-seasonal
pattern (Figure 2), and samples collected at one facility, Site 3, had
mean concentrations approximately 20–30 µg g<sup>-1</sup> higher than
those representing the other two facilities. Observations exhibited mild
serial correlation, which quantifies the dependence of each observation
on the previous one, after accounting for trends and site-specific
variation. The serial correlation parameter in the model, *ϕ*, had a
posterior median of 0.11 with a 95% credible interval spanning 0.04–0.25
In general, accounting for serial correlation improves the accuracy of
predictions and helps avoid overfitting.<sup>17</sup>

## A censored predictor

Left-censoring may also occur in the predictor variable. One potential
application is building a linear regression model to predict missing
values in one variable using another, partially censored, variable.
Left-censored predictors, though, are not amenable to substituting a
cumulative distribution function in the likelihood—another strategy is
required.

<figure>
<img
src="https://github.com/bentrueman/censored-env-data-analysis/blob/main/figures/figure-censored-predictor.png"
alt="Figure 3. Cobalt concentrations as a function of cadmium concentrations in biosolids from three treatment facilities. Nondetect cadmium concentrations are represented as horizontal lines extending to the detection limit. A robust (Student t) linear model is superimposed in blue and the shaded region represents a 95% credible interval on the posterior mean. The equivalent non-robust model yields an extremely wide credible interval due to the unusually high cobalt concentration of 309 µg g-1. Coordinates outside the plotting limits are annotated in parentheses." />
<figcaption aria-hidden="true"><strong>Figure 3.</strong> Cobalt
concentrations as a function of cadmium concentrations in biosolids from
three treatment facilities. Nondetect cadmium concentrations are
represented as horizontal lines extending to the detection limit. A
robust (Student t) linear model is superimposed in blue and the shaded
region represents a 95% credible interval on the posterior mean. The
equivalent non-robust model yields an extremely wide credible interval
due to the unusually high cobalt concentration of 309 µg g<sup>-1</sup>.
Coordinates outside the plotting limits are annotated in
parentheses.</figcaption>
</figure>

<br> Fortunately, there is a straightforward alternative: all the
non-detects can be treated as missing values with an upper bound and
represented by parameters in the model. Since Bayesian modeling results
in a set of posterior draws—vectors of plausible parameter values—this
is equivalent to multiple imputation of missing values with an upper—and
optionally a lower—bound. But since it is done in one step, we get a
joint distribution that quantifies the uncertainty and
interrelationships among all the parameters, including the censored
values.<sup>19</sup> This might be important, for instance, when there
is serial dependence in the data.

Another advantage is the size of the imputed dataset: since the model is
fitted to the data just once, it is straightforward to generate several
thousand imputed values, even for complex models. This may not be
practical if values are imputed before model fitting: the conventional
imputation strategy entails fitting one model for each set of imputed
values. Furthermore, it may be difficult to find a multiple imputation
routine that meets all of the needs of a particular data analysis.

Using the Bayesian approach, we can specify any predictive model we
would like to impute the censored values. Since there were only a few
censored observations in this pair of variables (Figure 3), we chose a
simple intercept-only imputation model, fitted using
*bgamcar1*<sup>4</sup>. It was defined as follows:

$$
(4)~~
\begin{align}
\text{likelihood:} \\
\[Co\]\_i \sim T(\mu\_{\[Co\]\_i}, \sigma\_{\[Co\]}, \nu\_{\[Co\]}) \\
\[Cd\]\_i \sim T(\mu\_{\[Cd\]}, \sigma\_{\[Cd\]}, \nu\_{\[Cd\]}) \\
\\
\text{model for }\mu\text{:} \\
\mu\_{\[Co\]\_i} = \beta\_{0\_{\[Co\]}} + \beta\_1 \[Cd\]\_i \\
\mu\_{\[Cd\]\_i} = \beta\_{0\_{\[Cd\]}} \\
\\
\text{priors:} \\
\beta\_j \sim T(\mu\_\beta = 0, \sigma\_\beta = 2.5, \nu\_\beta = 3), \text{ for } \textit{j} = 0,1 \\
\sigma \sim T(\mu\_\sigma = 0, \sigma\_\sigma = 2.5, \nu\_\sigma = 3) \\
\nu \sim Gamma(\mu\_\nu=2, \alpha\_\nu=0.1) \\
\end{align}
$$

When the model was fitted with a Gaussian likelihood instead, the
extreme cobalt concentration of 309 µg g<sup>-1</sup> yielded a
posterior mean with an extremely wide credible interval (Figure 3). The
robust model, fitted with a Student t likelihood, yielded a much
narrower credible interval and a posterior mean that was much less
heavily influenced by the extreme value. A disadvantage of both models
is that simulating from them may generate negative concentrations, even
though the posterior mean remains positive over its range. This could be
solved by modeling log-transformed Co concentrations instead, but the
model would then predict geometric mean concentrations on the scale of
measurement.<sup>20</sup>

## Multivariate models

In a multivariate context, the one-step multiple imputation strategy is
often simpler to apply, since multivariate cumulative distribution
functions can be difficult to work with. Two multivariate models with
applications in environmental science are the intercept-only model, used
to estimate a correlation matrix, and principal component analysis.

### A Bayesian correlation matrix

In addition to handling non-detects, Bayesian correlation has the
advantage that it can be readily applied to variables that are best
described using distributions other than the Gaussian. A relevant
example for environmental sciences is robust correlation, where a
Student t distribution is assigned to each variable and its parameters
estimated. This tends to limit the influence of extreme values, which
might otherwise exert undue influence on the estimated correlation
coefficients.

Given *y*, an *N* × *D* matrix containing *N* concentrations of *D*
elements, we can estimate the pairwise correlations as follows:

$$
(5)~~
\begin{align}
\text{likelihood:} \\
y \sim MultivariateNormal(\begin{bmatrix}\mu\_1 \\\\\ldots \\\\\mu\_D\end{bmatrix}, \Sigma) \\
\Sigma = \begin{bmatrix} \sigma\_1 & \ldots & 0 \\\\\vdots & \ddots & \vdots \\\\0 & \ldots & \sigma\_D\end{bmatrix} R \begin{bmatrix} \sigma\_1 & \ldots & 0 \\\\\vdots & \ddots & \vdots \\\\0 & \ldots & \sigma\_D\end{bmatrix} \\
\\
\text{priors:} \\
\mu\_j \sim T(\mu\_\mu=0,\sigma\_\mu=10,\nu\_\mu=3), \text{ for } \textit{j} = 1,..,D \\
\sigma\_j \sim T(\mu\_\sigma=0,\sigma\_\sigma=10,\nu\_\sigma=3), \text{ for } \textit{j} = 1,..,D \\
R \sim LKJcorr(2)
\end{align}
$$

where the *μ*<sub>1...*D*</sub> are the column means of *y*, *Σ* is the
covariance matrix, the *σ*<sub>1...*D*</sub> are the column standard
deviations of *y*, and *R* is the correlation matrix.
*L**K**J**c**o**r**r*(2) is a regularizing prior that encodes mild
skepticism of extreme correlation coefficients near -1 or
1.<sup>14</sup>

<figure>
<img
src="https://github.com/bentrueman/censored-env-data-analysis/blob/main/figures/figure-correlation-matrix.png"
alt="Figure 4. (a) Pairwise Bayesian correlations among the elemental concentrations in the dataset, estimated using Student t and Gaussian likelihoods. (b) The robust model—fitted with Student t likelihoods—identifies more correlation than the non-robust model fitted with Gaussian likelihoods." />
<figcaption aria-hidden="true"><strong>Figure 4. (a)</strong> Pairwise
Bayesian correlations among the elemental concentrations in the dataset,
estimated using Student t and Gaussian likelihoods. <strong>(b)</strong>
The robust model—fitted with Student t likelihoods—identifies more
correlation than the non-robust model fitted with Gaussian
likelihoods.</figcaption>
</figure>

<br> Arsenic, vanadium, and cadmium concentrations were most strongly
correlated in biosolids (Figure 4). And overall, the robust
model—incorporating Student t likelihoods—identified more correlation
among the variables than the conventional, non-robust model fitted with
Gaussian likelihoods. This is due to the much smaller influence that
extreme values have on the robust model.

### Probabilistic principal components analysis

Principal component analysis is a method for summarizing a multivariate
dataset using a subset of derived variables that capture the majority of
the data’s variance.<sup>21</sup> Here we implement probabilistic
principal component analysis, a Bayesian generalization of the classical
approach.<sup>22</sup> Our model is modified from the approach described
in a recent paper<sup>23</sup> to accommodate left-censoring of the data
and is written in Stan<sup>8,15</sup>. Given *y*, a *D* × *N* matrix
containing *N* concentrations of *D* elements,

$$
(6)~~
\begin{align}
likelihood: \\
Y \sim MultivariateNormal(Wz+\mu, \sigma I) \\
\\
priors: \\
z \sim N(0, I) \\
W \sim N(0, \sigma \alpha) \\
\mu \sim Lognormal(\mu\_\mu=2.5, \sigma\_\mu=1) \\
\sigma \sim Lognormal(\mu\_\sigma=0,\sigma\_\sigma=1) \\
\alpha \sim Invgamma(\alpha\_\alpha=1,\beta\_\alpha=1)
\end{align}
$$

where *z* is a *k* × *N* matrix of latent (i.e., unobserved) variables
with *k* ≤ *D*, *W* is a *D* × *k* transformation matrix mapping from
the latent space to the data space, *σ* is the standard deviation of the
error (also a latent parameter), *I* is the identity matrix, and
*InvGamma* is the inverse gamma distribution.<sup>13</sup>

<figure>
<img
src="https://github.com/bentrueman/censored-env-data-analysis/blob/main/figures/figure-ppca.png"
alt="Figure 5. (a) The dataset projected onto the first two probabilistic principal components (z in equation (6)). Values appearing outside the extents of the plot are annotated in parentheses at the margins. (b) The first two principal axes; that is, the orthonormalized columns of the transformation matrix W." />
<figcaption aria-hidden="true"><strong>Figure 5. (a)</strong> The
dataset projected onto the first two probabilistic principal components
(<em>z</em> in equation (6)). Values appearing outside the extents of
the plot are annotated in parentheses at the margins.
<strong>(b)</strong> The first two principal axes; that is, the
orthonormalized columns of the transformation matrix <span
class="math inline"><em>W</em></span>.</figcaption>
</figure>

<br> Differences in metals concentrations among the three sites are
apparent in Figure 5a. In particular, Site 3 scored differently on the
two principal components, resulting in substantial separation in the
two-component space from the data representing the other two sites.
Differences in titanium concentrations among treatment facilities play a
strong role here: the principal axes—that is, the orthonormalized
columns of the transformation matrix *W* (equation 6)—load titanium
concentrations heavily. And titanium concentrations were high in
biosolids from Site 3 relative to Sites 1 and 2 (Figure 2).

# Conclusion

Replacing non-detects with a constant—often one-half the detection
limit—biases estimates of means, regression slopes, and correlation
coefficients. Simple alternatives exist, but they are limited and not
always applicable to complex environmental datasets that exhibit
hierarchy, complex dependence structures, and heterogeneity. Bayesian
methods have the flexibility to model all of these complexities, and
they can easily accommodate left-censoring by either modifying the
likelihood or one-step multiple imputation as a part of model fitting.

# Acknowledgements

This work was supported by NSERC through an Alliance Grant (ALLRP
568507–21). We acknowledge Halifax Water staff for facilitating sample
collection at the treatment facilities and Heather Daurie, Alyssa
Chiasson, Evelyne Doré, Jorginea Bonang, Genevieve Erjavec, Sebastian
Munoz, Bofu Li, and Madison Gouthro for laboratory assistance.

# References

<span class="csl-left-margin">(1)
</span><span class="csl-right-inline">Helsel, D. R. *Statistics for
Censored Environmental Data Using Minitab and R*, 2nd ed.; Wiley series
in statistics in practice; Wiley: Hoboken, N.J, 2012.</span>

<span class="csl-left-margin">(2)
</span><span class="csl-right-inline">US Environmental Protection
Agency. Method 3050B: Acid Digestion of Sediments, Sludges, and Soils.
*Test methods for evaluating solid waste, physical/chemical methods*
**1996**.</span>

<span class="csl-left-margin">(3)
</span><span class="csl-right-inline">American Public Health
Association. 3125 Metals by Inductively Coupled Plasma—Mass
Spectrometry. In *Standard methods for the examination of water and
wastewater*; 2018. <https://doi.org/10.2105/SMWW.2882.048>.</span>

<span class="csl-left-margin">(4)
</span><span class="csl-right-inline">Trueman, B.
*<span class="nocase">bgamcar1</span>: Fit bayesian GAMs with CAR(1)
errors to censored data*.
<https://github.com/bentrueman/bgamcar1>.</span>

<span class="csl-left-margin">(5)
</span><span class="csl-right-inline">R Core Team. *R: A language and
environment for statistical computing*.
<https://www.R-project.org/>.</span>

<span class="csl-left-margin">(6)
</span><span class="csl-right-inline">Wickham, H.; Averick, M.; Bryan,
J.; Chang, W.; McGowan, L. D.; François, R.; Grolemund, G.; Hayes, A.;
Henry, L.; Hester, J.; Kuhn, M.; Pedersen, T. L.; Miller, E.; Bache, S.
M.; Müller, K.; Ooms, J.; Robinson, D.; Seidel, D. P.; Spinu, V.;
Takahashi, K.; Vaughan, D.; Wilke, C.; Woo, K.; Yutani, H. Welcome to
the <span class="nocase">tidyverse</span>. *Journal of Open Source
Software* **2019**, *4* (43), 1686.
<https://doi.org/10.21105/joss.01686>.</span>

<span class="csl-left-margin">(7)
</span><span class="csl-right-inline">Bürkner, P.-C.
*<span class="nocase">brms</span>: Bayesian regression models using
stan*. <https://github.com/paul-buerkner/brms>.</span>

<span class="csl-left-margin">(8)
</span><span class="csl-right-inline">Gabry, J.; Češnovar, R.; Johnson,
A. *<span class="nocase">cmdstanr</span>: R interface to CmdStan*.
<https://mc-stan.org/cmdstanr/>.</span>

<span class="csl-left-margin">(9)
</span><span class="csl-right-inline">Fischetti, T.
*<span class="nocase">assertr</span>: Assertive programming for R
analysis pipelines*. <https://docs.ropensci.org/assertr/>.</span>

<span class="csl-left-margin">(10)
</span><span class="csl-right-inline">Müller, K.
*<span class="nocase">here</span>: A simpler way to find your files*.
<https://here.r-lib.org/>.</span>

<span class="csl-left-margin">(11)
</span><span class="csl-right-inline">Pedersen, T. L.
*<span class="nocase">patchwork</span>: The composer of plots*.
<https://patchwork.data-imaginist.com>.</span>

<span class="csl-left-margin">(12)
</span><span class="csl-right-inline">Lawlor, J. *PNWColors: Color
palettes inspired by nature in the US pacific northwest*.
<https://github.com/jakelawlor/PNWColors>.</span>

<span class="csl-left-margin">(13)
</span><span class="csl-right-inline">Gelman, A.; Carlin, J. B.; Stern,
H. S.; Dunson, B., David; Vehtari, A.; Rubin, D. B. *Bayesian Data
Analysis*, Third edition.; Chapman & Hall/CRC texts in statistical
science; CRC Press: Boca Raton, 2014.</span>

<span class="csl-left-margin">(14)
</span><span class="csl-right-inline">McElreath, R. *Statistical
Rethinking: A Bayesian Course with Examples in R and Stan*; Chapman &
Hall/CRC texts in statistical science series; CRC Press/Taylor & Francis
Group: Boca Raton, 2016.</span>

<span class="csl-left-margin">(15)
</span><span class="csl-right-inline">Stan Development Team. *Stan
modeling language users guide and reference manual, version 2.34*.
<http://mc-stan.org/>.</span>

<span class="csl-left-margin">(16)
</span><span class="csl-right-inline">Trueman, B. F.; James, W.; Shu,
T.; Doré, E.; Gagnon, G. A. Comparing Corrosion Control Treatments for
Drinking Water Using a Robust Bayesian Generalized Additive Model. *ACS
ES&T Engineering* **2022**, acsestengg.2c00194.
<https://doi.org/10.1021/acsestengg.2c00194>.</span>

<span class="csl-left-margin">(17)
</span><span class="csl-right-inline">Simpson, G. L. Modelling
Palaeoecological Time Series Using Generalised Additive Models.
*Frontiers in Ecology and Evolution* **2018**, *6*, 149.
<https://doi.org/10.3389/fevo.2018.00149>.</span>

<span class="csl-left-margin">(18)
</span><span class="csl-right-inline">Wood, S. N. *Generalized Additive
Models: An Introduction with R*, 2nd ed.; Chapman; Hall/CRC,
2017.</span>

<span class="csl-left-margin">(19)
</span><span class="csl-right-inline">Hopke, P. K.; Liu, C.; Rubin, D.
B. Multiple Imputation for Multivariate Data with Missing and
Below‐Threshold Measurements: Time‐Series Concentrations of Pollutants
in the Arctic. *Biometrics* **2001**, *57* (1), 22–33.
<https://doi.org/10.1111/j.0006-341X.2001.00022.x>.</span>

<span class="csl-left-margin">(20)
</span><span class="csl-right-inline">Helsel, D. R.; Hirsch, R. M.;
Ryberg, K. R.; Archfield, S. A.; Gilroy, E. J. *Statistical Methods in
Water Resources*; U.S. Department of the Interior; U.S. Geological
Survey, 2020; Vol. Techniques and Methods 4–A3.</span>

<span class="csl-left-margin">(21)
</span><span class="csl-right-inline">Hastie, T.; Tibshirani, R.;
Friedman, J. *The Elements of Statistical Learning*; Springer Series in
Statistics; Springer New York: New York, NY, 2009.
<https://doi.org/10.1007/978-0-387-84858-7>.</span>

<span class="csl-left-margin">(22)
</span><span class="csl-right-inline">Bishop, C. M. *Pattern Recognition
and Machine Learning*; Information science and statistics; Springer: New
York, 2006.</span>

<span class="csl-left-margin">(23)
</span><span class="csl-right-inline">Kucukelbir, A.; Tran, D.;
Ranganath, R.; Gelman, A.; Blei, D. M. [Automatic Differentiation
Variational Inference](http://jmlr.org/papers/v18/16-107.html). *Journal
of Machine Learning Research* **2017**, *18* (14), 1–45.</span>
