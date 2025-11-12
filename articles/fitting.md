# fitting

It helps to understand the difference between a distribution and a
distribution *family* for the use of `famish`: the *family* is simply a
set of distributions, not just one distribution. When you hear about
“the Normal distribution”, for example, usually it’s actually referring
to the entire *family* of Normal distributions, each having their own
mean and variance. Once you’ve identified the specific mean and
variance, you’ve identified a specific *distribution* within that
family.

Usually we call a family of distributions one that can be index by one
or more numeric parameters. For the Normal family, it can be indexed by
the mean and standard deviation. For the Beta family, it’s often indexed
by two shape parameters, $\alpha > 0$ and $\beta > 0$. Don’t get
confused: just because the Beta family is not (usually) indexed by the
mean and standard deviation, doesn’t mean that the mean and standard
deviation don’t exist: they can still be calculated, it’s just that the
family is more usefully indexed by $\alpha$ and $\beta$. The Normal
family is more usefully indexed by the mean and standard deviation.

With that understanding, the goal of `famish`, then, is to take a family
and reduce it to those families that are compatible with the data
provided – whether the data be a usual recorded dataset, or elicited
expert judgement in the form of quantiles or another reference
distribution. There are two approaches:

- **Fitting** (or **estimation**) narrows the family down to a single
  distribution, and is achieved in `famish` by functions prefixed by
  `dst_`.
- **Refining** narrows the family down to a smaller family of
  distributions based on provided criteria. This functionality is not
  yet available in `famish`.

Distribution *fitting* is more strict because it requires exactly one
distribution. You can specify what to do if it fails to do so with
`on_unres` – either throw an error or return a Null distribution
(default). Fitting can fail as a result of an error in the fitting
process (e.g., trying to fit an Exponential distribution to negative
data), or because the infrastructure is not in place (e.g., currently no
support is available for fitting Binomial or Hypergeometric
distributions).

`famish` right now is a thin wrapper with smart defaults. A future
version will allow for user control about the fitting procedure.

The use of the thin wrappers like
[`fit_dst_gev()`](https://famish.netlify.app/reference/fit_dst_family_wrappers.md)
are also useful because they tell you what `methods` are available.
**definitive source!**

- How it works: wrappers are made around each package, and are
  dispatched by
  [`fit_dst()`](https://famish.netlify.app/reference/fit_dst.md). These
  wrappers keep the behaviour of the wrapped packages: throw an error.
  The `fitdistrplus::fit_dist()` function is similar to the
  [`famish::fit_dst()`](https://famish.netlify.app/reference/fit_dst.md)
  function, except returns a probaverse distribution. It is therefore
  fallen back on if `ismev` and `lmom` are not called.

- When calling fitdistrplus::fitdist(), default arguments are used
  except where specified here otherwise (I think just `start` values).
  There are no options to change other arguments at the moment; if you
  need that level of control, for now you can call fitdistrplus directly
  and then make a distionary distribution from the parameters.

- Warning happens when a distribution is not supported.

- Why some combinations are not supported:

  - “mse”: the estimate does not appear to be consistent (i.e. does not
    converge to the true parameter as sample size increases) for many
    distributions, and is therefore excluded.

- Testing:

  - Failures:
    - Data that deliberately trips up the fitting process are checked.
      e.g., incompatible moments; support issues (either before or after
      fitting). Method-distribution pairs that do not exist.
  - Check convergence to within a specified tolerance of the actual
    parameters. If tolerance is not reached, increase sample size, but
    only up to a point. Sometimes fitting fails because the algorithm
    fails (even after adding more data), but four seeds are used, as a
    stress test to ensure the fitting still resolves to a distribution,
    and at least one is required to pass. Four different seeds ensures
    that convergence to within tolerance is less likely to be a fluke.
    - Checked multiple distributions under each supported method.

- Most distributions in distionary are covered, except for
  Hypergeometric and Binomial.

- The most flexible fitting package is fitdistrplus, and is fallen back
  on. Other wrapped packages include lmom (for l-moments based
  estimation) and ismev (for MLE estimation of the GEV and GP
  distributions).

- scoring functions (uscore, etc) are included so you can compare the
  fitted distribution with empirical scores.
