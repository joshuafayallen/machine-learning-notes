#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#

prior_heads = rbinom(n = 2, size = 1000, prob = 0.5)

prior_heads[1]/sum(c(prior_heads[1], prior_heads[2]))


#
#
#
#
#
#
prior_heads_biased = rbinom(n = 2, size = 1000, prob = .61)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: check2
#| eval: true

library(tidyverse)


sim_data = \(beta_val = 0, beta_variance = 1, n = 100){
  
  sim_dat = tibble(beta_values = rnorm(n, mean = beta_val, sd = beta_variance),
                   beta_mean = rep(beta_val, length.out = n),
                   beta_variance = rep(beta_variance, length.out = n))

  return(sim_dat)
    
}

pop_sims = map_dfr(c(5, 10, 15), \(x) sim_data(beta_val = x)) 

wide_version = pop_sims |> 
    mutate(id = row_number()) |>
    pivot_wider(names_from = beta_mean, values_from = beta_values, names_glue = 'mean_{beta_mean}') 

plot_dat = wide_version |>
    mutate(pop_total = pop_sims$beta_values) |>
    pivot_longer(cols = mean_5:mean_15) |>
    mutate(nice_labs = as.factor(str_to_title(str_replace(name, '_', " "))), 
           nice_labs  = fct_relevel(nice_labs, 'Mean 5', 'Mean 10', 'Mean 15'))


ggplot(plot_dat) +
geom_density(mapping = aes(x = value, y = after_stat(ncount),   fill = nice_labs), 
             stat = "bin",  size = 0.5,
             alpha = 0.7) + 
geom_density(mapping = aes(x = pop_total, y = after_stat(ncount)), 
                 alpha = 0.9,
                 color = "gray30", size = 0.6, 
             stat = "bin",
             direction = "mid") +
   facet_wrap(vars(nice_labs)) +
   MetBrewer::scale_fill_met_d(name = 'Lakota') +
   theme_minimal() +
   labs(fill = NULL, y = 'Scaled Count') +
   theme(legend.position = 'none')
  
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#


n = 1000

prior_df = tibble(
    pop = rnorm(n),
    `Kinda Flat` = rnorm(n, sd = 5), 
    informative = rnorm(n, mean = 0.5, sd = 1)
) |>
    pivot_longer(everything(),
        names_to = 'prior',
        values_to = 'value'
    )


ggplot(prior_df, aes(x = value, fill = prior)) +
    geom_density(alpha = 0.5) +
    scale_fill_manual(values = c('Kinda Flat' = '#a40000', 'informative' = '#00b7a7', 'pop' = '#ffcd12')) +
    theme_minimal()


#
#
#
#
#
#
#
#
#
#
#
#
#
#

coin_flips = replicate(10000, sample(c('heads', 'tails'), 1))

coin_flips |>
    as_tibble() |>
    group_by(value) |>
    summarise(counts = n()) |>
    ungroup() |>
    mutate(probs = counts/sum(counts))


#
#
#
#
#
#
#

num_flips = 100

flips = sample(c('heads', 'tails'), size = num_flips, replace = TRUE)

coin_flips = tibble(
    heads_frequency = cumsum(flips == 'heads')/1:num_flips,
    flip_number = 1:num_flips

)


ggplot(coin_flips, aes(x = flip_number, y = heads_frequency)) +
    geom_line() +
    geom_hline(yintercept = 0.5) +
    scale_y_continuous(limits = c(0,1)) +
    labs(x = 'Flip Number', y = 'Proportion of Heads') +
    theme_minimal()

#
#
#
#
#
#
#
#

options(scipen = 999)
library(DeclareDesign)

rct <-
  declare_model(N = 100,
                U = rnorm(N),
                potential_outcomes(Y ~ 0.2 * Z + U)) +
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_assignment(Z = complete_ra(N, prob = 0.5)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ Z, inquiry = "ATE")


fake_data = draw_data(rct)


#
#
#
#
#
#
#
diagnose_design(rct)
#
#
#
#
#
#
#
#
control_for_confounding = lm(Y ~ Z + U, data = fake_data)

no_controls = lm(Y ~ Z, data = fake_data)


modelsummary::modelsummary(list('Controls Added' = control_for_confounding,
                                 'No Controls' = no_controls),
                          gof_map = 'nobs',
                          stars = TRUE)
#
#
#
#
#
#
#
#
#
library(patchwork)
library(brms)
library(modelsummary)
library(tidybayes)
library(broom.mixed)

conversions_tibble = tibble(
    Control = runif(100, min = 0, max = 100),
    Treatment = runif(100, min = 0, max = 100),
) |>
    pivot_longer(everything(),
    names_to = 'condition',
    values_to = 'conversion_rate') 

ggplot(conversions_tibble, aes(x = conversion_rate, fill = condition)) +
    geom_density() +
    facet_wrap(vars(condition), ncol = 1) +
    theme_minimal() + 
    labs(x = 'Conversion Rate', y = NULL) +
    theme(legend.position = 'none') 


#
#
#
#
#
#
#
#
#| code-fold: true

conversions_tibble_reasonable = tibble(
    # most in the control group don't convert 
    Control = rbeta(n = 100, shape1 = 3, shape2 = 7) * 10,
    # people in the control group are slightly more likely to convert
    Treatment = rbeta(n = 100, shape1 = 6, shape2 = 4) * 10,
) |>
    pivot_longer(everything(),
    names_to = 'condition',
    values_to = 'conversion_rate') 

ggplot(conversions_tibble_reasonable, aes(x = conversion_rate, fill = condition)) +
    geom_density() +
    facet_wrap(vars(condition), ncol = 1) +
    theme_minimal() + 
    labs(x = 'Conversion Rate', y = NULL) +
    theme(legend.position = 'none') 

#
#
#
#
#
#
#
#
#
#
#| results: hide 
#| cache: true



titanic <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/Stat2Data/Titanic.csv")
titanic <- subset(titanic, PClass != "*")

f <- Survived ~ SexCode + Age + PClass



mod_prior <- brm(PClass ~ SexCode + Age,
    data = titanic,
     prior = c(
        prior(normal(0, 3), class = b, dpar = "mu2nd"),
        prior(normal(0, 3), class = b, dpar = "mu3rd")),
    family = categorical(link = logit),
    sample_prior = "only")


pp_check(mod_prior)   
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#

mod_prior |>
    modelsummary()


#
#
#
#
#
#
#
#
#
# this is a bad model don't judge 
logit_fe = glm(Survived ~ Age + SexCode + factor(PClass), data = titanic, family = binomial(link = 'logit'))

modelsummary(logit_fe)

#
#
#
#
#
#
#| warning: false
#| message: false
#| results: 'hide'
#| cache: true

brm(Survived ~ Age + (1|PClass), data = titanic) |>
   modelsummary()

#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#

data('Howell1', package = 'rethinking')

just_adults = Howell1 |>
    filter(age >= 18) |>
    mutate(height_z = scale(height))

all.equal(just_adults$height_z, (just_adults$height - mean(just_adults$height))/sd(just_adults$height))



priors = c(# this is just weight in kilos so approx 132 lbs 
           prior(normal(60, 10), class = Intercept),
           # this is just centering on the new variable. Now we are also 
           # this is kilos/cm with a spread of 10^2 
           prior(normal(0, 10), class = b, coef = 'height_z'),
           prior(uniform(0,50), class = sigma, ub = 50))

first_cut = brm(
    weight ~ 1 + height_z,
    family = gaussian,
    prior = priors,
    data = just_adults
)


plot(first_cut)

#
#
#
#
#
#

sample_prior = first_cut = brm(
    weight ~ 1 + height_z,
    family = gaussian,
    prior = priors,
    data = just_adults,
    sample_prior = 'only'
)

height_scale <- attributes(just_adults$height_z) %>%
  set_names(janitor::make_clean_names(names(.)))

draws_prior <- tibble(height_z = seq((130 - height_scale$scaled_center) / height_scale$scaled_scale, 
                                     (170 - height_scale$scaled_center) / height_scale$scaled_scale, 
                                     length.out = 1000)) |>
                add_epred_draws(sample_prior, ndraws = 1000) |>
  mutate(height_unscaled = (height_z * height_scale$scaled_scale) + height_scale$scaled_center)

ggplot(draws_prior, aes(x = height_unscaled, y = .epred, group = .draw)) +
    geom_line(alpha = 0.2) +
    coord_cartesian(xlim = c(130, 170), ylim = c(10, 100)) +
    theme_minimal()

#
#
#
#
#
#
#
pp_check(sample_prior)
#
#
#
#
#
#
#

priors2 = c(prior(normal(60, 10), class = Intercept),
            # here we are actually just constraining 
            # beta to be positive 
           prior(lognormal(0, 1), class = b, lb = 0 ),
            prior(uniform(0,10), class = sigma, ub = 10))

sample_prior2 =  brm(
    weight ~ 1 + height_z,
    family = gaussian,
    prior = priors2,
    data = just_adults,
    sample_prior = 'only'
)



draws_prior2 <- tibble(height_z = seq((130 - height_scale$scaled_center) / height_scale$scaled_scale, 
                                     (170 - height_scale$scaled_center) / height_scale$scaled_scale, 
                                     length.out = 1000)) |>
                add_epred_draws(sample_prior2, ndraws = 1000) |>
  mutate(height_unscaled = (height_z * height_scale$scaled_scale) + height_scale$scaled_center)

ggplot(draws_prior2 ,aes(x = height_unscaled,  y = .epred, group = .draw)) +
    geom_line(alpha = 0.2) +
    theme_minimal()
 

#
#
#
#
#
#
pp_check(sample_prior2)
#
#
#
#
#
#
#
#

final_model = first_cut = brm(
    weight ~ 1 + height_z,
    family = gaussian,
    prior = priors2,
    data = just_adults
)


final_draws = tibble(height_z = seq(min(just_adults$height_z), max(just_adults$height_z), length.out = 500)) |> 
  add_predicted_draws(final_model, ndraws = 100) |>
  mutate(height_unscaled = (height_z * height_scale$scaled_scale) + height_scale$scaled_center)


ggplot(just_adults, aes(x = height, y  = weight)) +
    geom_point(alpha = 0.5) +
    stat_lineribbon(data = final_draws,
    aes(x = height_unscaled, y = .prediction), .width = 0.95, alpha = 0.2, inherit.aes = FALSE) +
    theme_minimal() +
    theme(legend.position = 'none')


#
#
#
#
#
#
pp_check(final_model)
#
#
#
#
#
#

ggplot(just_adults, aes(x = height, y = weight, color = as.factor(male))) +
    geom_point() +
    theme_minimal()



#
#
#
#
#
#
library(ggdag)
library(dagitty)

coords = list(x = c(Height = 0, Sex = 1, Weight = 2),
              y = c(Height = 0, Sex  = 1, Weight = 0))

labs = c(Height = 'Height', Sex = 'Sex', Weight = 'Weight')

dagify(Weight ~ Height + Sex,
        Height ~ Sex,
        outcome = 'Weight',
        exposure = 'Height',
        labels = labs,
        coords = coords) |>
    ggdag_status(use_labels = 'label', text = FALSE) +
    guides(fill = 'none', color = 'none') +
    theme_dag()




#
#
#
#
#
#

just_adults = just_adults |>
    mutate(sex = factor(male),
           height_z = as.numeric(height_z))

sex_priors = c(
    # we do be because we are going to specify a model sans intercept
    prior(normal(60, 10), class = b),
    prior(uniform(0,10), class = sigma, lb = 0, ub = 10)
)

sex_model = brm(weight ~ 0 + sex,
                prior = sex_priors,
                data = just_adults)

sex_model
#
#
#
#
#
#

sw_post_means = sex_model |>
    gather_draws(b_sex0, b_sex1)


ggplot(sw_post_means, aes(x = .value, fill = .variable)) +
    stat_halfeye() +
    labs(fill = NULL, x = 'Posterior mean weight') +
    theme_minimal() +
    theme(legend.position = 'bottom')

#
#
#
#
#
#
diffs_manual = sw_post_means |>
    pivot_wider(names_from = .variable, values_from = .value) |>
    mutate(diff = b_sex1 - b_sex0)

diffs_brms = sex_model |>
    spread_draws(b_sex0, b_sex1) |>
    mutate(diff = b_sex1 - b_sex0)

tinytest::expect_equal(diffs_brms, diffs_manual)


#
#
#
#
#
#
#

library(marginaleffects)

me_contrasts = get_draws(avg_comparisons(sex_model))

tinytest::expect_equal(me_contrasts$draw, diffs_brms$diff)

ggplot(me_contrasts, aes(x = draw)) +
    stat_halfeye() +
    labs(x = 'posterior contrasts') +
    theme_minimal()

#
#
#
#
#
#

preds =  tibble(sex = c("0", "1")) |> 
  add_predicted_draws(sex_model, ndraws = 1000)

ggplot(preds, aes(x = .prediction, fill = sex)) +
    stat_halfeye() +
    theme_minimal() +
    theme(legend.position = 'bottom') 

#
#
#
#
#
#
#

check_sex_priors = brm(
    weight ~ 0 + sex,
    prior = sex_priors,
    data = just_adults,
    sample_prior = 'only'
)


pp_check(check_sex_priors)

#
#
#
#
#
#
#

priors <- c(prior(normal(60, 10), class = b, coef = 'sex1'),
            prior(lognormal(0, 1), class = b, coef = 'sex1:height_z'),
            prior(uniform(0, 10), class = sigma, lb = 0, ub = 10))

model_height_sex <- brm(
  bf(weight ~ 0 + sex + sex:height_z),
  data = just_adults,
  family = gaussian(),
  prior = priors
)

priors2 <- c(prior(normal(60, 10), class = b, nlpar = a),
            prior(lognormal(0, 1), class = b, nlpar = b, lb = 0),
            prior(uniform(0, 10), class = sigma, lb = 0, ub = 10))

model_height_sex_wonky =brm(
  bf(weight ~ 0 + a + b * height_z,
     a ~ 0 + sex,
     b ~ 0 + sex,
     nl = TRUE),
  data = just_adults,
  family = gaussian(),
  prior = priors2,
  chains = 4, cores = 4
)

# model_height_sex 
# Regression Coefficients:
#               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sex0             45.15      0.45    44.27    46.00 1.00     2560     2372
# sex1             45.15      0.46    44.22    46.04 1.00     2734     2716
# sex0:height_z     5.08      0.48     4.14     6.02 1.00     2846     2773
# sex1:height_z     4.65      0.42     3.82     5.47 1.00     2716     2618
# Further Distributional Parameters:
#       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sigma     4.27      0.16     3.96     4.60 1.00     4054     2961

# model_height_sex_wonky
# Regression Coefficients:
#               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sex0             45.15      0.45    44.27    46.00 1.00     2560     2372
# sex1             45.15      0.46    44.22    46.04 1.00     2734     2716
# sex0:height_z     5.08      0.48     4.14     6.02 1.00     2846     2773
# sex1:height_z     4.65      0.42     3.82     5.47 1.00     2716     2618
# 
# Further Distributional Parameters:
#       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sigma     4.27      0.16     3.96     4.60 1.00     4054     2961
#
#
#
#
#
#

sex_height_weight_post_pred <- expand_grid(
  height_z = seq(min(just_adults$height_z), max(just_adults$height_z), length.out = 50),
  sex = 0:1
) |> 
  add_predicted_draws(model_height_sex, ndraw = 4000) |>
  compare_levels(variable = .prediction, by = sex, comparison = list(c("0", "1"))) |> 
  mutate(height_unscaled = (height_z * height_scale$scaled_scale) + height_scale$scaled_center)


ggplot(sex_height_weight_post_pred, aes(x = .prediction)) +
  stat_halfeye(fill = 'red') +
  labs(x = "Posterior mean weight contrast (kg)\nWomen − Men", y = "Density") +
  theme_minimal()



#
#
#
#
#
#
#
#
#
#
#
dagify(Weight ~ Height,
       Height ~ Sex) |>
    ggdag() +
    theme_dag()
#
#
#
#
#
#
#
#| cache: true
priors <- c(prior(normal(60, 10), resp = weight, class = b, nlpar = a),
            prior(lognormal(0, 1), resp = weight, class = b, nlpar = b, lb = 0),
            prior(uniform(0, 10), resp = weight, class = sigma, lb = 0, ub = 10),
            # prior(normal(160, 10), resp = height, class = b),
            prior(normal(0, 1), resp = heightz, class = b),
            prior(uniform(0, 10), resp = heightz, class = sigma, lb = 0, ub = 10))

model_luxury <- brm(
  bf(weight ~ 0 + a + b * height_z,
     a ~ 0 + sex,
     b ~ 0 + sex,
     nl = TRUE) + 
    bf(height_z ~ 0 + sex) + 
    set_rescor(TRUE),
  data = just_adults,
  family = gaussian(),
  prior = priors
)


luxury_post_mean_diff <- expand_grid(
  height_z = seq(min(just_adults$height_z), max(just_adults$height_z), length.out = 50),
  sex = 0:1
) |> 
  add_epred_draws(model_luxury) |>
  compare_levels(variable = .epred, by = sex, comparison = list(c("1", "0")))

luxury_post_mean_diff |> 
  filter(.category == "weight") |> 
  ggplot(aes(x = .epred)) +
  stat_halfeye(fill = 'pink') +
  labs(x = "Posterior mean weight contrast (kg)\nWomen − Men", y = "Density") +
  theme_minimal()

#
#
#
#
#
#
#
#
#
#
#
#| code-fold: true
library(palmerpenguins)
library(scales)
library(patchwork)
library(ggtext)

theme_pred <- function() {
  theme_minimal(base_family = "Roboto Condensed") +
    theme(panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "white", color = NA),
          plot.title = element_text(face = "bold"),
          strip.text = element_text(face = "bold"),
          strip.background = element_rect(fill = "grey80", color = NA),
          axis.title.x = element_text(hjust = 0),
          axis.title.y = element_text(hjust = 0),
          legend.title = element_text(face = "bold"))
}

theme_pred_dist <- function() {
  theme_pred() +
    theme(plot.title = element_markdown(family = "Roboto Condensed", face = "plain"),
          plot.subtitle = element_text(family = "Roboto Mono", size = rel(0.9), hjust = 0),
          axis.text.y = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank())
}

theme_pred_range <- function() {
  theme_pred() +
    theme(plot.title = element_markdown(family = "Roboto Condensed", face = "plain"),
          plot.subtitle = element_text(family = "Roboto Mono", size = rel(0.9), hjust = 0),
          panel.grid.minor.y = element_blank())
}

clrs <- MetBrewer::met.brewer("Java")

penguins =   penguins |>
drop_na(sex) |> 
  mutate(is_gentoo = species == "Gentoo") |> 
  mutate(bill_ratio = bill_depth_mm / bill_length_mm)


model_normal <- brm(
  bf(body_mass_g ~ flipper_length_mm),
  family = gaussian(),
  data = penguins
)



penguins_avg_flipper <- penguins |> 
  summarize(flipper_length_mm = mean(flipper_length_mm))

# Extract different types of posteriors
normal_linpred <- model_normal |> 
  linpred_draws(newdata = penguins_avg_flipper)

normal_epred <- model_normal |> 
  epred_draws(newdata = penguins_avg_flipper)

normal_predicted <- model_normal |> 
  predicted_draws(newdata = penguins_avg_flipper,
                  seed = 12345)  


p1 <- ggplot(normal_linpred, aes(x = .linpred)) +
  stat_halfeye(fill = clrs[3]) +
  scale_x_continuous(labels = label_comma()) +
  coord_cartesian(xlim = c(4100, 4300)) +
  labs(x = "Body mass (g)", y = NULL,
       title = "**Linear predictor** <span style='font-size: 14px;'>*µ* in the model</span>",
       subtitle = "posterior_linpred(..., tibble(flipper_length_mm = 201))") +
  theme_pred_dist() +
  theme(plot.title = element_markdown())

p2 <- ggplot(normal_epred, aes(x = .epred)) +
  stat_halfeye(fill = clrs[2]) +
  scale_x_continuous(labels = label_comma()) +
  coord_cartesian(xlim = c(4100, 4300)) +
  labs(x = "Body mass (g)", y = NULL,
       title = "**Expectation of the posterior** <span style='font-size: 14px;'>E[*y*] and *µ* in the model</span>",
       subtitle = "posterior_epred(..., tibble(flipper_length_mm = 201))") +
  theme_pred_dist()

p3 <- ggplot(normal_predicted, aes(x = .prediction)) +
  stat_halfeye(fill = clrs[1]) +
  scale_x_continuous(labels = label_comma()) +
  coord_cartesian(xlim = c(2900, 5500)) +
  labs(x = "Body mass (g)", y = NULL,
       title = "**Posterior predictions** <span style='font-size: 14px;'>Random draws from posterior Normal(*µ*, *σ*)</span>",
       subtitle = "posterior_predict(..., tibble(flipper_length_mm = 201))") +
  theme_pred_dist()

(p1 / plot_spacer() / p2 / plot_spacer() / p3) +
  plot_layout(heights = c(0.3, 0.05, 0.3, 0.05, 0.3))
#
#
#
#
#
#
#
#
#
#
#
#
#
#

linear_predictions_manual = model_normal |>
spread_draws(b_Intercept, b_flipper_length_mm) |>
 mutate(mu = b_Intercept + 
           (b_flipper_length_mm * penguins_avg_flipper$flipper_length_mm))

tinytest::expect_equal(linear_predictions_manual$mu, normal_linpred$.linpred)

#
#
#
#
#
#
#
#
postpred_manual = model_normal |>
spread_draws(b_Intercept, b_flipper_length_mm, sigma) |>
 mutate(mu = b_Intercept + 
        (b_flipper_length_mm * penguins_avg_flipper$flipper_length_mm),
       predictions = rnorm(n(), mu, sigma))

tinytest::expect_equal(postpred_manual$predictions, normal_predicted$.prediction)

#
#
#
#
#

manual = ggplot(postpred_manual, aes(x = predictions)) + 
    stat_halfeye(fill = 'hotpink') +
    labs(title = "**Posterior predictions** <span style='font-size: 14px;'>Random draws from posterior Normal(*µ*, *σ*)</span>", subtitle = "rnorm(b_Intercept + (b_flipper_length_mm * 201), sigma)") +
    theme_pred_dist() +
    theme(plot.title = element_markdown())

manual /p3

#
#
#
#
#
#
#
#
#
#

logit_mod = brm(
    is_gentoo ~ flipper_length_mm,
    family = bernoulli(link = 'logit'),
    data = penguins
)


epreds_logit = logit_mod |>
    epred_draws(newdata = penguins_avg_flipper)

linear_pres_logit = logit_mod |>
    linpred_draws(newdata = penguins_avg_flipper)

epreds_logit_plot = ggplot(epreds_logit, aes(x = .epred)) +
    stat_halfeye() +
    labs(title = 'Expected Predictions') +
    theme_minimal()

logit_linpreds = ggplot(linear_pres_logit, aes(x = .linpred)) +
    stat_halfeye() +
    labs(title = 'Linear Predicitions')+ 
    theme_minimal()


epreds_logit_plot/logit_linpreds

#
#
#
#
#
#
#
#
#
#

data('WaffleDivorce', package = 'rethinking')




waffle_clean = WaffleDivorce |>
    janitor::clean_names() |>
    mutate(across(c(marriage, divorce, median_age_marriage),  \(x) scale(x), .names = "{col}_scaled")) |>
    mutate(across(c(marriage, divorce, median_age_marriage), \(x)  as.numeric(scale(x)), .names = "{col}_z"))

waffle_priors = c(
    prior(normal(0, 0.2), class = Intercept),
    prior(normal(0, 0.5), class = b, coef = 'median_age_marriage_z'),
    prior(normal(0,0.5), class = b, coef = 'marriage_z'),
    prior(exponential(1), class = sigma))


scaled_waffles = brm(
    divorce_z ~ marriage_z + median_age_marriage_z,
    data = waffle_clean,
    prior = waffle_priors,
    sample_prior = 'only' 
)

# range(waffle_clean$median_age_marriage_z)

check_priors = tibble(median_age_marriage_z = seq(-3, 3),
                     marriage_z = 0) |>
                add_epred_draws(scaled_waffles, ndraws = 100)

ggplot(check_priors, aes(x = median_age_marriage_z, y = .epred, group = .draw)) +
    geom_line(alpha = 0.5)

#
#
#
#
#
#
#
#
#| eval: false

brm(divorce_z ~ marriage_z + age_z, data = d)
#
#
#
#
#
#
#

waffle_priors_two = c(
    prior(normal(0, 0.2), class = Intercept, resp = divorcez),
    prior(normal(0, 0.5), class = b, coef = 'median_age_marriage_z', resp = divorcez),
    prior(normal(0,0.5), class = b, coef = 'marriage_z', resp = divorcez),
    prior(exponential(1), class = sigma, resp = divorcez),
    prior(normal(0, 0.2), class = Intercept, resp = marriagez),
    prior(normal(0, 0.5), class = b, coef = 'median_age_marriage_z', resp = marriagez),
    prior(exponential(1), class = sigma, resp = marriagez))


#
#
#
#
#
#
#
#

divorce_model = bf(divorce_z ~ median_age_marriage_z + marriage_z)

marriage_rate_model = bf(marriage_z ~ median_age_marriage_z)

conflicted::conflict_prefer('stanfit', 'rstan')

full_luxury_model = brm(
    divorce_model + marriage_rate_model + set_rescor(FALSE),
    prior = waffle_priors_two,
    data = waffle_clean,
    cores = 4
)


#
#
#
#
#

divorce_predictions = tibble(median_age_marriage_z = seq(-2, 2, length.out = 40 ),
                             # make sure we don't use a grid of marriage rates 
                             marriage_z = 0) |>
                       add_predicted_draws(full_luxury_model, resp = 'divorcez')

marriage_predictions = tibble(median_age_marriage_z = seq(-2, 2, length.out = 40 )) |>
                       add_predicted_draws(full_luxury_model, resp = 'marriagez')


#
#
#
#
#
#
#
#
#
#
#
#

# getting tired of  + theme_minmal

theme_set(theme_minimal())

 d <- 
  tibble(species = c("afarensis", "africanus", "habilis", "boisei", "rudolfensis", "ergaster", "sapiens"), 
         brain   = c(438, 452, 612, 521, 752, 871, 1350), 
         mass    = c(37.0, 35.5, 34.5, 41.5, 55.5, 61.0, 53.5))
ggplot(d, aes(x = mass, y = brain)) +
    geom_point()

#
#
#
#
#
#
#
#
#
#
#
#

d = d |>
    mutate(mass_std  = (mass - mean(mass)) / sd(mass),
         brain_std = brain / max(brain))

brain_model_priors = c(
    prior(normal(0.5, 1), class = Intercept),
    prior(normal(0,10), class = b),
    prior(lognormal(0,1), class = sigma))


brain_model = brm(
    brain_std  ~  mass_std,
    prior = brain_model_priors,
    data = d,
    file = 'fits/brain_model',
    chains = 4, cores = 4
)

brain_sqr = update(
    brain_model,
    newdata = d,
    brain_std ~ mass_std + I(mass_std^2),
    chains =4, cores =4,
    file = 'fits/brain_model_sqr'
)

brain_cubed = update(
    brain_model,
    newdata = d, 
         formula = brain_std ~ mass_std + I(mass_std^2) + I(mass_std^3) + I(mass_std^4),
         iter = 2000, warmup = 1000, chains = 4, cores = 4,
         seed = 7,
         control = list(adapt_delta = .995)
)

#
#
#
#
#
#
#
#| eval: true
brain_loo_lins =\(mod, ylim = range(d$brain_std)){

 nd <- tibble(mass_std = seq(from = -2, to = 2, length.out = 200))
  
  # simulate and wrangle
  fitted(mod, newdata = nd, probs = c(.055, .945)) |> 
    data.frame() |> 
    bind_cols(nd) |> 
    
    # plot!  
    ggplot(aes(x = mass_std)) +
    geom_lineribbon(aes(y = Estimate, ymin = Q5.5, ymax = Q94.5)) +
    geom_point(data = d,
               aes(y = brain_std)) +
    labs(
         x = "body mass (std)",
         y = "brain volume (std)") +
             coord_cartesian(xlim = c(-1.2, 1.5),
                    ylim = c(0.3, 1.0))
  

}

mod_list = list(brain_model, brain_sqr, brain_cubed)

plots = purrr::map(mod_list, \(x) brain_loo_lins(x)) 

wrap_plots(plots, ncol =1)


#
#
#
#
#
#

dat = tibble(x = seq(from = - 3.5, to = 3.5, by = 0.01)) |>
  mutate(a = dnorm(x, mean = 0, sd = 0.2),
         b = dnorm(x, mean = 0, sd = 0.5),
         c = dnorm(x, mean = 0, sd = 1.0)) |> 
  pivot_longer(-x) 


ggplot(dat, aes(x = x, y = value, color = name, fill = name)) +
    geom_area(alpha = 0.4, linewidth = 0.5, position = 'identity') +
    theme(legend.position = 'none')

#
#
#
#
#
#| eval: false 
n_sim   <- 1e3
kseq    <- 1:5

make_sim <- function(n, b_sigma) {
  sapply(kseq, function(k) {
    print(k);
    r <- replicate(n_sim, rethinking::sim_train_test(N = n, k = k, b_sigma = b_sigma));c(mean(r[1, ]), mean(r[2, ]), stats::sd(r[1, ]), stats::sd(r[2, ])) 
    }
    ) |> 
    
    # this is a new line of code
    data.frame()
}

s <-
  crossing(n  = c(20, 100),
           b_sigma = c(1, 0.5, 0.2)) |> 
  mutate(sim = map2(n, b_sigma, make_sim)) |> 
  unnest(sim)


n_sim   <- 1e3
n_cores <- 8
kseq    <- 1:5

# define the simulation function
my_sim <- function(k) {
  
  print(k);
  r <- replicate(n_sim, rethinking::sim_train_test(N = n, k = k));
  c(mean(r[1, ]), mean(r[2, ]), stats::sd(r[1, ]), stats::sd(r[2, ]))
  
}


# here's our dev object based on `N <- 20`
n      <- 20
dev_20 <-
  sapply(kseq, my_sim)

# here's our dev object based on N <- 100
n       <- 100
dev_100 <- 
  sapply(kseq, my_sim)




dev_tibble <-
  rbind(dev_20, dev_100) |> 
  data.frame() |> 
  mutate(statistic = rep(c("mean", "sd"), each = 2) |> rep(x = _, times = 2),
         sample    = rep(c("in", "out"), times = 2) |> rep( x = _, times = 2),
         n         = rep(c("n = 20", "n = 100"), each = 4)) |> 
  pivot_longer(-(statistic:n)) |> 
  pivot_wider(names_from = statistic, values_from = value) |>
  mutate(n     = factor(n, levels = c("n = 20", "n = 100")),
         npar = str_extract(name, "\\d+") |> as.double()) |> 
  mutate(npar = ifelse(sample == "in", npar - .075, npar + .075))

#
#
#
#
#
#| echo: false

clean_sim = read_csv('data/parameters_by_time.csv')

dev_tibble = read_csv('data/sample_by_time.csv')

# clean_sim = s |>
#     mutate(statistic = rep(c('mean', 'sd'), each = 2) |> rep(x = _, times = 3 * 2),
#            sample = rep(c("in", "out"), times = 2) |> rep(x = _, times = 3 * 2)) |>
#             pivot_longer(starts_with('X')) |>
#             pivot_wider(names_from = statistic, values_from = value) |>
#             mutate(n = factor(paste0('n = ', n), levels = c('n = 20', 'n = 100')),
#                    n_par = as.numeric(str_extract(n, '\\d+')))
# 

ggplot(data = clean_sim,
           aes(x = npar, y = mean, group = interaction(sample, b_sigma))) +
geom_line(aes(color = sample, size = b_sigma |> as.character())) +
geom_point(data = dev_tibble, aes(group = sample, fill = sample), ,
             color = "black", shape = 21, size = 2.5, stroke = .1) +
scale_size_manual(values = c(1, 0.5, 0.2)) +
facet_wrap(vars(n), scale = 'free_y') +
theme(legend.position = 'none')


#
#
#
#
#
#
#
#
#
#
#

library(animation)

myU2 <- function( q , a=0 , b=1 , k=0 , d=0.5 ) {
    s <- exp(q[2]) # sigma on log latent scale
    mu <- q[1]
    U <- sum( dnorm(y,mu,s,log=TRUE) ) + dnorm(mu,a,b,log=TRUE) + dnorm(q[2],k,d,log=TRUE)
    return( -U )
}

# gradient function
# need vector of partial derivatives of U with respect to vector q
myU_grad2 <- function( q , a=0 , b=1 , k=0 , d=0.5 ) {
    mu <- q[1]
    s <- exp(q[2])
    G1 <- sum( y - mu ) * exp(-2*q[2]) + (a - mu)/b^2 #dU/dmu
    G2 <- sum( (y - mu)^2 ) * exp(-2*q[2]) - length(y) + (k-q[2])/d^2 #dU/ds
    return( c( -G1 , -G2 ) ) # negative bc energy is neg-log-prob
}

# test data
set.seed(7)
y <- abs(rnorm(50))
y <- c( y , -y ) # ensure mean is zero

###########
# example paths
library(shape) # for good arrow heads
# blank(bty="n")

# priors
priors <- list()
priors$a <- 0
priors$b <- 1
priors$k <- 0
priors$d <- 0.3

#ss <- ss + 1
set.seed(42) # seed 9 for examples

# init
n_samples <- 4
Q <- list()
Q$q <- c(-0.4,0.2)
xr <- c(-0.6,0.6)
yr <- c(-0.25,0.4)

step <- 0.02
L <- 12 # 0.02/12 okay sampling --- 0.02/20 is good for showing u-turns
xpos <- c(4,2,1,2) # for L=20
#xpos <- c(2,3,2,1) # for L=55
path_col <- col.alpha("black",0.5)

draw_bg <- function() {
    plot( NULL , ylab="log_sigma" , xlab="mu" , xlim=xr , ylim=yr )
    # draw contour of log-prob
    cb <- 0.2
    mu_seq <- seq(from=xr[1]-cb,to=xr[2]+cb,length.out=50) 
    logsigma_seq <- seq(from=yr[1]-cb,to=yr[2]+cb,length.out=50)
    z <- matrix(NA,length(mu_seq),length(logsigma_seq))
    for ( i in 1:length(mu_seq) )
        for ( j in 1:length(logsigma_seq) )
            z[i,j] <- myU2( c( mu_seq[i] , logsigma_seq[j] ) , a=priors$a , b=priors$b , k=priors$k , d=priors$d )
    cl <- contourLines( mu_seq , logsigma_seq , z , nlevels=30 )
    for ( i in 1:length(cl) ) lines( cl[[i]]$x , cl[[i]]$y , col=col.alpha("black",0.5) , lwd=1 )
}


Q <- list()
Q$q <- c(-0.4,0.2) # start point
xr <- c(-0.4,0.4) # x range in plot
yr <- c(-0.25,0.3) # y range in plot

draw_bg()

n_samples <- 10
# points( Q$q[1] , Q$q[2] , pch=4 , col="black" )
pts <- matrix(NA,nrow=n_samples,ncol=3)

for ( i in 1:n_samples ) {

    Q <- HMC2( myU2 , myU_grad2 , step , L , Q$q , a=priors$a , b=priors$b , k=priors$k , d=priors$d )

    draw_bg()

    # draw previous points
    if ( i > 1 ) {
        for ( j in 1:(i-1) ) {
            V <- 0.9
            points( pts[j,1] , pts[j,2] , pch=ifelse( pts[j,3]==1 , 1 , 16 ) , col=grau(V) , lwd=2 )
        }
    }

    # draw trajectory
    for ( l in 1:L ) {
        lines( Q$traj[l:(l+1),1] , Q$traj[l:(l+1),2] , col="white" , lwd=8 )
        lines( Q$traj[l:(l+1),1] , Q$traj[l:(l+1),2] , col=4 , lwd=5 )
        ani.record()
    }
    #points( Q$traj[2:L+1,] , pch=16 , col="white" , cex=0.3 )

    # draw new point
    pts[i,1:2] <- Q$traj[L+1,]
    pts[i,3] <- Q$accept

    #Arrows( Q$traj[L,1] , Q$traj[L,2] , Q$traj[L+1,1] , Q$traj[L+1,2] , arr.length=0.3 , arr.adj = 0.7 , col=4 )
    #text( Q$traj[L+1,1] , Q$traj[L+1,2] , i , cex=1.2 , pos=1 , offset=0.4 )
    
    points( Q$traj[L+1,1] , Q$traj[L+1,2] , pch=ifelse( Q$accept==1 , 1 , 16 ) , col=ifelse( Q$accept==1 , 4 , 2 ) , lwd=2 )

    invisible( replicate( 3 , ani.record() ) )

}


#
#
#
#
#
#
#


data('Wines2012', package = 'rethinking')


wine_clean = Wines2012 |>
    mutate(score_z = scale(score),
           wine = as.numeric(wine))

priors = c(
    prior(normal(0,1), class = b, coef = 'wine'),
    prior(exponential(1), class = sigma)
)

wine_score_model = brm(score_z ~ 0 + wine,
prior = priors,
 data = wine_clean)



plot(wine_score_model)
#
#
#
#
#
#
#
library(MetBrewer)
manual_trace_data = as_draws_df(wine_score_model)

ggplot(manual_trace_data, aes(x = .iteration, y  = b_wine, color = as.factor(.chain))) +
    geom_line(alpha = 0.5) +
    scale_color_met_d(name = 'Lakota')


#
#
#
#
#
#
priors_bad = c(
    prior(lognormal(0,10), class = b, coef = 'wine'),
    prior(exponential(4), class = sigma)
)

bad_wine_score_model = brm(score_z ~ 0 + wine,
prior = priors_bad,
 data = wine_clean)

bad_chains = as_draws_df(bad_wine_score_model)

ggplot(bad_chains, aes(x = .iteration, y = b_wine,
                       color = as.factor(.chain),
                       group = as.factor(.chain))) +
    geom_line() +
 scale_color_met_d(name = 'Lakota')


#
#
#
#
#
#
#
#
#
#

as_draws_df(bad_wine_score_model) |>
    bayesplot::mcmc_rank_overlay()

#
#
#
#
#
#
#
as_draws_df(wine_score_model) |>
    bayesplot::mcmc_rank_overlay() +
    scale_y_continuous(limits = c(30,65)) +
    scale_color_met_d(name = 'Lakota')
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#

summary(bad_wine_score_model)

#
#
#
#
#

summary(wine_score_model)


#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#

data('rugged', package = 'rethinking')

rugged = rugged |>
    drop_na(rgdppc_2000) |>
    mutate(log_gdp = log(rgdppc_2000),
           log_gdp_z = log_gdp/mean(log_gdp),
           rugged_std = rugged/max(rugged),
           rugged_std_c = rugged_std - mean(rugged_std))
#
#
#
#
#
#

priors_first_cut = c(
    prior(normal(1, 1), class = Intercept),
    prior(normal(0,1), class = b),
    prior(exponential(1), class = sigma))



check_priors = brm(
    log_gdp_z ~ rugged_std_c,
    data = rugged, 
    prior = priors_first_cut,
    sample_prior = 'only'
)

priors_dat = tibble(rugged_std_c = seq(-2,2)) |>
    add_linpred_draws(check_priors, ndraws = 100) |>
    mutate(unscaled_rugged = rugged_std_c + mean(rugged$rugged_std))


ggplot(priors_dat, aes(x = unscaled_rugged, y = .linpred, group = .draw)) +
    geom_line() +
    coord_cartesian(xlim = c(0, 1),
                    ylim = c(0.5, 1.5))
#
#
#
#
#
#
#
#
#

priors_rich = c(
    prior(normal(1, 0.1), class = Intercept),
    prior(normal(0,0.5), class = b),
    prior(exponential(1), class = sigma))



check_priors = brm(
    log_gdp_z ~ rugged_std_c,
    data = rugged, 
    prior = priors_rich,
    sample_prior = 'only'
)

priors_dat = tibble(rugged_std_c = seq(-2,2)) |>
    add_linpred_draws(check_priors, ndraws = 100) |>
    mutate(unscaled_rugged = rugged_std_c + mean(rugged$rugged_std))


ggplot(priors_dat, aes(x = unscaled_rugged, y = .linpred, group = .draw)) +
    geom_line() +
    coord_cartesian(xlim = c(0, 1),
                    ylim = c(0.5, 1.5))

#
#
#
#
#
#

priors_rich = c(
    prior(normal(1, 0.1), class = Intercept),
    prior(normal(0,0.3), class = b),
    prior(exponential(1), class = sigma))



check_priors = brm(
    log_gdp_z ~ rugged_std_c,
    data = rugged, 
    prior = priors_rich,
    sample_prior = 'only'
)

priors_dat = tibble(rugged_std_c = seq(-2,2)) |>
    add_linpred_draws(check_priors, ndraws = 100) |>
    mutate(unscaled_rugged = rugged_std_c + mean(rugged$rugged_std))


ggplot(priors_dat, aes(x = unscaled_rugged, y = .linpred, group = .draw)) +
    geom_line() +
    coord_cartesian(xlim = c(0, 1),
                    ylim = c(0.5, 1.5))

#
#
#
#
#
#
#

rugged = rugged |>
    mutate(cid = ifelse(cont_africa == 1, '1', '2'))

int_priors = c(prior(normal(1, 0.1), class = b, coef = cid1, nlpar = a),
                prior(normal(1, 0.1), class = b, coef = cid2, nlpar = a),
                prior(normal(0, 0.3), class = b, coef = cid1, nlpar = b),
                prior(normal(0, 0.3), class = b, coef = cid2, nlpar = b),
                prior(exponential(1), class = sigma))


int_model_binary = brm(
    bf(log_gdp_z ~ 0 + a  + b * rugged_std_c,
    a ~ 0 + cid,
    b ~ 0 + cid,
    nl = TRUE),
    data = rugged,
    prior = int_priors)



#
#
#
#
#
#
#
#


prds  = predictions(int_model_binary,
                    newdata = datagrid(cid = c('1', '2'),
                                       rugged_std_c = mean))

preds_plot = get_draws(prds) |>
    transform(type = 'response') |>
    mutate(cid = ifelse(cid == '1', 'Africa', 'Not Africa'))


ggplot(preds_plot, aes(x = draw, fill = cid)) +
    stat_halfeye() +
    scale_fill_met_d('Lakota') +
    labs(y = 'Density', x = 'Predicted Values of Log GDP')




#
#
#
#
#

unscaled = preds_plot |>
    mutate(draw = exp(draw * mean(rugged$log_gdp)))


ggplot(unscaled, aes(x = draw, fill = cid)) +
    stat_halfeye() +
    scale_fill_met_d('Lakota') +
    labs(y = 'Density', x = 'Predicted Values GDP')

#
#
#
#
#
#
nd <- 
  crossing(cid= 1:2,
           rugged_std = seq(from = -0.2, to = 1.4, length.out = 30)) |> 
  mutate(rugged_std_c = rugged_std - mean(rugged$rugged_std))

countries <- c("Equatorial Guinea", "South Africa", "Seychelles", "Swaziland", "Lesotho", "Rwanda", "Burundi", "Luxembourg", "Greece", "Switzerland", "Lebanon", "Yemen", "Tajikistan", "Nepal")

fit = fitted(
    int_model_binary,
    newdata = nd, 
    probs = c(0.015, 0.985)
) |>
    data.frame() |>
    bind_cols(nd) |>
    mutate(cont_africa = ifelse(cid == 1, 'African Nations', 'Non-African Nations'))

cleaned = rugged |>
    mutate(cont_africa = ifelse(cid == 1, 'African Nations', 'Non-African Nations'))


ggplot(cleaned, aes(x = rugged_std, y = log_gdp_z, fill = cont_africa, color = cont_africa)) +
    geom_smooth(data = fit,
                aes(y = Estimate, ymin = Q1.5, ymax = Q98.5)) +
    coord_cartesian(xlim = c(0, 1)) +
    theme(legend.position = "none") +
    facet_wrap(vars(cont_africa))

#
#
#
#
#
#
#
#
data(tulips, package = 'rethinking')

d = tulips |>
    mutate(blooms_std  = blooms/max(blooms),
           water_cent = water - mean(water),
           shade_cent = shade - mean(shade))

    

#
#
#
#
#
#
#

cont_priors = c(
    prior(normal(0.5, 0.25), class = Intercept),
    prior(normal(0, 0.25), class = b, coef = water_cent),
    prior(normal(0, 0.25), class = b, coef = shade_cent),
    prior(normal(0, 0.25), class =b, coef = 'water_cent:shade_cent'),
    prior(exponential(1), class = sigma)
)

cont_inter = brm(
    blooms_std ~ water_cent:shade_cent + water_cent + shade_cent,
    data = d,
    prior = cont_priors
)


#
#
#
#
#
#
#

preds = get_draws(
    predictions(cont_inter, newdata = datagrid(
        shade_cent = -1:1,
        water_cent = -1:1
    ))
) |>
    mutate(nice_labs = glue::glue('Shade (centered) = {shade_cent}'))

ggplot(preds, aes(x = water_cent, y = draw, group = drawid)) +
    geom_smooth(alpha = 1/5) +
    geom_point(alpha = 0.5) +
    facet_wrap(vars(nice_labs)) +
    labs(y = 'Blooms (Standardized)', x = 'Water (centered)')



#
#
#
#
#
#
#
#
#
#
#

data(UCBadmit,package = 'rethinking')

admit_data = UCBadmit

#
#
#
#
#
#
#
#| code-fold: true
data(weather_perth, package = 'bayesrules')

weather = weather_perth |>
    select(day_of_year, raintomorrow, humidity9am, humidity3pm, raintoday) |>
   mutate(across(c(humidity9am, humidity3pm), 
                ~scale(., scale = FALSE), .names = "{col}_centered")) |> 
  mutate(across(c(humidity9am, humidity3pm), 
                ~as.numeric(scale(., scale = FALSE)), .names = "{col}_c")) |> 
  mutate(raintomorrow_num = as.numeric(raintomorrow) - 1)

extract_attributes <- function(x) {
  attributes(x) %>%
    set_names(janitor::make_clean_names(names(.))) %>%
    as_tibble() %>%
    slice(1)
}

unscaled <- weather %>%
  select(ends_with("_centered")) |> 
  summarize(across(everything(), ~extract_attributes(.))) |> 
  pivot_longer(everything()) |> 
  unnest(value) |> 
  split(~name)


#
#
#
#
#
qlogis(.2)
#
#
#
#
#
#

plogis(qlogis(0.2) - (2 * 0.5))

plogis(qlogis(0.2) + (2 * 0.5))
#
#
#
#
#
exp(0.07 - 0.07)
## [1] 1
exp(0.07 + 0.07)
#
#
#
#
#
#

priors_first = c(prior(normal(-1.39, 0.5), class = Intercept),
                 prior(normal(0.07, 0.035), class = b, coef = 'humidity9am_c'))

clrs <- MetBrewer::met.brewer("Lakota", 6)

check_weather_priors = brm(
    raintomorrow ~ humidity9am_c,
    data = weather, 
    family = bernoulli(link = 'logit'), 
    prior = priors_first,
    sample_prior = 'only'
)

p1 <- tibble(
  humidity9am = seq(0, 100, by = 0.1)
) |> 
  mutate(humidity9am_c = humidity9am - unscaled$humidity9am_centered$scaled_center) |> 
  add_epred_draws(check_weather_priors, ndraws = 100) |> 
  ggplot(aes(x = humidity9am, y = .epred)) +
  geom_line(aes(group = .draw), alpha = 0.5, size = 0.5, color = clrs[6]) +
  labs(x = "9 AM humidity", y = "Probability of rain tomorrow")

p2 <- tibble(
  humidity9am = seq(0, 100, by = 0.1)
) |> 
  mutate(humidity9am_c = humidity9am - unscaled$humidity9am_centered$scaled_center) |> 
  add_predicted_draws(check_weather_priors, ndraws = 100) |> 
  group_by(.draw) |> 
  summarize(proportion_rain = mean(.prediction == 1)) |> 
  ggplot(aes(x = proportion_rain)) +
  geom_histogram(binwidth = 0.02, color = "white", fill = clrs[1]) +
  labs(x = "Proportion of rainy days in each draw", y = "Count")

p1 | p2
#
#
#
#
#
#

real_model =  brm(
    raintomorrow ~ humidity9am_c,
    data = weather, 
    family = bernoulli(link = 'logit'), 
    prior = priors_first
)

pp_check(real_model, ndraws = 50)

#
#
#
#
#
#
#
#
#
#
#
#
#
data('reedfrogs', package = 'rethinking')

add_tank = reedfrogs |>
    mutate(tank = row_number(),
           preds = ifelse(pred == 'no' ,1L, 2L),
           group = ifelse(size == 'small', 1L, 2L),
           log_density_sd = scale(log(density)))

#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| eval: false
#| code-fold: true 
with_trials <- 
  brm(data = add_tank, 
      family = binomial,
      surv | trials(density) ~  (1 | tank),
      prior = c(prior(normal(0, 1.5), class = Intercept),  # alpha bar
                prior(exponential(1), class = sd)),        # sigma
      iter = 5000, warmup = 1000, chains = 4, cores = 4)

check_mod_priors = \(sigma_values = 1.5){
   
   new_prior = c(set_prior(paste0("normal(0, ", sigma_values, ")"), class = 'Intercept'),  # alpha bar
                 set_prior('exponential(1)', class = 'sd'))
   
   m = brm(data = add_tank, 
      family = binomial,
      surv | trials(density) ~ 1 +  (1 | tank),
      prior = new_prior,        # sigma
      iter = 5000, warmup = 1000, chains = 4, cores = 10)

  crit = add_criterion(m, criterion = 'loo') 

  return(crit)
}

sig_values = seq(0.1, 5, by = 0.1)

names_vec = paste('Sigma = ', sig_values)

names(sig_values) = names_vec


check_models = map(sig_values, check_mod_priors)

get_psis = map(check_models, \(x){
    psis(x)
})



#
#
#
#
#
#

tank_priors = c(prior(cauchy(0, 1.5), class = Intercept),  
                prior(exponential(1), class = sd))
with_trials <- 
  brm(data = add_tank, 
      family = binomial,
      surv | trials(density) ~  (1 | tank),
      prior = tank_priors,        
      iter = 5000, warmup = 1000, chains = 4, cores = 4)


with_trials |>
    tidy()

#
#
#
#
#
#
#
#
data('cherry_blossom_sample', package = 'bayesrules')

running = cherry_blossom_sample |>
    select(runner, age, net) |>
    drop_na() |>
    mutate(runner_nice = glue::glue('Runner {runner}'),
           runner_nice = fct_inorder(runner_nice),
           across(c(net, age), \(x) scale(x, scale = FALSE), .names = '{col}_c'))

unscaled = running |>
    select(ends_with('c')) |>
    summarise(across(everything(), \(x) extract_attributes(x))) |>
    pivot_longer(everything()) |>
    unnest(value) 

#
#
#
#
#
ggplot(running, aes(x = net)) +
    geom_histogram(color = 'white')


#
#
#
#
#

running_priors = c(
    prior(normal(100, 10), class = Intercept),
    prior(exponential(1), class = sd))


first_cut = brm(
    net ~ (1| runner),
    data = running,
    prior = running_priors,
    sample_prior = 'only'
)

p1 = running |>
add_predicted_draws(first_cut)  |>
ggplot(aes(x = .prediction, group = .draw)) +
geom_density() +
labs(title = 'SD = 1')

#
#
#
#
#
running_priors2 = c(
    prior(normal(100, 10), class = Intercept),
    prior(exponential(0.5), class = sd))


first_cut = brm(
    net ~ (1| runner),
    data = running,
    prior = running_priors2,
    sample_prior = 'only'
)


plot_dat = running |>
add_predicted_draws(first_cut) 


p2 = ggplot(plot_dat,aes(x = .prediction, group = .draw)) +
geom_density() +
labs(title = 'SD = 0.5')

#
#
#
#
#

plot_dat |>
    ungroup() |>
    reframe(actual_avg = mean(net),
              predicted_avg = mean(.prediction), 
              actual_range = range(net),
              predicted_range = range(net))


#
#
#
#
#

running_priors3 = c(
    prior(normal(100, 10), class = Intercept),
    prior(exponential(0.1), class = sd))


first_cut = brm(
    net ~ (1| runner),
    data = running,
    prior = running_priors3,
    sample_prior = 'only'
)

plot_dat = running |>
add_predicted_draws(first_cut) 


p3 = ggplot(plot_dat,aes(x = .prediction, group = .draw)) +
geom_density() +
labs(title = 'SD = 0.1')

#
#
#
#
#
plot_dat |>
    ungroup() |>
    reframe(actual_avg = mean(net),
              predicted_avg = mean(.prediction), 
              actual_range = range(net),
              predicted_range = range(net))
#
#
#
#
#
p1 / p2/ p3
#
#
#
#
#

sd_1 = ggplot() +
    stat_function(fun = ~dexp(.,1/10), geom = 'area') +
    xlim(c(0, 60)) 
sd_5 = ggplot() +
    stat_function(fun = ~dexp(., 5/10), geom = 'area') +
    xlim(c(0, 60)) 

sd_un = ggplot() +
    stat_function(fun = ~dexp(., 1), geom = 'area') +
    xlim(c(0, 60)) 

sd_1 | sd_5 | sd_un


#
#
#
#
#
#

priors_runners = c(
    prior(normal(100, 10), class = Intercept),
    prior(normal(2.5, 1), class = b),
    prior(exponential(0.1), class = sigma),
    prior(exponential(0.1), class = sd)
)

runner_model = brm(
    net ~ age_c + (1|runner), 
    data = running, 
    prior = priors_runners,
    sample_prior = 'only'
)

running |>
    add_linpred_draws(runner_model, ndraws = 8)  |>
    ggplot(aes(x = age, y = net)) +
    geom_line(aes(y = .linpred, group = paste(runner, .draw))) +
    facet_wrap(vars(.draw))

#
#
#
#
#

runner_model_final = brm(
    net ~ age_c + (1|runner), 
    data = running, 
    prior = priors_runners,
    iter = 4000, cores = 10, threads = 5
)

runner_predictions = 
     predictions(runner_model_final, re_formula = NA, ndraws = 200, type = 'prediction') |>
     get_draws() |>
     mutate(age = (age_c + mean(running$age)))


 me = ggplot(runner_predictions, aes(x = age, y = draw)) +
     stat_lineribbon(alpha = 0.5) +
     labs(x = 'Age', y = 'Race Time(Minutes)', title = 'Made with marginaleffects') +
     theme(legend.position = 'none') + 
     scale_y_continuous(breaks = seq(80, 100, by = 5))

lpd = runner_model_final |>
    linpred_draws(running, ndraws = 200, re_formula = NA) |>
    ggplot(aes(x = age, y = net)) +
    stat_lineribbon(aes(y = .linpred)) +
    labs(x = 'Age', y = 'Race time', title = 'made with linepredraws') +
    theme(legend.position = 'none')

#
#
#
#
#
me + lpd 

#
#
#
#
#

runner_effects = runner_model_final |>
    spread_draws(b_Intercept, r_runner[runner,]) |>
    mutate(runner_intercepts = b_Intercept + r_runner) |>
    ungroup() |>
    mutate(runner = fct_reorder(factor(runner), runner_intercepts, .fun = mean))

global_effect = runner_model_final |>
    tidy(effects = c('fixed'), conf.level = 0.89) |>
    filter(term == '(Intercept)')

ggplot(runner_effects, aes(x = runner_intercepts, y = runner)) +
    stat_pointinterval() +
    annotate(geom = 'rect', ymin = -Inf, ymax = Inf,
              xmin = global_effect$conf.low, xmax = global_effect$conf.high, alpha = 0.5) +
    geom_vline(xintercept = global_effect$estimate)


#
#
#
#
#
#
#
#
running |> 
  ggplot(aes(x = age, y = net, group = runner)) + 
  geom_smooth(method = "lm", se = FALSE, size = 0.5, color = clrs[1]) +
  coord_cartesian(ylim = c(60, 125)) +
  labs(x = "Age", y = "Race time",
       title = "Observed data",
       subtitle = "Basic per-runner OLS models")
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
withr::with_seed(123, {
  rho_plots <- tibble(rho = c(-0.99, 0, 0.99)) |> 
    mutate(title = glue::glue("ρ = {rho}"),
           subtitle = c("Strong negative correlation\nbetween slope and intercept",
                        "No correlation\nbetween slope and intercept",
                        "Strong positive correlation\nbetween slope and intercept")) |> 
    mutate(Sigma = map(rho, ~matrix(c(1, .x, .x, 1), 2, 2))) |> 
    mutate(data = map(Sigma, ~{
      MASS::mvrnorm(n = 100, mu = c(2, 3), Sigma = .x) |> 
        as_tibble() |> 
        rename(b0 = V1, b1 = V2)
    })) |> 
    mutate(plot = pmap(list(data, title, subtitle), ~{
      ggplot(..1) +
        geom_abline(aes(intercept = b0, slope = b1, color = b0),
                    size = 0.3) +
        scale_color_viridis_c(option = "rocket", begin = 0.1, end = 0.85,
                              limits = c(-1, 5)) +
        labs(title = ..2, subtitle = ..3, color = "β<sub>0</sub>") +
        lims(x = c(0, 3), y = c(0, 10)) +
        theme(axis.line = element_line(),
              legend.title = element_markdown())
    }))
})

wrap_plots(rho_plots$plot) + 
  plot_layout(guides = "collect")
#
#
#
#
#
#
#
#
#
#
#
#
#
#

varying_slope_priors_1 = c(prior(normal(100, 10), class = Intercept),
            prior(normal(2.5, 1), class = b),
            prior(exponential(0.1), class = sigma),
            prior(exponential(0.1), class = sd),
            prior(lkj(1), class = cor))


var_mod1 = brm(
    net ~ age_c + (1 + age_c | runner),
    data = running,
    prior = varying_slope_priors_1,
    iter = 4000, threads = threading(5), cores = 10, 
    control = list(adapt_delta = 0.9),
    sample_prior = 'only' 
)


#
#
#
#
#
#

sim1 = running |>
    add_predicted_draws(var_mod1) 


ggplot(sim1, aes(x = .prediction, group = .draw)) +
    geom_density() +
    coord_cartesian(xlim = c(0, 300))


#
#
#
#
#
varying_slope_priors_2 = c(prior(normal(100, 10), class = Intercept),
            prior(normal(2.5, 1), class = b),
            prior(exponential(0.1), class = sigma),
            prior(exponential(0.1), class = sd),
            prior(lkj(1.5), class = cor))


var_mod2 = brm(
    net ~ age_c + (1 + age_c | runner),
    data = running,
    prior = varying_slope_priors_2,
    iter = 4000, threads = threading(5), cores = 10, 
    control = list(adapt_delta = 0.9),
    sample_prior = 'only' 
)
#
#
#
#
#
#

running |>
    add_predicted_draws(var_mod2) |>
    ggplot(aes(x = .prediction, group = .draw)) +
    geom_density() +
    coord_cartesian(xlim = c(0, 300))


#
#
#
#
#
#

var_mod1 = update(
    var_mod1,
    sample_prior = 'no'
)

var_mo2 = update(var_mod2,
    sample_prior = 'no')

#
#
#
#
#
#

p1 = pp_check(runner_model_final, ndraws = 25) +
labs(title = 'Random Intercepts only')

p2 = pp_check(var_mod1, ndraws = 25) +
    labs(title = 'Random Intercepts & Slopes', subtitle =  'ETA = 1')

p3 = pp_check(var_mo2, ndraws = 25) +
    labs(title = 'Random Intercepts & Slopes', subtitle = 'ETA = 1.5')

p1 | p2 | p3


#
#
#
#
#

loo_stats = tribble(~model_name, ~model,
                    'Random Intercepts', runner_model_final,
                    'Random Intercepts & Slopes \n ETA = 1', var_mod1,
                    'Random Intercepts & Slopes \n ETA = 1.5', var_mo2) |>
            mutate(loo = map(model, \(x) loo(x))) |>
            mutate(loo_stuff = map(loo, ~as_tibble(.$estimates, rownames = "statistic"))) |>
            select(model_name, loo_stuff) |> 
  unnest(loo_stuff) |> 
  filter(statistic == "elpd_loo") |> 
  arrange(desc(Estimate))

loo_stats
#
#
#
#
#
#

priors_runners = c(
    prior(normal(100, 10), class = Intercept),
    prior(normal(2.5, 1), class = b),
    prior(cauchy(0,10), class = sigma),
    prior(cauchy(0,10), class = sd)
)

runner_model = brm(
    net ~ age_c + (1|runner), 
    data = running, 
    prior = priors_runners,
    sample_prior = 'only'
)

check_models_cauchy = running |>
    add_predicted_draws(runner_model) 

ggplot(check_models_cauchy, aes(x = .prediction, group = .draw)) +
    geom_density() +
    coord_cartesian(xlim = c(0, 300))



#
#
#
#
#
#
#| echo: false
runner_model_cauchy = brm(
    net ~ age_c + (1|runner), 
    data = running, 
    prior = priors_runners,
    cores = 10, threads = threading(5), iter = 4000
)

#
#
#
#
#
pp_check(runner_model_cauchy, ndraws = 25)
#
#
#
#
#

cauchy_preds = running |>
add_linpred_draws(runner_model_cauchy, ndraws = 200, re_formula = NA) |>
ggplot(aes(x = age, y = net)) +
     stat_lineribbon(aes(y = .linpred)) +
     labs(title = 'Runner Model with Cauchy Priors') 


lpd + cauchy_preds
#
#
#
#
#
#

loo_stats = tribble(~model_name, ~model,
                    'Random Intercepts exponential prior', runner_model_final,
                    'Random Intercepts Cauchy prior', runner_model_cauchy,
                    'Random Intercepts & Slopes \n ETA = 1', var_mod1,
                    'Random Intercepts & Slopes \n ETA = 1.5', var_mo2) |>
            mutate(loo = map(model, \(x) loo(x))) |>
            mutate(loo_stuff = map(loo, ~as_tibble(.$estimates, rownames = "statistic"))) |>
            select(model_name, loo_stuff) |> 
  unnest(loo_stuff) |> 
  filter(statistic == "elpd_loo") |> 
  arrange(desc(Estimate))

loo_stats |>
    tinytable::tt()

#
#
#
#
#
#
#
#
#
#

data('climbers_sub', package = 'bayesrules')

climbers = climbers_sub |>
    select(expedition_id, member_id, success, year, season,
         age, expedition_role, oxygen_used) |> 
  mutate(age_c = scale(age, scale = FALSE))

extract_attributes <- function(x) {
  attributes(x) %>%
    set_names(janitor::make_clean_names(names(.))) %>%
    as_tibble() %>%
    slice(1)
}

unscaled_climbers <- climbers %>%
  select(ends_with("_c")) |> 
  summarize(across(everything(), ~extract_attributes(.))) |> 
  pivot_longer(everything()) |> 
  unnest(value) |> 
  split(~name)



#
#
#
#
#

climber_expedition = climbers |>
    group_by(expedition_id) |>
    summarise(total = n(),
              prop_success = mean(success))

success_by_size = ggplot(climber_expedition, aes(x = total, y = prop_success)) +
    geom_count() +
    labs(x = 'Expedition Team Size', y = 'Proportion of Expedition Team That Finished')

dist_of_team_finished = ggplot(climber_expedition, aes(x = prop_success)) +
    geom_histogram() +
    labs(x = 'Proportion of Expedition Team That Finished')

success_by_size + dist_of_team_finished


#
#
#
#
#
#

climbers |>
    mutate(total = n(),
              prop_success = mean(success), .by = expedition_id) |>
          ggplot( aes(x = total, y = prop_success)) +
    geom_count() +
    labs(x = 'Expedition Team Size', y = 'Proportion of Expedition Team That Finished') +
    facet_wrap(vars(season))      


#
#
#
#
#
#
#

climbers |>
    group_by(age, oxygen_used) |>
    summarise(success_rate = mean(success)) |>
    ggplot(aes(x = age, y = success_rate, color = oxygen_used)) +
    geom_point()



#
#
#
#
#

climbing_priors = c(
    prior(normal(0, 2.5), class = Intercept),
    prior(normal(0, 2.5), class = b),
    prior(exponential(1), class = sd)
)

climbing_prior_mod = brm(
    success ~ age_c + oxygen_used + (1 | expedition_id),
    data = climbers, 
    family = bernoulli(link = 'logit'),
    prior = climbing_priors,
    sample_prior = 'only'
)

check_prds = climbers |>
    add_predicted_draws(climbing_prior_mod) |>
    mutate(age = age_c - unscaled$scaled_center) |>
    unnest(age)

glimpse(check_prds)

ggplot(check_prds, aes(x = age, y = .prediction, color = oxygen_used)) +
    geom_dots(aes(side = ifelse(.prediction == 1, 'bottom', 'top'))) +
    geom_line(aes(group = .draw))

#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
