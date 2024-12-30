
# Model with services_count as response
glm.poisson.services <- glm(services_count ~ tenure + Contract + InternetService + MonthlyCharges,
                            family = poisson,
                            data = d.cleaned_telecom_customer_churn)

# Look at summary
summary(glm.poisson.services)

# Model predicting tenure
glm.poisson.tenure <- glm(tenure ~ MonthlyCharges + Contract + InternetService,
                          family = poisson,
                          data = d.cleaned_telecom_customer_churn)

summary(glm.poisson.tenure)


# Model predicting tencure with interaction 
glm_tenure_interaction <- glm(tenure ~ MonthlyCharges * Contract + InternetService,
                              family = poisson,
                              data = d.cleaned_telecom_customer_churn)
summary(glm_tenure_interaction)

#comparing the models
anova(glm.poisson.tenure, glm_tenure_interaction, test = "Chisq")

