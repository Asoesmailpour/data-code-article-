> install.packages(c("quantreg", "readxl", "gmm"))
> library(quantreg)
> library(readxl)
> library(gmm)
> mydata <- read_excel("data-article.tax.xlsx")
> colnames(data-article.tax)[colnames(data-article.tax) == "شاخص فلاکت"] <- "Misery"
> colnames(data-article.tax)[colnames(data-article.tax) == "مالیات بر عایدی سرمایه"] <- "CAPITAL_TAX"
> colnames(data-article.tax)[colnames(data-article.tax) == "درامد متوسط خانوار"] <- "AVG_INCOME"
> colnames(data-article.tax)[colnames(data-article.tax) == "نرخ اشتغال صنعتی"] <- "IND_EMP_RATE"
> colnames(data-article.tax)[colnames(data-article.tax) == "سطح تحصیلات"] <- "EDUCATION_LEVEL"
> model_formula <- Misery ~ CAPITAL_TAX + AVG_INCOME + IND_EMP_RATE + EDUCATION_LEVEL
> qr_025 <- rq(model_formula, data = data-article.tax, tau = 0.25)
> qr_050 <- rq(model_formula, data = data-article.tax, tau = 0.50)
> qr_075 <- rq(model_formula, data = data-article.tax, tau = 0.75)
> summary(qr_025)
> summary(qr_050)
> summary(qr_075)
> X_mat <- model.matrix(model_formula, data = data-article.tax)
> Z_mat <- model.matrix(~ CAPITAL_TAX + AVG_INCOME + 0, data = data-article.tax)
> moment_function_gmm <- function(theta, y, x, z) {
  resid <- y - (x %*% theta)
> return(t(z) %*% resid) 
}
> ols_init <- lm(model_formula, data = data-article.tax)
theta.init <- coef(ols_init)
> gmm_model_result <- gmm(moment_function_gmm, 
                        x = list(y = data-article.tax$Misery, 
                                 x = X_mat, 
                                 z = Z_mat), 
                        theta = theta.init, 
                        type = "cue") 
> summary(gmm_model_result)