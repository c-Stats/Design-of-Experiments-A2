####################################################################
###############  INIT                  #############################
####################################################################

suppressMessages(library("data.table"))
suppressMessages(library("dplyr"))
suppressMessages(library("ggplot2"))
suppressMessages(library("gridExtra"))
suppressMessages(library("reshape2"))
suppressMessages(library("glmnet"))
suppressMessages(library("plotly"))

####################################################################


####################################################################
###############  Q1                    #############################
####################################################################

data_path <- "C:/Users/frank/OneDrive/Documents/Assignments/DoE 5.24.csv"
data <- fread(data_path) %>%
			.[, Unit := NULL] %>%
			.[, (c("CycleTime", "Operator", "Temperature")) := lapply(.SD, function(x){as.factor(x)}), 
																				.SDcols = c("CycleTime", "Operator", "Temperature")]


linear_model <- lm(Score ~., data)
SUMMARY <- summary(linear_model)
ANOVA <- anova(linear_model)

print(SUMMARY)
print(ANOVA)

#Check if we can drop the variables with low t-scores
rmv <- which(SUMMARY$coefficients[, 4] > 0.05)
modified_data <- data[, .SD, .SDcols = names(data)]

for(i in 1:length(rmv)){

	col <- gsub('[[:digit:]]+', '', names(rmv)[i])
	val <- gsub("[^0-9.]", "",  val)
	base <- levels(modified_data[, col, with = FALSE][[1]])[1]

	modified_data[get(col) == val | get(col) == base, (col) := "BaseLevel"] %>%
		.[, (col) := lapply(.SD, function(x){as.factor(x)}), .SDcols = col]

}

new_model <- lm(Score ~., modified_data)
print(summary(new_model))
print(anova(new_model))

delta_SSR <- sum(new_model$residuals^2) - sum(linear_model$residuals^2)
F_stat_pval <- round(1 - pchisq(delta_SSR / length(rmv), 3), 4)

print(paste("P-value of F statistic:", F_stat_pval))


#Residual plot
data[, Residual := linear_model$residuals] %>%
	.[, Fitted_Value := linear_model$fitted.values]


residual_plots <- list()
residual_plots$plot1 <- ggplot(data, aes(y = Residual, x = CycleTime, color = Operator)) + geom_point(size = 3) +
															geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
															ggtitle("Residuals, by Cycle and Operator")

residual_plots$plot2 <- ggplot(data, aes(y = Residual, x = CycleTime, color = Temperature)) + geom_point(size = 3) +
															geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
															ggtitle("Residuals, by Cycle and Temperature")

residual_plots$plot3 <- ggplot(data, aes(y = Residual, x = Temperature, color = Operator)) + geom_point(size = 3) +
															geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
															ggtitle("Residuals, by Temperature and Operator")	
															
gridExtra::grid.arrange(grobs = residual_plots, ncol=2, nrow=2)	



normal_plots <- list()
normal_plots$plot1 <- ggplot(data, aes(y = Residual, x = Fitted_Value, color = CycleTime, shape = Operator)) + geom_point(size = 3) +
																geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
																ggtitle("Residuals versus Fit")


normal_plots$plot2 <- ggplot(data, aes(sample = Residual)) + stat_qq() + 
										stat_qq_line(color = "red", linetype = "dashed") +
										ggtitle("Residuals QQplot") +
										xlab("Theoritical") +
										ylab("Residuals")	


gridExtra::grid.arrange(grobs = normal_plots, ncol=1, nrow=2)	





####################################################################
###############  Q2                    #############################
####################################################################


data_path <- "C:/Users/frank/OneDrive/Documents/Assignments/DoE 5.30.csv"
data <- fread(data_path) %>%
			.[, Unit := NULL] 

backup <- data[, .SD, .SDcols = names(data)]

#Compute conditional means before converting to factor
by_clauses <- list(a = "Doping",
					b = "Anneal",
					c = c("Doping", "Anneal"))

conditional_means <- lapply(by_clauses, function(x){data[, lapply(.SD, mean), by = x, .SDcols = "BaseCurrent"]})
							
g <- function(x){

	copy <- x[, .SD, .SDcols = names(x)]

	if(ncol(x) > 2){

		copy[, Group := paste("D:", Doping, "A:", Anneal, sep = "")] %>%
			.[, Doping := NULL] %>%
			.[, Anneal := NULL]

	} else {

		names(copy)[which(names(copy) != "BaseCurrent")] <- "Group"

	}

	return(copy)

}

conditional_means <- lapply(conditional_means, g)

for(i in 1:3){conditional_means[[i]] <- g(conditional_means[[i]])}
names(conditional_means) <- c("Doping", "Anneal", "Doping and Anneal")


#Convert to factors, and run anova
data[, (c("Doping", "Anneal")) := lapply(.SD, function(x){as.factor(x)}), .SDcols = c("Doping", "Anneal")]

linear_model <- lm(BaseCurrent ~., data)
SUMMARY <- summary(linear_model)
ANOVA <- anova(linear_model)

print(SUMMARY)
print(ANOVA)


#Plot the conditional means
mu <- mean(data$BaseCurrent)
p <- function(x){

	out <- ggplot(x, aes(x = Group, y = BaseCurrent)) + geom_bar(stat = "identity", fill = "darkorange1", color = "black") +
														geom_hline(yintercept = mu, color = "black", linetype = "dashed", size = 1.2)

}
conditional_means_plots <- lapply(conditional_means, p)
for(i in 1:length(conditional_means_plots)){

	conditional_means_plots[[i]] <- conditional_means_plots[[i]] + xlab(names(conditional_means)[[i]])

}
conditional_means_plots[[3]] <- conditional_means_plots[[3]] + theme(axis.text.x = element_text(angle = 90))

gridExtra::grid.arrange(grobs = conditional_means_plots, ncol=2, nrow=2)	


data[, Residual := linear_model$residuals] %>%
	.[, Fitted_Value := linear_model$fitted.values]


plots <- list()
plots$plot1 <- ggplot(data, aes(y = Residual, x = Doping, color = Anneal)) + geom_point(size = 3) +
															geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
															ggtitle("Residuals, by Doping")

plots$plot2 <- ggplot(data, aes(y = Residual, x = Anneal, color = Doping)) + geom_point(size = 3) +
															geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
															ggtitle("Residuals, by Anneal")

plots$plot3 <- ggplot(data, aes(y = Residual, x = Fitted_Value, color = Doping, shape = Anneal)) + geom_point(size = 3) +
																geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
																ggtitle("Residuals versus Fit")


plots$plot4 <- ggplot(data, aes(sample = Residual)) + stat_qq() + 
										stat_qq_line(color = "red", linetype = "dashed") +
										ggtitle("Residuals QQplot") +
										xlab("Theoritical") +
										ylab("Residuals")	

gridExtra::grid.arrange(grobs = plots, ncol=2, nrow=2)	



backup[, Doping_Sq := Doping^2] %>%
		.[, Doping_times_Anneal := Doping * Anneal]

linear_model <- lm(BaseCurrent ~., backup)
SUMMARY <- summary(linear_model)

print(SUMMARY)


backup[, Fitted_Value := linear_model$fitted.values]
plot_ly() %>% 
  add_trace(data = backup,  x = data$Doping, y = data$Anneal, z = data$Fitted_Value, type="mesh3d") %>%
  layout(scene = list(xaxis = list(title = "Doping"),
  						yaxis = list(title = "Anneal"),
  						zaxis = list(title = "Fitted Value")))




####################################################################
###############  Q2                    #############################
####################################################################


