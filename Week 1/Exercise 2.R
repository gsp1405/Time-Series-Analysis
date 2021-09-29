#Load packages
require(ggplot2)

#Load dataset
data = c(4.4, 3.4, 3.3, 2.5, 7.3, 4.9, 4.8, 4.4)
anemo_loc = c(rep(0, 4), rep(1, 4))
df = data.frame(t = 1:length(data), roof = anemo_loc, Y = data)

ggplot(df, aes(x = t, y = Y, color = roof)) + geom_point()

#Define linear model
lm1 <- lm(Y ~ t + roof, data = df)
summary(lm1)

new_data = data.frame(t = 9, roof = 0)
new_data
predict(lm1, newdata = new_data)
