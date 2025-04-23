data = read.csv("TurtleHumerus.csv")
data$Species = as.factor(data$Species)

model1 = lm(Short_d~Length + Species, data=data)
summary(model1)

model2 = lm(Short_d~Length * Species, data=data)
summary(model2)

anova(model1, model2)
AIC(model1, model2)

dev.new(width=16, height=8)
par(mfrow=c(1, 2))

plot(Short_d~Length, pch=c(0, 1)[Species], data=data)

with(
    data,
    curve(
        model1$coef[1] + model1$coef[2] * x,
        from=min(Length[Species=="Cc"]),
        to=max(Length[Species=="Cc"]),
        add=T
    )
)

with(
    data,
    curve(
        model1$coef[1] + model1$coef[2] * x + model1$coef[3],
        from=min(Length[Species=="Cm"]),
        to=max(Length[Species=="Cm"]),
        lty=2,
        add=T
    )
)

legend("topleft", legend=c("Cc", "Cm"), pch=c(0, 1), lty=c(1, 2))

plot(Short_d~Length, pch=c(0, 1)[Species], data=data)

with(
    data,
    curve(
        model2$coef[1] + model2$coef[2] * x,
        from=min(Length[Species=="Cc"]),
        to=max(Length[Species=="Cc"]),
        add=T
    )
)

with(
    data,
    curve(
        model2$coef[1] + model2$coef[3] + (model2$coef[2] + model2$coef[4]) * x,
        from=min(Length[Species=="Cm"]),
        to=max(Length[Species=="Cm"]),
        lty=2,
        add=T
    )
)

legend("topleft", legend=c("Cc", "Cm"), pch=c(0, 1), lty=c(1, 2))