library(ggplot2)
str(titanic)

# Use ggplot() for the first instruction
ggplot(titanic, aes(x=factor(Pclass), fill=factor(Sex))) +
geom_bar(position="dodge")


# Use ggplot() for the second instruction
ggplot(titanic, aes(x=factor(Pclass), fill=factor(Sex))) +
geom_bar(position="dodge") +
facet_grid(". ~ Survived") +
scale_fill_manual("Gender", values=c("pink","lightblue"))

# Position jitter (use below)
posn.j <- position_jitter(0.6, 0)
titanic$Survived <- factor(titanic$Survived, labels = c("Not Survived", "Survived"))

# Use ggplot() for the last instruction
ggplot(titanic, aes(x=factor(Pclass), y=Age, col=factor(Sex))) +
facet_grid(". ~ Survived") +
scale_color_manual("Gender", values=c("violet", "royalblue")) +
scale_x_discrete("Passenger Class") +
geom_jitter(size=3, alpha=0.6, position=posn.j) 

