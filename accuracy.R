# Load ggplot2
library(ggplot2)



specie <- c(rep("Bloco" , 2) , rep("flip direira" , 2) ,rep("Flip esquerda" , 2), rep("top_spin_direita" , 2),rep("top_spin_esquerda" , 2) )
condition <- rep(c("precision" , "accuracy") , 5)
value <- c(98,100,100,49,65,99,47,100,0,0)
data <- data.frame(specie,condition,value)

# Stacked
ggplot(data, aes(fill=condition, y=value, x=specie)) +
  geom_bar(position="fill", stat="identity")+ggtitle("Teste 1")
