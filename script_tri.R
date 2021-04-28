#######################################################
#                                                     #
# MONOGRAFIA DE GRADUAÇÃO - THIAGO VALENTIM MARQUES   #
#                                                     #
# ORIENTADOR: PROF. DR ELIARDO GUIMARÃES DA COSTA     #
#                                                     #
#######################################################

#------------ FIGURAS UTILIZADAS NO TCC --------------#

require(plotrix) #Arcos de círculo

par(mfrow=c(1,1),
    mar=c(4,4.3,0.5,1))

### Figura 1

curve(1/(1+exp(-2*(x-0.5))),xlim=c(-4,4),
      ylab=expression(P(theta)),
      xlab=expression(theta),
      lwd=2,cex.lab=1.3, cex.axis=1.1)

### Figura 2

curve((1+exp(0.2-x))^(-1),xlim=c(-4,4),
      ylab="Probabilidade de resposta correta",
      xlab=expression(theta),
      lwd=2,cex.lab=1.3, cex.axis=1.1)
curve((1+exp(1.2-x))^(-1),xlim=c(-4,4),
      lwd=2,cex.lab=1.3, cex.axis=1.1,col="red",
      add=TRUE)
curve((1+exp(-0.7-x))^(-1),xlim=c(-4,4),
      lwd=2,cex.lab=1.3, cex.axis=1.1,col="blue",
      add=TRUE)
legend("bottomright",legend=c(expression(b[k]==0.2),
                              expression(b[k]==1.2),
                              expression(b[k]==-0.7)),
      col=c("black","red","blue"),lwd=2)
segments(-5,0.5,0.2-log(0.5^(-1)-1),0.5,lty=2)
segments(-5,0.5,1.2-log(0.5^(-1)-1),0.5,lty=2)
segments(0.2-log(0.5^(-1)-1),-1,0.2-log(0.5^(-1)-1),
         0.5,lty=2)
segments(1.2-log(0.5^(-1)-1),-1,1.2-log(0.5^(-1)-1),
         0.5,lty=2)
segments(-0.7-log(0.5^(-1)-1),-1,-0.7-log(0.5^(-1)-1),
         0.5,lty=2)
axis(side=1,at=c(-0.7,0.2,1.2),cex.axis=1.1)
axis(side=2,at=c(0.5),cex.axis=1.1)

### Figura 3

curve((exp(0.8*(x+0.5))/(1+exp(0.8*(x+0.5)))),
      xlim=c(-6,6),ylim=c(0,1),
      ylab="Probabilidade de resposta correta",
      xlab=expression(theta),
      lwd=2,cex.lab=1.3, cex.axis=1.1)
abline(h=0.5,lty=2)
segments(-0.5,-1,-0.5,0.5,lty=2)
points(-0.5,0.50,pch=16)
segments(-1,
         (1*0.8/4)*(-1+0.5)+0.50,
         0,
         (1*0.8/4)*(-1+0.5)+0.50)
segments(0,
         (1*0.8/4)*(-1+0.5)+0.50,
         0,
         (1*0.8/4)*(0+0.5)+0.50)
legend(-7,1.05,
       c(expression(a[k]==0.8),
         expression(b[k]==-0.5)),bty="n")
axis(1,at=c(-0.5),labels=c(expression(b[k])))
axis(2,at=c(0.5),cex.axis=1.1)
text(1.8,0.54,c(expression(paste("inclinação = ",
                                 a[k]/4))))
curve((1*0.8/4)*(x+0.5)+0.50,xlim=c(-2.5,1.5),
      lty=2,add=TRUE)
### Figura 4

curve(x+15,xlim=c(-4,4),ylim=c(0,1),
      ylab="Probabilidade de resposta correta",
      xlab=expression(theta),
      lwd=2,cex.lab=1.3, cex.axis=1.1)
segments(-5,0,0,0,lty=1,lwd=2)
segments(0,0,0,1,lty=1,lwd=2)
segments(0,1,5,1,lty=1,lwd=2)

### Figura 5

curve((1+exp(-0.8*(x-1)))^(-1),xlim=c(-4,4),ylim=c(0,1),
      ylab="Probabilidade de resposta correta",
      xlab=expression(theta),
      lwd=2,cex.lab=1.3, cex.axis=1.1)
curve((1+exp(-1.6*(x+1.5)))^(-1),xlim=c(-4,4),ylim=c(0,1),
      ylab="Probabilidade de resposta correta",
      xlab=expression(theta),col="red",
      lwd=2,cex.lab=1.3, cex.axis=1.1,add=TRUE)
legend(1.2,0.16,
       legend=c(expression(paste(a[1]==1.6," e ",
                           b[1]==-1.5)),
                expression(paste(a[2]==0.8," e ",
                           b[2]==1))),
       col=c("red","black"),lwd=2,bty="n",cex=1.0)
abline(h=0.5,lty=2)
axis(2,at=0.5,cex.axis=1.1)
segments(-1.5,-1,-1.5,0.5,lty=2)
segments(1,-1,1,0.5,lty=2)
text(0.25,0.86,labels="Item 1",cex=1.2)
text(2.95,0.74,labels="Item 2",cex=1.2)
axis(1,at=c(-1.5,1),
     labels=c(expression(b[1]),expression(b[2])),
     cex.axis=1.1)
curve((1.6/4)*x-(1.6/4)*(-1.5)+0.5,
      xlim=c(-2.5,-0.5),lty=2,add=TRUE)
curve((0.8/4)*x-(0.8/4)*(1)+0.5,
      xlim=c(-0.9,2.8),lty=2,add=TRUE)
draw.arc(-1.5,0.5, 0.5:1.2, deg2 = 1:64, col = "black")
draw.arc(1,0.5, 0.5:1.2, deg2 = 1:47, col = "black")
text(-0.3,0.562,labels=expression(paste(tg^-1,"(",
                       0.25 %*% a[1],")")),cex=1.0)
text(2.2,0.538,labels=expression(paste(tg^-1,"(",
                       0.25 %*% a[2],")")),cex=1.0)

### Figura 6

curve(0.2+(1-0.2)*(exp(1*(x-0.5))/(1+exp(1*(x-0.5)))),
      xlim=c(-4,4),ylim=c(0,1),
      ylab="Probabilidade de resposta correta",
      xlab=expression(theta),
      lwd=2,cex.lab=1.3, cex.axis=1.1)
abline(h=0.2,lty=2)
curve((exp(1*(x-0.5))/(1+exp(1*(x-0.5)))),
      xlim=c(-4,4),ylim=c(0,1),
      ylab="Probabilidade de resposta correta",
      xlab=expression(theta),col="red",
      lwd=2,cex.lab=1.3, cex.axis=1.1,add=TRUE)
legend("topleft",c("ML3","ML2"),
       col=c("black","red"),lwd=2)

### Figura 7
curve(0.10+(1-0.1)*(exp(0.8*(x+0.5))/(1+exp(0.8*(x+0.5)))),
      xlim=c(-6,6),ylim=c(0,1),
      ylab="Probabilidade de resposta correta",
      xlab=expression(theta),
      lwd=2,cex.lab=1.3, cex.axis=1.1)
abline(h=0.1,lty=2)
segments(-7,0.55,-0.5,0.55,lty=2)
segments(-0.5,-1,-0.5,0.55,lty=2)
points(-0.5,0.55,pch=16)
segments(-1,
         (0.9*0.8/4)*(-1+0.5)+0.55,
         0,
         (0.9*0.8/4)*(-1+0.5)+0.55)
segments(0,
         (0.9*0.8/4)*(-1+0.5)+0.55,
         0,
         (0.9*0.8/4)*(0+0.5)+0.55)
legend(-7,1.05,
       c(expression(a[k]==0.8),
         expression(b[k]==-0.5),
         expression(c[k]==0.1)),bty="n")
axis(1,at=c(-0.5),labels=c(expression(b[k])))
axis(2,at=c(0.1),labels=c(expression(c[k])),las=1)
text(-5.6,0.58,c(expression((1+c[k])/2)))
text(1.8,0.54,c(expression(paste("inclinação = ",
                                  a[k](1-c[k])/4))))
curve((0.9*0.8/4)*(x+0.5)+0.55,xlim=c(-2.5,1.5),
      lty=2,add=TRUE)

### Figura 8
curve(dnorm(x,0,1),
      xlim=c(-4,4),ylim=c(0,0.55),
      ylab="Densidade",
      xlab=expression(theta),
      lwd=2,cex.lab=1.3, cex.axis=1.1)

### Figura 9 (Régua - ENEM)

### Figura 10

inf <- function(x,a,b,c){
 tri = c+(1-c)*((exp(a*(x-b)))/(1+exp(a*(x-b))))
 ik = a^(2)*((1-tri)/(tri))*((tri-c)/(1-c))^(2)
 return(ik)
}

curve(inf(x,3,0,0),lwd=2,xlim=c(-4,4),
      xlab=expression(theta),
      ylab="Informação do Item",
      cex.lab=1.3, cex.axis=1.1)
curve(inf(x,2,0,0),lwd=2,col="red",add=TRUE)
curve(inf(x,3,1,0.2),lwd=2,col="green",add=TRUE)
curve(inf(x,0.9,-2,0),lwd=2,col="blue",add=TRUE)

inf(0,3,0,0)
inf(0,2,0,0)
inf(1,3,1,0.2)
inf(-2,0.9,-2,0)
#Tabela 2 (em .tex)

tab2 <- data.frame(item=1:4,a=c(3,2,3,0.9),
                   b=c(0,0,1,-2),c=c(0,0,0.2,0))
require(xtable)
xtable(tab2)

### Figura 11

curve(inf(x,3,0,0)+inf(x,2,0,0)+
        inf(x,3,1,0.2)+inf(x,0.9,-2,0),
      lwd=2,xlim=c(-4,4),
      xlab=expression(theta),
      ylab="Informação do Teste",
      cex.lab=1.3, cex.axis=1.1,xaxt="n")
abline(v=c(-2,-1,0,1,2),lty=2,col="grey50")
axis(1,at=c(-4:4),labels=c(-4:4),cex.axis=1.3)

integrate(function(x){inf(x,3,0,0)+inf(x,2,0,0)+
            inf(x,3,1,0.2)+inf(x,0.9,-2,0)},-2,2)
integrate(function(x){inf(x,3,0,0)+inf(x,2,0,0)+
    inf(x,3,1,0.2)+inf(x,0.9,-2,0)},-100, 100)
(7.018935/7.692922)*100

### Figura 12

curve(inf(x,3,0,0)+inf(x,2,0,0)+
        inf(x,3,1,0.2)+inf(x,0.9,-2,0),
      lwd=2,xlim=c(-4,4),
      xlab=expression(theta),
      ylab="Informação do Teste",
      cex.lab=1.3, cex.axis=1.1,xaxt="n")
axis(1,at=c(-4:4),labels=c(-4:4),cex.axis=1.3)

curve(1/(inf(x,3,0,0)+inf(x,2,0,0)+
        inf(x,3,1,0.2)+inf(x,0.9,-2,0))^(0.5),
      lwd=2,xlim=c(-4,4),add=TRUE,lty=2,col="red")

##########################################################
##########################################################

#--------- DADOS DO EXAME DE SELEÇÃO --------------------#

library(readxl)
exame <- read_excel("exam_sel_2020_pre.xlsx")
exame2 <- read_excel("exam_sel_2020.xlsx")

require(tidyverse) #Pacote tidyverse

exame %>%
group_by(`Tipo de escola que cursou o ensino fundamental`)%>%
summarize(n = n())

exame %>%
  filter(Sexo == "F")%>%
  group_by(`Tipo de escola que cursou o ensino fundamental`)%>%
  summarize(n = n())

exame %>%
  filter(Sexo == "M")%>%
  group_by(`Tipo de escola que cursou o ensino fundamental`)%>%
  summarize(n = n())

exame2 %>%
  group_by(`Você se autodeclara preto, pardo ou indígena?`)%>%
  summarize(n = n())

exame2 %>%
  filter(`Você se autodeclara preto, pardo ou indígena?`=="Sim")%>%
  group_by(`Tipo de escola que cursou o ensino fundamental`)%>%
  summarize(n = n())

exame %>%
  group_by(`Tipo de escola que cursou o ensino fundamental`)%>%
  summarize_at(vars(escore_matematica), c(mean, sd))

exame %>%
  summarize_at(vars(escore_portugues), c(summary))
exame %>%
  summarize_at(vars(escore_matematica), c(summary))

### Figura 14

ggplot(exame,aes(x=escore_portugues))+
  geom_histogram(aes(y = ..density..),col = "red")+
  geom_density(fill="blue",col="blue", alpha = 0.2,col="transparent")+
  xlab("Escores de Língua Portuguesa")+
  coord_cartesian(xlim = c(100, 900),ylim=c(0,0.0045))+ 
  ylab("Densidade") +theme_light()+
  #annotate("text",x=70,y=2450,hjust=0,vjust=0,
           #label="(a)",colour="black",size=4.5)
theme(axis.title.y = element_text(size = rel(1.2), angle = 90,
                                  vjust=2),
      axis.title.x = element_text(size = rel(1.2), angle = 0,
                                  vjust = -1.0),
      axis.text.x = element_text(angle = 0, hjust = 0.5, 
                                 size=14,color="black"),
      axis.text.y = element_text(angle = 90, hjust = 0.5, 
                                 size=14,color="black"))

### Figura 15

ggplot(exame,aes(x=escore_matematica))+
  geom_histogram(aes(y = ..density..),col = "red")+
  geom_density(fill="blue",col="blue", alpha = 0.2,col="transparent")+
  xlab("Escores de Matemática")+
  coord_cartesian(xlim = c(100, 900),ylim=c(0,0.0045))+ 
  ylab("Densidade") +theme_light()+
  #annotate("text",x=70,y=2450,hjust=0,vjust=0,
  #label="(a)",colour="black",size=4.5)
  theme(axis.title.y = element_text(size = rel(1.2), angle = 90,
                                    vjust=2),
        axis.title.x = element_text(size = rel(1.2), angle = 0,
                                    vjust = -1.0),
        axis.text.x = element_text(angle = 0, hjust = 0.5, 
                                   size=14,color="black"),
        axis.text.y = element_text(angle = 90, hjust = 0.5, 
                                   size=14,color="black"))

#install.packages("moments")
require(moments)

skewness(exame$escore_matematica) #Assimetria positiva
skewness(exame$escore_portugues) #Assimetria negativa

#Folhas de respostas na forma de matriz
n=dim(exame)[1]
oficial<-matrix(NA,nrow=n,ncol=40)
oficial<-as.data.frame(oficial)
for(i in 1:dim(exame)[1]){
  for(j in 1:40){
    oficial[i,j]<-substr(exame$leitura_objetivas[i],j,j)
  }
}

oficial<-oficial[,-c(7,26)] #Retirar as questões anuladas
colnames(oficial)<-1:38
head(oficial)

gabarito <- c("A","C","B","A","B","C","C","A","C","A",
              "A","D","D","A","C","A","C","A","D",
              "B","B","B","A","A","A","C","A","B","C",
              "D","A","A","C","C","A","A","D","C")

#Acertos dos itens (dicotomizados)
total<-matrix(NA,nrow=n,ncol=38)
for(i in 1:n){
  total[i,]<-as.numeric(oficial[i,]==gabarito)
}
colnames(total)<-1:38

#Proporção de acertos
prop<-round(100*apply(total,2,mean),2) #Proporção de acertos
exame.prop <- data.frame(questao=as.factor(1:38),prop)

#Figura 16
prop.port<-exame.prop[1:19,]
  
ggplot(prop.port,aes(x=questao,y=prop))+geom_col()+
  xlab("Questões")+coord_cartesian(ylim=c(0,100))+
  ylab("Proporção de acertos em Língua Portuguesa")+
    theme_light()+
  theme(axis.title.x = element_text(size = rel(1.2), angle = 0,
                                    vjust = 0.0),
        axis.text.x = element_text(size = rel(1.2), angle = 0,
                                   vjust = 0.5),
         axis.title.y = element_text(size = rel(1.2), angle = 90,
                                    vjust = 1.5),
        axis.text.y = element_text(angle = 0, hjust = 0.5, 
                                   size=14,color="black"))
#Figura 17
prop.math<-exame.prop[20:38,]

ggplot(prop.math,aes(x=questao,y=prop))+geom_col()+
  xlab("Questões")+coord_cartesian(ylim=c(0,100))+
  ylab("Proporção de acertos em Matemática")+
  theme_light()+
  theme(axis.title.x = element_text(size = rel(1.2), angle = 0,
                                    vjust = 0.0),
        axis.text.x = element_text(size = rel(1.2), angle = 0,
                                   vjust = 0.5),
        axis.title.y = element_text(size = rel(1.2), angle = 90,
                                    vjust = 1.5),
        axis.text.y = element_text(angle = 0, hjust = 0.5, 
                                   size=14,color="black"))

### Dispersão entre escore e proporção de acertos
### Língua Portuguesa
summary(exame$escore_portugues)
corte<-cut(exame$escore_portugues,
           breaks = seq(96.4652,817,60))
n<-length(table(corte))
ok <- data.frame(total,corte)
levels(ok$corte)<-seq(126,786,60)

matriz <- matrix(NA,nrow=n,ncol=19)
for(i in 1:19){
  matriz[,i]<-tapply(ok[,i],corte,mean)
}
matriz<-as.vector(matriz)
questao<-rep(1:19,each=n)
score<-rep(seq(126,786,60),19)

data<-data.frame(matriz,questao,score)

#Figura 18
ggplot(data,aes(x=score,y=matriz))+geom_point()+
  facet_wrap(~questao)+coord_cartesian(ylim = c(0, 1))+
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE)+xlab("Escore")+
  ylab("Proporção de acertos nas questões de Língua Portuguesa")+
  theme_light()+
  theme(axis.title.x = element_text(size = rel(1.2), angle = 0,
                                    vjust = 0.0),
        axis.text.x = element_text(angle = 0, vjust = 0.5, 
                                   size=14,color="black"),
        axis.title.y = element_text(size = rel(1.2), angle = 90,
                                    vjust = 1.5),
        axis.text.y = element_text(angle = 0, hjust = 0.5, 
                                   size=14,color="black"))
#Inches 10 x 11

### Dispersão entre escore e proporção de acertos
### Matemática

summary(exame$escore_matematica)
corte<-cut(exame$escore_matematica,
           breaks = seq(240,960,60))
n<-length(table(corte))
ok <- data.frame(total[,20:38],corte)
levels(ok$corte)<-seq(270,930,60)

matriz <- matrix(NA,nrow=n,ncol=19)
for(i in 1:19){
  matriz[,i]<-tapply(ok[,i],corte,mean)
}
mean(matriz[,12])
matriz<-as.vector(matriz)
questao<-rep(20:38,each=n)
score<-rep(seq(270,930,60),19)

data<-data.frame(matriz,questao,score)

#Figura 19
ggplot(data,aes(x=score,y=matriz))+geom_point()+
  facet_wrap(~questao)+geom_smooth(method = "glm", 
                                   method.args = list(family = "binomial"), 
                                   se = FALSE)+
  coord_cartesian(ylim = c(0, 1))+ xlab("Escore")+
  ylab("Proporção de acertos nas questões de Matemática")+
  theme_light()+
  theme(axis.title.x = element_text(size = rel(1.2), angle = 0,
                                    vjust = 0.0),
        axis.text.x = element_text(angle = 0, vjust = 0.5, 
                                   size=14,color="black"),
        axis.title.y = element_text(size = rel(1.2), angle = 90,
                                    vjust = 1.5),
        axis.text.y = element_text(angle = 0, hjust = 0.5, 
                                   size=14,color="black"))
#Inches 10 x 11

#################################################################
######## ESTIMAÇÃO DOS PARÂMETROS E DAS PROFICIÊNCIAS ###########
#################################################################

require(ltm)
require(xtable)

###################################
##------ LÍNGUA PORTUGUESA ------## 
###################################

# Birnbaum's Three Parameter Model (pacote ltm)
fit.port <- tpm(total[,1:19], IRT.param = TRUE,
                type="latent.trait", max.guessing = 0.5)
coef(fit.port)
# Latent Trait Model
#fit.port2 <- ltm(total[,1:19] ~ z1,IRT.param = TRUE)

require(mirt)
citation("ltm")
citation("mirt")

info<-NULL
for(i in 1:19){
x <- plot(fit.port,type="IIC",items=i)
info[i]<-max(x[,2])
}
dados2<-data.frame(round(coef(fit.port)[,c(3,2,1)],2),
                   round(info,2))
xtable(dados2)

TRI<-function(x,a,b,c){
  tri = c+(1-c)*((exp(a*(x-b)))/(1+exp(a*(x-b))))
  return(tri)
}

par<-NULL
for(i in 1:19){
n<-length(seq(-5,5,0.01))
par[((i-1)*n+1):(n*i)]<-TRI(seq(-5,5,0.01),
                            dados2[i,1],
                            dados2[i,2],
                            dados2[i,3])
}
x<-rep(seq(-5,5,0.01),19)
item<-rep(as.factor(1:19),each=n)
data2<-data.frame(x,par,item)

# Figura 20
ggplot(data2,aes(x=x,y=par))+geom_line(col="blue",cex=1.05)+
  facet_wrap(~item)+xlab(expression(theta))+
  ylab("Probabilidade de acerto")+
  theme_light()+
  theme(axis.title.x = element_text(size = rel(1.2), angle = 0,
                                    vjust = 0.0),
        axis.text.x = element_text(angle = 0, vjust = 0.5, 
                                   size=14,color="black"),
        axis.title.y = element_text(size = rel(1.2), angle = 90,
                                    vjust = 1.5),
        axis.text.y = element_text(angle = 0, hjust = 0.5, 
                                   size=14,color="black"))
#Inches 7 x 11

inf <- function(x,a,b,c){
  tri = c+(1-c)*((exp(a*(x-b)))/(1+exp(a*(x-b))))
  ik = a^(2)*((1-tri)/(tri))*((tri-c)/(1-c))^(2)
  return(ik)
}

par<-NULL
for(i in 1:19){
  n<-length(seq(-5,5,0.01))
  par[((i-1)*n+1):(n*i)]<-inf(seq(-5,5,0.01),
                              dados2[i,1],
                              dados2[i,2],
                              dados2[i,3])
}
x<-rep(seq(-5,5,0.01),19)
item<-rep(as.factor(1:19),each=n)
data2<-data.frame(x,par,item)

# Figura 21
ggplot(data2,aes(x=x,y=par))+geom_line(col="blue",cex=1.05)+
  facet_wrap(~item)+xlab(expression(theta))+
  ylab("Função de Informação do Item")+
  theme_light()+
  theme(axis.title.x = element_text(size = rel(1.2), angle = 0,
                                    vjust = 0.0),
        axis.text.x = element_text(angle = 0, vjust = 0.5, 
                                   size=14,color="black"),
        axis.title.y = element_text(size = rel(1.2), angle = 90,
                                    vjust = 1.5),
        axis.text.y = element_text(angle = 0, hjust = 0.5, 
                                   size=14,color="black"))
#Inches 7 x 11

data3 <- data.frame(xx=seq(-5,5,0.01),
               yy=as.vector(tapply(data2$par,data2$x,sum)),
       zz=1/(as.vector(tapply(data2$par,data2$x,sum)^(0.5))))

# Figura 22

ggplot(data3,aes(x=xx,y=yy))+geom_line(col="blue",cex=1.05)+
  xlab(expression(theta))+geom_line(aes(x=xx,y=zz),
                                    col="red",lty=2,cex=1.05)+
  ylab("Função de Informação do Teste")+
  theme_light()+
  theme(axis.title.x = element_text(size = rel(1.2), angle = 0,
                                    vjust = 0.0),
        axis.text.x = element_text(angle = 0, vjust = 0.5, 
                                   size=14,color="black"),
        axis.title.y = element_text(size = rel(1.2), angle = 90,
                                    vjust = 1.5),
        axis.text.y = element_text(angle = 0, hjust = 0.5, 
                                   size=14,color="black"))
#plot(fit.port,type="ICC",main="")
#plot(fit.port,type="ICC",main="",zrange=c(-5,5),items=c(12,17))
#plot(fit.port,type="IIC",main="",zrange=c(-5,5),items=c(12,17))
#information(fit.port,c(-10,10))

#Empirical Bayes
theta<-factor.scores(fit.port,method="EAP",
                     resp.patterns = total[,1:19]) 

theta$score.dat #Traços latentes estimados 
names(theta$score.dat)

# Figura 23
theta$score.dat%>%
  ggplot(.,aes(x=z1))+geom_histogram(col="blue",fill="grey50",
                                 breaks=seq(-4, 4, by=0.25))+
  ylab("Frequência")+
  xlab(expression(paste("Traço Latente"," (",
                       theta,")")))+
  theme_light()+
  theme(axis.title.x = element_text(size = rel(1.2), angle = 0,
                                    vjust = 0.0),
        axis.text.x = element_text(angle = 0, vjust = 0.5, 
                                   size=14,color="black"),
        axis.title.y = element_text(size = rel(1.2), angle = 90,
                                    vjust = 1.5),
        axis.text.y = element_text(angle = 0, hjust = 0.5, 
                                   size=14,color="black"))


############## JUNTAR O ESCORE AO DATASET EXAME

exame.teste<-exame %>%
  mutate(lat.port = theta$score.dat[,22],
         acertos.port = apply(total[,1:19],1,sum))

lat.port = theta$score.dat[,22]

# Figura 24
ggplot(exame.teste,aes(x=acertos.port,y=lat.port,
                       group=acertos.port))+
  geom_boxplot(aes(y=lat.port,group=acertos.port),
               fill="grey60")+
  #geom_point(alpha=0.1,color="yellow")+
  #geom_jitter(color="yellow", size=0.3, alpha=0.1) +
  coord_cartesian(xlim=c(-0.5,19.5))+
  scale_x_discrete(name ="Acertos em Língua Portuguesa", 
                   limits=c(0:19))+
  ylab(expression(paste("Traço latente"," (",
                        theta,")")))+
  theme_light()+
  theme(axis.title.x = element_text(size = rel(1.2), angle = 0,
                                    vjust = 0.0),
        axis.text.x = element_text(angle = 0, vjust = 0.5, 
                                   size=14,color="black"),
        axis.title.y = element_text(size = rel(1.2), angle = 90,
                                    vjust = 1.5),
        axis.text.y = element_text(angle = 0, hjust = 0.5, 
                                   size=14,color="black"))
ordem.tri <- exame %>%
  mutate(lat.port = theta$score.dat[,22],
         acertos.port = apply(total[,1:19],1,sum))%>%
  select(Nome,escore_portugues,lat.port,acertos.port)%>%
  filter(exame$Unidade == "Natal - Zona Norte")%>%
  arrange(desc(lat.port))

ordem.esc.port <- exame %>%
  mutate(lat.port = theta$score.dat[,22],
         acertos.port = apply(total[,1:19],1,sum))%>%
  select(Nome,escore_portugues,lat.port,acertos.port)%>%
  filter(exame$Unidade == "Natal - Zona Norte")%>%
  arrange(desc(escore_portugues))

# Figura 25

head(total[,1:19])

comp5.11<-cbind(total[,1:19],
                lat.port = theta$score.dat[,22],
      acertos.port = apply(total[,1:19],1,sum))

maior.5 <- as.data.frame(comp5.11)%>%
  filter(acertos.port %in% c(5))%>%
  arrange(desc(lat.port))
maior.5 <- maior.5[1,]

menor.11 <- as.data.frame(comp5.11)%>%
  filter(acertos.port %in% c(11))%>%
  arrange(desc(lat.port))
menor.11 <- menor.11[dim(menor.11)[1],]

tabela3<-rbind(maior.5[,1:19],menor.11[,1:19])
xtable(tabela3)

###################################
##--------- MATEMÁTICA ----------## 
###################################

# Birnbaum's Three Parameter Model (pacote ltm)
fit.mat <- tpm(total[,20:38], IRT.param = TRUE,
                type="latent.trait", max.guessing = 0.5)
fit.mat
coef(fit.mat)

info<-NULL
for(i in 1:19){
  x <- plot(fit.mat,type="IIC",items=i)
  info[i]<-max(x[,2])
}
dados2<-data.frame(round(coef(fit.mat)[,c(3,2,1)],2),
                   round(info,2))
xtable(dados2)

TRI<-function(x,a,b,c){
  tri = c+(1-c)*((exp(a*(x-b)))/(1+exp(a*(x-b))))
  return(tri)
}

par<-NULL
for(i in 1:19){
  n<-length(seq(-5,5,0.01))
  par[((i-1)*n+1):(n*i)]<-TRI(seq(-5,5,0.01),
                              dados2[i,1],
                              dados2[i,2],
                              dados2[i,3])
}
x<-rep(seq(-5,5,0.01),19)
item<-rep(as.factor(20:38),each=n)
data2<-data.frame(x,par,item)

# Figura 26
ggplot(data2,aes(x=x,y=par))+geom_line(col="blue",cex=1.05)+
  facet_wrap(~item)+xlab(expression(theta))+
  ylab("Probabilidade de acerto")+
  theme_light()+
  theme(axis.title.x = element_text(size = rel(1.2), angle = 0,
                                    vjust = 0.0),
        axis.text.x = element_text(angle = 0, vjust = 0.5, 
                                   size=14,color="black"),
        axis.title.y = element_text(size = rel(1.2), angle = 90,
                                    vjust = 1.5),
        axis.text.y = element_text(angle = 0, hjust = 0.5, 
                                   size=14,color="black"))
#Inches 7 x 11

inf <- function(x,a,b,c){
  tri = c+(1-c)*((exp(a*(x-b)))/(1+exp(a*(x-b))))
  ik = a^(2)*((1-tri)/(tri))*((tri-c)/(1-c))^(2)
  return(ik)
}

par<-NULL
for(i in 1:19){
  n<-length(seq(-5,5,0.01))
  par[((i-1)*n+1):(n*i)]<-inf(seq(-5,5,0.01),
                              dados2[i,1],
                              dados2[i,2],
                              dados2[i,3])
}
x<-rep(seq(-5,5,0.01),19)
item<-rep(as.factor(20:38),each=n)
data2<-data.frame(x,par,item)

# Figura 27
ggplot(data2,aes(x=x,y=par))+geom_line(col="blue",cex=1.05)+
  facet_wrap(~item)+xlab(expression(theta))+
  ylab("Função de Informação do Item")+
  theme_light()+
  theme(axis.title.x = element_text(size = rel(1.2), angle = 0,
                                    vjust = 0.0),
        axis.text.x = element_text(angle = 0, vjust = 0.5, 
                                   size=14,color="black"),
        axis.title.y = element_text(size = rel(1.2), angle = 90,
                                    vjust = 1.5),
        axis.text.y = element_text(angle = 0, hjust = 0.5, 
                                   size=14,color="black"))
#Inches 7 x 11

data3 <- data.frame(xx=seq(-5,5,0.01),
        yy=as.vector(tapply(data2$par,data2$x,sum)),
        zz=1/(as.vector(tapply(data2$par,data2$x,sum)^(0.5))))

# Figura 28

ggplot(data3,aes(x=xx,y=yy))+geom_line(col="blue",cex=1.05)+
  xlab(expression(theta))+geom_line(aes(x=xx,y=zz),
                                    col="red",lty=2,cex=1.05)+
  coord_cartesian(ylim=c(0,12))+
  ylab("Função de Informação do Teste")+
  theme_light()+
  theme(axis.title.x = element_text(size = rel(1.2), angle = 0,
                                    vjust = 0.0),
        axis.text.x = element_text(angle = 0, vjust = 0.5, 
                                   size=14,color="black"),
        axis.title.y = element_text(size = rel(1.2), angle = 90,
                                    vjust = 1.5),
        axis.text.y = element_text(angle = 0, hjust = 0.5, 
                                   size=14,color="black"))

#Empirical Bayes
theta<-factor.scores(fit.mat,method="EAP",
                     resp.patterns = total[,20:38]) 

theta$score.dat #Traços latentes estimados 

# Figura 29
theta$score.dat%>%
  ggplot(.,aes(x=z1))+geom_histogram(col="blue",fill="grey50",
                                     breaks=seq(-4, 4, by=0.25))+
  ylab("Frequência")+
  xlab(expression(paste("Traço Latente"," (",
                        theta,")")))+
  theme_light()+
  theme(axis.title.x = element_text(size = rel(1.2), angle = 0,
                                    vjust = 0.0),
        axis.text.x = element_text(angle = 0, vjust = 0.5, 
                                   size=14,color="black"),
        axis.title.y = element_text(size = rel(1.2), angle = 90,
                                    vjust = 1.5),
        axis.text.y = element_text(angle = 0, hjust = 0.5, 
                                   size=14,color="black"))

############## JUNTAR O ESCORE AO DATASET EXAME

exame.teste<-exame %>%
  mutate(lat.mat = theta$score.dat[,22],
         acertos.mat = apply(total[,20:38],1,sum))

lat.mat = theta$score.dat[,22]

# Figura 30
ggplot(exame.teste,aes(x=acertos.mat,y=lat.mat,
                       group=acertos.mat))+
  geom_boxplot(aes(y=lat.mat,group=acertos.mat),
               fill="grey60")+
  coord_cartesian(xlim=c(-0.5,19.5))+
  scale_x_discrete(name ="Acertos em Matemática", 
                   limits=c(0:19))+
  ylab(expression(paste("Traço latente"," (",
                        theta,")")))+
  theme_light()+
  theme(axis.title.x = element_text(size = rel(1.2), angle = 0,
                                    vjust = 0.0),
        axis.text.x = element_text(angle = 0, vjust = 0.5, 
                                   size=14,color="black"),
        axis.title.y = element_text(size = rel(1.2), angle = 90,
                                    vjust = 1.5),
        axis.text.y = element_text(angle = 0, hjust = 0.5, 
                                   size=14,color="black"))

ordem.tri <- exame %>%
  mutate(lat.mat = theta$score.dat[,22],
         acertos.mat = apply(total[,20:38],1,sum))%>%
  select(Nome,escore_matematica,lat.mat,acertos.mat)%>%
  filter(exame$Unidade == "Natal - Zona Norte")%>%
  arrange(desc(lat.mat))

ordem.esc.mat <- exame %>%
  mutate(lat.mat = theta$score.dat[,22],
         acertos.mat = apply(total[,20:38],1,sum))%>%
  select(Nome,escore_matematica,lat.mat,acertos.mat)%>%
  filter(exame$Unidade == "Natal - Zona Norte")%>%
  arrange(desc(escore_matematica))

# Figura 31

head(total[,1:19])

comp5.11<-cbind(total[,1:19],
                lat.port = theta$score.dat[,22],
                acertos.port = apply(total[,1:19],1,sum))

maior.5 <- as.data.frame(comp5.11)%>%
  filter(acertos.port %in% c(5))%>%
  arrange(desc(lat.port))
maior.5 <- maior.5[1,]

menor.11 <- as.data.frame(comp5.11)%>%
  filter(acertos.port %in% c(11))%>%
  arrange(desc(lat.port))
menor.11 <- menor.11[dim(menor.11)[1],]

tabela3<-rbind(maior.5[,1:19],menor.11[,1:19])
xtable(tabela3)

#--------------------------------------------#
#-------------- PONTUAÇÃO FINAL -------------#
#--------------------------------------------#

final.tct <- exame%>%
  select(Nome,escore_final,pontos_redacao,Unidade)%>%
  mutate(acer.port = apply(total[,1:19],1,sum),
         acer.mat = apply(total[,20:38],1,sum),
         lat.mat = lat.mat,
         lat.port = lat.port)%>%
  filter(exame$Unidade == "Natal - Zona Norte",
         exame$`Opção de vaga`=="nº 51 Eletrônica (Res. 30/2011-CONSUP) - Vespertino - Natal - Zona Norte")%>%
  arrange(desc(escore_final))

final.tri <- exame%>%
  select(Nome,escore_final,pontos_redacao,Unidade)%>%
  mutate(acer.port = apply(total[,1:19],1,sum),
         acer.mat = apply(total[,20:38],1,sum),
         lat.mat = lat.mat*100+500,
         lat.port = lat.port*100+500,
         red = 10*pontos_redacao,
  escore.tri = (lat.mat+lat.port+red)/3)%>%
  filter(exame$Unidade == "Natal - Zona Norte",
         exame$`Opção de vaga`=="nº 51 Eletrônica (Res. 30/2011-CONSUP) - Vespertino - Natal - Zona Norte")%>%
  arrange(desc(escore.tri))

pos.tct<-data.frame(final.tct$Nome,pos.tct=rownames(final.tct))
names(pos.tct)[1]<-"Nome"
pos.tri<-data.frame(final.tri$Nome,pos.tri=rownames(final.tri))
names(pos.tri)[1]<-"Nome"

pos<-merge(pos.tri,pos.tct,by.x="Nome",sort = FALSE)

tabela_final <- data.frame(pos[,c(3)],
  final.tri[,c(5,6,8,7,9,2,10)])
xtable(tabela_final[1:50,],digits = 0)

table(as.numeric(as.character(pos$pos.tct[1:50]))>50)
