
######################
# CHARGE LES DONNEES #
######################
data <- read.table("tableau_all.txt",header=T)	



##############################################################
# INDIQUE QUELLES SONT LES VARIABLES FACTORIELS ET ORDONNÉES #
##############################################################
data$Espece =as.factor(data$Espece)			                    #1				
data$N_inventaire =as.factor(data$N_inventaire)				      #2
data$Taille =as.ordered(data$X.Taille)						          #3
data$Couleur_Dominante =as.factor(data$X.Couleur_Dominante)	#4
data$Degre_de_couleur = as.ordered(data$Degre_de_couleur)	  #5
data$Queue = as.factor(data$Queue)				  		            #6
data$Posture =as.factor(data$Posture)						            #7
data$Accessoires =as.factor(data$Accessoires)				        #8
data$Hauteur_yeux =as.ordered(data$Hauteur_yeux)		        #9	
data$Hauteur_tete =as.ordered(data$Hauteur_tete)		        #10	
data$membres =as.factor(data$membres)						            #11
data$hauteur_corps =as.ordered(data$hauteur_corps)	        #12		
data$propor_tete =as.ordered(data$propor_tete)			        #13	
data$longueur_museau =as.ordered(data$longueur_museau)	    #14	
data$profondeur_tete =as.ordered(data$profondeur_t_te)		  #15
data$taille_poils =as.ordered(data$taille_poils)			      #16
data$Matiere =as.factor(data$Matiere)						            #17
data$Truffe_nature =as.factor(data$Truffe_nature)			      #18
data$Truffe_couleur =as.factor(data$Truffe_couleur)			    #19
data$Yeux_nature =as.factor(data$Yeux_nature)				        #20
data$Yeux_couleur =as.factor(data$Yeux_couleur)				      #21
data$Oreilles =as.factor(data$Oreilles)						          #22
data$Mecanisme =as.factor(data$Mecanisme)					          #23
data$Long_mbr_ant =as.ordered(data$Long_mbr_ant)			      #24
data$Long_mbr_post =as.ordered(data$Long_mbr_post)			    #25



#####################################################################
# CREE UN TABLEAU AVEC LES DONNEES UTILISEES POUR LA CLASSIFICATION #
#####################################################################
subdata <- c(4:4)  # ou n'importe quelles serie (ici je n'en prend que la couleur dominante; les codes sont en # au dessus)
dataclasifi=as.data.frame(data[,subdata])
rownames(dataclasifi) <- data$Espece



##################
# CLASSIFICATION #
##################
library(cluster)
library(ape)

# Obtention de la matrice de dissimilarite
Dist_dataclasifi=as.matrix(daisy(dataclasifi, metric ="gower")) # on fabrique une matrice de distance de gower http://stat.ethz.ch/R-manual/R-patched/library/cluster/html/daisy.html
rownames(Dist_dataclasifi)<-data$Espece
colnames(Dist_dataclasifi)<-data$Espece

write.tree(bionj(Dist_dataclasifi),file="NounoursusALL.tre") # créé un fichier de type *.tree qui peut être lu par Fig.tree : http://tree.bio.ed.ac.uk/software/figtree/
hc.dist <- hclust(dist(Dist_dataclasifi)) 
dend.dist <- as.dendrogram(hc.dist) # "print()" method

# Obtention de l'arbre phylogenetique
par(mar=c(5, 1, 1, 11))
nP <- list(col = 3:2, cex = c(0.0, 0.0), pch =  21:22,lab.cex = 0.75, lab.col = "red")
plot(dend.dist, edgePar = list(col = "black", lwd = 2), nodePar= nP,horiz = TRUE, edge.root=FALSE)



#############################################################################################################  
# VISUALISATION (création d'un pdf qui te met les nounours en colone pour ensuite les ajouter à la classif) #
#############################################################################################################  
library(imager)
fichiers <- dir(path = "Photos_all/",pattern=".png")

nsp=length(fichiers)
bidon=c(1:length(fichiers))
nscr <-data.frame(rank=bidon,name=bidon)

for(i in 1:length(fichiers)){
  nscr$rank[i]=hc.dist[[3]][i]
  nscr$name[i]=hc.dist[[4]][hc.dist[[3]][i]]
}

pdf('Nounours_Classif.pdf',bg='white')
def.par <- par(no.readonly = TRUE)
x=length(fichiers)
y=1
dim=x*y
#nf <- layout(matrix(c(1:dim,1), x, y, byrow = TRUE), respect = TRUE)
nf <- layout(matrix(c(1:dim), x, y, byrow = TRUE), respect = TRUE)

for (i in 1:length(nscr[,2])){
  par(mai = c(0, 0, 0, 0),mar=c(0, 0, 0, 0))
  n1=paste('Photos_all/', nscr[,2][28-i],sep="")
  n1=paste(n1,'.png',sep="")
  im <- load.image(n1)
  #plot(im,axes = FALSE) # les images mettent du temps à s'imprimer à l'écran donc à n'utiliser que pour faire du final
  # avec im_small on réduit la taille des images pour gagner du temps, 
  #le size_x et wize_y te donne le % par rapport à l'immage originale (-100 c'est 100%)
  im_small=resize(im, size_x = -20L, size_y = -20L, size_z = -100L,size_c = -100L,interpolation_type =1L)
  plot(im_small,axes = FALSE)
}
dev.off()



##########################  
# ANALYSE RGB DES PHOTOS # 
##########################  
library(png)
fichiers <- dir(path = "Photos_all/",pattern=".png")


#Exemple pour un nounours (code i=)
i=2
breakhist<-seq(0,1,.1)
n1=paste("Photos_all/",fichiers[i],sep="")
yo<- readPNG(n1)
im <- load.image(n1)

yoR <-yo[,,1] #Red
yoG <-yo[,,2] #Green
yoB <-yo[,,3] #Blue

par(mfrow=c(2,2))
FR <- hist(yoR[(yoR<1)&(yoR>0)],main="Red band",xlim=c(0,1),breaks=breakhist,xlab="")
FG <- hist(yoG[(yoG<1)&(yoG>0)],main="Green band",xlim=c(0,1),breaks=breakhist,xlab="")
FB <- hist(yoB[(yoG<1)&(yoG>0)],main="Blue band",xlim=c(0,1),breaks=breakhist,xlab="")
plot(im,main=fichiers[i],cex=0.75)


#Tous les RGB dans un fichier pdf 
breakhist<-seq(0,1,.1)

pdf('ALL_RGB.pdf',bg='white')
for (i in 1:length(fichiers)){
  n1=paste("Photos_all/",fichiers[i],sep="")
  yo<- readPNG(n1)
  im <- load.image(n1)
  
  yoR <-yo[,,1] #Red
  yoG <-yo[,,2] #Green
  yoB <-yo[,,3] #Blue
  
  par(mfrow=c(2,2))
  FR <- hist(yoR[(yoR<1)&(yoR>0)],main="Red band",xlim=c(0,1),breaks=breakhist,xlab="")
  FG <- hist(yoG[(yoG<1)&(yoG>0)],main="Green band",xlim=c(0,1),breaks=breakhist,xlab="")
  FB <- hist(yoB[(yoG<1)&(yoG>0)],main="Blue band",xlim=c(0,1),breaks=breakhist,xlab="")
  plot(im,main=fichiers[i],cex=0.75)
}
dev.off()



#Création d'une table avec 10 classes pour chaque canal 
nsp=length(fichiers)
dataRGB<-as.data.frame(matrix(NA, nrow=nsp, ncol=30))
colnames(dataRGB) <- c("R1", "R2", "R3", "R4", "R5", "R6", "R7", "R8", "R9", "R10", "G1", "G2", "G3", "G4", "G5", "G6", "G7", "G8", "G9", "G10", "B1", "B2", "B3", "B4", "B5", "B6", "B7", "B8", "B9", "B10")
rownames(dataRGB)<-gsub(".png","",fichiers[1:nsp])

for (i in 1:nsp)  {
  n1=paste("Photos_all/",fichiers[i],sep="")
  yo<- readPNG(n1)
  yoR <-yo[,,1] #Red
  yoG <-yo[,,2] #Green
  yoB <-yo[,,3] #Blue
  
  FR <- hist(yoR[(yoR<1)&(yoR>0)],breaks=breakhist,plot=F)
  FG <- hist(yoG[(yoG<1)&(yoG>0)],breaks=breakhist,plot=F)
  FB <- hist(yoB[(yoB<1)&(yoB>0)],breaks=breakhist,plot=F)
  
  for (j in 1:10)	{
    dataRGB[i,j]=FR$counts[j]/sum(FR$counts)
    dataRGB[i,j+10]=FG$counts[j]/sum(FG$counts)
    dataRGB[i,j+20]=FB$counts[j]/sum(FB$counts)
  }
}



#CLASSIFICATIION RGB 
library(cluster)
library(ape)

Dist_dataRGB=as.matrix(daisy(dataRGB, metric ="gower"))
rownames(Dist_dataRGB)<-gsub(".png","",fichiers[1:nsp])
colnames(Dist_dataRGB)<-gsub(".png","",fichiers[1:nsp])

write.tree(bionj(Dist_dataRGB),file="NounoursusRGB.tre")

hc.dist <- hclust(dist(Dist_dataRGB)) 
dend.dist <- as.dendrogram(hc.dist) 

par(mar=c(5, 1, 1, 11))
nP <- list(col = 3:2, cex = c(0.0, 0.0), pch =  21:22,lab.cex = 0.75, lab.col = "red")
plot(dend.dist, edgePar = list(col = "black", lwd = 2), nodePar= nP,horiz = TRUE, edge.root=FALSE)

nsp=length(fichiers)
bidon=c(1:length(fichiers))
nscr <-data.frame(rank=bidon,name=bidon)

for (i in 1:length(fichiers))
{
  nscr$rank[i]=hc.dist[[3]][i]
  nscr$name[i]=hc.dist[[4]][hc.dist[[3]][i]]
}

pdf('NounoursRGB.pdf',bg='black')
def.par <- par(no.readonly = TRUE)

x=length(fichiers)
y=1
#x=4
#y=7
dim=x*y
#nf <- layout(matrix(c(1:dim,1), x, y, byrow = TRUE), respect = TRUE)
nf <- layout(matrix(c(1:dim), x, y, byrow = TRUE), respect = TRUE)

for (i in 1:length(nscr[,2])){
  par(mai = c(0, 0, 0, 0),mar=c(0, 0, 0, 0))
  n1=paste('Photos_all/', nscr[,2][28-i],sep="")
  n1=paste(n1,'.png',sep="")
  im <- load.image(n1)
  #plot(im,axes = FALSE) # les images mettent du temps à s'imprimer à l'écran donc à n'utiliser que pour faire du final
  # avec im_small on réduit la taille des images pour gagner du temps, 
  #le size_x et wize_y te donne le % par rapport à l'immage originale (-100 c'est 100%)
  im_small=resize(im, size_x = -20L, size_y = -20L, size_z = -100L,size_c = -100L,interpolation_type =1L)
  plot(im_small,axes = FALSE)
}
dev.off()


# Et si j'ajoute ça ?
