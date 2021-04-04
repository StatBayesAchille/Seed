#Fichier pour tester notre fonction sur nos données

nchain = 10000

init_chain = c(0,0,0,0,1/sqrt(10))

init_bi = 0.1

prop.sd = c(0.185,0.30,0.25,0.40,0.1)

data #pour afficher les données qui ont été récupérées dans le fichier data_seed.R

result = seed(nchain,init_chain,init_bi,prop.sd, data )

#On regarde acc.rate pour voir si notre prop.sd est choisit correctement

result$acc.rate

#alpha_0 :

alpha_0 = result$chain[,1]
alpha_0_burn = alpha_0[1000:10000]

plot(alpha_0_burn,type="l")
plot(density(alpha_0_burn),type="l")

mean(alpha_0_burn)
sd(alpha_0_burn)

#alpha_1 :

alpha_1 = result$chain[,2]
alpha_1_burn = alpha_1[1000:10000]

plot(alpha_1_burn,type="l")
plot(density(alpha_1_burn),type="l")

mean(alpha_1_burn)
sd(alpha_1_burn)

#Alpha_2 :

alpha_2 = result$chain[,3]
alpha_2_burn = alpha_2[1000:10000]

plot(alpha_2_burn,type="l")
plot(density(alpha_2_burn),type="l")

mean(alpha_2_burn)
sd(alpha_2_burn)

#Alpha_12 :

alpha_12 = result$chain[,4]
alpha_12_burn = alpha_12[1000:10000]

plot(alpha_12_burn,type="l")
plot(density(alpha_12_burn),type="l")

mean(alpha_12_burn)
sd(alpha_12_burn)

#sigma :

sigma = sqrt(result$chain[,5])
sigma_burn = sigma[1000:10000]

plot(sigma_burn,type="l")
plot(density(sigma_burn),type="l")

mean(sigma_burn)
sd(sigma_burn)

#Figures pour le rapport

par(mfrow = c(2,3))
plot(alpha_0_burn,type="l",main="alpha_0")
plot(alpha_1_burn,type="l",main="alpha_1")
plot(alpha_2_burn,type="l",main="alpha_2")
plot(alpha_12_burn,type="l",main="alpha_12")
plot(sigma_burn,type="l",main="sigma")

par(mfrow = c(2,3))
plot(density(alpha_0_burn),type="l",main="alpha_0")
plot(density(alpha_1_burn),type="l",main="alpha_1")
plot(density(alpha_2_burn),type="l",main="alpha_2")
plot(density(alpha_12_burn),type="l",main="alpha_12")
plot(density(sigma_burn),type="l",main="sigma")
