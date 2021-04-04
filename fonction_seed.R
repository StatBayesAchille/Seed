seed <- function(nchain,init_chain,init_bi, prop.sd,data){
#nchain = taille de la chaine
#init_chain = initialisation des valeurs de alpha_0,alpha_1,alpha_2,alpha_12 et sigma
#init_chain_bi = initialisations des b_i. On va considérer que c'est un entier, donc que tout les b_i seront égaux au départ.
#prop.sd = proposition d'écart types pour les variables alpha_0,alpha_1,alpha_2,alpha_12 et b_i
#data = données de seed
  
 #On initialise les variables correspondantes aux données
  
 r = data$r
 x1 = data$x1
 x2 = data$x2
 n = data$n
 taille = length(x1)
 dim = 5
 
 #Initialisation des chaînes :
 
 chain = matrix(NA, nrow=nchain+1, ncol=dim)
 chain[1,] = init_chain

 chain_bi = matrix(NA, nrow = nchain+1, ncol = taille)
 chain_bi[1,]= init_bi 

 acc.rate = rep(0,dim) #la cinquième valeur correspondra aux b_i 
 
 #Boucle pour construire les chaînes :
 
 for (iter in 1:nchain){
   
   ## Current
   b = chain_bi[iter,]
   current = chain[iter, ]
   current_save = current
   
   
   #On actualise les alpha_0, alpha_1, alpha_2 et alpha_12
   
   for (j in 1:4){
     
     
     # Noyau de proposition
     prop = current_save
     current = current_save

     prop[j] = rnorm(1, current[j], prop.sd[j])
     
     # Calcul de p
     
     p_prop = 1/(1+exp(-prop[1] - prop[2]*x1 - prop[3]*x2 - prop[4]*x1*x2 - b))
       
     p_current = 1/(1+exp(-current[1] - current[2]*x1 - current[3]*x2 - current[4]*x1*x2 - b))
     
     # Passage à l'echelle log
     top = (-1/2)*(10^(-6))*prop[j]^2 + sum( (r*log(p_prop)) + (n-r)*log((1-p_prop)))
     bottom = (-1/2)*(10^(-6))*current[j]^2 + sum( (r*log(p_current)) + (n-r)*log((1-p_current)) )
     
     # Calcul de la probabilite d'acceptation
     acc.prob = exp(top - bottom)
     
     # Acceptation ou non de la proposition
     
     if (runif(1) < acc.prob){
       current[j] = prop[j]
       acc.rate[j] = acc.rate[j] + 1
     }
     
     chain[iter+1,j] = current[j]
     
   }
   
   
   
   #On actualise les sigma 
   
   sigma = 1/sqrt(rgamma(1,shape = 10^(-3)+(taille/2), rate= (10^(-3)+sum(b^2)/2)))
   chain[iter+1,5] = sigma
   
   
   
   #on actualise les b_i

   for (k in 1:taille){
     
     prop = rnorm(1,(b[k]),prop.sd[5])

     
     
     # Calcul de p
     p_prop =  1/(1+exp(-current[1] - current[2]*x1 - current[3]*x2 - current[4]*x1*x2 - prop))
   
     p_current = 1/(1+exp(-current[1] - current[2]*x1 - current[3]*x2 - current[4]*x1*x2 - b[k]))
   
     # A l'echelle log
     
     top = (-1/2)*(1/current[5]^2)*prop^2 + sum((r*log(p_prop)) + (n-r)*log((1-p_prop)))
    
     bottom = (-1/2)*(1/current[5]^2)*b[k]^2 + sum((r*log(p_current)) + (n-r)*log((1-p_current)))

     
     # Calcul de la probabilite d'acceptation
     acc.prob = exp(top - bottom)
   
     # Acceptation ou non de la proposition
     if (runif(1) < acc.prob){
       b[k] = prop
       acc.rate[5] = acc.rate[5]+(1/taille)
     }
   
   chain_bi[iter+1,k] = b[k]
   
   }
 }  
 
 
 return(list(chain=chain, chain_bi=chain_bi, acc.rate=acc.rate/nchain))
 
}
