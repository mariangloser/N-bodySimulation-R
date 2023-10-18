library(plotly)

N<-10  #number of bodies in the simulation
G<-0.01  #gravitational constant
exponent<-1 #exponent used for calculating gravity
m<-100 #mass of each body, this can be easily made so that each body has different mass
R<-m #radius of each point
pos_x<-rep(0,N)
pos_y<-rep(0,N)
for(i in 1:N){
  pos_x[i]<-400*cos(2*(pi/N)*i)
  pos_y[i]<-400*sin(2*(pi/N)*i)
}
v_x<-rep(0,N)
v_y<-rep(0,N)
r_x<-rep(0,N)
r_y<-rep(0,N)
r_size<-rep(0,N)
a<-c(0,0)


run_sim <- function(t) {
  frames<-list()
  x_coord<-list()
  y_coord<-list()
  for(time in 1:t){
    for(i in 1:N){
      r_x<-pos_x - pos_x[i]
      r_y<-pos_y - pos_y[i]
      r_size<-(r_x^2+r_y^2)^0.5
        
      a<-c(0,0)
      for(j in 1:N){
        if (j==i){
          next
        }
        if (r_size[j]==0){
          break
        }
        a[1] = a[1] + G*m/(r_size[j]^exponent)*r_x[j]
        a[2] = a[2] + G*m/(r_size[j]^exponent)*r_y[j]
      }
      v_x[i] = v_x[i] + a[1]
      v_y[i] = v_y[i] + a[2]
      pos_x[i] = pos_x[i] + v_x[i]
      pos_y[i] = pos_y[i] + v_y[i]
    }
    x_coord<-append(x_coord, pos_x)
    y_coord<-append(y_coord, pos_y)
    frames<-append(frames, rep(time,N))
  }
  plot_ly(x = ~unlist(x_coord), y = ~unlist(y_coord), type = "scatter", mode = "markers", frame = ~unlist(frames), size = I(R))
}


