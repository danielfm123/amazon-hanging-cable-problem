#Related to video: https://youtu.be/UB99P1piTns
# Reglated Blog Post: http://www.geekosas.com/?p=2569&preview=true

cuts = 10000
cable_length = 80

block_length = function(x,dx,d){
  sqrt(dx^2 + (30*(2*x*dx+dx^2)/d^2)^2)
}

# Formula con la integral
# block_length = function(x,dx,d){
#   sqrt(dx^2 + (30*2*x*dx/d^2)^2)
# }

total_cable_length = function(poles_dist){
  d = poles_dist/2
  
  x = seq(from = 0, to = d, length = cuts+1)
  2*sum(block_length(x,d/cuts,d))
}

error = function(poles_dist){
  (total_cable_length(poles_dist)-cable_length)^2
}

sol = optimise(error,c(0,cable_length))$minimum
sol

print("Distance Between Poles")
print(sol)
print("Cable Length")
total_cable_length(sol)


