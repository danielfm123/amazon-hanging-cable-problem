poles_heigth = 50
cable_heigth = 20
cable_length = 80
cuts = 10000

block_length = function(x,dx,a){
  sqrt(1 + (2*a*x)^2)*dx
}

total_cable_length = function(poles_dist){
  half_dist = poles_dist/2
  a = (poles_heigth-cable_heigth)/(half_dist^2)

  x = seq(from = 0, to = half_dist, length = cuts+1)
  2*sum(block_length(x,poles_dist/cuts/2,a))
}

error = function(poles_dist){
  (total_cable_length(poles_dist)-cable_length)^2
}

sol = optimise(error,c(0,cable_length))$minimum

print("Distance Between Poles")
print(sol)
print("Cable Length")
total_cable_length(sol)

print("Validation")
#validate with triangular shape approximation, should be shorter
print("Triangular Shape, Lower Bound")
2*sqrt((sol/2)^2+(poles_heigth-cable_heigth)^2)
#validate with square shape approximation, should be bigger
print("Square Shape Upper Bound")
2*(poles_heigth-cable_heigth) + sol
