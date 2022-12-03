a <- "Olivia"
b1 <- 45.6
b2 <- "45.6"
c1 <- c(0,1,2,3)

print(c1)

b1 + b2
b1 + c1

v1<- c(-2,-1,0,1,2)
v1

v2<- v1 * 3
v2
sum(v2)

"/Users/oliviadinkelacker/Documents/ECo/Data Management/baseR-V2022.2/data/exercise_dat" #path1



c_1 = c(1, 2, 3)
c_2 = "c(1, 2, 3)"
c_1


vec_4 <- c(1:12)
vec_4
mat2 <- matrix(vec_4, nrow =  3)
mat2



mat_1 = matrix(my_vec, nrow = 3)
mat_1

mat_1[3,1]

my_vec <- c(1:6)
mat_2 = matrix(my_vec, nrow = 3)
mat_2

mat_3= matrix(my_vec, nrow = 3)
mat_3

mat_4 =  matrix(my_vec,ncol = 4)
mat_4
mat5 =  rbind(mat_1, mat_3)
mat5

vec1 <- 5.2
vec2 <- "five point two"
vec3 <- c(1:5)


my_list_1 <- list("two" = vec1, "one"  =vec2, "three"  = vec3) 
my_list_1

my_list_1[[1]]
my_list_1[[as.numeric("1")]]
my_list_1[["1"]]
my_list_1[["one"]]  
my_list_1$one  
my_list_1$"one" 
my_list_1$1
my_list_1$"1"




my_vec = rep(1:3, 5)
my_vec[]


data.frame(my_vec, my_bool_vec)


my_bool_vec  <- my_vec == 3
my_bool_vec

data.frame(my_vec, my_bool_vec)

my_vec[my_bool_vec]


my_vec[!my_bool_vec]



print_number = function(n)
{
  print(paste0("The value of the number is ", n))
}

print_number(5)




n = 12345
vec_1 = sample(12, n, replace = TRUE)
head(vec_1)

vec_2 = vec_1 == 3
vec_2
vec_1[vec_2]

---

n = 12345
vec_1 = sample(12, n, replace = TRUE)
head(vec_1)

length(vec_1)

sum(vec_1 == 3)


n = 10
vec_1 = sample(12, n, replace = TRUE)
vec_1
paste0("Sum of elements with value 3: ", sum(vec_1 == 3))


it cannot run string for numeric 

n = 3

for (i in 1:n)
{
  print(
    paste0("This is loop iteration: ", i))
}



---
#vec1 with length of n and random number 
n = 17

vec_1  = sample(10, n, replace = TRUE)   
vec_1


for (i in 1:n)
{
print(
  paste0(
    " The element of vec_1 at index ", i,  " is: ", vec_1[[i]]))
  }

#9

create_and_print_vec()




n = 11
min=1
max=10

vector = sample(min:max, n, replace = TRUE) 
create_and_print_vec = function(n, min = 1, max = 10)
{
  vector = sample(min:max, n, replace = TRUE) 
  for (i in 1:n)
  {
    print(paste0(" The element of vec_1 at index ", i,  " is: ", vector[[i]]))
  }
}

create_and_print_vec(10, min = 100, max =2000)








