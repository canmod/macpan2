# Class Style
#
# Goals:
#   (1) no add-on packages for object oriented programming (e.g. no R6)
#   (2) 'real' object oriented programming inspired by python
#   (3) SOLID principles should naturally arise
#
# Requirements:
#   (1) init methods
#   (2) self-style namespace protection
#   (3) optional pseudo-privacy
#   (4) simple inheritance
#   (5) getters and setters
#   (6) abstract methods
#   (7) dunder-style customization
#
# Proposals:
#   (1) init methods are ordinary functions that return environments
#   (2) the first thing __init__ methods do is create the 'self' environment
#   (3) place private fields and methods in a field called private
#   (4) the first line in an init method initializes using the parent class
#   (5) public methods named get_<field> and set_<field>
#   (6) methods with an empty function body
#   (7) S3 method dispatch
#
# Non-Requirements:
#   (1) multiple inheritance
#   (2) exactly like python
#   (3) worry about whether this is a benefit over R6 etc, b/c simplicity
#       is what i want without having to worry about whether i understand
#       something like R6

# init methods are just functions that return environments
AddN = function(n) {
  self = environment()

  stopifnot(is.numeric(n))
  stopifnot(length(n) == 1L)

  self$compute = function(x) {
    return(x + self$n)
  }

  return(self)
}

adder = AddN(5)
adder$compute(10)


# all methods and fields are public by default
adder$n = 100
adder$compute(10)

# to hide fields you can make them 'private'
AddN = function(n) {
  self = environment()

  stopifnot(is.numeric(n))
  stopifnot(length(n) == 1L)

  # 'privatize' the n field
  self$private = list(n = n)
  self$n = NULL
  self$get_n = function() self$private$n
  self$set_n = function(value) self$private$n = value

  self$compute = function(x) {
    return(x + self$get_n())
  }

  return(self)
}

adder = AddN(10)
is.null(adder$n) # TRUE
adder$get_n()
adder$compute(100)

adder$set_n(1)
adder$compute(100)


# inheritance is just the ability to ...
BinaryAsUnary = function(n) {
  self = environment()

  stopifnot(is.numeric(n))
  stopifnot(length(n) == 1L)

  self$private = list(n = n)
  self$n = NULL
  self$get_n = function() self$private$n
  self$set_n = function(value) self$private$n = value

  # abstract methods can just have no return value
  self$compute = function(x) {}

  return(self)
}

abstract = BinaryAsUnary(10)
is.null(abstract$compute(10)) # TRUE


AddN = function(n) {
  # inheritance by creating an abstract or parent instance,
  # and then modifying abstract methods
  self = BinaryAsUnary(n)

  self$compute = function(x) {
    return(x + self$get_n())
  }

  # don't need to do this in python, but who cares
  return(self)
}

adder = AddN(10)
adder$compute(100)


# python-style dunder method functionality can be handled with S3
AddN = function(n) {
  # inheritance by creating an abstract or parent instance,
  # and then modifying abstract methods
  self = BinaryAsUnary(n)

  self$compute = function(x) {
    return(x + self$get_n())
  }

  # don't need to do this in python, but who cares
  return(structure(self, class = "AddN"))
}

# similar to defining __str__ in python
print.AddN = function(x, ...) {
  print(paste("add", x$get_n(), "to x", sep = " "))
}

adder = AddN(10)
print(adder) # "add 10 to x"
adder$compute(10)


# if you don't use self to access data and methods, you can run
# into problems
AddN = function(n) {
  # inheritance by creating an abstract or parent instance,
  # and then modifying abstract methods
  self = BinaryAsUnary(n)

  self$compute = function(x) {
    return(x + self$get_n() + problem)
  }

  # don't need to do this in python, but who cares
  return(structure(self, class = "AddN"))
}

problem = 100000
adder = AddN(5)
adder$compute(5) # 100010 != 5 + 5


# if self$problem was used the issue would have been detected
AddN = function(n) {
  self = BinaryAsUnary(n)

  self$compute = function(x) {
    # self$problem doesn't exist
    return(x + self$get_n() + self$problem)
  }

  # don't need to do this in python, but who cares
  return(structure(self, class = "AddN"))
}

problem = 100000
adder = AddN(5)
adder$compute(5) # 100010 != 5 + 5



# this still correctly fails when the 'base' environment class is used
AddN = function(n) {
  self = environment()

  stopifnot(is.numeric(n))
  stopifnot(length(n) == 1L)

  self$compute = function(x) {
    return(x + self$n + self$problem)
  }

  return(self)
}

problem = 100000
adder = AddN(5)
adder$compute(5)


# but in other cases things can get bad if the 'base' environment class is used
AddN = function(n) {
  self = environment()

  stopifnot(is.numeric(n))
  stopifnot(length(n) == 1L)

  self$compute = function(x) {
    return(x + self$n + n)
  }

  return(self)
}

problem = 100000
adder = AddN(5)
adder$compute(5)  # 15 != 10

# the way to handle this is to always
# inherit from the following class generator
Base = function() environment()
# declare that the only things in the initializing
# environment should be self and private
init_cleaner = function(e) {
  rm(list = setdiff(ls(envir = e), c("self", "private")), envir = e)
}

## This is how it is done
AddN = function(n) {

  # step 1:
  # capture the initializing environment containing the arguments
  # of the init method so that it can be cleaned up
  #  -- analogous to typing `def __init__(self` in python --
  init = environment()

  # step 2:
  # inherit from at least the base class defined above, containing
  # the clean method for signaling the end of the init method
  #  -- analogous to typing `Child(Parent)` in python --
  self = Base()

  # step 3:
  # create a pseudo-private environment
  #  -- not really analogous to anything in python, but allows for pseudo-privacy --
  private = Base()

  # step 4:
  # the body of the init method
  #  -- analogous to the body of __init__ in python --
  stopifnot(is.numeric(init$n))
  stopifnot(length(init$n) == 1L)
  private$n = init$n

  # step 5:
  # declare the end of the init method by cleaning up
  # the init environment so that only self and private
  # are available to the bodies of the methods below
  init_cleaner(init)

  # step 6:
  # define methods

  self$get_n = function() {
    return(private$n)
  }

  self$set_n = function(value) {
    private$n = value
  }

  self$compute = function(x) {
    return(x + self$get_n())
  }

  # step 7:
  # return newly created object
  structure(self, class = "AddN")
}

# step 8:
# define 'dunder-style' methods
print.AddN = function(x, ...) {
  print(paste("add", x$get_n(), "to x", sep = " "))
}

problem = 100000
adder = AddN(5)
adder$compute(5)
print(adder)

adder$set_n(100)
adder$get_n()
adder$compute(5)
print(adder)
# you can still get the private stuff,
# but it is a little difficult (i think
# this is pretty similar to the R6 setup)
ls(environment(adder$compute)$private)
adder
