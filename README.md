# Genetic Search

A library for building and running genetic algorithms.

The library is made to be highly parameterisable, providing default implementations for operators and functions. Users can, of course, write their own versions of these operators and functions and use these inside the genetic algorithm instead.

### Overview 

The library is split into three key packages: 
  - algorithm: Holds a class for the genetic algorithm, a builder and a class for holding genetic algorithm operators
  - genotype: Holds the Genotype trait and Sequence class (probably the most useful implementation of genotype)
  - operators: Holds objects which contain implementations of the genetic algorithm operators. This will be where the core of the expansion of this project happens going forward.
  
Another important file is the Types object, containing many of the types used throughout the project.

### Testing

Each component and operator implementation is well unit-tested. There is also a small implementation of genetic algorithm: playing the game Mastermind, in the GeneticAlgorithmTests file. This is a good place to start for undestanding how to build and use a genetic algorithm.
