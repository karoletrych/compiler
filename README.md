# InferLang
Compiler of a simple language featuring local type inference and targeting Common Language Runtime.
[![Build Status](https://travis-ci.org/karoletrych/compiler.svg?branch=master)](https://travis-ci.org/karoletrych/compiler)
## Syntax
### Modules
All code in InferLang is placed in modules.
Module namespaces can be specified with ``module`` keyword.

    module ExampleModule
    module Company::Project::Component::ExampleModule2

If the module namespace is not given it will be set based on file location and name.
For example if the code is placed in file `ProjectRoot/A/B/Program.inf` and the compiler is launched in `ProjectRoot` the module namespace will be: `A::B::Program`.



### Functions
Functions are defined with ``fun`` keyword.
Here is a function taking parameters of types ``int`` and ``float`` and returning ``string``:

    fun exampleFunction1 (a : int) (b : float) : string
    {
        return a.ToString() + b.ToString();
    }
Types of parameters need to be specified.
The return type can be omitted. The compiler is be able to infer it.

    fun exampleFunction2 (a : int) (b : float)
    {
        return a.ToString() + b.ToString();
    }
### Built-in types
* ``bool`` 
* ``int`` 32-bit integer
* ``float`` 32-bit floating point
* ``double`` 64-bit floating point
* ``char`` 
* ``string`` 
* ``void`` 
* ``obj`` 

### Local variables
Variables are defined using ``var``. Constants are defined using ``val``. 
The compiler can infer types of variables and constants or they can be specified.

    var c = 42;
    val d = calculateD();
    var e : int = computeE(c, d);
### Lists
    val stringList = ["a","b","c"];
    val intList = [1;2;3]
    val objList = [1;"A";3.2]
### Classes

Classes can be created using ``class`` keyword. Example:

    class Animal 
    {
        val _color : System::Drawing::Color
        val _noise : string

        construct(color : System::Drawing::Color, noise : string)
        {
            _color = color;
            _noise = noise;
        }

        fun MakeNoise
    }
Class members need to be declared in the following order:

1. Fields
2. Constructor (defined with ``construct`` keyword)
3. Functions

All functions are virtual and can be overloaded in inheriting classes.

### Inheritance


## Type Inference
Type inference is looking for a least upper bound of types.

Closest common ancestor of ``int``, ``string`` and ``float`` is ``obj``.

    val objList = [1;"A";3.2]

Closest common type of Dog and Cat is Animal so the return type will be inferred to it.

    fun createAnimal(animalType : string)
    {
        if(animalType == "dog")
            return new Dog();
        else if(animalType == "cat")
            return new Cat();
    }

## Setup and compiler usage
