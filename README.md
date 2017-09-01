
# Description

This document describes proposal for new language which will be implemented
as part of diploma thesis.

New language called **Preon** will provide mixtures of object-oriented and
functional language features. In fact it will be *pure functional* language
in which everything is polymorphic object.

# Preon language proposal

Every Preon program consists of set of object definitions. Each of this objects
contains fields and every field can have assigned function of specific signature.

## Objects

Declaring objects will be possible with folowing syntax:
```
object Foo
  field1 : Int
  field2 : Int -> String
  field3 : Int -> Float -> String
end
```
This not only declare object but also create default instance of it which
can be accessed by object name. Every field can have assigned default function like this:
```
object Foo
  # constants are also functions
  field1 : Int = || -> 10

  # but there will be syntactic sugar for constants
  field1Alternative : Int = 10

  # one expression functions can be expresed with this syntax
  field2 : Int -> String = |num| -> "Number as string : " + num

  # more expressions are allowed in function body with folowing syntax
  field3 : Int -> Float -> String = |num, dec| -> do
    helpVar = num * num + 1
    helpVar = dec * dec + helpVar

    # last expression is used as return value
    "Computation result : " + helpVar
  end
end
```
This code shows two interesting features that make pure functional code more
imperative-like - *multi-expression function bodies* and *automatic variable renaming*.

## Object fields altering

Declaring object creates just one instance with declared name and fields
in global scope. This instance can be used as follows:
```
someVar = Foo.field1 + Foo.field1Alternative
```

Non-const fields of this default instance can be altered to create new instances with
following syntax:
```
newFoo = Foo { field1 = || -> 15 , field1Alternative = 16 }
```

**TODO : some more info here... something that types do not exist in Preon,
and signature means that instance of this object can be passed here/is returned**

## Constant object fields

Fields can be marked as constant which means that they cannot be altered
(but still can be overridden). These constant fields are useful for declaring
something that would be called *method* in OOP language, although there is no
difference between field (in OOP sense) and method in Preon language.

Syntax for declaring constant fields is following:
```
object Foo
  const method : Int -> String = |num| -> "Number is : " + num

  const methodAlternative1 : Int -> String = |num| -> do
    "Number is : " + num
  end

  # syntactic sugar for declaring constant field
  def methodAlternative2(num : Int) : String
    "Number is : " + num
  end
end
```

## Object inheritance and field overriding

TODO

## Generic objects

TODO

# State-Update architecture

Pure functional languages are great in sense of program stability, code expresivness
and parallelization. However it is not possible to directly perform side-effects
in such languages. Following actions are all considered as side-effects:
  - writing/reading data to/from file, network socket etc.
  - getting random number
  - writing data to video memory and displaying something on screen

There are few options for performing such side-effects in pure functional
languages. Two of these are:
  1. *Monads* - this mechanism is used for side-effects in languages like Haskell.
  Monads are general pattern in pure functional languages and with right use
  they can force sequential computation of expressions and allow IO interaction of
  pure functional program with outside world. This pattern is quite confusing to
  understand for beginners which might be one of the reasons why are languages
  like Haskell not used widely.

  Implementing this pattern in Preon language would be perfectly possible but I want to
  focus on more specific-purpose and straightforward option.

  2. *State, update and runtime system* - with this pattern resposibility for performing
  side-effects is moved completly out of pure functional code into runtime system.
  Main idea here is to have one function which is periodically called by runtime
  system with *state* and *message* supplied as arguments. This function
  returns new *state* and *list of messages*. Runtime system uses returned messages
  to perform appropriate actions which produces side-effects and messages which
  are one-by-one used to update *state* by mentioned function. This way program is
  live while there are any actions still to do and therefore messages which might change
  *state* somehow.

In my diploma thesis I will focus on implementing mentioned *State-Update architecture*
in newly created Preon language. Besides that I will implement automatic parallelization
of calling *state-update function* for different incoming messages which do not
alter same portion of the *state*. To make this possible compiler of Preon language
has to perform some static analysis of the code which makes it possible to find out
during runtime if two messages are in conflict or not.

Proposal of implementation of such architecture follows:
```
# architecture library
abstract object Msg
end

abstract object Cmd
  msg : Msg
end

abstract object StateUpdateApp<TState>
  const update : TState -> Msg -> (TState, List<Cmd>)

  # function which is called by runtime system to check if messages can be processed in parallel
  def hasConflict(state : TState, msg1 : Msg, msg2 : Msg) : Bool
    # usage of special built-in function into every object 'alteredPaths'
    # this function is generated during compilation according to queried function body
    paths1 = this.alteredPaths("update", 0, 0, state, msg1)
    paths2 = this.alteredPaths("update", 0, 0, state, msg2)

    paths1.intersection(paths2).isEmpty == False
  end
end

# example application
object State
  data1 : List<String>
  data2 : String
  data3 : Dict<String, String>
  data4 : String
end

object Msg1 : Msg
  payload : String
end

object Msg2 : Msg
  payload : String
end

object App : StateUpdateApp<State>
  def update(state : State, msg : Msg) : (State, List<Cmd>)
    case msg of
      m : Msg1 -> (state { data2 = "Msg1 processed : " + m.payload } , EmptyList<Cmd>)
      m : Msg2 -> (state { data4 = "Msg2 processed : " + m.payload } , EmptyList<Cmd>)
      _ -> (state , EmptyList<Cmd>)
    end
  end
end
```
