



# Create a GADT data structure in Scala3

Initial version, traits and classes.  Index is Int.  Extract only.
https://github.com/scala-bones/typed-csv/tree/example/int-index-extract-only

The goal of this article is to highlight a type of library design which
is enabled by Dotty/Scala 3.  The general idea, is that we build up a data
structure based on traits and classes which describes our data, then we
pass the data structure to an interpreter which spits out the function
which converts a Vector of String to the type specified by our data structure.


In this article we are going to being creating a CSV row processor.
This is an application in which the input is a vector of column values
and the output is a typed based on a schema.  We will also support validation.  
The goal of this library is to create a clean DSL which builds a data structure which 
describes the data.


First, lets define our data structure.


```scala 3
trait TupleDef[A<:Tuple] { this: DataDef[A] =>
    def ::[AA](head: DataDef[AA]) = Cons[AA,A](head, this)
}

sealed trait DataDef[A]
case class StringDef(header: String, validations: List[Validation[String]]) extends DataDef[String]    
case class IntDef(headerName: String, validations: List[Validation[Int]]) extends DataDef[Int]
case class BigDecimalDef(headerName: String, validations: List[Validation[BigDecimal]]) extends DataDef[BigDecimal]
case class NilCons() extends DataDef[EmptyTuple] with TupleDef[EmptyTuple]
case class Cons[A, B <: Tuple](dataDef: DataDef[A], tail: DataDef[B]) extends DataDef[A *: B] with TupleDef[A *: B]
```



Note that the types String,Int and BigDecimal are not used within the scope
of the enum.  This is called a phantom type.  They do have a purpose, as I will demonstrate
in the interpreter article.

Using the DataDef enum, we can create a structure which describes a row of data.  The
important thing to notice here is the phantom type maintains the expected type.

```scala 3
    val person: DataDef[(String,Int,BigDecimal)] = Combine(Combine(StringDef("name"), IntDef("age")), BigDecimal("weight"))
```

I have demonstrated that we can create a type aware data structure, however this
would be clunky to use in a program, so instead we will create a DSL.

# Traits and Classes instead of GADT

A Generalized Algebraic Data Type (GADT) contains many type constructors, but returning a single type.
So consider the following:

```
enum DataDef[A] {
    case StringData(index: Int) extends DataDef[String]
    case IntData(index:Int) extends DataDef[Int]
    case Combine[T<:Tuple,I<:Tuple.InverseMap[T,DataDef],B](
        f: I => B, 
        dataDefs: T) extends DataDef[B]
}
```
This means that no matter which enumeration we choose, they will all end up being DataDef[A].
StringData becomes DataDef[String], IntData becomes DataDef[Int] and Combine becomes
DataDef[B].

The fact that Combine becomes DataDef[B] creates problems in our interpreter, because we
need to know what the nest types are, but because Combine is down-cast to DataDef[B], we lose
the value T and I.

For example:

```
def extract[A](dataDef: DataDef[A]): A = {
    
}
```

Both StringData and IntData work fine because the type we need to keep is included in the
DataDef parameter type.  However, in the case of combine, we lose the type T.  Keeping type
T is really important

# Tried using Tuple over higher kinded types

Eg 
val x = (StringDef(0), IntDef(1))
But I kept getting stuck.  I believe this has to do with Tuple.IsMappedBy and Tuple.InverseMap
work well with GADTs, but not necessarily traits and clasess.  
 
For the last several years, I have been creating a Scala library whose purpose is to 
describe data using
an Abstract Syntax Tree (AST) which serves at a schema.  Interpreters take
the AST as an argument and returns a function which is then used to process data.
A few examples of data processing this is supported include marshalling and unmarshalling JSON,
creating OpenAPI documentation, creating scalacheck generators, creating a database 
schema and performing Create/Read/Update/Delete operations on a database. This pattern 
has shown up a few places, such as the 
[Schema in Tapir](https://github.com/softwaremill/tapir/blob/master/core/src/main/scala/sttp/tapir/Schema.scala),
the [ConfigDescriptor in Zio Config](https://github.com/zio/zio-config/blob/master/core/src/main/scala/zio/config/ConfigDescriptorModule.scala),
as well as my own library [Bones](https://github.com/scala-bones/bones).


The Bones library has a dependency on the [Shapeless library](https://github.com/milessabin/shapeless) 
in order to build up the AST, however,
with the Tuple redesign in the forthcoming Scala 3 (aka Dotty) release, we no longer have
to depend on the Shapeless library, and the implementation is much simpler.  
In this series, I will demonstrate this style of programming as we create a full featured
type-safe CSV parser.  There are many other posts about how to use GADTs and interpreters to
process program state, but what I am going to show here is slightly different as it 
describes data and not program state.  The
library will be pure (no side effects), but that's not to say that one cannot create an interpreter which executes
a command to the outside world, however we can achieve a full-featured library
without it.