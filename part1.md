
Reduce Your Boilerplate - Code using AST

# Overview

In this blog post I would like to show how coding using Abstract Syntax Trees (AST)
can reduce the amount of boilerplate code used to process data.  In this blog post 
we will create a CSV library in order to
demonstrate how to create an algebra, how to build an AST using the algebra and how to
create reusable interpreters which produces functions from an AST.  When
creating a CSV library, we are going to assume that data has been loaded from 
the file into an Iterable of Vector[String] where the Iterable is a row and
each item in the Vector is a column within that row.
  
The concepts demonstrated in this post can be applied to all kinds of data-based systems such as
JSON parsing, binary data format parsing and even database manipulation -- basically
any time we want to change the data into different formats.
I will also demonstrate
Scala 3's new tuple syntax and how it will aid is in keeping track of types.

In this blog series, I intend to show how to add validation and conversion to the AST, 
and how to extend an existing algebra.  In each post we will build on
what was presented in the previous post.  I believe the concepts
presented here are intermediate level, but a knowledge of recursion 
and how to use type parameters is an important prerequisite. 


# Tuples
Before we dive into the ASTs, it would be good to get a 
quick overview of Scala 3's Tuple syntax.  Scala 2.x introduced the TupleN syntax (where 0 <= N <= 22) and
some syntactic sugar so that when a tuple is defined using parentheses such as, 
`val x = ("Bob", 7, true)`, the compiler converts this syntax into 
and object `Tuple3("Bob",7,true)`.  Tuple3 is a simple case class defined as
`Tuple3[+T1,+T2,+T3](_1: T1, _2: T2, _3: T3)`.
Having this syntax allows us to use the convenient syntax of `x._1` to get the 
first item from the tuple, `x._2` to get the second and `x._N` to get the
Nth item from the tuple up to `_22`.

Scala 3 introduces a new Tuple trait and Tuple object which works with the TupleN syntax and
adds many convenience functions.  Though there are many, I will only demonstrate those which 
are applicable to this post.  

There is a new singleton object called `EmptyTuple` which 
extends Tuple.  
There is also a method on the Tuple class  
called `*:` which allows us to [Cons](https://en.wikipedia.org/wiki/Cons) any value with 
an existing tuple.  For example, the expression `1 *: EmptyTuple` adds the value 1 
to an empty tuple creating a tuple of size 1.  When a
method in Scala ends with colon `:`, a method on the right operand is executed 
using the left operand as the argument.  So in this case 
`*:` is being called on `EmptyTuple` with the argument `1`.  We can build up a tuple
using `*:` by chaining this syntax like so `"Bob" *: 7 *: true *: EmptyTuple`.  In this example,
the value `true` is prefixed to EmptyTuple, `7` is then prefixed to `true *: EmptyTuple` and finally,
`"Bob"` is prefixed to `7 *: true *: EmptyTuple`.  

Scala 3 tuple syntax also introduces a type
called `*:`.  Note that the type `*:` and the method `*:` are the same symbol.
This may sound a bit confusing at first, however the type is only used with other
types, whereas the method is only used with tuple values. In this example, the left 
side of the equals sign is using the type `*:` and the right side is using the method `*:`.

```scala 3
val a: String *: Int *: Boolean *: EmptyTuple = "Bob" *: 7 *: true *: EmptyTuple 
```

This is stating that the instance produced by the expression `"Bob" *: 7 *: true *: EmptyTuple`
is equal to the type `String *: Int *: Boolean *: EmptyTuple`.  Using this syntax, if we
wanted to access the 3rd item in tuple, we would use the tuple syntax `x.tail.tail.head`.

The type `String *: Int *: Boolean *: EmptyTuple` can be converted to and from the TupleN syntax
`Tuple3[String,Int,Boolean]` aka `(String,Int,Boolean)` by casting it.
There is a special case in the Scala type checker which takes
care of this conversion. For example:
```scala 3
val a: (String, Int, Boolean) = "Bob" *: 7 *: true *: EmptyTuple //convert a Tuple with arity 3 to Tuple3
val b: String *: Int *: Boolean *: EmptyTuple = a   // convert a Tuple3 to a Tuple with arity 3
```
Because TupleN can be converted to Tuple, we can also use `*:` on a TupleN.
```scala 3
val a = ("Bob", 7, true)
val b: (Long, String, Int, Boolean) = 10L *: a
```

# Defining the AST

We're going to dive right in with the implementation and discuss what is going on once
each piece is introduced.  Let us begin by defining our data structure using Scala 3's new enum 
feature.

```scala 3
enum DataDef[A] {
    case StringDef(index: Int) extends DataDef[String]    
    case IntDef(index: Int) extends DataDef[Int]
    case ListDef[T<:Tuple](list: DataList[T]) extends DataDef[T]    
    case MapDef[A,B](
        f: A => B, 
        dataDef: DataDef[A]) extends DataDef[B]
}

enum DataList[T<:Tuple] {
    def ::[A](dataDef: DataDef[A]) = Cons(dataDef, this)
    case EmptyList extends DataList[EmptyTuple]
    case Cons[H,T<:Tuple](dataDef: DataDef[H], tail: DataList[T]) extends DataList[H*:T]
}
```

The enum `DataDef[A]` is essentially our base sealed trait.  The `[A]`
represents a type, which at this point we don't know what it is.  What we
are saying is that there is a Data Definition for some type A.  That type `[A]` 
is constrained by the enumerations.

The first two concrete types `StringDef` and `IntDef` take an index which
represents which column of the CSV is being referenced.  Also, notice that they both 
extend DataDef with the type parameter `String` and `Int` respectively.  At this point
we know that the `A` in `DataDef[A]` can only be of type String or Int.

The next type is `ListDef` which is a wrapper for the `DataList` enum, so let's
look at `enum DataList[T<:Tuple]` first.  Note the type `T` is constrained so that it has to
be a tuple.  There are two cases of the DataList enum.  The `EmptyList` represents
an EmptyTuple and a Cons represents a "head" dataDef and a tail which itself is a DataList.
The intent here to be able to concatenate many `DataDef` instances while keeping track
of the type as a tuple.  In this example, we are concatenating a StringDef with an IntDef with an
Empty tuple, resulting in a `DataList[String*:Int*:EmptyTuple]` (which we know is equivalent to
`DataList[(String,Int)]`).

```scala 3
val strInt:  DataList[String*:Int*:EmptyTuple] = Cons[String,(Int)](StringDef(0), Cons[Int,EmptyTuple](IntDef(1), EmptyList))
```

To bring this type back as a DataDef, we wrap it in a ListDef, so that the
type parameter of the DataList is reflected as the type parameter in the DataDef.

```scala 3
val strIntDef: DataDef[(String,Int)] = ListDef(strInt)
```

Our fourth and final type we will define is our MapDef which takes
two parameters, a function from `A => B` and a `DataDef[A]`.  MapDef
extends `DataDef[B]`, meaning this type is describing a functional conversion from type A to type B.
For example, using our `strIntDef` defined above:

```scala 3
case class Person(name: String, age: Int)
val personDef: DataDef[Person] = MapDef( (Person.apply _).tupled, strIntDef)
```

The last three examples build up a description of a CSV extraction where row 0 is a String,
row 1 is an Int and then both are concatenated together and converted to a Person.  Scala 3's tuple
syntax is really powerful here as it is allowing us to build up an AST while
keeping track of the desired type.  Without Scala 3's new tuple syntax a 3rd party library such as 
Shapeless, or a huge amount of boilerplate overhead would be needed to achieve the same
functionality.

Note that the domain is being limited to String, Int, any combination of String and Int, 
and any type which can be built
from combinations of String and Int.  This is for demonstration purposes only so as to keep
it simple.  Later, we will expand our domain to include any type. 


# Extraction Interpreter

In order for the AST data structure above to actually do anything, we need to create an
interpreter for the DataDef.  We would like to transform the data structure into a function 
which takes
a Vector[String] and returns the type `A` in DataDef[A].  We don't want to a function
just for `Person`, we want to be able to do this for all values of type `A`.  
The function look like this:
 
```scala 3
def extract[A](dataDef: DataDef[A]): RowValues => Either[Errors, A] = {
    dataDef match {
        case l: ListDef[Tuple] @unchecked => extractList(l)
        case c: MapDef[t,b] => extractMap[t,b](c)
        case s: StringDef => extractString(s)
        case i: IntDef => extractInt(i)
    }
}
```

The extract function will simply determine which enumeration we have and then call
the function to handle that specific case.  Since all enumerations are
handled, this function is guaranteed to work for all permutations of `DataDef[A]`.

Now let's define the function for each case starting with our concrete types `StringDef`
and `IntDef`.

```scala 3
def extractString(strDef: StringDef): Vector[String] => Either[Errors, String] = {
    row =>
        if (index < row.length) Right(row(index))
        else Left( (error(s"Index ${index} is out of bounds, the Vector is of length: ${v.length}")) )
}

def extractInt(intDef: IntDef): Vector[String] => Either[Errors, Int] =
        row => {
            row.getOrError(intDef.index).flatMap(str => {
                Try {  str.toInt }
                    .toEither.left
                    .map(ex => { error(s"Value ${str} could not be converted to an Int") })
            })                
        }
```

The `extractString` checks to see if the index actually exists in the Vector, if it does
the value is returned, otherwise it returns an error.  The `extractInt` does the same
thing as `extractString`, but takes the extra step in trying to convert the String to an Int,
returning an Error if the conversion fails.

Here is the implementation to extract `MapDef[A]`

```scala 3
def extractMap[A,B](map: MapDef[A,B]): Vector[String] => Either[Errors, B] = {
    val extractA: Vector[String] => Either[Errors,A] = extract[A](map.dataDef)
    row => extractA(row).map(map.f)
}
```

Let us take a look at the guarantees this function uses.

1. A function `extract` exists that given a `DataDef[A]` will return an instance of `A` or an Error.
2. MapDef has a property called `dataDef` of type `DataDef[A]` describing the data
    to extract (using guarantee 1) before applying the function (using guarantee 3).
3. There is a function supplied by MapDef, which converts an instance of type `A` into 
an instance of type `B`.

Next, lets look at the implementation for DataList and ListDef:

```scala 3
def extractList[T<:Tuple](listDef: ListDef[T]): RowValues => Either[Errors,T] = {
    extractDefList(listDef.list)
}

def extractDefList[T<:Tuple](dataList: DataList[T]): Vector[String] => Either[Errors,T] = {
    dataList match {
        case EmptyList => row => Right(EmptyTuple)
        case c: Cons[h,t] => {
            val headF: Vector[String] => h = extract(c.dataDef)
            val tailF: Vector[String] => t = extractDefList(c.tail)
            row => {
                val headResult = headF(row) 
                val tailResult = tailF(row)
                (headResult, tailResult) match {
                    case (Left(e1), Left(e2)) => Left(concatErrors(e1,e2))
                    case (Left(e), _) => Left(e)
                    case (_, Left(e)) => Left(e)
                    case (Right(h), Right(t)) => Right(h *: t)
                }
            }
        }
    }
}    
```

The `extractList` function just delegates to the `extractDefList` which is a recursive 
function.  The recursion stops when the `dataList` passed is of type `EmptyList` -- meaning we
have reached then end of the DataDefs.  However, if the `dataList` passed is of type `Cons`,
this means we have a head definition and a tail definition to deal with.  
Like we did in the `extractMap` function
above, we will call `extract` to get a function from `Vector[String] => Either[Error,a]`
where `a` is the type of the head.
There is also a recursive call to `extractDefList` to get a 
function from `Vector[String] => Either[Error,t]` where `t` is the type of the tail.

This is where the implementation become a bit abstract, so let's deconstruct this a bit.  
As a reminder, here is the definition of cons:  
```scala 3
  case Cons[H,T:<Tuple](dataDef: DataDef[H], tail: DataList[T]) extends DataList[H*:T]
```
When we pattern match on `Cons[a,t]`, we do not actually know what the types `a` and `t` are.
However, we do know that based on the Cons definition, that we can combine `a` and `t` 
into type `A*:T`, which is the type `T` in the function `extractDefList[T<:Tuple]`
and should be in the output to the function returned by `extractDefList`.
We also have a function `extract[A](dataDef: DataDef[A])` which works for any type.
So if we pass the `DataDef[a]` from our Cons enumeration, we are guaranteed to have
a function of type `Vector[String] => Either[Errors,a]`.  Finally, we have our recursive
call `extractDefList[T<:Tuple](dataList: DataList[T])` which guarantees to return
a function of type `Vector[String] => Either[Errors,t]` where t is a tuple.
We can then create a new function which first instantiates the function to the `headF` and `tailF`
and then combines the result into type `H*:T`.

For example given we have some DataList whose type parameters are `String *: Int *: EmptyTuple`.
```scala 3
val dataList: DataList[String *: Int *: EmptyTuple] = ???
val dataDefF: Vector[String] => Either[Errors, String *: Int *: EmptyTuple] = extractDefList(dataList)
```

The first time `dataList` is matched on Cons in the `extractDefList` function, the head type with be 
of type `String` and the tail will be of type `Int *: EmptyTuple`.  The value
`headF` becomes `Vector[String] => Either[Errors,String]`.  The first recursive call
to `extractDefList` is executed with the type `DataList[Int *: EmptyTuple]`.
On this first recursive call, the Cons will be matched and the `h` type will be `Int`
and the tail will be of type `EmptyTuple`.  On the second recursive call,
the match will be on `EmptyList` and the type returned will be `Vector[String] => Right[EmptyTuple]`
which guarantees that a tuple is returned.  When we exit out of the second recursive call,
we are back to the first recursive call with `headF` being 
`Vector[String] => Either[Errors,Int]` and `tailF` as `Vector[String] => EmptyTuple`.
If no errors are returned then Int is prefixed to EmptyTuple and the return type from the
first recursive call becomes `Vector[String] => Either[Errors, Int *: EmptyTuple]`.
This is now the type of `tailF` in our initial call to `extractDefList`.
Once again, if no errors occur, we combine the types and the final return type
becomes `Vector[String] => Either[Errors, String *: Int *: EmptyTuple]`.

That was a specific example, however this is guaranteed to work for any
type of `DataList[T]` because of our guarantees.

We now have a complete interpreter, so here is an example usage:

```scala 3
// Setup
val strInt = Cons[String,(Int)](StringDef(0), Cons[Int,EmptyTuple](IntDef(1), EmptyList))
val strIntDef = ListDef(strInt)
val personDef: DataDef[Person] = MapDef( (Person.apply _).tupled, strIntDef)
val f = VectorInterpreter.extract(personDef)

// run
val result = f(Vector("Bob", 7))
assert(result == Person("Bob", 7))
```

This may seem like an extra layer of extraction to extract a Person from a Vector[String]
when libraries such as [Kantan](https://nrinaudo.github.io/kantan.csv/) can 
do the same thing with less boiler plate:
```scala
Vector("Bob",7).asCsvReader[Person](rfc)
```

However, the advantage to creating an Abstract Syntax Tree is that we can pass
the same data structure to a different interpreter.  So let's create a simple
interpreter which, given an AST, returns a description of the CSV file
expected.

# Describe Interpreter

The purpose of the describe interpreter is to print out a list of columns 
that are expected in the CSV file along with their data types.  We begin
by defining our entry point method.  Just like in the extraction
interpreter, we match on the type and delegate to the appropriate
function.

```scala 3
def describe[A](ddef: DataDef[A]): List[(Index,Description)] = {
    ddef match {
        case l: ListDef[Tuple] @unchecked => listDesc(l)
        case c: MapDef[t,b] => mappedDesc[t,b](c)
        case s: StringDef => List(strIndexDesc(s))
        case i: IntDef => List(intIndexDesc(i))
    }
}
```

Since ListDef and MapDef both require a member of `DataDef`, we can assume that
`StringDef` and `IntDef` are the enumerations which can be the leaf nodes.
Let us start by creating an implementation to describe the leaf nodes.
Each leaf will return a description containing the index, and the expected type.


```scala 3
    def strIndexDesc(sdef: StringDef): (Index, String) =
        ( Index(sdef.index), s"Expect index ${sdef.index} to be of type String")

    def intIndexDesc(sdef: IntDef): (Index, String) =
        ( Index(sdef.index), s"Expect index ${sdef.index} to be of type Int")
```

`MapDef` is an internal nodes and does not describe a column, so it will
return the list of descriptions from its children. 

```scala 3
    def mappedDesc[T,B](mdef: MapDef[T,B]): List[(Index, Description)] = 
        toIndexDesc(mdef.dataDef)
```

The implementation for `ListDef` will delegate to the implementation for `DataList`
using its `DataList` member.

```scala 3
    def listDesc[T<:Tuple](ldef: ListDef[T]): List[(Index,Description)] = 
        dataList(ldef.list)
```

Finally, the implementation for `DataList` will iterate through each of its 
children recursively and return the collection of descriptions.

```scala 3
    def dataList[T<:Tuple](dlist: DataList[T]): List[(Index, Description)] =
        dlist match {
            case EmptyList => List.empty
            case Cons(head, tail) => toIndexDesc(head) ::: dataList(tail)
        }
```

```scala 3

// Setup
val strInt = Cons[String,(Int)](StringDef(0), Cons[Int,EmptyTuple](IntDef(1), EmptyList))
val strIntDef = ListDef(strInt)
val personDef: DataDef[Person] = MapDef( (Person.apply _).tupled, strIntDef)
val result = DescriptionInterpreter.describe(personDef)
```
The result would be:
``` 
Expect index 0 to be of type String
Expect index 1 to be of type Int
```

# Conclusion

Using ASTs to describe data and creating interpreters to create functionality is
a powerful technique for reducing boiler plate.  In this post I demonstrated
a trivial example which extracts data from a CSV file and describes the
format of the CSV file.

For the next blog post, we will build on this technique by adding validation to
the data definition.  This will allow us to more concisely describe the data. 
