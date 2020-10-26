#Scala3’s GADTs Can Change How we Program

Over the last year or so, I’ve been working on a library I call Scala Bones.  
It is  a Scala 2 library which uses GADTs to build up a data structure, similar to a schema.  
This schema is in input to an interpreter and the output is a function.  Which does 

In order to create this library, I had to learn a library called Shapeless.  Shapeless is a library which enbables
typelevel programming is Scala.  The two main constructs in shapeless are HList and Coproduct.  Although HList and Corproduct
are fairly trivial concepts, actually using them at the typelevel becomes complex.  For example, here is a
constructor for a class whose purpose is to be able to both prepend and split an HList.

```scala
final case class KvpHListCollectionHead[ ALG[_], HO <: HList, NO <: Nat, H <: HList, HL <: Nat, T <: HList, TL <: Nat](
  head: KvpHListCollection[ALG, H, HL],
  typeNameOfA: String,
  tail: KvpHListCollection[ALG, T, TL],
  prepend: Prepend.Aux[H, T, HO],
  split: Split.Aux[HO, HL, H, T], // analogous: Split.Aux[prepend.OUT,HL,H,T] with lpLength: Length.Aux[H,HL],
  validations: List[ValidationOp[HO]]
)
```


In the example above, the Prepend.Aux 
