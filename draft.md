Typekernel Proposal (Draft)
========

Typekernel tries to find a better approach (e.g. language) to implement an operating system kernel.

Embedded DSL in Haskell
--------
Typekernel tries to implement itself in a DSL embedded in Haskell. It has following advantages:
- Make full use of the powerful type system provided by Haskell. One of the most important shortcomings of C is the terrible type system, and implementing a sound type system can be difficult. Embedding the DSL in Haskell allows us to reuse the type system.
- Zero-overhead abstraction. Haskell is a powerful language, but it requires a large runtime and a GC to run. Rather than implementing the OS in Haskell itself, we use Haskell only for AST generation, and finally compile AST to C, then to machine code.

Bottom level: C4m (C----) proposal
--------

C4m (a.k.a. C----) is a strict subset of C (i.e. can be compiled by any C compiler like GCC) that Typekernel generates to. C4m works as a "structured assembly" in our design, removing platform dependence and unstructured jumps from low-level by using C instead of some assembly-like language. High-level tricks of Typekernel are achieved through manipulating C4m AST.

The design targets of C4m are:
- Platform-independent in most cases.
- Allow platform-dependent codes (e.g. inline assembly and C codes) to be embedded easily.
- Safe enough so that there is no things as UB in C4m.
- Easy enough to be generated and to implement a C4m compiler from scratch, even though we usually use C compiler to compile C4m.

The design targets of C4m do not contain a beautiful syntax or a C4m parser from hand-written C4m to C4m AST. It is only used as an intermediate representation during compilation.

### Basic Datatype

There are several basic datatypes used in C4m. C4m uses platform-independent types, and platform-dependent types are defined on the basis of platform-independent types.

- Platform independent types: `Int8`, `UInt8`, `Int16`, `UInt16`, `Int32`, `UInt32`, `Int64`, `UInt64`.
- Platform dependent types: `Word` (may be `Int32` or `UInt32`), `UWord`, `Size`, `USize`.
- Second-class function type: `Fn a b`, where `b` is a first-class member and `a` is a first-class list.
- Pointer: `Ptr a`, referring to a pointer to a.
- Array: `Arr a` where `a` is a first-class member. Only arrays represent locations in memory: think of a machine with infinite register, and then only Array type is addressable.

C4m itself does not allow defining complex structures. Handling of complex structures are handled at higher level.
Just think of the fact that assembly does not allow structures, while C does.

### Operations

C4m allows a type safe subset of C operations.

- Unary operations: `Invert` for integers and `Not` for `Boolean`.
- Arithmatic operations: `Add`, `Sub`, `Mul`, `Div`, `Mod` for integers.
- Bit operations: bit logic operatios for integers.
- Shift operations: shift amount limited by `UInt8`.
- Compare operations: comparing integers.
- Logic operations: `And`, `Or`, `Xor` for `Boolean`.

### Program Structure

C4m AST organize programs in subroutines, just like how C organizes its program. We define that only several operations are allowed in program structure.

- Introducing immediate value. These converts a Haskell value into C value, but only at compile time.
- Unary and binary operations. They take values of first-class types and convert then into first-class types.
- Casting values. All castings in C4m are explicit. This is what introduces unsafety to C4m and what makes C4m complete.
- Defining functions.
- Invoking functions defined earlier. Since functions in C4m are second-class as C, invoking function from both function name (function identifier) and function pointer are required.
- Referencing an array.
- Dereferencing a pointer, as long as the pointer is not a function pointer. Some architectures may encounter alignment problems here, but we don't consider this, since this can only happen when you had ever cast the pointer to another type.




### Code generation

While generating from C4m AST to C, we use the code style that resembles TAC code.

- Functions are organized as functions.
- AST generates into TAC code. Each unary operation, binary operation, and function call generates a temporary variable.

### C4mAST versus C4mParser

C4mAST includes operations and types that are supported by C4m. It behaves like a "current" C4m spec.

C4mParser includes what is supported in our code generator to C.
For example, emitting C statements are allowed here, but not in a general C4mAST, since C4mAST does not necessarily compile to C.

Generally, features should be added to C4mAST, but when we try to solve problems with dirty hacks we prefer using C directly.
One way to solve this conflict is to implement our compiler as a subset of C compiler, which will be costly.

### Optimization strategy

Although in the short term we are not going to throw GCC away and develop a C4m compiler on our own, C4m tries to be friendly to compiler as possible. We purpose several necessary optimizations required when compiling C4m.

- Constant folding. Most variables are assigned only once in C4m, so constant folding should be easy and important.
- Copy elision. C4m uses memory block (array in C) as first-class members, so hard copy elisions are required.
- Pointer analysis. Thanks to constant folding most local pointer operations have constant offsets. This allows us to do more aggressive optimizations.
- Tail-recursion optimization. C4m uses recursions a lot, most of which should be easily written in tail recursion form, thus into loops.

Memory structures
--------

One most important feature that a high level language provides is management of states, usually represented by composition of basic types. 

Most languages use structures and unions for composite types. In languages like C, C++ or Rust, structures and unions are stored as-is in memory, and direct memory manipulations (e.g. modifying page table items or CR register states) are achieved by manual field alignment (think of the "pack" annotation in both C and Rust) or OO style wrapper. In higher level languages like Java and Haskell, composite types are represented in a more complex way, while direct memory manipulations can be hard.

Instead of representing memory operations in structures, we try to use a more composable approach, by defining memory operations in a way that resembles [lens](http://hackage.haskell.org/package/lens), or precisely, ["mutable" lens](https://stackoverflow.com/questions/18794745/can-i-make-a-lens-with-a-monad-constraint).

In thie way, precise memory operations can be implemented in terms of defining lens and using them on memory level, while getters and setters are implemented in terms of "generating corresponding C4m code returing an identifier or setting values". High level structure (whose memory layouts you don't care, like the one in PCB or TCB) that is usually allocated on stack or heap is only a special case of the memory view pattern, where memory is handled by stack or heap allocator.

At the very low level, this abstraction is zero-cost (as long as the compiler optimizes out intermediate variables and merges bit operations) and high-level operations are composed into bit operations in C4m.


### Memory model

All structures, except the virtual structures used for type magics, occupy some memory represented by a start address and a size. We may use `data Memory=Memory {address::USize, size::USize}` to store the start address as well as the size, unlike the C-style malloc. The substructure of a memory piece is also a memory piece.

### Composable memory structure operations

Modifications to a substructure of memory can be described in terms of mutable lens. For simplicity purpose we only consider lenses `type MLens m a s=forall f. (Functor f)=>(a->m (f a))->(s->m (f ()))`, where "side effects" are allowed in both parts.

The most basic lenses are like byte operations, for example, `MLens C Memory UInt8`. Composing basic lenses result into complexier lenses and allow complex operations (generation).

### Type-family based generic type

Generic type is one of most important reasons we decided to build an embedded DSL: implementing basic types can be easy, but implementing a sound generic type can be quite hard. But embedding the DSL allows us to reuse the types.
One way to handle generic types is to use the "template" approach: generic types only exist "before" compilation and are erased(Java) or specialized(C++ and Rust) as soon as type check finishes. 

Take `Option a` for example: in most cases we want the `Maybe` equivalent thing to store a tag to distinguish `Some a` and `None`. But not for pointers, where we can use the value "0" to represent `None`.

```haskell
class Option' a where
    data Option a
```

For some generic type, we can write:

```haskell
instance Option' (G a) where
    data Option (G a)=Option Memory
```
And for the specified type (Some Ptr), we can write:

```haskell
instance Option' (Ptr a) where
    data Option (Ptr a)=Option USize
```

The problem is only about when to use the specified generic type.

### Move semantic

All mutable 

### Memory policy: Region-based RAII
A good idea is to use RAII for resource recycling.

Resource management is also a problem even in pure FP languages like Haskell, since there is lazy evaluation and IO operations. One common mistake is using resource after closing it.

```haskell
handleFile = do
        h<-openFile "/tmp/test.txt" ReadMode
        hClose h
        content<-hGetLine h -- Error!
        print content
readAll = do
        h<-openFile "/tmp/test.txt" ReadMode
        content<-hGetContents h -- but this is a lazy handle
        hClose h
        print content -- Error!
```

One way to solve this problem is to limit resources in a scope.
However, the resource can still leak out of scope with some trick...
```haskell
withFile f = do
        h<-openFile "/tmp/test.txt" ReadMode -- Open file.
        ret<-f h
        hClose h -- After using the handle, close the file to prevent leakage.
        return ret
abuseWithFile = do
        bad_handle=withFile return --Leaked.
        content<-hGetLine h --Error!
        print content
```

To prevent resource from leaking out of scope, we use a rank-two type to prevent certain resources from leaking out, by adding a scope to the type of both resource and scope monad.
```haskell
data RegionT s pr α
runRegionT :: RegionControlIO pr => (forall s. RegionT s pr α) -> pr α
-- Trying to runRegionT on some α with s in type will cause a type error.
``` 

After implementing move semantic and region-based RAII, we are given the ability to implement a real RAII: allocating resources in a scope and recycling them all after scope is ended.