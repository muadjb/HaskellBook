## Exercises: Eq Instances

I'm working through this simple example and I'm confused.  
TisAnInteger is the data type name. This is equivalent to Bool.
TisAn is the constructor. This is equivalent to True.
Integer is an argument to the constructor. Boolean doesn't take an argument.

So I would think that TisAn 2 is a different value than TisAn 4. But when I compare them with my Eq definition, I get True. Perhaps my Eq insance is wrong. Here's what I have:

```haskell
instance Eq TisAnInteger where
   a == a' = True
```

Also, I don't know how to check the correctness of this code.
Type => TisAn :: Integer -> TisAnInteger

Okay, got it. Instance definition was wrong. This is correct:

```haskell
instance Eq TisAnInteger where
   TisAn a == TisAn b = a == b
```

So, I define the equality operation for the data constructor on the left of the = and then the implementation on the right of the =. I'm not sure that's the correct way to think of it. I'll complete the next exercises in this section to see if they help.

## Type confusion

I think part of my problem is mapping the Haskell terminology onto what I'm familiar with in C# and Typescript. I went back through previous chapters and I'll layout the definitions here:

Data Declarations - define datatypes / types => defined by using the data keyword
Type Constructor - name of the type => left hand side of = in a data declaration
Data Constructor - values inhabiting the type => right hand side of = in a data declaration

I tried to make a union type of strings, but it didn't work:

```haskell
data JBool = "jb" | "jj"

<interactive>:101:14: error:
    Cannot parse data constructor in a data/newtype declaration: "jb"
```
