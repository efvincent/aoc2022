# Advent of Code 2022

I'm happy to say that this year I've got a few peers from [Aetion](https://aetion.com) on a private leaderboard! It's always more fun when you're playing along with people you know.

Like I've done with some degree of success before, I'm going to put some notes on each day (until I get lazy and stop doing so). Also as I've done before I'm doing AoC in Haskell again this year. Alghough I've branched out a bit in the last 12 months and have been studying language theory in the context of lambda calculus based purely functional languages (interpreters, compilers, type theory, proof assistants), Haskell is my default favorite language and I want to keep practicing and getting more adept it in its use.

I'm also adding to this readme backwards, the latest AoC day that I've commented on will be just below this introduction. So if you want to go in order, head to the bottom :)


## Day 01 - [Camp Cleanup](https://adventofcode.com/2022/day/1)

As usual, day 1 is trivially easy:
```haskell
sln :: String -> (Int,Int)
sln s = 
  let xs = map ((sum . map read) . words) . splitOn "\n\n" $ s in
  (maximum xs, sum . take 3 . sortBy (flip compare) $ xs)
```

### let expression

I'll go into more detail for this one since it's so simple. One interesting bit which is different in lambda calculus based functional languages like Haskell and OCaml is the `let` construct. It's an expression consisting of two parts:

```haskell
let ([v1] = [exp1]; ... [vn] = [expn]) in [exp]
```
It allows you to bind one or more expressions `exp1..expn` to a variables `v1..vn`, and then use those bound variables in the final expression `exp`. During an early phase of compilation, Haskell (more accurately, GHC, the defacto Haskell compiler) will _effectively_ substitute the bound expressions into the final expression, so this construct is computationally free. It does allow you to simplify the expression and make it easier to understand.

In this case, use the `let` construct to bind `xs` to a list of integers `[Int]`. We'll take a look at how we get that list of integers, but first a couple of Haskell basics.

### Curried Functions

Haskell is based on Lambda Calculus (LC), and in LC a funciton only ever has a single parameter. The same is sorta true for Haskell, but there's _syntactic sugar_ to make things simpler. In Haskell, this function looks like it takes two parameters.

```haskell
bigger :: Int -> Int -> Int
bigger a b = if a > b then a else b
```

But the reality is closer to this:

```haskell
bigger = (\a -> 
            (\b -> 
                (if a > b then a else b)))
```

`bigger` is assigned a function that binds a variable `a` and returns a function that binds a variable `b` and returns the result of the `if...then` expression. Looking at it like this, it's obvious if I pass bigger  a 10, I end up with a situation like this:

```haskell
tenOrBigger = bigger 10
-- tenOrBigger = \b -> (if 10 > b then 10 else b)
```
Passing 10 to the funtion `bigger` returns a function where all the instances of `a` have ben replaced with the value 10. The new function returns the integer passed or 10, whichever is bigger. This funciton definition and calling style is called currying. You'll see this being used in different ways all over Haskell code, including the next example.

### Function Composition

The human centipede, except for functions. I'm sure someone else has said that before just now be if not I'll accept credit for the best and worst analogy in CS. Function composition is used a lot in Haskell (and other functional programming languages), so it's worth an explanation.

In Haskell, the "dot" operator (period character), is not for accessing record fields. Many argue this is a big mistake on Haskell's part, and it makes working with records in Haskell weird as hell. But never mind that for now. Let's look at a fragment of one of the lines from Day 01 solution:

```haskell
sum . take 3
```
First an explanation of each function and thier signatures, starting with `sum`. A simplified way of thinking of the type signature is as:

```haskell
sum :: [Int] -> Int
``` 

In reality `sum` is generic; the first parameter is any "foldable" (think iterable) type; like the list `[Int]`. and rather than specifically `Int` the sum function can work over any `Num`. But for this discussion, but this works for now. **Sum** takes an array of `Int` and returns their sum.

Next is `take`

```haskell
take :: Int -> [Int] -> [Int]
```
Again I've shown a non-generic type sig, play along. **Take** has two parameters, first is the number of elements to take from the head of a list, and second is the list. From the previous section on **Curried functions**, we know that `take 3` then is a function that takes three elements off the head of any list you pass it.

```haskell
λ » take' = take 3
λ » :type take'
take :: [a] -> [a]

λ » take' [7,6,5,4,3]
[7,6,5]
```
_Note: the_ "` λ » `" _indicates a command entered in the Haskell repl. The_ `:type` _is a command entered into the repl to have it return the type of the expression._

### Back to Day 01
Now we have all the tools we need to understand my generally not really interesting Day 01 solution.

```haskell
sln :: String -> (Int,Int)
sln s = 
  let xs = map ((sum . map read) . words) . splitOn "\n\n" $ s in
  (maximum xs, sum . take 3 . sortBy (flip compare) $ xs)
```
The **let** binding defines this function:

```haskell
map ((sum . map read) . words) . splitOn "\n\n"
```
It's easiest to read this from right to left, because in our human centipede analogus functional composition, the "mouth" is on the right, and the ... output is on the left. So we're looking at the `splitOn "\n\n"` function passing the list of strings to the `map` function. The `map` is running **each string in the list** through a function:

```haskell
(sum . map read) . words
```
`words` takes the string and breaks it into a list of its substrings separated by whitespace. It's like getting the words out of a sentence. The output of this is fed to

```haskell
sum . map read
```
which goes first to `map read`, which maps the read function over each string. The read function parses a string into another type (in this case `Int`) so long as there's an instance of the `Read` type class. So now our list of strings is a list of integers.

Finally that list of integers is passed to the `sum` function, which returns the sum of all th eintegers in the list as an integer. This is the value assigned to `xs`. _Note another Haskell convention - integers are often represented as `x..z` or `n..p` variables, and lists are often pluralized of some other name_.

Now for the rest of the solution function:

#### Part A

```haskell
(maximum xs, sum . take 3 . sortBy (flip compare) $ xs)
```
This is a tuple, which is a heterogenious pair of two values. The first part of the tuple is `maximum xs`. This is the answer to part A of the puzzle. Once we've calulated all the sums, what's the biggest? The `maximum` function has this signature:

```haskell
maximum :: (Foldable t, Ord a) => t a -> a
```
This is a generic function with constraints. It's saying there's one parameter `t a` which is constrained to be a generic type `t` that has an implementation of the type class `Foldable`, and generic type `a` that implements `Ord`. Loosly translated, in our case, it takes a list of integers, and returns the biggest one. Only it doesn't have to be a list, other data structures are "foldable", and it doesn't have to be an integer, it just has to be orderable in order to find the "biggest" one.

#### Part B

The next part of the tuple returns the answer to Part B:

```haskell
(maximum xs, sum . take 3 . sortBy (flip compare) $ xs)
```
We're buiding another human centipede. The first thing in the chain:

```haskell
sortBy (flip compare)
```

Ok there's few things happening here. First, the `sortBy` function has this signature:

```haskell
sortBy :: (a -> a -> Ordering) -> [a] -> [a]
```
The first parameter is a _function_ with the type signature `(a -> a -> Ordering)`, which we read as a function that takes any value of type `a`, another value of type `a`, and returns something of type `Ordering`, which is definied as a sum type.

```haskell
data Ordering = LT | EQ | GT
```
This makes sense, `sortBy` wants a function that compares two elements and tells us if the first is less than, equal to, or greater than the second. What comparison function are we going to pass to `sortBy`? Let's see:

```haskell
(flip compare)
```

Where the function types are

```haskell
 λ » :t flip
flip :: (a -> b -> c) -> b -> a -> c

 λ » :t compare
compare :: Ord a => a -> a -> Ordering
```
First let's look at `flip`. It takes a function that takes two arguments and returns a result: `(a -> b -> c)`.

`flip` then returns a function that takes two arguments, and returns a result, but the arguments are flipped: `(b -> a -> c)`. It's easy to see this effect with an example:

```haskell
λ » appendWithSpace a b = a ++ " " ++ b
 
λ » appendWithSpace "Hello" "World"
"Hello World"
 
λ » (flip appendWithSpace) "Hello" "World"
"World Hello"
```

This is the effect it has on the compare function:

```haskell
λ » compare 10 20
LT
λ » (flip compare) 10 20
GT
```
When this is passed to the `sortBy` funciton, the sort comes out as descending rather than ascending. Let's see where we are now:

```haskell
(maximum xs, sum . take 3 . sortBy (flip compare) $ xs)
```
Next in the chain is `take 3` which we now know is a _curried function_ that's been _partially applied_. It will return a list of the first 3 elements from the input list, which is now sorted descending. In other words, the three biggest elements from the list. 

Next is the `sum` function, which takes the list of 3 biggest elements, and returns the sum. Which is the answer to part B... we're done!