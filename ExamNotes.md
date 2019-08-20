### SOPING RULES:
DYNAMIC, STATIC  
- REMEMBER  DYNNAMIC ALWAYS ACTIVE X... STATIC ACTIVE VARIABLE LOCAL TO BLOCK.    

- STATIC CHAIN  - above layer
- DISPLAY - one box per layer
- ASSOCIATION LISTS - from active layer to first layer
- CRT - make table and 1 0 if reachable or not
-----
### NOTATION
- INFIX, POSTFIX, PREFIX NOTATION  
- INFIX: **Like you would usually write expressions**  
- POSTFIX: **Values, then operators**  
- PREFIX: **Operator, then value from right to left.**  

- PREFIX:
```+ * A B / C D ```
POSTFIX:
```A B * C  D / + ``` INFIX: ```A * B + C / D```    
- PREFIX:
```/ * A + B C D``` POSTFIX: ```A B C + * D / ``` INFIX: ```( A * ( B + C ) ) / D```     
- PREFIX:
``` * A + B / C D ```  POSTFIX: ```A B C D / + * ``` INFIX: ```A * ( B + C / D )```     
-----

### IMPERATIVE VS DECLARATIVE PROGRAMMING:
- IMPERATIVE - STATE HOW TO GET WHAT YOU WANT  
- DECLARATIVE - STATE WHAT YOU WANT  

## Declarative
```python
small_nums = [x for x in range(20) if x < 5]  
```
## Imperative
```python
small_nums = []  
for i in range(20):  
    if i < 5:  
        small_nums.append(i)  
```
---
### HIGHER ORDER FUNCTIONS
- **A function is higher order when it accepts as parameters, or returns another function as its result.**  
- Deep Binding: **Deep binding binds the environment at the time a procedure is passed as an argument.**
- Shallow Binding: **Shallow binding binds the environment at the time a procedure is actually called.**

```js
function f1()
{
    var x = 10;
    function f2(fx)
    {
        var x;
        x = 6;
        fx();
    };

    function f3()
    {
        print x;
    };

    f2(f3);
};
```
**Deep binding**  
Here f3() gets the environment of f1() and prints the value of x as 10 which is local variable of f1().

**Shallow binding**  
f3() is called in f2() and hence gets the environment of f2() and prints the value of x as 6 which is local to f2()


---

### TAIL RECURSION vs RECURSION
In **traditional recursion**, the typical model is that you perform your recursive calls first, and then you take the return value of the recursive call and calculate the result. In this manner, you don't get the result of your calculation until you have returned from every recursive call.  

In **tail recursion**, you perform your calculations first, and then you execute the recursive call, passing the results of your current step to the next recursive step. This results in the last statement being in the form of (return (recursive-function params)). Basically, the return value of any given recursive step is the same as the return value of the next recursive call.  

TLDR;  
- Traditional recursion: Go through every recursive call on stack, then calculate result.
- Tail recursion: Calculate the result and pass them to the next recursive call, this reduces used space on stack, as you no longer need the current stack frame anymore.
- **Too many recursive calls on stack can cause stack overflow, which is why tail recursion can be useful.**

**Recursion**
```javascript
function recsum(x) {
    if (x===1) {
        return x;
    } else {
        return x + recsum(x-1);
    }
}
```

```
recsum(5)
5 + recsum(4)
5 + (4 + recsum(3))
5 + (4 + (3 + recsum(2)))
5 + (4 + (3 + (2 + recsum(1))))
5 + (4 + (3 + (2 + 1)))
15
```

**Tail Recursion**
```javascript
function tailrecsum(x, running_total=0) {
    if (x===0) {
        return running_total;
    } else {
        return tailrecsum(x-1, running_total+x);
    }
}
```
```
tailrecsum(5, 0)
tailrecsum(4, 5)
tailrecsum(3, 9)
tailrecsum(2, 12)
tailrecsum(1, 14)
tailrecsum(0, 15)
15
```

---

### CONTROL ABSTRACTION

- Call by Value: **Copies values of actual parameter into formal parameter. During execution, there is no link between formal and actual parameter (in contrast to call by reference).**  
```
  int y = 1;
  void foo (int x) {
      x = x+1;
      }
    ...
y = 1;
foo(y+1);
    // here y = 1
```
- Call by Reference: **Actual value is linked with formal value, meaning if you make changes to formal value, the actual value is ALSO changed.**
```
  int y = 0;
  void foo (reference int x) {
      x = x+1;
      }
y=0;
foo(y);
    // here y = 1
```
- Call by Constant: **Call by Value, but read only. It cannot modify any formal parameters in the body.**  

- Call by Result: **Would associate actual and formal parameter, but would not make a change to the actual parameter, before function execution.** *note: Don't think of call by result and call by value-result as different, they are in a sense the same, with call by result being "half" of call by value-result. Call by result is the part of call by value-result, that lets you apply changes to an l value on function termination*
```
void foo (result int x) {x = 8;}
    ...
int y = 1;
foo(y);
    // here y is 8
```

- Call by Value-Result: **An argument passing convention where the actual argument is a variable V whose value is copied to a local variable L inside the called function or procedure. If the procedure modifies L, these changes will not affect V, which may also be in scope inside the procedure, until the procedure returns when the final value of L is copied to V. Under call-by-reference changes to L would affect V immediately.**
```
void foo (reference/valueresult int x,
          reference/valueresult int y,
                      reference int z){
            y = 2;
            x = 4;
            if (x == y) z = 1;
          }
          ...
int a = 3;
int b = 0;
foo(a,a,b);
```
In Example above, if its value-result, **b** will not be modified, but if its call by reference, **b** will be modified, to have the value 1.
- Call by Name: **Replaces the formal parameter with the actual parameter.**
```
int x=0;
int foo (name int y){
    int x = 2;
    return x + y;
    }
    ...
int a = foo(x+1);
```
*note: In the above example the result is 3,* NOT 5, *because the passed (active) x is ```x = 0``` initially. Dont make this mistake !! (the variable is captured - a way to go around this is replacing the local x with z, it will make it easier as you calculate.)*

#### Exceptions
**Exceptions** are used to avoid potential crashes and bad outcome in a program. They are checked for during execution of a program, and if they are to be found, they can be dealt with in desired manner, avoiding potential stack overflow and what not. It is a way of saying "if this happens, do this  instead - and tell me what happened."

---
### STRUCTURING DATA
- Dangling references: **Pointers to memory that is no longer allocated in the program**

- Type safe language: **When no program can violate the distincions between types defined in that language**
- Denoteable: **If a type can be associated with a name**
- Expresssible: **If a type can be the result of a complex expression**
- Storable: **If a type can be stored in a variable**

- Static Typing: **Type constraints checked at compile time**
- Dynamic Typing: **Type coinstraints checked at runtime**

Unions: **Much like structs in C, but only one member can be accessed at a time, and data will be stored in same memory. Unions can be used to make variant records in C**

Variant Records: **Example (gives us some information from possible variants)**
```c
type Stud = record
  name : array [1..6] of char;
  reg_no : integer;

  case graduated : boolean of
    true: (lastyear : 2000..maxint);
    false:(major : boolean;
            year : (first,second,third)
            )
    end;
```

### Equivalence
- Name equivalence: **Only equivalent if, and only if they have the same name.**
```c
type T1 = 1..10;
type T2 = 1..10;
type T3 = int;
type T4 = int;
```
TLDR; No equivalence in example above, even though they may be assigned the same type or value.

- Structural equivalence: **Two types are equivalent if, and only if, they have the same "structure"**
TLDR; *Names and types of each component of the two types must be same, and must be listed in the same order. Example:*

```C
type T1 = int;
type T2 = char;
type T3 = struct{
            T1 a;
            T2 b;
          }
type T4 = struct{
            int a;
            char b;
          }
```
Example above **is** structurally equivalent, but:

```C
type S = struct{
          int a;
          int b;
        }
type T = struct{
          int n;
          int m;
        }
type U = struct{
          int m;
          int n;
        }
```
is **not**.

### Compatibility and Conversion
Compatibility: **We say that type T is compatible with type S, if a
value of type T is permitted in any context in which a value of type S would be
admissible.** (Its pretty logical, just think of it as used to)

- Implicit Conversion: **When the abstract machine handles the conversion when there is no such indication in the high-level language.**

- Explicit Conversion: **When the conversion is indiciated in the text of the program**

### Polymorphism
Polymorphism is when an object can be of multiple instances(or forms).
Java example:
```java
public interface Vegetarian{}
public class Animal{}
public class Deer extends Animal implements Vegetarian{}
```
```md
- A Deer IS-A Animal
- A Deer IS-A Vegetarian
- A Deer IS-A Deer
- A Deer IS-A Object
```
```java
Deer d = new Deer();
Animal a = d;
Vegetarian v = d;
Object o = d;
```
Another example:  
We need a function that sorts a vector of integers:  
 ```c
 void int_sort(int A[])
 ```
 Ok, but now we need to define another function, because we want to sort a vector of *characters* instead:
 ```c
 void char_sort (char C[])
 ```
 In an inflexible language, we would have to define two different, but very similar functions, but in a **polymorphic language**, it would allow the definition of a single function:
 ```c
 void sort(<T> A[])
 ```

 - Adhoc polymorphism/**Overloading**: **For example the operator (name) "+"** **refers to different functions**  Example:
 ```md
- 1 + 2 = 3  
- 3.14 + 0.0015 = 3.1415  
- 1 + 3.7 = 4.7
- [1, 2, 3] + [4, 5, 6] = [1, 2, 3, 4, 5, 6]
- [true, false] + [false, true] = [true, - false, false, true]
- "bab" + "oon" = "baboon"
```
All corresponds to different functions for the operator **+**  
- In the first case, integer addition must be invoked.    
- In the second and third cases, floating-point addition must be invoked (with type promotion, or type coercion, in the third case).  
- In the fourth and fifth cases, list concatenation must be invoked.  
- In the last case, string concatenation must be invoked.

- Universal Parametric Polymorphism: **When a function is composed of a single piece of code which operates uniformly on all the instances of its general type** Example:

```c
null
//or as earlier mentioned
void sort(<T> A[]) // sorts an array of any type whatsoever
```
    Explicit polymorphism:
    when explicit notations such as <T> indicate a "generic type". Used in c++ and java.

    Implicit polymorphism:
    When types are automatically matched depending on input. Example:

    fun Ide(x){return x;}

- Subtype Univeral Polymorphism: **Subtypes**

### Type inference
**Refers  to the automatic detection of the data type of an expression in a programming language (wikipedia)**

---
### Avoidance of Dangling References
- Tombstone: **Have a tombstone inbetween every pointer and stack reference.**
Using tombstones, every pointer will point to the tombstone. If we make an assignment  p=q, and p is freed, q will know that it is a dangling reference, because it is referencing a tombstone(a special value is stored in the tombstone).

- Locks and keys: **Lock is given to object when created. A pointer is given address and key. If a pointer is deallocated 0 will be stored as lock key, and dangling references will know if they are dangling if they dont have the matching key.**

---
### Garbage Collection
- Reference counting: **Counters for each object pointed to. If a counter is 0, it will be collected and memory will be returned to free list NOTE: Circular structure BAD because counter will never reach 0, also heavy work load, EXTERNAL FRAGMENTATION**

- Mark and sweep: **Objects in heap are traversed and marked as "in use". All marked blocks are left alone and "not in use" blocks are returned to the free list. EXTERNAL FRAGMENTATION, time consuming, differnt aged blocks**

- Mark and compact: **Same as mark and sweep, but compacts free memory to reduce external fragmentation.**

- (Pointer Reversal (used for marking)): **Helps traverse the heap to find potential unmarked objects deep in the heap. Once it has traversed through all objects it will flip the pointers so it can go back through the heap. Smart because it does not require additional memory allocation and stores the information about the objects in  already allocated memory.**

- stop and Copy: **No marking, ONLY copying and compaction of live blocks. Memory is split into 2 equal parts, and only one half is used for memory allocation. Once it becomes exhausted, blocks will be copied over to the other half and compacted. The more memory the lower cost.**

---
### Data Abstraction
 - Abstract data type: **An abstract data type is a data type, that does not give a view of implementation. The user only knows how to use it, but not how it is implemented. It is all about hiding the implementation from the user**
---
### The Object Oriented Paradigm
- Encapsulation: **When you group the variables and methods of an ADT into a class. - "A protective shield that prevents the data from being accessed by the code outside this shield. SETTERS AND GETTERS"**

- Subtypes: **Derived methods, think of keyword extends/implements in JAVA. (Subclasses)**

- Method overriding: **When a method in the superclass is overridden, or modified by its subclass.**

- Shadowing: **Same as method overriding, but for instance variables.**

- Subtype relation: **A subtype B and B subtype A is not possible. No cycles. Subtypes do not have to have an immediate subtype. Type C can be subtype of A and B at same time.**

- Constructors: **Usually used to set initial values for a new instance of a class. NOTE: In JAVA you can have multiple constructors for a class, as long as the parameters they take are not the same.**

- Multiple Inheritance: **Not supported in JAVA, same name conflicts.**

- Dynamic Lookup:

- Late binding: **Using keyword this to bind current object dynamically.**

- Subtype polymorphism: **A class A has as type, all superclasses of A.** This means that all subclasses of A are compatible Example could be Integer extends number, float extends number, so both has number as supertype, and could be compatible this way.

- Generics: **Allows us to use types as parameters, and allows us to reuse code more often, as generics allows us to use the same code for several different types. Example List<T>. Supports Several types and with a bunch of predefined methods to handle the list. Eliminates casting.**




---

### Exam(s) Solutions

```haskell
import Control.Monad
import Data.Char


-- Exam Solutions Haskell Part

data QT a = C a | Q (QT a) (QT a) (QT a) (QT a) deriving (Eq, Show)

-- Mockup Exam Question 6
centerSymmetric :: (Eq a) => QT a -> Bool
centerSymmetric (C x) = True
centerSymmetric (Q a b c d) = (compareQ a d) && (compareQ b c)

compareQ :: (Eq a) => QT a -> QT a -> Bool
compareQ (C x ) (C y ) = x == y
compareQ (Q a1 b1 c1 d1) (Q a2 b2 c2 d2) =
  (compareQ a1 d2 ) &&
  (compareQ b1 c2 ) &&
  (compareQ c1 b2 ) &&
  (compareQ d1 a2 )

-- Exam 1 Question 6
myFlip :: [QT a] -> [QT a]
myFlip list = map (\x -> flipper x) list

flipper :: QT a -> QT a
flipper (C a) = C a
flipper (Q a b c d) = Q (flipper c) (flipper d) (flipper a) (flipper b)

-- Mockup Exam Question 7
zipWithQ :: (a -> b -> c) -> QT a -> QT b -> QT c
zipWithQ f (C a) (C b) = C (f a b)
zipWithQ f (Q a b c d) (Q a1 b1 c1 d1) =
    Q (zipWithQ f a a1)
     (zipWithQ f b b1)
     (zipWithQ f c c1)
     (zipWithQ f d d1)

zipWithQ f x (Q a b c d) =
    Q (zipWithQ f x a) (zipWithQ f x b) (zipWithQ f x c) (zipWithQ f x d)

zipWithQ f (Q a b c d) x =
    Q (zipWithQ f a x) (zipWithQ f b x) (zipWithQ f c x) (zipWithQ f d x)

-- Exam1 Question 7
beauty :: (a -> Bool) -> QT a -> Bool
beauty f (C x)
  | f x = True
  | otherwise = False

beauty f (Q a b c d) = (beauty f b) && (beauty f d)


```
