# Programming
## Variables
### Rust
```rs
let a = 5;
let mut a: i32 = 5;
const a: i32 = 5;
```
### Go
```go
a := 5 
var a int = 5
const a int = 5
```
### Typescript(Deno)
```ts
let a = 5;
let a: number = 5;
const a:number = 5;
```
### Zig
```zig
var a: i32 = 5;
const a:i32 = 5;
```
### C
```c
int a = 5;
```
### Haskell
```hs
let a = 5

let a :: Int
    a = 5
```
## Types
### Rust
```rs
i8 i32 i64 i128 isize
u8 u32 u64 u128 usize
f32 f64  
bool
str
String
char
[T;n]           
(T1,T2)
Vec<T>
Option<T>
Result<T,E>
Box<T>
Arc<T>
Rc<T>
Mutex<T>
```
### Go
```go
int int8 int16 int32 int64
uint uint8 uint16 uint32 uint64
float32 float64
bool
string
error
rune
[n]T
nil
any
```
### Typescript(Deno)
```ts
number
string
boolean
T[]
?T
null
any
```
### Zig
```zig
i8 i32 i64 i128 isize
u8 u32 u64 u128 usize
f32 f64  
bool
void
?T
!T
null
type
anytype
undefined
```
### C
```c
int
double
unsigned int
char
long
unsigned long
null
```
### Haskell
```hs
Int
Integer
Float
Double
Char
String
(T1,T2)
[T]
```
## Functions
### Rust
```rs
fn add(a:i32,b:i32) -> i32{
    a + b
    //or
    return a+b;
}
//or
let add = |a:i32,b:i32| {
    return a + b;
};
```
### Go
```go
func add(a int, b int) int {
    return a + b
}
//or
add := func(a int, b int) int {
    return a + b
}
```
### Typescript(Deno)
```ts
function add(a:number,b:number): number{
    return a + b;  
}
//or
const add = (a:number,b:number): number => {
    return a + b;
};
```
### Zig
```zig
fn add(a: i32, b: i32) i32 {
    return a + b;
}
```
### C
```c
int add(int a, int b) {
    return a + b;
}
```
### Haskell
```hs
add a b :: Int -> Int -> Int
add a b = a + b

let add = \a b -> a + b
```
## Loops
### Rust
```rs
for i in 0..100 {}
for i in x {}
while i <= 100 {}
loop{}
break;
continue;
```
### Go
```go
for i:=0;i<=100;i+=1{}
for i,item := range x{}
for i <= 100 {}
for {}
break
continue
```
### Typescript(Deno)
```ts
for (let i = 0;i<=100;i+=1){}
for (let i of x){}
while (i <= 100){}
while (true) {}
break;
continue;
```
### Zig
```zig
for (items) |i|{}
for (items,0..) |_,i|{}
for (0..100) |i|{}
while(i <= 100){}
while(true){}
break;
continue;
```
### C
```c
for (int i=0;i<=100;i+=1){}
for (int i=0;i<sizeof(arr)/sizeof(arr[0]);i+=1){}
while(i <= 100){}
while(true){}
break;
continue;
```
### Haskell
```hs
```
## Control flow
### Rust
```rs
if value1 {
}else if value2 {
}else{
}

if let Some(x) = value{}

match value {
    Some(x) if x > 10 => c,
    1..100 => x,
    101 | 102 => b,
    _ => panic!(),
}
```
### Go
```go
if value1 {
}else if value2 {
}else{
}

switch value {
    case 1,2,3:
        fmt.Println("")
    default:
        fmt.Println("")
}

switch {
    case value < 10:
        fmt.Println("")
    case value > 10:
        fmt.Println("")
    default: 
        fmt.Println("")
}
```
### Typescript(Deno)
```ts
if (value1){
}else if(value2){
}else{
}

switch (value) {
    case 1:
        console.log("");
        break;
    case 2:
        console.log("");
        break;
    default:
        console.log("");
        break;
}
```
### Zig
```zig
if (value1) {
} else if (value2) {
} else {
}

if (value) |v| {
} else {
}

if (value) |val| {
} else |err| {
}

const b = switch (a) {
        1, 2, 3 => 0,
        5...100 => 1,
        Item.a, Item.e => |item| item,
        Item.c => |*item| blk: {
            item.*.x += 1;
            break :blk 6;
        },
        Item.d => 8,
        else => 9,
    };
```
### C
```c
if (value1) {
} else if (value2) {
} else {
}

switch (value) {
    case 1:
        printf("\n");
        break;
    case 2:
        printf("\n");
        break;
    default:
        printf("\n");
        break;
}
```
### Haskell
```hs
if condition
        then x
        else y

    case value of
        1 -> x
        _ -> y

function x :: Int -> String
function x
    | x < 0     = "Negative value"
    | x <= 10    = "Between 0 and 10 (inclusive)"
    | otherwise = "Greater than 10"
```
## Structs/Classes
### Rust
```rs
struct Person {
    name: String,
    age: u8,
}

impl Person {
    fn new(name:String,age:u8) -> Self {
        Self{
            name: name,
            age: age,
        }
    }
}

let person = Person::new("Someone",20);
```
### Go
```go
type Person struct{
    name string
    age uint
}

func newPerson(name string,age uint) *Person {
    return &Person{
        name: name,
        age: age,
    }
}

person := newPerson("Someone",20)
```
### Typescript(Deno)
```ts
class Person{
    name: string;
    age: number;
    constructor(name:string,age:number){
        this.name = name;
        this.age = age;
    }
}

let person = new Person("Someone",20);
```
### Zig
```zig
const Person = struct {
    name: []u8,
    age: u8,
    
    pub fn init(name:[]u8,age:u8) Person {
        return Person {
            .name = name,
            .age = age,
        }
    }
}
```
### C
```c
typedef struct {
    char* name;
    uint8 age;
} Person;

Person* newPerson(char* name,uint8 age){
    return &Person{name,age}
}
```
### Haskell
```hs
```
## Enums
### Rust
```rs
```

### Go
```go
```

### Typescript(Deno)
```ts
```

### Zig
```zig
```

### C
```c
```

### Haskell
```hs
```
## Traits/Interfaces
### Rust
```rs
```

### Go
```go
```

### Typescript(Deno)
```ts
```

### Zig
```zig
```

### C
```c
```

### Haskell
```hs
```
## Borrowing/Pointers
### Rust
```rs
```

### Go
```go
```

### Typescript(Deno)
```ts
```

### Zig
```zig
```

### C
```c
```

### Haskell
```hs
```
## Error handling
### Rust
```rs
```

### Go
```go
```

### Typescript(Deno)
```ts
```

### Zig
```zig
```

### C
```c
```

### Haskell
```hs
```
## Concurrency
### Rust
```rs
```

### Go
```go
```

### Typescript(Deno)
```ts
```

### Zig
```zig
```

### C
```c
```

### Haskell
```hs
```
## Modules
### Rust
```rs
```

### Go
```go
```

### Typescript(Deno)
```ts
```

### Zig
```zig
```

### C
```c
```

### Haskell
```hs
```
## Type casting
### Rust
```rs
```

### Go
```go
```

### Typescript(Deno)
```ts
```

### Zig
```zig
```

### C
```c
```

### Haskell
```hs
```
## Generics
### Rust
```rs
```

### Go
```go
```

### Typescript(Deno)
```ts
```

### Zig
```zig
```

### C
```c
```

### Haskell
```hs
```

# Configuration

## Docker

## Kubernetes

## Ansible

## Logging

## CI/CD
