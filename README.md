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
struct TupleStruct(String,i32);

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
    fn changeName(&mut self,name:String){
        self.name = name;
    }
}

let mut person = Person::new("Someone",20);
person.changeName("Noone");
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

func (*Person p) changeName(name string) {
    p.name = name
}

person := newPerson("Someone",20)
person.changeName("Noone")
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

    changeName(name:string){
        this.name = name;
    }
}

let person = new Person("Someone",20);
person.changeName("Noone");
```
### Zig
```zig
const Person = struct {
    name: []u8,
    age: u8,

    pub fn init(name: []u8, age: u8) Person {
        return Person{
            .name = name,
            .age = age,
        };
    }

    pub fn changeName(self: *Person, name: []u8) void {
        self.*.name = name;
    }
};

const name1 = "Someone";
const name2 = "Noone";
var buffer = [_]u8{0} ** 256;
buffer[0..name1.len].* = name1.*;
var person = Person.init(&buffer, 20);
buffer[0..name2.len].* = name2.*;
person.changeName(&buffer);
```
### C
```c
typedef struct {
    char* name;
    uint8 age;
} Person;

Person* newPerson(char* name,uint8 age){
    return &{name,age};
}

void changeName(Person* p,char* name){
    p->name = name;
}

Person* person = newPerson("Someone",20);
changeName(person);
```
### Haskell
```hs
data Person = Person
    { name :: [Char]
    , age  :: Int 
    } deriving (Show)

initPerson :: [Char] -> Int -> Person
initPerson n a = Person { name = n, age = a }
```
## Enums
### Rust
```rs
enum Result<T,E>{
    Ok(T),
    Err(E),
}
enum Option<T>{
    Some(T),
    None,
}
enum Colors{
    Red,
    Blue,
    Green,
}
```
### Go
```go
const (
    Red = iota
    Blue
    Green
)
```
### Typescript(Deno)
```ts
enum Color {
    Red,
    Blue,
    Green,
}
```
### Zig
```zig
const Color = enum(u8) {
    Red,
    Blue,
    Green,
}

const Enum = union(enum) {
    a: void,
    b: f32,
}
```
### C
```c
enum Color {
    Red,
    Blue,
    Green,
}
```
### Haskell
```hs
data Color = Red | Blue | Green
     deriving (Show)
```
## Traits/Interfaces
### Rust
```rs
trait PrintInfo {
    fn print_info(&self);
}

struct Person {
    name: String,
    age: u8,
}

impl PrintInfo for Person {
    fn print_info(&self) {
        println!("Name: {}, Age: {}", self.name, self.age);
    }
}

struct Car {
    model: String,
    year: u32,
}

impl PrintInfo for Car {
    fn print_info(&self) {
        println!("Model: {}, Year: {}", self.model, self.year);
    }
}

fn printPrintable<T:PrintInfo>(printable: &T){
    printable.print_info();
}
//or
fn printPrintable<T>(printable: &T)
where 
    T: PrintInfo
{
    printable.print_info();
}
//or
fn printPrintable(printable: Box<&dyn PrintInfo>){
    printable.print_info();
}
```
### Go
```go
type PrintInfo interface {
    print_info()
}

type Car struct {
    model string
    year u32
}

type Person struct {
    name string
    age u8
}

func (p Person) print_info() {
    fmt.Printf("Name: %s, Age: %d\n", p.name, p.age)
}

func (c Car) print_info() {
    fmt.Printf("Model: %s, Year: %d\n", c.model, c.year)
}

func printPritable(printable PrintInfo){
    printable.print_info()
}
```
### Typescript(Deno)
```ts
interface PrintInfo {
    print_info();
}

class Car implements PrintInfo{
    model:string;
    year:number;

    print_info(){
        console.log(`${this.model} ${this.year}`);
    }
} 

class Person implements PrintInfo {
    name:string;
    age:number;

    print_info(){
        console.log(`${this.name} ${this.age}`);
    }
} 

function printPritable(printable: PrintInfo){
    printable.print_info();
}
```
### Zig
```zig
const std = @import("std");

const Car = struct {
    model: []const u8,
    year: u32,

    pub fn print_info(self: Car) void {
        std.debug.print("Model: {s}, Year: {d}", .{ self.model, self.year });
    }
};

const Person = struct {
    name: []const u8,
    age: u8,

    pub fn print_info(self: Person) void {
        std.debug.print("Name: {s}, Age: {d}", .{ self.name, self.age });
    }
};

const PrintInfo = union(enum) {
    car: Car,
    person: Person,

    pub fn print_info(self: PrintInfo) void {
        switch (self) {
            inline else => |case| case.print_info(),
        }
    }
};
pub fn main() !void {
    var car = Car{ .model = "Car", .year = 2002 };
    printPritable(PrintInfo{ .car = car });
}

fn printPritable(printable: PrintInfo) void {
    printable.print_info();
}
```
### Haskell
```hs
class Printable a where
    printMe :: a -> String

instance Printable Int where
    printMe x = show x

instance Printable Char where
    printMe c = [c]

printTwice :: Printable a => a -> String
printTwice x = printMe x ++ ", " ++ printMe x
```
## Borrowing/Pointers
### Rust
```rs
let a: i32 = 5;
let b: &i32 = &a;
let c: &i32 = &a;

let mut a: i32 = 5;
let b: &mut i32 = &mut a;
*b = 7;

let a: i32 = 42;
let raw_pointer: *const i32 = &a;
```
### Go
```go
var a int = 5;
var b *int = &a;
*b = 7;
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
