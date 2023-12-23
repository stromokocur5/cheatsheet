1. [Programming](#programming)
   - [Rust](#rust)
   - [Go](#go)
   - [Typescript(Deno)](#typescript-deno)
   - [Zig](#zig)
   - [Haskell](#haskell)
   - [C](#c)
2. [Devops](#devops)
   - [Docker](#docker)
   - [Kubernetes](#kubernetes)
   - [Ansible](#ansible)
   - [CI/CD](#ci-cd)
   - [Prometheus](#prometheus)
   - [Grafana](#grafana)
   - [Firewalld](#firewalld)
   - [Nginx](#nginx)
   - [Opentofu(Terraform)](#opentofu-terraform)
3. [Regex](#regex)
4. [Fresh(Deno web framework)](#fresh-deno-web-framework)
# Programming
<https://cheat.sh/>
## Rust
<https://docs.rs/>
<https://lib.rs/>
### Variables
```rs
let a = 5;
let mut a: i32 = 5;
const a: i32 = 5;
```
### Types
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
### Functions
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
### Loops
```rs
for i in 0..100 {}
for i in x {}
while i <= 100 {}
loop{}
break;
continue;
```
### Control flow
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
### Structs
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
### Enums 
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
### Traits
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
### Borrowing/Poiters
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
### Error handling
```rs

let option: Option<i32> = Some(5);
let option: Option<i32> = None;
let result: Result<i32,String> = Ok(5);
let result: Result<i32,String> = Err("err".to_string());

if let Some(s) = option{}
       ----
       None

if let Ok(o) = result{}
       ----
       Err(e)

match option{
    Some(s) => s,
    _ => panic!(),
}

match result{
    Ok(o) => o,
    _ => panic!(),
}
option.unwrap();
result.unwrap();
result?;
```
### Concurrency
```rs
fn main() {
    let (tx, rx) = mpsc::channel();

    thread::spawn(move || {
        for i in 0..5 {
            tx.send(i).expect("Send error");
        }
    });

    for received in rx {
        println!("Received: {}", received);
    }
}

//or

fn main() {
    let counter = Arc::new(Mutex::new(0));

    let mut handles = vec![];

    for _ in 0..10 {
        let counter = Arc::clone(&counter);
        let handle = thread::spawn(move || {
            let mut num = counter.lock().unwrap();
            *num += 1;
        });
        handles.push(handle);
    }

    for handle in handles {
        handle.join().unwrap();
    }

    println!("Result: {}", *counter.lock().unwrap());

//or

async fn worker(id: usize) {
    println!("Worker {} is starting.", id);
    
    sleep(Duration::from_secs(2)).await;
    
    println!("Worker {} is done.", id);
}

#[tokio::main]
async fn main() {
    let mut handles = vec![];
    for i in 0..5 {
        let handle = tokio::spawn(worker(i));
        handles.push(handle);
    }

    for handle in handles {
        handle.await.expect("Failed to join worker thread");
    }

    println!("All workers are done!");
}

//or

#[tokio::main]
async fn main() {
    let (tx, mut rx) = mpsc::channel(32);

    tokio::spawn(async move {
        for i in 0..5 {
            tokio::time::sleep(Duration::from_secs(1)).await;
            tx.send(i).await.expect("Send error");
        }
    });

    while let Some(received) = rx.recv().await {
        println!("Received: {}", received);
    }
}
```
### Modules
```rs
// In main.rs or lib.rs
mod my_module;
use my_module::public_function;

fn main() {
    my_module::public_function();
    public_function();
    // my_module::private_function(); // This would be an error since it's private
}

// In my_module.rs
pub fn public_function() {
    println!("Public function");
}

fn private_function() {
    println!("Private function");
}
```
### Type casting
```rs
let integer_number: i32 = 42;
let float_number: f64 = integer_number as f64;

//or

struct MyStruct {
    value: i32,
}

impl From<i32> for MyStruct {
    fn from(value: i32) -> Self {
        MyStruct { value }
    }
}

let my_struct: MyStruct = 42.into();
```
### Generics
```rs
fn find_larger<T>(a: T, b: T) -> T
where
    T: PartialOrd,
{
    if a >= b {
        a
    } else {
        b
    }
}
```
### Lifetime
```rs
fn longer_string<'a>(s1: &'a str, s2: &'a str) -> &'a str {
    if s1.len() > s2.len() {
        s1
    } else {
        s2
    }
}
```
## Go 
<https://go.dev/doc/>
<https://gobyexample.com/>
### Variables
```go
a := 5 
var a int = 5
const a int = 5
```
### Types
```go
int int8 int16 int32 int64
uint uint8 uint16 uint32 uint64
float32 float64
bool
string
error
rune
[n]T
map[K]V
nil
any
```
### Functions
```go
func add(a int, b int) int {
    return a + b
}

//or

add := func(a int, b int) int {
    return a + b
}
```
### Loops
```go
for i:=0;i<=100;i+=1{}
for i,item := range x{}
for i <= 100 {}
for {}
break
continue
```
### Control Flow
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
### Structs
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

func (p *Person) changeName(name string) {
    p.name = name
}

person := newPerson("Someone",20)
person.changeName("Noone")
```
### Enums
```go
const (
    Red = iota
    Blue
    Green
)
```
### Interfaces
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
### Pointers
```go
var a int = 5;
var b *int = &a;
*b = 7;
```
### Error Handling
```go
func strOrErr(name string) (r string, err error){
    if name != "Someone" {
        return "", errors.New("name not allowed")
    }
    return "Someone",nil
}

r,err := strOrErr("Someone")
if err != nil {}

if _,err := strOrErr("Someone"); err != nil {}


func CopyFile(dstName, srcName string) (written int64, err error) {
    src, err := os.Open(srcName)
    if err != nil {
        return
    }
    defer src.Close()

    dst, err := os.Create(dstName)
    if err != nil {
        return
    }
    defer dst.Close()

    return io.Copy(dst, src)
}
```
### Concurrency
```go
func f(n int) {
  for i := 0; i < 10; i++ {
    fmt.Println(n, ":", i)
    amt := time.Duration(rand.Intn(250))
    time.Sleep(time.Millisecond * amt)
  }
}

for i := 0; i < 10; i++ {
    go f(i)
}

//or 

func pinger(c chan string) {
  for i := 0; ; i++ {
    c <- "ping"
  }
}

func ponger(c chan string) {
  for i := 0; ; i++ {
    c <- "pong"
  }
}

func printer(c chan string) {
  for {
    msg := <- c
    fmt.Println(msg)
    time.Sleep(time.Second * 1)
  }
}

var c chan string = make(chan string)
go pinger(c)
go ponger(c)
go printer(c)
```
### Modules
```go
// In file main.go
package main;
import (
    "fmt"
    "feature"
)

func main() {
   feature.ExportedFunction()
}

// In file feature.go
package feature;

// ExportedFunction is visible outside the package
func ExportedFunction() {
    // Function implementation
}

// nonExportedFunction is only visible within the package
func nonExportedFunction() {
    // Function implementation
}
```
### Type casting
```go
var x int = 42
var y float64 = float64(x)
```
### Generics
```go
func findLarger[T any](a, b T) T {
    if a.(int) > b.(int) {
        return a
    }
    return b
}
```
## Typescript(Deno) 
<https://docs.deno.com/runtime/manual>
<https://deno.land/std>
### Variables
```ts
let a = 5;
let a: number = 5;
const a:number = 5;
```
### Types
```ts
number
string
boolean
T[]
?T
null
any
undefined
```
### Functions
```ts
function add(a:number,b:number): number{
    return a + b;  
}
//or
const add = (a:number,b:number): number => {
    return a + b;
};
```
### Loops
```ts
for (let i = 0;i<=100;i+=1){}
for (let i of x){}
while (i <= 100){}
while (true) {}
break;
continue;
```
### Control flow
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
### Classes
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
### Enums
```ts
enum Color {
    Red,
    Blue,
    Green,
}
```
### Interfaces
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
### Error handling
```ts
try {
  const data = await Deno.readFile("nonexistent-file.txt");
  console.log(data);
} catch (error) {
  console.error("Error reading file:", error);
}
```
### Concurrency
```ts
async function fetchData(url: string) {
  const response = await fetch(url);
  const data = await response.json();
  return data;
}

const url1 = "https://jsonplaceholder.typicode.com/todos/1";
const url2 = "https://jsonplaceholder.typicode.com/todos/2";

const promise1 = fetchData(url1);
const promise2 = fetchData(url2);

const result1 = await promise1;
const result2 = await promise2;

console.log("Result from URL 1:", result1);
console.log("Result from URL 2:", result2);
```
### Modules
```ts
//In file main.ts
import PI1 from "feature.ts";
import {PI2} from "feature.ts";
import { someFunction } from "https://example.com/some-module.ts";

//In file feature.ts
export default const PI1 = 3.14;
export const PI2 = 3.13
```
### Type casting
```ts
let x: number = 42;
let y: number = Number(x);

let str: string = "123";
let num: number = parseInt(str, 10);
```
### Generics
```ts
function findLarger<T>(a: T, b: T): T {
    return a > b ? a : b;
}
```
## Zig 
<https://ziglang.org/documentation/>
### Variables
```zig
var a: i32 = 5;
const a:i32 = 5;
```
### Types
```zig
i8 i32 i64 i128 isize
u8 u32 u64 u128 usize
f32 f64  
bool
void
[]T
?T
!T
null
type
anytype
undefined
```
### Functions
```zig
fn add(a: i32, b: i32) i32 {
    return a + b;
}
```
### Loops
```zig
for (items) |i|{}
for (items,0..) |_,i|{}
for (0..100) |i|{}
while(i <= 100){}
while(true){}
break;
continue;
```
### Control flow
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
### Structs
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
### Enums
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
### Interfaces
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
### Pointers
```zig
var a: i32 = 5;
var b: *i32 = &a;
b.* = 7;
```
### Error handling
```zig
fn doAThing(str: []u8) !void {
    const number = try parseU64(str, 10);
}
//or
fn doAThing(str: []u8) !void {
    const number = parseU64(str, 10) catch |err| return err;
}

fn doAnotherThing(str: []u8) error{InvaidChar}!void {
    if (parseU64(str, 10)) |number| {
        doSomethingWithNumber(number);
    } else |err| switch (err) {
        error.Overflow => {
        },
        else => |leftover_err| return leftover_err,
    }
}
```
### Concurrency
```zig
const net = @import("std").net;

pub const io_mode = .evented;

pub fn main() !void {
    const addr = try net.Address.parseIp("127.0.0.1", 7000);

    var sendFrame = async send_message(addr);
    // ... do something else while
    //     the message is being sent ...
    try await sendFrame;
}

// Note how the function definition doesn't require any static
// `async` marking. The compiler can deduce when a function is
// async based on its usage of `await`.
fn send_message(addr: net.Address) !void {
    // We could also delay `await`ing for the connection
    // to be established, if we had something else we
    // wanted to do in the meantime.
    var socket = try net.tcpConnectToAddress(addr);
    defer socket.close();

    // Using both await and async in the same statement
    // is unnecessary and non-idiomatic, but it shows
    // what's happening behind the scenes when `io_mode`
    // is `.evented`.
    _ = try await async socket.write("Hello World!\n");
}
```
### Modules
```zig
// mymodule.zig
const std = @import("std");

pub fn myFunction() void {
    std.debug.print("Hello from myFunction!\n", .{});
}

// main.zig
const mymodule = @import("mymodule.zig");

pub fn main() void {
    mymodule.myFunction();
}
```
### Type casting
```zig
var a: u8 = 1;
var b = @as(u16, a);
```
### Generics
```zig
fn findLarger(comptime T: type, a: T, b: T) T {
    if (a > b) {
        return a;
    } else {
        return b;
    }
}
```
## Haskell 
<https://www.haskell.org/documentation/>
### Variables
```hs
let a = 5

let a :: Int
    a = 5
```
### Types
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
### Functions
```hs
add a b :: Int -> Int -> Int
add a b = a + b

let add = \a b -> a + b

squares :: [Int] -> [Int]
squares numbers = [x * x | x <- numbers]

showHeadAndTail :: [a] -> String
showHeadAndTail [] = "Empty list"
showHeadAndTail (x:xs) = "Head: " ++ show x ++ ", Tail: " ++ show xs
```
### Control flow
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
### Structs
```hs
data Person = Person
    { name :: [Char]
    , age  :: Int 
    } deriving (Show)

initPerson :: [Char] -> Int -> Person
initPerson n a = Person { name = n, age = a }
```
### Enums
```hs
data Color = Red | Blue | Green
     deriving (Show)
```
### Interfaces
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
### Error handling
```hs
data Maybe a = Nothing | Just a
data Either a b = Left a | Right b

let result = divide 10 2
    case result of
        Just x -> putStrLn $ "Result: " ++ show x
        Nothing -> putStrLn "Error: Division by zero"

let result = divide 10 2
    case result of
        Right x -> putStrLn $ "Result: " ++ show x
        Left err -> putStrLn $ "Error: " ++ err
```
### Concurrency
```hs
import Control.Concurrent

main :: IO ()
main = do
    putStrLn "Start"

    -- Create a new thread
    tid <- forkIO $ do
        putStrLn "Async operation"
        threadDelay 2000000 -- Simulate a delay

    -- Continue with main thread
    putStrLn "Main thread"

    -- Wait for the asynchronous thread to finish
    threadDelay 3000000
    putStrLn "End"

--or

import Control.Concurrent
import Control.Concurrent.Async

main :: IO ()
main = do
    putStrLn "Start"

    -- Start an asynchronous operation
    a <- async $ do
        putStrLn "Async operation"
        threadDelay 2000000

    -- Continue with main thread
    putStrLn "Main thread"

    -- Wait for the asynchronous task to finish
    wait a

    putStrLn "End"
```
### Modules
```hs
-- ModuleExample.hs

module ModuleExample
    ( add       -- Exporting the 'add' function
    , subtract  -- Exporting the 'subtract' function
    ) where

-- Function Definitions
add :: Int -> Int -> Int
add a b = a + b

subtract :: Int -> Int -> Int
subtract a b = a - b

-- Main.hs

import ModuleExample (add, subtract)

main :: IO ()
main = do
    let result1 = add 3 4
        result2 = subtract 7 2

    putStrLn $ "Addition Result: " ++ show result1
    putStrLn $ "Subtraction Result: " ++ show result2
```
## C 
<https://devdocs.io/c/>
### Variables
```c
int a = 5;
const int a = 5;
```
### Types
```c
int
double
float
unsigned int
char
long
unsigned long
null
```
### Functions
```c
int add(int a, int b) {
    return a + b;
}
```
### Loops
```c
for (int i=0;i<=100;i+=1){}
for (int i=0;i<sizeof(arr)/sizeof(arr[0]);i+=1){}
while(i <= 100){}
while(true){}
break;
continue;
```
### Control flow
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
### Structs
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
### Enums
```c
enum Color {
    Red,
    Blue,
    Green,
}
```
### Pointers
```c
int a = 5;
int* b = &a;
*b = 7;
```
### Error handling
```c
int divide(int a, int b, int* result) {
    if (b == 0) {
        return 1;
    }
    *result = a / b;
    return 0;
}
    int a = 10, b = 2, result;
    int errorCode = divide(a, b, &result);
    if (errorCode != 0) {
        printf("Error: Division by zero\n");
        exit(1);
    }
    printf("%d / %d = %d\n", a, b, result);
}
```
### Concurrency
```c
#include <stdio.h>
#include <pthread.h>

void* myAsyncFunction(void* arg) {
    printf("Async function is running\n");
    return NULL;
}

int main() {
    pthread_t thread;

    // Create a thread for asynchronous-like behavior
    if (pthread_create(&thread, NULL, myAsyncFunction, NULL) != 0) {
        fprintf(stderr, "Error creating thread\n");
        return 1;
    }

    if (pthread_join(thread, NULL) != 0) {
        fprintf(stderr, "Error joining thread\n");
        return 1;
    }

    printf("Main function is continuing\n");

    return 0;
}
```
### Modules
```c
// module.h

// Function declaration
int add(int a, int b);

// module.c
#include "module.h"

// Function definition
int add(int a, int b) {
    return a + b;
}

// main.c
#include <stdio.h>
#include "module.h"

int main() {
    int result = add(3, 4);
    printf("Result: %d\n", result);

    return 0;
}
```
### Type casting
```c
double x = 20.5;
int y;
y = (int)x;
```
# Devops 

## Postgres
<https://www.postgresql.org/docs/current/index.html>
### General Commands
```sql
\l: List all databases
\l+: List all databases and their sizes
\du: List all users
\conninfo: Display connection information
\?: Display help for PostgreSQL commands
\h <command>: Display help for a specific command
\g: Execute the previous query
\q: Quit PostgreSQL
```
### Data Types
| Data Type | Description | Example |
|---|---|---|
| Boolean | True or false values | `BOOLEAN` |
| CHAR, VARCHAR, TEXT | Fixed-length or variable-length character strings | `CHAR(255)`, `VARCHAR(255)`, `TEXT` |
| NUMERIC | Decimal numbers with precision and scale | `NUMERIC(10,2)` |
| Integer | Whole numbers | `INTEGER` |
| SERIAL | Automatically incrementing integer values | `SERIAL` |
| DATE | Date values | `DATE` |
| TIMESTAMP | Date and time values | `TIMESTAMP` |
| Interval | Time duration values | `INTERVAL` |
### Creating and Managing Tables
```sql
CREATE TABLE <table_name> (
  <column1_name> <data_type>,
  <column2_name> <data_type>,
  ...
);

ALTER TABLE <table_name>
ADD <new_column_name> <data_type>;

ALTER TABLE <table_name>
DROP COLUMN <column_name>;

DESCRIBE <table_name>;
```
### Inserting and Selecting Data
```sql
INSERT INTO <table_name> (
  <column1_name>,
  <column2_name>,
  ...
)
VALUES (
  <value1>,
  <value2>,
  ...
);

SELECT * FROM <table_name>;

SELECT <column1_name>, <column2_name>
FROM <table_name>
WHERE <condition>;

SELECT * FROM <table_name>
ORDER BY <column_name> ASC/DESC;
```
### Filtering Data
```sql
WHERE <column_name> = <value>;

WHERE <column_name> IN (<value1>, <value2>, ...);

WHERE <column_name> BETWEEN <value1> AND <value2>;

WHERE <column_name> LIKE '%<pattern>%';
```
### Aggregating Data
```sql
SELECT COUNT(*) FROM <table_name>;

SELECT SUM(<column_name>) FROM <table_name>;

SELECT AVG(<column_name>) FROM <table_name>;

SELECT MIN(<column_name>), MAX(<column_name>) FROM <table_name>;
```
### Connecting to PostgreSQL
```sql
psql -h <hostname> -p <port> -U <username> -d <database>
```
### Navigating Database Objects

```sql
\d <table_name>: Describe the table structure
\d+ <table_name>: Describe the table structure in detail
\d schema_name.table_name: Describe the table structure in a specific schema
```
## Docker 
<https://docs.docker.com/>
### Dockerfile
```dockerfile
FROM image:version AS builder
RUN dnf update
USER user
WORKDIR /app
COPY . .

FROM image:version
COPY --from=builder /app /app
ENV var=5
CMD ["echo",${var}]
```
### Docker compose file
```yml
version: 3.8

services:
  web:
    image: nginx:latest
    ports:
      - 8080:80
    volumes:
      - ./nginx.conf:/etc/nginx/nginx.conf
    environment:
      - NGINX_ENV=production
    networks:
      - frontend
      - backend

  app:
    image: myapp:latest
    restart: always
    environment:
      - APP_ENV=production
    env_file: 
        - .env
    volumes:
      - ./app:/app
    depends_on:
      - db
    networks:
      - backend

  db:
    image: mysql:latest
    environment:
      - MYSQL_ROOT_PASSWORD=root_password
      - MYSQL_DATABASE=mydatabase
      - MYSQL_USER=myuser
      - MYSQL_PASSWORD=mypassword
    volumes:
      - mysql_data:/var/lib/mysql
    networks:
      - backend

networks:
  frontend:
  backend:

volumes:
  mysql_data:
```
## Kubernetes 
<https://kubernetes.io/docs/home/>
### Deployments
```yml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: your-app
  namespace: your-namespace
spec:
  replicas: 3
  selector:
    matchLabels:
      app: your-app
  template:
    metadata:
      labels:
        app: your-app
    spec:
      containers:
      - name: your-app-container
        image: your-docker-image:latest
        ports:
        - containerPort: 80
```
### Service
```yml
apiVersion: v1
kind: Service
metadata:
  name: your-app-service
spec:
  selector:
    app: your-app
  ports:
    - name: http
      protocol: TCP
      port: 80
      targetPort: 80
  type: ClusterIP
```
### Ingress
```yml
apiVersion: networking.k8s.io/v1
kind: Ingress
metadata:
  name: your-app-ingress
  annotations:
    nginx.ingress.kubernetes.io/rewrite-target: /
spec:
  rules:
  - host: your-app.example.com
    http:
      paths:
      - path: /
        pathType: Prefix
        backend:
          service:
            name: your-app-service
            port:
              number: 80
```
### ConfigMap
```yml
apiVersion: v1
kind: ConfigMap
metadata:
  name: your-config-map
data:
  key1: value1
  key2: value2
```
### Secret
```yml
apiVersion: v1
kind: Secret
metadata:
  name: your-secret
type: Opaque
data:
  username: base64encodedusername
  password: base64encodedpassword
```
### PersistentVolume
```yml
apiVersion: v1
kind: PersistentVolume
metadata:
  name: your-pv
spec:
  capacity:
    storage: 1Gi
  accessModes:
    - ReadWriteOnce
  hostPath:
    path: /path/on/host
```
### PersistentVolumeClaim
```yml
apiVersion: v1
kind: PersistentVolumeClaim
metadata:
  name: your-pvc
spec:
  accessModes:
    - ReadWriteOnce
  resources:
    requests:
      storage: 1Gi
```
### Job
```yml
apiVersion: batch/v1
kind: Job
metadata:
  name: pi
spec:
  template:
    spec:
      containers:
      - name: pi
        image: perl:5.34.0
        command: ["perl",  "-Mbignum=bpi", "-wle", "print bpi(2000)"]
      restartPolicy: Never
  backoffLimit: 4
```
### CronJob
```yml
apiVersion: batch/v1beta1
kind: CronJob
metadata:
  name: your-cronjob
spec:
  schedule: "*/5 * * * *"
  jobTemplate:
    spec:
      template:
        spec:
          containers:
          - name: your-cronjob-container
            image: your-docker-image:latest
  successfulJobsHistoryLimit: 3
  failedJobsHistoryLimit: 1
```
### Role
```yml
apiVersion: rbac.authorization.k8s.io/v1
kind: Role
metadata:
  name: your-role
rules:
- apiGroups: [""]
  resources: ["pods"]
  verbs: ["get", "list"]
```
### Namespace
```yml
apiVersion: v1
kind: Namespace
metadata:
  name: your-namespace
```
## Ansible 
<https://docs.ansible.com/ansible/>
### Playbook
```yml
---
- name: Comprehensive Ansible Playbook
  hosts: all

  vars:
    common_variable: "This is a common variable"

  tasks:
    - name: Ensure packages are installed
      package:
        name: "{{ item }}"
        state: present
      with_items:
        - nginx
        - php
      tags:
        - install

    - name: Copy configuration files
      template:
        src: templates/nginx.conf.j2
        dest: /etc/nginx/nginx.conf
      notify: Restart Nginx
      tags:
        - configure

    - name: Manage files and directories
      file:
        path: "/tmp/my_file.txt"
        state: touch

    - name: Execute a command
      command: echo "Hello, Ansible!"

    - name: Manage users
      user:
        name: myuser
        state: present
        shell: /bin/bash
        groups: users
        append: yes

    - name: Manage groups
      group:
        name: mygroup
        state: present

    - name: Install Python package
      pip:
        name: requests

    - name: Manage services
      service:
        name: nginx
        state: restarted

    - name: Manage systemd units
      systemd:
        name: myservice
        state: started

    - name: Manage cron jobs
      cron:
        name: "My Cron Job"
        minute: "0"
        hour: "1"
        job: "/path/to/my/script.sh"

    - name: Fetch files from remote hosts
      fetch:
        src: "/path/to/remote/file.txt"
        dest: "/tmp/"

    - name: Execute a script
      script:
        src: scripts/my_script.sh

    - name: Make HTTP request
      uri:
        url: "https://example.com/api"
        method: GET

    - name: Add or remove a line from a file
      lineinfile:
        path: "/path/to/file.txt"
        line: "my_line"
        state: present

    - name: Set and register a fact
      set_fact:
        my_fact: "This is a fact"
      register: fact_result

    - name: Debug information
      debug:
        var: fact_result
      when: ansible_distribution == 'CentOS'

  handlers:
    - name: Restart Nginx
      service:
        name: nginx
        state: restarted
```
### Inventory
```yml
web_servers:
  hosts:
    webserver[1:3]:
      ansible_user: ubuntu
      ansible_ssh_private_key_file: /path/to/private_key.pem
      environment: production
      my_variable: web_var

    webserver2:
      ansible_host: 192.168.1.102
      ansible_user: centos
      ansible_ssh_private_key_file: /path/to/private_key.pem
      environment: staging
      my_variable: web_var

  vars:
    common_variable: value

db_servers:
  hosts:
    dbserver1:
      ansible_host: 192.168.1.103
      ansible_user: root
      ansible_ssh_private_key_file: /path/to/private_key.pem
      environment: production
      my_variable: db_var

    dbserver2:
      ansible_host: 192.168.1.104
      ansible_user: root
      ansible_ssh_private_key_file: /path/to/private_key.pem
      environment: development
      my_variable: db_var

  vars:
    common_variable: value
```
## CI/CD 
### Gitlab CI
<https://docs.gitlab.com/ee/ci/>
```yml
default:
    image: denoland/deno:latest

stages:
    - build
    - test
    - deploy

build-job:
    stage: build
    script: 
        - echo "Building..."
        - deno task build
        - echo "Successfully built"

test-job:
    stage: test
    script: 
        - echo "Testing ..."
        - deno task test
        - echo "Successfully built"

deploy-job-prod:
  stage: deploy 
  environment: production
  script:
    - echo "Installling deployctl"
    - deno install -Arf https://deno.land/x/deploy/deployctl.ts
    - echo "Deploying application..."
    - env DENO_DEPLOY_TOKEN=$DENO_DEPLOY_TOKEN deployctl deploy --prod --project=project main.ts
    - echo "Application successfully deployed."
  rules:
    - if: $CI_COMMIT_BRANCH == "main"

deploy-job-dev:
  stage: deploy 
  environment: development
  script:
    - echo "Installling deployctl"
    - deno install -Arf https://deno.land/x/deploy/deployctl.ts
    - echo "Deploying application..."
    - env DENO_DEPLOY_TOKEN=$DENO_DEPLOY_TOKEN deployctl deploy --prod --project=project-dev main.ts
    - echo "Application successfully deployed."
  rules:
    - if: $CI_COMMIT_BRANCH == "dev"    

---

end-to-end-tests:
  image: node:latest
  services:
    - name: selenium/standalone-firefox:${FIREFOX_VERSION}
      alias: firefox
    - name: registry.gitlab.com/organization/private-api:latest
      alias: backend-api
    - postgres:14.3
  variables:
    FF_NETWORK_PER_BUILD: 1
    POSTGRES_PASSWORD: supersecretpassword
    BACKEND_POSTGRES_HOST: postgres
  script:
    - npm install
    - npm test
```
## Prometheus
<https://prometheus.io/docs/>

## Grafana
<https://grafana.com/docs/grafana/>

## Firewalld
<https://firewalld.org/documentation/>
### Check the status of Firewalld
```bash
sudo firewall-cmd --state
```
### Reload Firewalld rules
```bash
sudo firewall-cmd --reload
```
### Add a new permanent rule
```bash
sudo firewall-cmd --permanent --add-rule type=<type> chain=<chain> protocol=<protocol> port=<port> source=<source> destination=<destination> action=<action>
```
### Remove a permanent rule
```bash
sudo firewall-cmd --permanent --remove-rule type=<type> chain=<chain> protocol=<protocol> port=<port> source=<source> destination=<destination> action=<action>
```
### Add an ICMP rule
```bash
sudo firewall-cmd --permanent --add-icmp-rule type=block chain=input icmp-type=<icmp-type>
```
### Remove an ICMP rule
```bash
sudo firewall-cmd --permanent --remove-icmp-rule type=block chain=input icmp-type=<icmp-type>
```
### Add a service
```bash
sudo firewall-cmd --permanent --add-service=<service>
```
### Remove a service
```bash
sudo firewall-cmd --permanent --remove-service=<service>
```
### Enable a zone
```bash
sudo firewall-cmd --permanent --set-default-zone=<zone>
```
### Disable a zone
```bash
sudo firewall-cmd --permanent --set-default-zone=drop
```
### View active zones
```bash
sudo firewall-cmd --get-active-zones
```
### View allowed incoming traffic
```bash
sudo firewall-cmd --list-ports --zone=<zone>
```
### View allowed outgoing traffic
```bash
sudo firewall-cmd --list-ports --zone=<zone>--source type=external
```
### Add a source IP range
```bash
sudo firewall-cmd --permanent --add-source-address=<address>
```
### Remove a source IP range
```bash
sudo firewall-cmd --permanent --remove-source-address=<address>
```
### Add a destination IP range
```bash
sudo firewall-cmd --permanent --add-destination-address=<address>
```
### Remove a destination IP range
```bash
sudo firewall-cmd --permanent --remove-destination-address=<address>
```
### Allow Ping from specific IP
```bash
sudo firewall-cmd --permanent --add-rich-rule rule family=ipv4 source address=192.168.1.1 type=icmp inspect_jump=allow
```
### Block Ping from all IPs
```bash
sudo firewall-cmd --permanent --add-rich-rule rule family=ipv4 type=icmp inspect_jump=drop
```
## Nginx
<https://docs.nginx.com/>
```nginx
user       www www;  ## Default: nobody
worker_processes  5;  ## Default: 1
error_log  logs/error.log;
pid        logs/nginx.pid;
worker_rlimit_nofile 8192;

events {
  worker_connections  4096;  ## Default: 1024
}

http {
  include    conf/mime.types;
  include    /etc/nginx/proxy.conf;
  include    /etc/nginx/fastcgi.conf;
  index    index.html index.htm index.php;

  default_type application/octet-stream;
  log_format   main '$remote_addr - $remote_user [$time_local]  $status '
    '"$request" $body_bytes_sent "$http_referer" '
    '"$http_user_agent" "$http_x_forwarded_for"';
  access_log   logs/access.log  main;
  sendfile     on;
  tcp_nopush   on;
  server_names_hash_bucket_size 128; # this seems to be required for some vhosts

  server { # php/fastcgi
    listen       80;
    server_name  domain1.com www.domain1.com;
    access_log   logs/domain1.access.log  main;
    root         html;

    location ~ \.php$ {
      fastcgi_pass   127.0.0.1:1025;
    }
  }

  server { # simple reverse-proxy
    listen       80;
    server_name  domain2.com www.domain2.com;
    access_log   logs/domain2.access.log  main;

    # serve static files
    location ~ ^/(images|javascript|js|css|flash|media|static)/  {
      root    /var/www/virtual/big.server.com/htdocs;
      expires 30d;
    }

    # pass requests for dynamic content to rails/turbogears/zope, et al
    location / {
      proxy_pass      http://127.0.0.1:8080;
    }
  }

  upstream big_server_com {
    server 127.0.0.3:8000 weight=5;
    server 127.0.0.3:8001 weight=5;
    server 192.168.0.1:8000;
    server 192.168.0.1:8001;
  }

  server { # simple load balancing
    listen          80;
    server_name     big.server.com;
    access_log      logs/big.server.access.log main;

    location / {
      proxy_pass      http://big_server_com;
    }
  }
}
```

## OpenTofu(Terraform)
<https://opentofu.org/docs>

# Regex
<https://regexr.com/>
### Quantifiers
| Quantifier | Description |
|---|---|
| `.` | Matches any single character |
| `*` | Matches zero or more occurrences of the preceding character |
| `+` | Matches one or more occurrences of the preceding character |
| `?` | Matches zero or one occurrence of the preceding character |
| `{n}` | Matches exactly `n` occurrences of the preceding character |
| `{n,m}` | Matches at least `n` and at most `m` occurrences of the preceding character |
### Character Classes
| Pattern | Description |
|---|---|
| `[abc]` | Matches any of the characters `a`, `b`, or `c` |
| `[^abc]` | Matches any character except `a`, `b`, or `c` |
| `[0-9]` | Matches any digit (0-9) |
| `[a-zA-Z]` | Matches any letter (a-z or A-Z) |
| `\w` | Matches any alphanumeric character (a-z, A-Z, 0-9, `_`) |
| `\s` | Matches any whitespace character (space, tab, newline, etc.) |
### Escape Sequences
| Escape Sequence | Description |
|---|---|
| `\d` | Matches a digit (0-9) |
| `\D` | Matches any character that is not a digit (0-9) |
| `\s` | Matches any whitespace character (space, tab, newline, etc.) |
| `\S` | Matches any character that is not a whitespace character (space, tab, newline, etc.) |
| `\w` | Matches any alphanumeric character (a-z, A-Z, 0-9, `_`) |
| `\W` | Matches any character that is not an alphanumeric character (a-z, A-Z, 0-9, `_`) |
### Anchors
| Anchor | Description |
|---|---|
| `^` | Matches the beginning of a string |
| `$` | Matches the end of a string |
| `\A` | Matches the beginning of a string, including newlines |
| `\Z` | Matches the end of a string, including newlines |
### Grouping and Capturing
| Pattern | Description |
|---|---|
| `()` | Groups the matched characters and captures them in a numbered group |
| `\1` | Refers to the first captured group |
| `\2` | Refers to the second captured group, and so on |
### Metacharacters
| Metacharacter | Description |
|---|---|
| `|` | Matches either of the two expressions |
| `()` | Groups the matched characters and captures them in a numbered group |
| `\1` | Refers to the first captured group |
| `\2` | Refers to the second captured group, and so on |
### Examples
| Regex | Description |
|---|---|
| `\d{3}-\d{3}-\d{4}` | Matches a U.S. telephone number in the format ###-###-#### |
| `[a-zA-Z]+` | Matches a word consisting of one or more letters |
| `[^0-9a-zA-Z]` | Matches any character that is not a digit or a letter |
| `(.*)\s(.*)` | Matches two words separated by one or more whitespace characters |
| `(.*)\1` | Matches an exact repetition of the first word |
# Fresh(Deno web framework)
<https://fresh.deno.dev/docs/>
