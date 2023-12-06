Table of contents:
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
   - [Nginx](#nginx)

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
    return ("Someone",nil)
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

## Docker 
<https://docs.docker.com/>
### Dockerfile
```dockerfile
FROM image:version
RUN dnf update
USER user
WORKDIR /data
COPY . .
ENV var=5
CMD ["echo",${var}]
```
### Docker compose file
```docker-compose
version: 3.8

services:
  web:
    image: nginx:latest
    ports:
      - "8080:80"
    volumes:
      - ./nginx.conf:/etc/nginx/nginx.conf
    environment:
      - NGINX_ENV=production
    networks:
      - frontend
      - backend

  app:
    image: myapp:latest
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

## Firewall
### Firewalld
<https://firewalld.org/documentation/>

## Nginx
<https://docs.nginx.com/>

## OpenTofu(Terraform)
<https://opentofu.org/docs>
