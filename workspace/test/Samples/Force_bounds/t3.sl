
struct lib {
     sum: (int, int) -> int; 
     sub: (int, int) -> int;
}

func decider(l: lib, i: int) : (int, int) -> int {
    if (i == 0){
        return lib.sum; 
    } else {
        return lib.sub;
    }
} 

func executer( (f: (int, int) -> int ), a: int, b: int) : int {
    return f(a,b);
}

func add(a: int, b:int): int {
    return a + b;
}

func rm(a: int, b:int): int {
    return a - b;
}

func main(): int{

    let operations: lib;
    lib.sum = add;
    lib.sub = sub;

    print(executer(decider(lib, 0),, 4, 2));
    return 1;
}