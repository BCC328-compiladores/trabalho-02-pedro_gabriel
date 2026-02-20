func soma1(x: int); int{ return x + 1; } 
func soma2(x: int); int{ return x + 2; }
func soma3(x: int); int{ return x + 3; }

func main(): int{
    let x: int = 0;
    let ops = ((int) -> int)[3];
    ops[0] = soma1;
    ops[1] = soma2;
    ops[2] = soma3;

    for(int i = 0; i < ops.size; i++){
       x = ops[i](x);
    }

    print(x);

    return 0;
}

