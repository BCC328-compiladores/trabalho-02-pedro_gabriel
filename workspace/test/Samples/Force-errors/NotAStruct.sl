// A variável x não é uma estrutura

struct Position {
    x: int; 
    y: int;
}

func main() : int {
    let x : int;
    x.y = 1;
    return x.y + 1;
}