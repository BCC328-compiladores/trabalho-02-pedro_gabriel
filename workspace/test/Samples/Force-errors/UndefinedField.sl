// Campo z não existe dentro da Estrutura Position

struct Position {
    x: int; 
    y: int;
}

func main() : int {
    let p : Position;

    p.z = 10;
    
    return 0;
}