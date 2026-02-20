// Campo z não existe dentro da Estrutura Position

struct Position {
    x: Int; 
    y: Int;
}

func main() : int {
    let p : Position;

    p.z = 10;
    
    return 0;
}