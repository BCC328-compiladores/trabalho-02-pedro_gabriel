// Função dentro do campo de uma estrutura

struct test {
     f: (int, int) -> int; 
}

func soma (a: int, b: int) : int { return a + b; }

func main(): int {
     let t: test = test{soma}
     return t.f(1, 2);
}