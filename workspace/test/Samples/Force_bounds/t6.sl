forall a b . func map(f: (a) -> b, v: a[], size: int) : b[] {
    let result: b[] = new b[size];
    let i: int = 0;

    while (i < size) {
        result[i] = f(v[i]);
        i = i + 1;
    }

    return result;
}

forall a . func filter(pred: (a) -> bool, v: a[], size: int) : a[] {
    let result: a[] = new a[size];
    let i: int = 0;
    let j: int = 0;

    while (i < size) {
        if (pred(v[i])) {
            result[j] = v[i];
            j = j + 1;
        }
        i = i + 1;
    }

    return result;
}

func toString(a: int) : string {
    return "Numero: " + a;
}

func isEven(a: int) : bool {
    return (a / 2) * 2 == a;
}

func main() : int {
    let size: int = 5;

    let vetor: int[] = new int[size];

    vetor[0] = 1;
    vetor[1] = 2;
    vetor[2] = 3;
    vetor[3] = 4;
    vetor[4] = 5;

    // filtra apenas pares
    let pares: int[] = filter(isEven, vetor, size);

    // converte para string
    let textos: string[] = map(toString, pares, size);

    let i: int = 0;

    while (i < size) {
        print(textos[i]);
        i = i + 1;
    }

    return 0;
}