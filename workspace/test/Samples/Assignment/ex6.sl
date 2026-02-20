forall a b . func map (f : (a) -> b, v : a[]) : b[] {
    let result = new b[v.size];
    for (let i = 0; i < v.size ; i++) {
        result[i] = f(v[i]);
    }
    return result;
}

func div(a:int): string {
    return "Numero: " +     a;
}

func main(){
    let vetor = [1, 2, 3, 4, 5];
    let float_vetor = map(div, vetor);
    for(let i = 0; i < float_vetor.size; i++){
        print(float_vetor[i]);
    }
}
