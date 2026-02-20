func id(x) {
    return x;
}

func sum(a:int , b:int ) : int{
    return a + b;
}

func main() {
    let f = id(sum);
    print(f(2, 3));
}
