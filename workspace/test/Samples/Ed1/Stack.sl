struct Stack {
    data: int[10];
    top: int;
}

func isEmpty(s: Stack) : bool {
    return s.top == -1;
}

func push(s: Stack, value: int) : void {
    if (s.top < 9) {
        s.top = s.top + 1;
        s.data[s.top] = value;
    } else {
        print(0);
    }
}

func pop(s: Stack) : int {
    if (s.top >= 0) {
        let value: int = s.data[s.top];
        s.top = s.top - 1;
        return value;
    } else {
        print(-1);
        return -1;
    }
}

func main() : int {
    let s: Stack;
    s.top = -1;

    push(s, 10);
    push(s, 20);
    push(s, 30);

    while (!isEmpty(s)) {
        let v: int = pop(s);
        print(v);
    }

    return 0;
}