struct Queue {
    data: int[10];
    front: int;
    rear: int;
}

func isEmpty(q: Queue) : bool {
    return q.front > q.rear;
}

func isFull(q: Queue) : bool {
    return q.rear == 9;
}

func ins_queue(q: Queue, value: int) : void {
    if (!isFull(q)) {
        q.rear = q.rear + 1;
        q.data[q.rear] = value;
    } else {
        print(-1); // erro: fila cheia
    }
}

func rm_queue(q: Queue) : int {
    if (!isEmpty(q)) {
        let value: int = q.data[q.front];
        q.front = q.front + 1;
        return value;
    } else {
        print(-1); // erro: fila vazia
        return -1;
    }
}

func main() : int {
    let q: Queue;
    q.front = 0;
    q.rear = -1;

    ins_queue(q, 10);
    ins_queue(q, 20);
    ins_queue(q, 30);

    while (!isEmpty(q)) {
        let v: int = rm_queue(q);
        print(v);
    }

    return 0;
}