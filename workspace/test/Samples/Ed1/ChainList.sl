struct Node {
    value: int;
    next: Node;
}

struct List {
    head: Node;
}

func insert(l: List, value: int) : void {
    let newNode: Node = new Node { value, l.head };
    l.head = newNode;
}

func printList(l: List) : void {
    let current: Node = l.head;

    while (current != current.next) {
        print(current.value);
        current = current.next;
    }
}

func main() : int {
    let list: List;
    list.head = new Node { 0, new Node { 0, 0 } };

    insert(list, 10);
    insert(list, 20);
    insert(list, 30);

    printList(list);

    return 0;
}