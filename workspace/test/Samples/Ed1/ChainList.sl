struct Node {
    value: int;
    next: int; 
}

struct Arena {
    memory: Node[100]; 
    next_free: int[1]; 
}

struct List {
    head: int; 
}

func allocNode(arena: Arena, val: int, nextNode: int) : int {
    let idx: int = arena.next_free[0];
    
    arena.next_free[0] = idx + 1;
    
    arena.memory[idx] = Node { val, nextNode };
    
    return idx;
}

func insert(arena: Arena, l: List, val: int) : void {
    let newNodeIdx: int = allocNode(arena, val, l.head);
    l.head = newNodeIdx;
}

func printList(arena: Arena, l: List) : void {
    let curr: int = l.head;
    
    while (curr != 0) {
        print(arena.memory[curr].value);
        curr = arena.memory[curr].next;
    }
}

func main() : int {
    let memory: Node[100];
    let next_free: int[1];
    next_free[0] = 1; 
    let arena: Arena = Arena { memory, next_free };
    
    let list: List = List { 0 };

    insert(arena, list, 10);
    insert(arena, list, 20);
    insert(arena, list, 30);

    printList(arena, list);

    return 0;
}
