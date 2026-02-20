struct Node {
    value: int;
    left: int;   
    right: int;
}

struct Arena {
    memory: Node[100];  
    next_free: int[1];  
}

func allocNode(arena: Arena, val: int) : int {
    let idx: int = arena.next_free[0];
    
    arena.next_free[0] = idx + 1;
    
    arena.memory[idx] = Node { val, 0, 0 };
    
    return idx;
}

func insertNode(arena: Arena, curr: int, val: int) : int {
    if (curr == 0) {
        return allocNode(arena, val);
    }

    if (val < arena.memory[curr].value) {
        arena.memory[curr].left = insertNode(arena, arena.memory[curr].left, val);
    } else {
        arena.memory[curr].right = insertNode(arena, arena.memory[curr].right, val);
    }

    return curr;
}

func inorder(arena: Arena, curr: int) : void {
    if (curr != 0) {
        inorder(arena, arena.memory[curr].left);
        print(arena.memory[curr].value);
        inorder(arena, arena.memory[curr].right);
    }
}

func main() : int {
    let memory: Node[100];
    let next_free: int[1];
    next_free[0] = 1; 
    
    let arena: Arena = Arena { memory, next_free };
    
    let root: int = 0;

    root = insertNode(arena, root, 50);
    root = insertNode(arena, root, 30);
    root = insertNode(arena, root, 70);
    root = insertNode(arena, root, 20);
    root = insertNode(arena, root, 40);
    root = insertNode(arena, root, 60);
    root = insertNode(arena, root, 80);

    inorder(arena, root);

    return 0;
}
