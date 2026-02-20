struct Node {
    value: int;
    left: Node;
    right: Node;
}

struct Tree {
    root: Node;
}

func insertNode(n: Node, value: int) : Node {
    if (n == 0) {
        return new Node { value, 0, 0 };
    }

    if (value < n.value) {
        n.left = insertNode(n.left, value);
    } else {
        n.right = insertNode(n.right, value);
    }

    return n;
}

func insert(t: Tree, value: int) : void {
    t.root = insertNode(t.root, value);
}

func inorder(n: Node) : void {
    if (n != 0) {
        inorder(n.left);
        print(n.value);
        inorder(n.right);
    }
}

func main() : int {
    let tree: Tree;
    tree.root = 0;

    insert(tree, 50);
    insert(tree, 30);
    insert(tree, 70);
    insert(tree, 20);
    insert(tree, 40);
    insert(tree, 60);
    insert(tree, 80);

    inorder(tree.root);

    return 0;
}