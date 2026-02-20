struct HashTable {
    keys: int[10];
    values: int[10];
    size: int;
}

func mod(key: int, size: int) : int {
    let r: int = key;

    while (r >= size) {
        r = r - size;
    }

    return r;
}

func hash(key: int, size: int) : int {
    return mod(key, size);
}

func insert(ht: HashTable, key: int, value: int) : void {
    let index: int = hash(key, ht.size);

    while (ht.keys[index] != 0) {
        index = index + 1;

        if (index >= ht.size) {
            index = 0;
        }
    }

    ht.keys[index] = key;
    ht.values[index] = value;
}

func search(ht: HashTable, key: int) : int {
    let index: int = hash(key, ht.size);

    while (ht.keys[index] != 0) {

        if (ht.keys[index] == key) {
            return ht.values[index];
        }

        index = index + 1;

        if (index >= ht.size) {
            index = 0;
        }
    }

    return -1;
}

func main() : int {
    let ht: HashTable;
    ht.size = 10;

    let i: int = 0;
    while (i < 10) {
        ht.keys[i] = 0;
        ht.values[i] = 0;
        i = i + 1;
    }

    insert(ht, 15, 100);
    insert(ht, 25, 200);
    insert(ht, 35, 300);

    print(search(ht, 15));
    print(search(ht, 25));
    print(search(ht, 99));

    return 0;
}