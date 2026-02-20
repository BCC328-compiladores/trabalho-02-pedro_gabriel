func partition(arr: int[], low: int, high: int) : int {
    let pivot: int = arr[high];
    let i: int = low - 1;
    let j: int = low;

    while (j < high) {
        if (arr[j] < pivot) {
            i = i + 1;

            let temp: int = arr[i];
            arr[i] = arr[j];
            arr[j] = temp;
        }
        j = j + 1;
    }

    let temp: int = arr[i + 1];
    arr[i + 1] = arr[high];
    arr[high] = temp;

    return i + 1;
}

func quickSort(arr: int[], low: int, high: int) : void {
    if (low < high) {
        let pi: int = partition(arr, low, high);

        quickSort(arr, low, pi - 1);
        quickSort(arr, pi + 1, high);
    }
}

func main() : int {
    let arr: int[6];

    arr[0] = 10;
    arr[1] = 7;
    arr[2] = 8;
    arr[3] = 9;
    arr[4] = 1;
    arr[5] = 5;

    quickSort(arr, 0, 5);

    let i: int = 0;
    while (i < 6) {
        print(arr[i]);
        i = i + 1;
    }

    return 0;
}