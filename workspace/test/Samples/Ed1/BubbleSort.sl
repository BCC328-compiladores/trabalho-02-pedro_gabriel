func bubbleSort(arr: int[], n: int) : void {
    let i: int = 0;

    while (i < n - 1) {
        let j: int = 0;

        while (j < n - i - 1) {
            if (arr[j] > arr[j + 1]) {
                let temp: int = arr[j];
                arr[j] = arr[j + 1];
                arr[j + 1] = temp;
            }
            j = j + 1;
        }

        i = i + 1;
    }
}

func main() : int {
    let arr: int[6];

    arr[0] = 64;
    arr[1] = 34;
    arr[2] = 25;
    arr[3] = 12;
    arr[4] = 22;
    arr[5] = 11;

    bubbleSort(arr, 6);

    let i: int = 0;
    while (i < 6) {
        print(arr[i]);
        i = i + 1;
    }

    return 0;
}