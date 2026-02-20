struct Person {
    name : string;
    age : int;
    height : float;
}

func main(): int{
    let Mob: Person[10][10][10][10][10][10][10][10];

    for(int i = 0; i < 10; i++){
        for(int j = 0; j < 10; j++){
            for(int k = 0; k < 10; k++){
                for(int l = 0; l < 10; l++){
                    for(int m = 0; m < 10; m++){
                        for(int o = 0; o < 10; o++){
                            for(int p = 0; p < 10; p++){
                                    Mob[i][j][k][l][m][n][o][p] = Person{"Mark", 21, 1.6};
                            }
                        }   
                    }
                }
            }
        }
    }
    return 1;
}
