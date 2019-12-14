#include<cstdio>

void solve(int n, int y);

int main(void){
    int n, y;

    scanf("%d %d", &n, &y);

    solve(n, y);

    return 0;
}

void solve(int n, int y){
    y /= 1000;
    for(int i = 0; i <= n; i++){
        for(int j = 0; j <= n - i; j++){
            if(i * 10 + j * 5 + (n - i - j) == y){
                printf("%d %d %d\n", i, j, n - i - j);
                return;
            }
        }
    }

    printf("-1 -1 -1\n");
}