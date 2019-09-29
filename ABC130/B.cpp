#include<cstdio>

int main(void){
    int n;
    int l[101];
    int x;

    scanf("%d %d\n", &n, &x);
    for(int i = 1; i <= n; i++){
        scanf("%d", &l[i]);
    }

    int count;
    int d = 0;
    for(count = 1; d <= x && count <= n + 1; count++){
        d = d + l[count];
    }

    printf("%d\n", count - 1);

    return 0;
}