#include<cstdio>

int main(void){
    int x, a;
    scanf("%d %d", &x, &a);
    printf("%d\n", x < a ? 0 : 10);
    return 0;
}