#include<cstdio>

int main(void){
    int a, b;

    scanf("%d %d", &a, &b);
    printf("%s\n", a * b % 2 ? "Odd" : "Even");

    return 0;
}