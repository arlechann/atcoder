#include<cstdio>

int main(void){
    int w, h, x, y;

    scanf("%d %d %d %d", &w, &h, &x, &y);

    printf("%lf %d\n", (double)w * (double)h / 2.0, (w == x * 2 && h == y * 2) ? 1 : 0);

    return 0;
}