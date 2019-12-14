#include<cstdio>

int main(void){
    int n;
    int a[200];
    int result = 0;

    scanf("%d", &n);
    for(int i = 0; i < n; i++){
        scanf("%d", a + i);
    }

    while(true){
        for(int i = 0; i < n; i++){
            if(a[i] % 2 == 0){
                a[i] /= 2;
            }else{
                goto exitloop;
            }
        }
        result++;
    }
    exitloop:

    printf("%d\n", result);

    return 0;
}