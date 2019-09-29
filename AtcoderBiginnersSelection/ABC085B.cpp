#include<cstdio>

int n;
int num_of_d[101];

int main(void){
    int d;
    int result = 0;

    scanf("%d\n", &n);
    for(int i = 0; i < n; i++){
        scanf("%d", &d);
        num_of_d[d]++;
    }

    for(int i = 1; i <= 100; i++){
        if(num_of_d[i]){
            result++;
        }
    }

    printf("%d\n", result);
}