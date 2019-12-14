#include<cstdio>

int main(void){
    int result = 0;
    char s[4];

    scanf("%s", s);
    for(int i = 0; i < 3; i++){
        result += s[i] == '1' ? 1 : 0;
    }
    printf("%d\n", result);

    return 0;
}