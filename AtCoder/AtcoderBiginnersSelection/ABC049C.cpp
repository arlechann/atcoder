#include<cstdio>
#include<cstring>

char s[100001];

bool solve(int i);

int main(void){
    scanf("%s", s);

    printf("%s\n", solve(0) ? "YES" : "NO");

    return 0;
}

bool solve(int i){
    bool result = false;
    if(s[i] == '\0'){
        return true;
    }

    //printf("i:%d s:%s\n", i, s + i);

    int len = strlen(s + i);

    if(strncmp(s + i, "dream", 5) == 0){
        result |= solve(i + 5);
    }

    if(strncmp(s + i, "erase", 5) == 0){
        result |= solve(i + 5);
    }

    if(strncmp(s + i, "eraser", 6) == 0){
        result |= solve(i + 6);
    }

    if(strncmp(s + i, "dreamer", 7) == 0){
        result |= solve(i + 7);
    }

    return result;
}