#include<cstdio>
#include<algorithm>

using namespace std;

int h, w;
int s[2002][2002][5];

int main(void){
    scanf("%d %d", &h, &w);
    

    char c[2002];

    for(int i = 1; i <= h; i++){
        scanf("%s", c + 1);
        for(int j = 1; j <= w; j++){
            s[j][i][0] = c[j] == '#' ? -1 : 0;
        }
    }

    for(int j = 0; j <= w + 1; j++){
        s[j][0][0] = -1;
        s[j][h + 1][0] = -1;
    }

    for(int i = 1 ; i <= h; i++){
        s[0][i][0] = -1;
        s[w + 1][i][0] = -1;
    }

    for(int i = 1; i <= h; i++){
        for(int j = 1; j <= w; j++){
            if(s[j][i][0] == -1){
                continue;
            }

            s[j][i][1] = s[j - 1][i][1] + 1;
            s[j][i][2] = s[j][i - 1][2] + 1;
        }
    }

    for(int i = h; i > 0; i--){
        for(int j = w; j > 0; j--){
            if(s[j][i][0] == -1){
                continue;
            }

            s[j][i][3] = s[j + 1][i][3] + 1;
            s[j][i][4] = s[j][i + 1][4] + 1;
        }
    }

    int result = 0;

    for(int i = h; i > 0; i--){
        for(int j = w; j > 0; j--){
            if(s[j][i][0] == -1){
                continue;
            }

            result = max(s[j][i][1] + s[j][i][2] + s[j][i][3] + s[j][i][4] - 3, result);
        }
    }

    printf("%d\n", result);

    return 0;
}