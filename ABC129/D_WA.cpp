#include<cstdio>
#include<algorithm>

using namespace std;

int h, w;
int s[2002][2002][3];

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

    int result = 0;

    for(int i = 1; i <= min(h, w); i++){
        if(s[i][i][1] == 0){
            int x_cnt = 0;
            for(int x = i - 1; s[x][i][0] != -1; x--){
                x_cnt++;
            }

            for(int x = i + 1; s[x][i][0] != -1; x++){
                x_cnt++;
            }

            for(int x = i - 1; s[x][i][0] != -1; x--){
                s[x][i][1] = x_cnt;
            }

            for(int x = i + 1; s[x][i][0] != -1; x++){
                s[x][i][1] = x_cnt;
            }

            int y_cnt = 0;
            for(int y = i + 1; s[i][y][0] != -1; y++){
                y_cnt++;
            }

            for(int y = i - 1; s[i][y][0] != -1; y--){
                y_cnt++;
            }

            for(int y = i - 1; s[i][y][0] != -1; y--){
                s[i][y][2] = y_cnt;
            }

            for(int y = i + 1; s[i][y][0] != -1; y++){
                s[i][y][2] = y_cnt;
            }
        }
    }

    for(int i = 1; i <= h; i++){
        for(int j = 1; j <= w; j++){
            if(i == j){
                continue;
            }

            if(s[j][i][0] != -1){
                if(s[j][i][1] == 0){
                    int x_cnt = 0;
                    for(int x = j - 1; s[x][i][0] != -1; x--){
                        s[j][i][1]++;
                    }

                    for(int x = j + 1; s[x][i][0] != -1; x++){
                        s[j][i][1]++;
                    }
                }

                if(s[j][i][2] == 0){
                    for(int y = i + 1; s[j][y][0] != -1; y++){
                        s[j][i][2]++;
                    }

                    for(int y = i - 1; s[j][y][0] != -1; y--){
                        s[j][i][2]++;
                    }
                }

                result = max(s[j][i][1] + s[j][i][2] + 1, result);
            }
        }
    }

    printf("%d\n", result);

    return 0;
}