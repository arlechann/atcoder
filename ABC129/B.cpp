#include<cstdio>
#include<climits>
#include<algorithm>

using namespace std;

int n;
int w[101];

int main(void){
    scanf("%d", &n);

    for(int i = 1; i <= n; i++){
        scanf("%d", &w[i]);
    }

    int m = INT_MAX;
    int s1, s2;

    for(int t = 1; t < n; t++){
        s1 = 0;
        for(int i = 1; i <= t; i++){
            s1 += w[i];
        }
        
        s2 = 0;
        for(int i = t + 1; i <= n; i++){
            s2 += w[i];
        }

        m = min(m, abs(s1 - s2));
    }

    printf("%d\n", m);

    return 0;
}