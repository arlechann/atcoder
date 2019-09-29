#include<cstdio>
#include<climits>
#include<algorithm>

#define MOD(x) ((x) % 1000000007)

typedef long long ll;

using namespace std;

int n;
int m;
int a;
int dp[100001];


int main(void){
    scanf("%d %d", &n, &m);

    for(int i = 1; i <= m; i++){
        scanf("%d", &a);
        dp[a] = -1;
    }

    dp[0] = 1;
    dp[1] = dp[1] == -1 ? 0 : 1;
    for(int i = 2; i <= n; i++){
        if(dp[i] != -1){
            dp[i] = MOD(dp[i - 1] + dp[i - 2]);
        }else{
            dp[i] = 0;
        }
    }

    printf("%d\n", dp[n]);

    return 0;
}