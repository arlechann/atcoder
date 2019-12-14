#include<cstdio>
#include<cstdlib>
 
typedef long long ll;
 
int main(void){
    int n;
    ll k;
    ll a[100001];
 
    scanf("%d %lld", &n, &k);
    for(int i = 0; i < n; i++){
        scanf("%lld", &a[i]);
    }
 
    ll sum = 0;
    ll result = 0;
    int right = 0;
    for(int left = 0; left < n; left++){
        while(sum < k){
            if(right == n){
                break;
            }

            sum += a[right];
            right++;
        }
 
        if(sum < k){
            break;
        }

        result += n - right + 1;
        sum -= a[left];
    }
 
    printf("%lld\n", result);
 
    return 0;
}