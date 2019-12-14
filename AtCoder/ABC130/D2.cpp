#include<cstdio>
#include<cstdlib>
 
typedef long long ll;
 
ll s[100002];
 
int main(void){
    int n;
    ll k;
    ll a[100001];
 
    scanf("%d %lld", &n, &k);
    for(int i = 1; i <= n; i++){
        scanf("%lld", &a[i]);
    }
 
    for(int i = 1; i <= n; i++){
        s[i] = s[i - 1] + a[i];
    }
 
    int left = 0;
    int right = 1;
    ll num = 0;
    ll p = 0;
 
    for(int i = 1; i <= n; i++){
        p += i;
    }
 
    while(true){
        if(left == n){
            break;
        }
 
        if(left == right){
            right++;
            continue;
        }
 
        if(right != n){
            while(s[right] - s[left] < k && right <= n){
                right++;
            }
            right--;
            num += right - left;
            left++;
        }else{
            if(s[right] - s[left] < k){
                num += right - left;
            }
            left++;
        }
    }
 
    printf("%lld\n", p - num);
 
    return 0;
}