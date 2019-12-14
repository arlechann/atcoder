#include <algorithm>
#include <cstdio>

#define MOD(x) (x % 2019)

typedef long long ll;
using namespace std;

int main() {
	ll l, r;

	scanf("%lld %lld", &l, &r);

	r = min(l + 2020, r);

	int result = 2019;
	for(ll i = l; i < r; i++) {
		for(ll j = i + 1; j <= r; j++) {
			result = min(MOD(MOD(i) * MOD(j)), (ll)result);
		}
	}

	printf("%d\n", result);

	return 0;
}