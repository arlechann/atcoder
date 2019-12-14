#include <algorithm>
#include <cstdio>

using namespace std;

typedef unsigned long long ll;

ll gcd(ll a, ll b) {
	if(a < b) {
		ll tmp = a;
		a = b;
		b = tmp;
	}

	ll r = a % b;
	while(r != 0) {
		a = b;
		b = r;
		r = a % b;
	}

	return b;
}

ll lcm(ll a, ll b) {
	return a * b / gcd(a, b);
}

int main(void) {
	ll a, b;
	ll c, d;
	scanf("%llu %llu %llu %llu", &a, &b, &c, &d);

	ll began = a - 1;
	ll end = b;

	ll dc = end / c - began / c;
	ll dd = end / d - began / d;
	ll dlcm = end / lcm(c, d) - began / lcm(c, d);

	// printf("dc:%lld dd:%lld dlcm:%lld\n", dc, dd, dlcm);

	printf("%llu\n", (end - began) - (dc + dd - dlcm));

	return 0;
}