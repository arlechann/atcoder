#include <cstdio>

typedef unsigned long long ull;

int main() {
	int n;
	int a[100001];
	int b[100001];

	scanf("%d", &n);
	for(int i = 0; i <= n; i++) {
		scanf("%d", &a[i]);
	}
	for(int i = 0; i < n; i++) {
		scanf("%d", &b[i]);
	}

	ull result = 0;
	for(int i = 0; i < n; i++) {
		if(a[i] >= b[i]) {
			result += b[i];
			continue;
		}

		if(a[i] + a[i + 1] >= b[i]) {
			result += b[i];
			b[i] -= a[i];
			a[i + 1] -= b[i];
		} else {
			result += a[i] + a[i + 1];
			b[i] -= a[i];
			a[i + 1] = 0;
		}
	}

	printf("%llu\n", result);

	return 0;
}