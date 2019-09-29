#include <cstdio>

#define MOD(x) (x % 1000000007)

int p(int n, int r) {
	if(n - r == 0) {
		return 1;
	}
}

int main(void) {
	int n, k;
	scanf("%d %d", &n, &k);

	int result[2000];
	for(int i = 0; i < k; i++) {
		printf("%d\n", result[i]);
	}

	return 0;
}