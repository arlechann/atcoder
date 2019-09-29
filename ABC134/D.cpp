#include <cstdio>

int main() {
	int n;
	int a[200001];
	scanf("%d", &n);
	for(int i = 1; i <= n; i++) {
		scanf("%d", &a[i]);
	}

	int result[200001];
	for(int i = n; i > 0; i--) {
		result[i] = 0;
		for(int j = 2; i * j <= n; j++) {
			result[i] += result[i * j];
		}
		result[i] = result[i] % 2 == a[i] ? 0 : 1;
	}

	int m = 0;
	for(int i = 1; i <= n; i++) {
		m += result[i];
	}

	printf("%d\n", m);
	for(int i = 1; i <= n; i++) {
		if(result[i]) {
			printf("%d ", i);
		}
	}
	putchar('\n');

	return 0;
}