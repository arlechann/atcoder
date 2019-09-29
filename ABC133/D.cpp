#include <cstdio>

int main() {
	int n;
	int a[100000];
	int edge[100000];

	int sum = 0;
	scanf("%d", &n);
	for(int i = 0; i < n; i++) {
		scanf("%d", &a[i]);
		sum += a[i];
	}

	int tmp = 0;
	for(int i = 0; i < n; i++) {
		tmp += i % 2 ? -a[i] : a[i];
	}
	edge[0] = tmp / 2;

	for(int j = 1; j < n; j++) {
		edge[j] = a[j - 1] - edge[j - 1];
	}

	for(int i = 0; i < n - 1; i++) {
		printf("%d ", edge[i] * 2);
	}
	printf("%d\n", edge[n - 1] * 2);

	return 0;
}