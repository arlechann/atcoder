#include <cstdio>

int main(void) {
	int n;
	int p[20];

	scanf("%d", &n);
	for(int i = 0; i < n; i++) {
		scanf("%d", &p[i]);
	}

	int result = 0;
	for(int i = 1; i < n - 1; i++) {
		if((p[i - 1] < p[i] && p[i] < p[i + 1]) ||
		   (p[i + 1] < p[i] && p[i] < p[i - 1])) {
			result++;
		}
	}

	printf("%d\n", result);

	return 0;
}