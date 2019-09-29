#include <cstdio>

int f(int y, int z) {
	return (y - z) * (y - z);
}

int main() {
	int n, d;
	int x[10][10];

	scanf("%d %d", &n, &d);
	for(int i = 0; i < n; i++) {
		for(int j = 0; j < d; j++) {
			scanf("%d", &x[i][j]);
		}
	}

	int result = 0;
	for(int i = 0; i < n - 1; i++) {
		for(int j = i + 1; j < n; j++) {
			int sum = 0;
			for(int k = 0; k < d; k++) {
				sum += f(x[i][k], x[j][k]);
			}

			for(int k = 0; k * k < 40 * 40 * 10 + 1; k++) {
				if(k * k == sum) {
					result++;
					break;
				}
			}
		}
	}

	printf("%d\n", result);

	return 0;
}