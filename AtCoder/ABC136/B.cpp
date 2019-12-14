#include <algorithm>
#include <cstdio>

bool keta_odd(int n) {
	int keta = 0;
	for(int i = 1; i <= n; i *= 10) {
		keta++;
	}
	return keta % 2;
}

int main() {
	int n;
	scanf("%d", &n);

	int result = 0;

	for(int i = 1; i <= n; i++) {
		result += keta_odd(i) ? 1 : 0;
	}

	printf("%d\n", result);

	return 0;
}