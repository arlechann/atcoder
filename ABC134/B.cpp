#include <cstdio>

int main() {
	int n, d;
	scanf("%d %d", &n, &d);
	printf("%d\n", n / (2 * d + 1) + (n % (2 * d + 1) ? 1 : 0));
	return 0;
}