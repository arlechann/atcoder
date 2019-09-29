#include <cstdio>

int main() {
	int n, a, b;
	scanf("%d %d %d", &n, &a, &b);
	printf("%d\n", a * n < b ? a * n : b);
	return 0;
}