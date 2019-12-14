#include <cstdio>

int main() {
	int a, b;
	scanf("%d%d", &a, &b);
	printf((a - b) % 2 ? "IMPOSSIBLE\n" : "%d\n", (a - b) / 2 + b);
	return 0;
}