#include <algorithm>
#include <cstdio>

int main() {
	int a, b, c;
	scanf("%d%d%d", &a, &b, &c);
	printf("%d\n", std::max(c - (a - b), 0));
	return 0;
}