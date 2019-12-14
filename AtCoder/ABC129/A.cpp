#include <algorithm>
#include <cstdio>

using namespace std;

int main(void) {
	int p, q, r;

	scanf("%d %d %d", &p, &q, &r);

	printf("%d\n", p + q + r - max(p, max(q, r)));

	return 0;
}