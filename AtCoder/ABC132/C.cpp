#include <algorithm>
#include <cstdio>
#include <utility>

using namespace std;

int main(void) {
	int n;
	int d[100001];

	scanf("%d", &n);
	for(int i = 0; i < n; i++) {
		scanf("%d", &d[i]);
	}

	sort(d, d + n);

	// for(int i = 0; i < n; i++) {
	// 	printf("%d ", d[i]);
	// }
	// putchar('\n');

	int result = 0;
	if(d[n / 2 - 1] == d[n / 2]) {
		result = 0;
	} else {
		result = d[n / 2] - d[n / 2 - 1];
	}

	printf("%d\n", result);

	return 0;
}