#include <cstdio>
#include <utility>

int main(void) {
	int n, l;
	int aji[200];
	int sum = 0;

	scanf("%d %d", &n, &l);

	for(int i = 0; i < n; i++) {
		aji[i] = l + i;
		sum += aji[i];
	}

	int result;

	if(aji[0] < 0 && aji[n - 1] > 0) {
		result = sum;
	} else if(aji[0] < 0) {
		result = sum - aji[n - 1];
	} else {
		result = sum - aji[0];
	}

	printf("%d\n", result);
	return 0;
}