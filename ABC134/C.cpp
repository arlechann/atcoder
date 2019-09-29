#include <cstdio>

int main() {
	int n;
	int a[200000];

	scanf("%d", &n);
	for(int i = 0; i < n; i++) {
		scanf("%d", &a[i]);
	}

	int max1, max2;
	for(int i = 0; i < n; i++) {
		if(a[i] > max1) {
			max2 = max1;
			max1 = a[i];

		} else if(a[i] > max2) {
			max2 = a[i];
		}
	}

	int num_m1 = 0;
	for(int i = 0; i < n; i++) {
		if(a[i] == max1) {
			num_m1++;
		}
	}

	if(num_m1 > 1) {
		for(int i = 0; i < n; i++) {
			printf("%d\n", max1);
		}
	} else {
		for(int i = 0; i < n; i++) {
			printf("%d\n", a[i] == max1 ? max2 : max1);
		}
	}
	return 0;
}