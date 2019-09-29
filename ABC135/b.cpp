#include <cstdio>

bool is_sorted(int p[], int n) {
	for(int i = 0; i < n - 1; i++) {
		if(p[i] > p[i + 1]) {
			return false;
		}
	}
	return true;
}

void swap(int* a, int* b) {
	int tmp;
	tmp = *a;
	*a = *b;
	*b = tmp;
}

int main() {
	int n;
	int p[50];
	scanf("%d", &n);
	for(int i = 0; i < n; i++) {
		scanf("%d", &p[i]);
	}

	int result = 0;
	for(int i = 0; i < n - 1; i++) {
		for(int j = i; j < n; j++) {
			swap(&p[i], &p[j]);
			result += is_sorted(p, n) ? 1 : 0;
			swap(&p[i], &p[j]);
		}
	}

	puts(result > 0 ? "YES" : "NO");

	return 0;
}