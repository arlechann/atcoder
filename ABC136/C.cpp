#include <algorithm>
#include <cstdio>

#define REP(i, n) for(int i = 0; i < (n); i++)

bool is_sorted(int a[], int n) {
	REP(i, n - 1) {
		if(a[i] > a[i + 1]) {
			return false;
		}
	}
	return true;
}

int main() {
	int n;
	int h[100000];
	scanf("%d", &n);
	REP(i, n) { scanf("%d", &h[i]); }

	int result;
	REP(i, n - 1) {
		if(i == 0) {
			if(i >= 2) {
				h[i]--;
			}
		}

		if(h[i] <= h[i + 1] - 1) {
			h[i + 1]--;
		}
	}
	printf(is_sorted(h, n) ? "Yes\n" : "No\n");

	return 0;
}