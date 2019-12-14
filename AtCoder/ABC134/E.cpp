#include <algorithm>
#include <cstdio>
#include <set>

int main() {
	int n;
	int a[100000];
	scanf("%d", &n);
	for(int i = 0; i < n; i++) {
		scanf("%d", &a[i]);
	}

	std::multiset<int> color;
	color.insert(a[0]);
	for(int i = 1; i < n; i++) {
		auto ittr = color.lower_bound(a[i]);
		if(ittr != color.begin()) {
			ittr--;
			color.erase(ittr);
		}
		color.insert(a[i]);
	}

	printf("%d\n", color.size());

	return 0;
}