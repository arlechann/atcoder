#include <algorithm>
#include <cstdio>
#include <utility>

using namespace std;

int main(void) {
	int n;
	pair<int, int> ab[200000];

	scanf("%d", &n);
	for(int i = 0; i < n; i++) {
		scanf("%d %d", &ab[i].first, &ab[i].second);
	}

	sort(&ab[0], &ab[n], [](pair<int, int>& a, pair<int, int>& b) {
		return a.second < b.second;
	});

	int time = 0;
	for(int i = 0; i < n; i++) {
		time += ab[i].first;
		if(time > ab[i].second) {
			printf("No\n");
			return 0;
		}
	}

	printf("Yes\n");

	return 0;
}