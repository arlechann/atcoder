#include <algorithm>
#include <cstdio>
#include <utility>
#include <vector>

using namespace std;

#define REP(i, n) for(int i = 0; i < (n); i++)

int main() {
	int n;
	vector<pair<int, int>> points;
	vector<pair<int, int>> x_sort;
	vector<pair<int, int>> y_sort;

	int x, y;
	scanf("%d", &n);
	REP(i, n) {
		scanf("%d%d", &x, &y);
		points.push_back(pair(x, y));
		x_sort.push_back(pair(x, y));
		y_sort.push_back(pair(y, x));
	}

	sort(x_sort);
	sort(y_sort);

	return 0;
}