#include <algorithm>
#include <boost/optional.hpp>
#include <climits>
#include <cmath>
#include <cstdio>
#include <cstring>
#include <functional>
#include <iomanip>
#include <iostream>
#include <limits>
#include <list>
#include <map>
#include <numeric>
#include <queue>
#include <set>
#include <sstream>
#include <string>
#include <tuple>
#include <type_traits>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

#define REP(i, n) for(int i = 0, i##_MACRO = (n); i < i##_MACRO; i++)
#define RANGE(i, a, b) for(int i = (a), i##_MACRO = (b); i < i##_MACRO; i++)
#define EACH(e, a) for(auto&& e : a)
#define ALL(a) std::begin(a), std::end(a)
#define RALL(a) std::rbegin(a), std::rend(a)
#define FILL(a, n) memset((a), n, sizeof(a))
#define FILLZ(a) FILL(a, 0)
#define INT(x) (static_cast<int>(x))
#define PRECISION(x) std::fixed << std::setprecision(x)

using namespace std;

using ll = long long;
using VI = vector<int>;
using VI2D = vector<vector<int>>;

constexpr int INF = 2e9;
constexpr double EPS = 1e-10;
constexpr double PI = acos(-1.0);

constexpr int dx[] = {-1, 0, 1, 0};
constexpr int dy[] = {0, -1, 0, 1};

template <typename T>
constexpr int sign(T x) {
	return x < 0 ? -1 : x > 0 ? 1 : 0;
}

template <>
constexpr int sign(double x) {
	return x < -EPS ? -1 : x > EPS ? 1 : 0;
}

template <typename T, typename U>
constexpr void chmax(T& m, U x) {
	m = max(m, x);
}

template <typename T, typename U>
constexpr void chmin(T& m, U x) {
	m = min(m, x);
}

template <typename T>
constexpr T square(T x) {
	return x * x;
}

bool is_connected(VI2D edges) {
	int n = edges.size();
	vector<bool> is_used(n, false);
	queue<int> q;
	q.push(0);
	while(!q.empty()) {
		int node = q.front();
		q.pop();
		if(is_used[node]) {
			continue;
		}
		is_used[node] = true;
		REP(i, n) {
			if(edges[node][i] != 0) {
				q.push(i);
			}
		}
	}
	return all_of(ALL(is_used), [](bool x) { return x; });
}

int main() {
	int n, m;
	cin >> n >> m;
	VI2D edges(n, VI(n, 0));
	REP(i, m) {
		int a, b;
		cin >> a >> b;
		edges[a - 1][b - 1] = edges[b - 1][a - 1] = 1;
	}

	int result = 0;
	REP(i, n) {
		REP(j, i) {
			if(edges[i][j] == 1) {
				VI2D copy_edges = edges;
				copy_edges[i][j] = 0;
				copy_edges[j][i] = 0;
				result += is_connected(copy_edges) ? 0 : 1;
			}
		}
	}
	cout << result << endl;
	return 0;
}
