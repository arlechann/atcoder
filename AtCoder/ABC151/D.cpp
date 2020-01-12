#include <algorithm>
#include <boost/optional.hpp>
#include <climits>
#include <cmath>
#include <cstdio>
#include <cstring>
#include <functional>
#include <iostream>
#include <limits>
#include <list>
#include <numeric>
#include <queue>
#include <sstream>
#include <string>
#include <tuple>
#include <type_traits>
#include <utility>
#include <vector>

#define REP(i, n) for(int i = 0, i##_MACRO = (n); i < i##_MACRO; i++)
#define RANGE(i, a, b) for(int i = (a), i##_MACRO = (b); i < i##_MACRO; i++)
#define EACH(e, a) for(auto&& e : a)
#define ALL(a) (a).begin(), (a).end()
#define AALL(a, n) (a), ((a) + (n))
#define FILL(a, n) memset((a), n, sizeof(a))
#define FILLZ(a) FILL(a, 0)
#define MODNUM (static_cast<int>(1e9 + 7))
#define MOD(x) ((x) % MODNUM)

using namespace std;

using ll = long long;
using VI = vector<int>;
using VI2D = vector<vector<int>>;

const int INF = 2e9;
const double EPS = 1e-10;
const double PI = acos(-1.0);

const int dx[] = {-1, 0, 1, 0};
const int dy[] = {0, -1, 0, 1};

template <typename T>
int sign(T x) {
	return x < 0 ? -1 : x > 0 ? 1 : 0;
}

template <>
int sign(double x) {
	return x < -EPS ? -1 : x > EPS ? 1 : 0;
}

template <typename T>
T square(T x) {
	return x * x;
}

inline int toInt(string s) {
	int v;
	istringstream sin(s);
	sin >> v;
	return v;
}

void chmax(int& m, int n) {
	m = max(m, n);
}

int bfs(const VI2D& maze, int h, int w, pair<int, int> s) {
	VI2D distances(h, VI(w, INF));
	distances[s.first][s.second] = 0;
	int max_distance = 0;
	queue<pair<int, int>> que;
	que.push(s);
	while(!que.empty()) {
		auto p = que.front();
		que.pop();
		if(maze[p.first][p.second]) {
			continue;
		};
		int d = distances[p.first][p.second];
		chmax(max_distance, d);
		REP(i, 4) {
			if(distances[p.first + dy[i]][p.second + dx[i]] == INF) {
				distances[p.first + dy[i]][p.second + dx[i]] = d + 1;
				que.push(pair<int, int>(p.first + dy[i], p.second + dx[i]));
			}
		}
	}
	return max_distance;
}

int main() {
	int h, w;
	scanf("%d %d\n", &h, &w);
	VI2D maze(h + 2, VI(w + 2, 1));
	REP(i, h) {
		REP(j, w) {
			char c = getchar();
			if(c == '.') {
				maze[i + 1][j + 1] = 0;
			}
		}
		getchar();
	}

	int result = 0;
	REP(i, h) {
		REP(j, w) {
			chmax(result,
				  bfs(maze, h + 2, w + 2, pair<int, int>(i + 1, j + 1)));
		}
	}
	printf("%d\n", result);
	return 0;
}