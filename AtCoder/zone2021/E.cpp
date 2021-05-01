#include <algorithm>
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
using VI = std::vector<int>;
using VI2D = std::vector<vector<int>>;
using VLL = std::vector<long long>;
using VLL2D = std::vector<vector<long long>>;

constexpr int INF = 2e9;
constexpr long long INFLL = 2e18;
constexpr double EPS = 1e-10;
constexpr double PI = acos(-1.0);

constexpr int dx[] = {-1, 1, 0};
constexpr int dy[] = {0, 0, 1};

template <typename T, std::size_t N>
struct make_vector_type {
	using type =
		typename std::vector<typename make_vector_type<T, (N - 1)>::type>;
};

template <typename T>
struct make_vector_type<T, 0> {
	using type = typename std::vector<T>;
};

template <typename T, size_t N>
auto make_vector_impl(const std::vector<std::size_t>& ls, T init_value) {
	if constexpr(N == 0) {
		return std::vector<T>(ls[N], init_value);
	} else {
		return typename make_vector_type<T, N>::type(
			ls[N], make_vector_impl<T, (N - 1)>(ls, init_value));
	}
}

template <typename T, std::size_t N>
auto make_vector(const std::size_t (&ls)[N], T init_value) {
	std::vector<std::size_t> dimensions(N);
	for(int i = 0; i < N; i++) {
		dimensions[N - i - 1] = ls[i];
	}
	return make_vector_impl<T, N - 1>(dimensions, init_value);
}

template <typename T>
std::vector<T> make_vector(std::size_t size, T init_value) {
	return std::vector<T>(size, init_value);
}

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
	m = max<T>(m, x);
}

template <typename T, typename U>
constexpr void chmin(T& m, U x) {
	m = min<T>(m, x);
}

template <typename T>
constexpr T square(T x) {
	return x * x;
}

template <typename T>
constexpr T diff(T a, T b) {
	return abs(a - b);
}

struct Edge {
	int ty;
	int tx;
	int cost;
	Edge(int ty, int tx, int c) : ty(ty), tx(tx), cost(c) {}
	bool operator<(const Edge& rhs) const { return this->cost > rhs.cost; }
};

Edge dijkstra(const VI2D& a, const VI2D& b, int sy, int sx, int gy, int gx) {
	int h = a.size();
	int w = b[0].size();
	vector<vector<bool>> used(h, vector<bool>(w, false));
	VI2D distances(h, VI(w, INF));

	distances[sy][sx] = 0;
	priority_queue<Edge> pq;
	pq.push(Edge(sy, sx, 0));
	while(!pq.empty()) {
		Edge e = pq.top();
		pq.pop();
		if(used[e.ty][e.tx]) {
			continue;
		}
		if(e.ty == gy && e.tx == gx) {
			return e;
		}
		used[e.ty][e.tx] = true;
		REP(i, 3) {
			int ny = e.ty + dy[i];
			int nx = e.tx + dx[i];
			if(ny < 0 || h <= ny || nx < 0 || w <= nx) {
				continue;
			}
			int alt = e.cost;
			if(dy[i] == 1) {
				alt += b[e.ty][e.tx];
			} else if(dx[i] == -1) {
				alt += a[e.ty][e.tx - 1];
			} else {
				alt += a[e.ty][e.tx];
			}
			if(alt < distances[ny][nx]) {
				distances[ny][nx] = alt;
				pq.push(Edge(ny, nx, alt));
			}
		}
		for(int i = 1; e.ty - i >= 0; i++) {
			int ny = e.ty - i;
			int nx = e.tx;
			int alt = e.cost + i + 1;
			if(alt < distances[ny][nx]) {
				distances[ny][nx] = alt;
				pq.push(Edge(ny, nx, alt));
			} else if(alt > distances[ny][nx]) {
				break;
			}
		}
	}
	return Edge(-1, -1, INF);
}

int main() {
	int r, c;
	cin >> r >> c;
	VI2D a(r, VI(c - 1)), b(r - 1, VI(c));
	REP(i, r) {
		REP(j, c - 1) { cin >> a[i][j]; }
	}
	REP(i, r - 1) {
		REP(j, c) { cin >> b[i][j]; }
	}

	auto edge = dijkstra(a, b, 0, 0, r - 1, c - 1);
	cout << edge.cost << endl;
	return 0;
}
