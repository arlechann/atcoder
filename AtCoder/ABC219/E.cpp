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
#define RREP(i, n) for(int i = (n)-1; i >= 0; i--)
#define RANGE(i, a, b) for(int i = (a), i##_MACRO = (b); i < i##_MACRO; i++)
#define RRANGE(i, a, b) for(int i = (b)-1, i##_MACRO = (a); i >= i##_MACRO; i--)
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

constexpr int INF = 1e9;
constexpr long long INFLL = 2e18;
constexpr double EPS = 1e-10;
constexpr double PI = acos(-1.0);

constexpr int dx[] = {-1, 0, 1, 0};
constexpr int dy[] = {0, -1, 0, 1};

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
constexpr bool chmax(T& m, U x) {
	m = max<T>(m, x);
	return m < x;
}

template <typename T, typename U>
constexpr bool chmin(T& m, U x) {
	m = min<T>(m, x);
	return m > x;
}

template <typename T>
constexpr T square(T x) {
	return x * x;
}

template <typename T>
constexpr T pow(T a, int n) {
	T ret = 1;
	while(n != 0) {
		if(n % 2) {
			ret *= a;
		}
		a *= a;
		n /= 2;
	}
	return ret;
}

template <typename T>
constexpr T diff(T a, T b) {
	return abs(a - b);
}

using Weight = int;

// 辺
struct Edge {
	size_t from;
	size_t to;
	Weight cost;
	Edge(size_t t, Weight c) : to(t), cost(c) {}
	Edge(size_t f, size_t t, Weight c) : from(f), to(t), cost(c) {}
	bool operator<(const Edge& rhs) const { return this->cost > rhs.cost; }
};

// グラフ G=(V,E)
struct Graph {
	size_t node;
	std::vector<std::vector<Edge>> edges;

	Graph(size_t n) : node(n), edges(n) {}
	void link(size_t from, size_t to, Weight cost = 1) {
		edges[from].push_back(Edge(from, to, cost));
	}
};

// 幅優先探索(重み無しグラフ)
std::vector<Weight> bfs(const Graph& graph, const size_t s) {
	size_t n = graph.node;
	std::vector<bool> used(n, false);
	std::vector<Weight> distances(n, INF);

	distances[s] = 0;
	used[s] = true;
	std::queue<Edge> q;
	q.push(Edge(s, s, 0));
	while(!q.empty()) {
		Edge edge = q.front();
		q.pop();
		for(auto&& e : graph.edges[edge.to]) {
			if(used[e.to]) {
				continue;
			}
			distances[e.to] = distances[e.from] + 1;
			q.push(Edge(e.from, e.to, 1));
			used[e.to] = true;
		}
	}
	return distances;
}

const int WIDTH = 4;
const int HEIGHT = 4;

int point_to_num(int h, int w) {
	return WIDTH * h + w;
}

pair<int, int> num_to_point(int n) {
	return make_pair(n / WIDTH, n % WIDTH);
}

bool is_valid_houses(int set, const VI& houses) {
	EACH(house, houses) {
		if((set & (1 << house)) == 0) {
			return false;
		}
	}
	return true;
}

int is_valid(int set, const VI& houses) {
	if(!is_valid_houses(set, houses)) {
		return false;
	}
	return true;

	Graph g(HEIGHT * WIDTH);
	REP(i, HEIGHT * WIDTH) {
		if((set & (1 << i)) == 0) {
			continue;
		}
		int right = i + 1;
		int down = i + WIDTH;
		if(right % WIDTH != 0 && (set & (1 << right)) != 0) {
			g.link(i, right);
			g.link(right, i);
		}
		if(down < HEIGHT * WIDTH && (set & (1 << down) != 0)) {
			g.link(i, down);
			g.link(down, i);
		}
	}
	auto distances = bfs(g, houses[0]);
	int count_reachable = 0;
	EACH(distance, distances) {
		if(distance != INF) {
			count_reachable++;
		}
	}
	int popcount = __builtin_popcount(set);
	return popcount == count_reachable;
}

int main() {
	VI2D a(HEIGHT, VI(WIDTH));
	REP(i, HEIGHT) {
		REP(j, WIDTH) { cin >> a[i][j]; }
	}

	VI houses;
	REP(i, HEIGHT) {
		REP(j, WIDTH) {
			if(a[i][j] == 1) {
				houses.push_back(point_to_num(i, j));
			}
		}
	}

	int result = 0;
	REP(i, 1 << (HEIGHT * WIDTH)) {
		if(is_valid(i, houses)) {
			result++;
		}
	}

	cout << result << endl;
	return 0;
}
