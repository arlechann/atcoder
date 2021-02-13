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
using VI = std::vector<int>;
using VI2D = std::vector<vector<int>>;
using VLL = std::vector<long long>;
using VLL2D = std::vector<vector<long long>>;

constexpr int INF = 2e9;
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

using PII = pair<int, int>;

const int N_MAX = 100000;

int use_turns[N_MAX];

int count_turn(VI2D& children, int node) {
	int& ret = use_turns[node];
	ret = 3;
	EACH(child, children[node]) { ret += count_turn(children, child); }
	return ret;
}

PII memo[N_MAX][2][2];

PII rec(VI2D& children, int node, int coin, int turn) {
	PII& ret = memo[node][coin][turn];
	if(ret != PII()) {
		return ret;
	}
	if(coin) {
		ret = rec(children, node, 0, !turn);
		(turn == 0 ? ret.first : ret.second) += 1;
		return ret;
	}
	int num_child = children[node].size();
	if(num_child == 0) {
		return make_pair(0, 0);
	} else if(num_child == 1) {
		return ret = rec(children, children[node][0], 1, !turn);
	}

	vector<PII> c(num_child);
	REP(i, num_child) {
		int child = children[node][i];
		c[i] = make_pair(
			child, use_turns[child] * (use_turns[child] % 2 == 1 ? 1 : -1));
	}
	sort(ALL(c), [](auto a, auto b) { return a.second < b.second; });
	int nt = turn;
	EACH(ct, c) {
		auto [child, tcount] = ct;
		if(use_turns[child] % 2 == 1) {
			nt = !nt;
			auto tmp = rec(children, child, 1, nt);
			ret.first += tmp.first;
			ret.second += tmp.second;
		} else {
			auto tmp = rec(children, child, 1, !nt);
			ret.first += tmp.first;
			ret.second += tmp.second;
		}
	}
	return ret;
}

int main() {
	int n;
	cin >> n;
	VI p(n - 1);
	EACH(e, p) {
		cin >> e;
		e--;
	}

	VI parents(n, -1);
	VI2D children(n);
	REP(i, n - 1) {
		parents[i + 1] = p[i];
		children[p[i]].push_back(i + 1);
	}

	count_turn(children, 0);
	auto [takahashi, aoki] = rec(children, 0, 1, 0);

	cout << takahashi << endl;
	return 0;
}
