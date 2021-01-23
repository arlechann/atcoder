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

struct Transform {
	ll e00;
	ll e01;
	ll e10;
	ll e11;
};

Transform dot(Transform a, Transform b) {
	Transform ret = {a.e00 * b.e00 + a.e01 * b.e10,
					 a.e00 * b.e01 + a.e01 * b.e11,
					 a.e10 * b.e00 + a.e11 * b.e10,
					 a.e10 * b.e01 + a.e11 * b.e11};
	return ret;
}

pair<ll, ll> dot(Transform a, pair<ll, ll> b) {
	pair<ll, ll> ret = {b.first * a.e00 + b.second * a.e01,
						b.first * a.e10 + b.second * a.e11};
	return ret;
}

pair<ll, ll> sum(pair<ll, ll> a, pair<ll, ll> b) {
	return make_pair(a.first + b.first, a.second + b.second);
}

int main() {
	int n;
	cin >> n;
	VLL x(n), y(n);
	REP(i, n) { cin >> x[i] >> y[i]; }
	int m;
	cin >> m;
	VI op(m), p(m);
	REP(i, m) {
		cin >> op[i];
		if(op[i] > 2) {
			cin >> p[i];
		}
	}
	int q;
	cin >> q;
	VI a(q), b(q);
	REP(i, q) { cin >> a[i] >> b[i]; }

	vector<Transform> transforms(m + 1, {1, 0, 0, 1});
	vector<pair<ll, ll>> linear(m + 1, {0, 0});
	Transform clockwise_rotate = {0, 1, -1, 0};
	Transform counter_clockwise_rotate = {0, -1, 1, 0};
	REP(i, m) {
		if(op[i] == 1) {
			transforms[i + 1] = dot(clockwise_rotate, transforms[i]);
			linear[i + 1] = dot(clockwise_rotate, linear[i]);
		} else if(op[i] == 2) {
			transforms[i + 1] = dot(counter_clockwise_rotate, transforms[i]);
			linear[i + 1] = dot(counter_clockwise_rotate, linear[i]);
		} else if(op[i] == 3) {
			transforms[i + 1] = dot(Transform({-1, 0, 0, 1}), transforms[i]);
			linear[i + 1] = sum(dot(Transform({-1, 0, 0, 1}), linear[i]),
								make_pair(p[i] * 2, 0));
		} else {
			transforms[i + 1] = dot(Transform({1, 0, 0, -1}), transforms[i]);
			linear[i + 1] = sum(dot(Transform({1, 0, 0, -1}), linear[i]),
								make_pair(0, p[i] * 2));
		}
	}

	REP(i, q) {
		pair<ll, ll> point = make_pair(x[b[i] - 1], y[b[i] - 1]);
		auto result = sum(dot(transforms[a[i]], point), linear[a[i]]);
		cout << result.first << " " << result.second << endl;
	}
	return 0;
}
