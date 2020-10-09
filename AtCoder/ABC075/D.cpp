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

template <typename T>
constexpr T diff(T a, T b) {
	return max(a, b) - min(a, b);
}

template <typename T>
constexpr T is_in_range(T a, T v, T b) {
	return a <= v && v <= b;
}

template <typename T, typename U, typename F>
std::vector<U> mapcar(std::vector<T>& v, F proc) {
	std::vector<U> ret(v.size());
	std::transform(std::begin(v), std::end(v), std::begin(ret), proc);
	return ret;
}

template <typename T, typename F>
std::vector<T> filter(std::vector<T>& v, F proc) {
	std::vector<T> ret;
	std::copy_if(std::begin(v), std::end(v), std::back_inserter(ret), proc);
	return ret;
}

// 終了判定
template <typename T,
		  typename enable_if<is_integral<T>::value>::type* = nullptr>
bool finds(T left, T right) {
	return abs(right - left) > 1;
}

// 二分探索
template <typename T>
T bin_search(T left, T right, auto pred) {
	while(finds<T>(left, right)) {
		T middle = (left + right) / 2;
		if(pred(middle)) {
			left = middle;
		} else {
			right = middle;
		}
	}
	return left;
}

int main() {
	int n, k;
	cin >> n >> k;
	VLL x(n), y(n);
	REP(i, n) { cin >> x[i] >> y[i]; }

	VLL sorted_x(x);
	sort(ALL(sorted_x));
	VLL sorted_y(y);
	sort(ALL(sorted_y));
	ll result = static_cast<ll>(INF) * INF;
	REP(i, n) {			// x right index
		REP(j, i) {		// x left index
			REP(l, n) { // y bottom index
				int y_top_index = bin_search(-1, l + 1, [&](int m) {
					if(m < 0) {
						return true;
					}
					if(m > l) {
						return false;
					}
					ll x_right = sorted_x[i];
					ll x_left = sorted_x[j];
					ll y_bottom = sorted_y[l];
					ll y_top = sorted_y[m];
					ll c = 0;
					REP(ii, n) {
						if(is_in_range(x_left, x[ii], x_right) &&
						   is_in_range(y_top, y[ii], y_bottom)) {
							c++;
						}
					}
					return c >= k;
				});
				if(y_top_index < 0 || l < y_top_index) {
					continue;
				}
				chmin(result,
					  diff(sorted_x[i], sorted_x[j]) *
						  diff(sorted_y[l], sorted_y[y_top_index]));
			}
		}
	}

	cout << result << endl;
	return 0;
}
