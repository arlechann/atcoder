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

// 終了判定
template <typename T,
		  typename enable_if<is_integral<T>::value>::type* = nullptr>
bool finds(T left, T right) {
	return abs(right - left) > 1;
}

// 終了判定(浮動小数)
template <typename T,
		  typename enable_if<is_floating_point<T>::value>::type* = nullptr>
bool finds(T left, T right) {
	return abs(right - left) > EPS;
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
	int n;
	ll t;
	cin >> n >> t;
	VLL a(n);
	REP(i, n) { cin >> a[i]; }

	VLL front(a.begin(), a.begin() + n / 2);
	VLL back(a.begin() + n / 2, a.end());
	int n1 = n / 2;
	int n2 = n - n1;

	VLL front_enum;
	REP(i, 1 << n1) {
		ll sum = 0;
		REP(j, n1) {
			if((i & (1 << j)) != 0) {
				sum += front[j];
			}
		}
		front_enum.push_back(sum);
	}

	VLL back_enum;
	REP(i, 1 << n2) {
		ll sum = 0;
		REP(j, n2) {
			if((i & (1 << j)) != 0) {
				sum += back[j];
			}
		}
		back_enum.push_back(sum);
	}
	sort(ALL(back_enum));

	ll result = 0;
	EACH(e, front_enum) {
		int index = bin_search<int>(-1, back_enum.size(), [&](int i) {
			if(i == -1) {
				return true;
			} else {
				return e + back_enum[i] <= t;
			}
		});
		if(index != -1) {
			chmax(result, e + back_enum[index]);
		}
	}
	cout << result << endl;
	return 0;
}
