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

constexpr int INF = 2e9;
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

// https://rsk0315.hatenablog.com/entry/2020/10/11/125049
struct nekoset {
	private:
	std::set<std::pair<int, int>> s;

	public:
	nekoset() {
		s.emplace(INT_MIN, INT_MIN);
		s.emplace(INT_MAX, INT_MAX);
	}

	bool contains(int x) const {
		auto it = std::prev(s.lower_bound(std::make_pair(x + 1, x + 1)));
		auto [l, u] = *it;
		return l <= x && x <= u;
	}

	bool insert(int x) {
		auto nit = s.lower_bound(std::make_pair(x + 1, x + 1));
		auto it = std::prev(nit);
		auto [l, u] = *it;
		auto [nl, nu] = *nit;
		if(l <= x && x <= u)
			return false;
		if(u == x - 1) {
			if(nl == x + 1) {
				s.erase(it);
				s.erase(nit);
				s.emplace(l, nu);
			} else {
				s.erase(it);
				s.emplace(l, x);
			}
		} else {
			if(nl == x + 1) {
				s.erase(nit);
				s.emplace(x, nu);
			} else {
				s.emplace(x, x);
			}
		}
		return true;
	}

	int mex(int x = 0) const {
		auto [l, u] = *std::prev(s.lower_bound(std::make_pair(x + 1, x + 1)));
		if(l <= x && x <= u) {
			return u + 1;
		} else {
			return x;
		}
	}
};

bool test_case() {
	int n;
	cin >> n;
	VI l(n), r(n);
	REP(i, n) {
		cin >> l[i] >> r[i];
		l[i]--;
	}

	vector<pair<int, int>> pairs(n);
	REP(i, n) { pairs[i] = make_pair(r[i], l[i]); }
	sort(ALL(pairs));
	nekoset ns;
	REP(i, n) {
		auto [right, left] = pairs[i];
		int mex = ns.mex(left);
		if(mex >= min(right, 1'000'000'000)) {
			return false;
		}
		ns.insert(mex);
	}
	return true;
}

int main() {
	int t;
	cin >> t;

	vector<bool> results(t);
	REP(i, t) { results[i] = test_case(); }

	REP(i, t) {
		if(results[i]) {
			cout << "Yes" << endl;
		} else {
			cout << "No" << endl;
		}
	}
	return 0;
}
