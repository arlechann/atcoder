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
class Doubling {
	std::size_t n;
	std::size_t k;
	std::vector<std::vector<T>> doubling;
	std::vector<std::vector<ll>> sum;

	public:
	template <typename F>
	Doubling(long long m, std::size_t n, F next)
		: n(n), k(([=]() {
			  std::size_t k = 1;
			  while((1LL << k) < m) {
				  k++;
			  }
			  return k;
		  })()),
		  doubling(k + 1, std::vector<T>(n)), sum(k + 1, std::vector<T>(n)) {
		for(int i = 0; i < this->n; i++) {
			this->doubling[0][i] = next(i);
			this->sum[0][i] = i;
		}
		for(int i = 0; i < this->k; i++) {
			for(int j = 0; j < this->n; j++) {
				this->doubling[i + 1][j] =
					this->doubling[i][this->doubling[i][j]];
				this->sum[i + 1][j] =
					this->sum[i][j] + this->sum[i][this->doubling[i][j]];
			}
		}
	}

	T operator()(std::size_t x, std::size_t p = 0) {
		ll ret = 0;
		for(std::size_t i = 0; i < this->k; i++) {
			if(x & (1LL << i)) {
				ret += this->sum[i][p];
				p = this->doubling[i][p];
			}
		}
		return ret;
	}
};

int main() {
	ll n, x, m;
	cin >> n >> x >> m;

	VLL2D sum(50, VLL(m));
	auto doubling = Doubling<ll>(1e10, m, [=](ll i) { return i * i % m; });

	cout << doubling(n, x) << endl;
	return 0;
}
