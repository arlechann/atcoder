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

template <typename T, long long K = 1000000000000000000>
class Doubling {
	public:
	std::size_t n;
	std::size_t k;
	std::vector<std::vector<T>> doubling;

	template <typename F>
	Doubling(std::size_t n, F next)
		: n(n), k(([]() {
			  std::size_t k = 1;
			  while((1LL << k) < K) {
				  k++;
			  }
			  return k;
		  })()),
		  doubling(k + 1, std::vector<T>(n)) {
		for(int i = 0; i < this->n; i++) {
			this->doubling[0][i] = next(i);
		}
		for(int i = 0; i < this->k; i++) {
			for(int j = 0; j < this->n; j++) {
				this->doubling[i + 1][j] =
					this->doubling[i][this->doubling[i][j]];
			}
		}
	}

	T operator()(std::size_t x) {
		std::size_t p = 0;
		for(std::size_t i = 0; i < this->k; i++) {
			if(x & (1LL << i)) {
				p = this->doubling[i][p];
			}
		}
		return p;
	}
};

int main() {
	ll n, k;
	cin >> n >> k;
	vector<ll> a(n);
	REP(i, n) {
		cin >> a[i];
		a[i]--;
	}

	auto doubling = Doubling<int>(n, [&](int i) { return a[i]; });

	cout << doubling(k) + 1 << endl;
	return 0;
}
