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

// 繰り返し2乗法
// 計算量 O(logn)
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

template <long long MOD = 1000000007>
class ModInt {
	public:
	long long n;

	constexpr ModInt() : n(0) {}
	constexpr ModInt(long long n) : n(n < 0 ? n + MOD : n % MOD) {}

	constexpr long long get() const { return this->n; }
	constexpr long long get_mod() const { return MOD; }

	constexpr ModInt inv() const { return pow<ModInt<MOD>>(*this, MOD - 2); }

	constexpr ModInt& operator=(const long long rhs) {
		return *this = ModInt(rhs);
	}
	constexpr ModInt& operator+=(const ModInt rhs) {
		return *this = ModInt(this->n + rhs.n);
	}
	constexpr ModInt& operator-=(const ModInt rhs) {
		return *this = ModInt(this->n - rhs.n);
	}
	constexpr ModInt& operator*=(const ModInt rhs) {
		return *this = ModInt(this->n * rhs.n);
	}
	constexpr ModInt& operator/=(const ModInt rhs) {
		return *this *= rhs.inv();
	}
	constexpr bool operator==(const ModInt rhs) const {
		return this->n == rhs.n;
	}
};

template <long long MOD>
constexpr ModInt<MOD> operator+(const ModInt<MOD>& lhs,
								const ModInt<MOD>& rhs) {
	return ModInt<MOD>(lhs) += rhs;
}
template <long long MOD>
constexpr ModInt<MOD> operator+(const ModInt<MOD>& lhs, const long long& rhs) {
	return ModInt<MOD>(lhs) += rhs;
}
template <long long MOD>
constexpr ModInt<MOD> operator+(const long long& lhs, const ModInt<MOD>& rhs) {
	return ModInt<MOD>(lhs) += rhs;
}

template <long long MOD>
constexpr ModInt<MOD> operator-(const ModInt<MOD>& lhs,
								const ModInt<MOD>& rhs) {
	return ModInt<MOD>(lhs) -= rhs;
}
template <long long MOD>
constexpr ModInt<MOD> operator-(const ModInt<MOD>& lhs, const long long& rhs) {
	return ModInt<MOD>(lhs) -= rhs;
}
template <long long MOD>
constexpr ModInt<MOD> operator-(const long long& lhs, const ModInt<MOD>& rhs) {
	return ModInt<MOD>(lhs) -= rhs;
}

template <long long MOD>
constexpr ModInt<MOD> operator*(const ModInt<MOD>& lhs,
								const ModInt<MOD>& rhs) {
	return ModInt<MOD>(lhs) *= rhs;
}
template <long long MOD>
constexpr ModInt<MOD> operator*(const ModInt<MOD>& lhs, const long long& rhs) {
	return ModInt<MOD>(lhs) *= rhs;
}
template <long long MOD>
constexpr ModInt<MOD> operator*(const long long& lhs, const ModInt<MOD>& rhs) {
	return ModInt<MOD>(lhs) *= rhs;
}

template <long long MOD>
constexpr ModInt<MOD> operator/(const ModInt<MOD>& lhs,
								const ModInt<MOD>& rhs) {
	return ModInt<MOD>(lhs) /= rhs;
}
template <long long MOD>
constexpr ModInt<MOD> operator/(const ModInt<MOD>& lhs, const long long& rhs) {
	return ModInt<MOD>(lhs) /= rhs;
}
template <long long MOD>
constexpr ModInt<MOD> operator/(const long long& lhs, const ModInt<MOD>& rhs) {
	return ModInt<MOD>(lhs) /= rhs;
}

template <long long MOD>
std::ostream& operator<<(std::ostream& os, const ModInt<MOD>& x) {
	return os << x.n;
}

template <long long MOD>
std::istream& operator>>(std::istream& is, ModInt<MOD>& x) {
	return is >> x.n;
}

using mint = ModInt<998244353>;

int main() {
	int h, w, k;
	cin >> h >> w >> k;
	VI y(k), x(k);
	vector<char> c(k);
	REP(i, k) {
		cin >> y[i] >> x[i] >> c[i];
		x[i]--;
		y[i]--;
	}

	vector<vector<char>> map(h, vector<char>(w, '.'));
	REP(i, k) { map[y[i]][x[i]] = c[i]; }

	vector<vector<mint>> dp(h + 2, vector<mint>(w + 2, 0));
	auto inv = mint(2) / 3;
	dp[0][0] = pow<mint>(mint(3), h * w - k);
	REP(i, h) {
		REP(j, w) {
			if(map[i][j] == 'R') {
				dp[i][j + 1] += dp[i][j];
			} else if(map[i][j] == 'D') {
				dp[i + 1][j] += dp[i][j];
			} else if(map[i][j] == 'X') {
				dp[i][j + 1] += dp[i][j];
				dp[i + 1][j] += dp[i][j];
			} else {
				dp[i][j + 1] += dp[i][j] * inv;
				dp[i + 1][j] += dp[i][j] * inv;
			}
		}
	}

	cout << dp[h - 1][w - 1] << endl;
	return 0;
}
