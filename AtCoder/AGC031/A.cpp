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
std::istream& operator>>(std::istream& is, const ModInt<MOD>& x) {
	return is >> x.n;
}

template <typename T>
inline T fact(int n) {
	T ret = T(1);
	REP(i, n) { ret *= (i + 1); }
	return ret;
}

int main() {
	int n;
	string s;
	cin >> n;
	cin >> s;

	VI count(26, 1);
	REP(i, n) { count[s[i] - 'a']++; }
	ModInt<> result = 1;
	REP(i, 26) { result *= count[i]; }

	cout << result - 1 << endl;
	return 0;
}
