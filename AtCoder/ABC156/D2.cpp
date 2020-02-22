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
#include <numeric>
#include <queue>
#include <sstream>
#include <string>
#include <tuple>
#include <type_traits>
#include <utility>
#include <vector>

#define REP(i, n) for(int i = 0, i##_MACRO = (n); i < i##_MACRO; i++)
#define RANGE(i, a, b) for(int i = (a), i##_MACRO = (b); i < i##_MACRO; i++)
#define EACH(e, a) for(auto&& e : a)
#define ALL(a) (a).begin(), (a).end()
#define AALL(a, n) (a), ((a) + (n))
#define FILL(a, n) memset((a), n, sizeof(a))
#define FILLZ(a) FILL(a, 0)

using namespace std;

using ll = long long;
using VI = vector<int>;
using VI2D = vector<vector<int>>;

const int INF = 2e9;
const double EPS = 1e-10;
const double PI = acos(-1.0);

const int dx[] = {-1, 0, 1, 0};
const int dy[] = {0, -1, 0, 1};

template <typename T>
int sign(T x) {
	return x < 0 ? -1 : x > 0 ? 1 : 0;
}

template <>
int sign(double x) {
	return x < -EPS ? -1 : x > EPS ? 1 : 0;
}

template <typename T>
void chmax(T& m, T x) {
	m = max(m, x);
}

template <typename T>
void chmin(T& m, T x) {
	m = min(m, x);
}

template <typename T>
T square(T x) {
	return x * x;
}

inline int toInt(string s) {
	int v;
	istringstream sin(s);
	sin >> v;
	return v;
}

template <long long MOD = 1000000007>
class ModInt {
	public:
	long long n;

	ModInt() : n(0) {}
	ModInt(long long n) : n(n) {
		while(this->n < 0) {
			this->n += MOD;
		}
		this->n %= MOD;
	}

	long long get() const { return this->n; }
	long long get_mod() const { return MOD; }

	ModInt inv() const { return pow<ModInt<>>(*this, MOD - 2); }

	ModInt& operator=(const long long rhs) { return *this = ModInt(rhs); }
	ModInt& operator+=(const ModInt rhs) {
		return *this = ModInt(this->n + rhs.n);
	}
	ModInt& operator-=(const ModInt rhs) {
		return *this = ModInt(this->n - rhs.n);
	}
	ModInt& operator*=(const ModInt rhs) {
		return *this = ModInt(this->n * rhs.n);
	}
	ModInt& operator/=(const ModInt rhs) { return *this *= rhs.inv(); }
	bool operator==(const ModInt rhs) const { return this->n == rhs.n; }
};

template <long long MOD>
ModInt<MOD> operator+(const ModInt<MOD>& lhs, const ModInt<MOD>& rhs) {
	return ModInt<MOD>(lhs) += rhs;
}
template <long long MOD>
ModInt<MOD> operator+(const ModInt<MOD>& lhs, const long long& rhs) {
	return ModInt<MOD>(lhs) += rhs;
}
template <long long MOD>
ModInt<MOD> operator+(const long long& lhs, const ModInt<MOD>& rhs) {
	return ModInt<MOD>(lhs) += rhs;
}

template <long long MOD>
ModInt<MOD> operator-(const ModInt<MOD>& lhs, const ModInt<MOD>& rhs) {
	return ModInt<MOD>(lhs) -= rhs;
}
template <long long MOD>
ModInt<MOD> operator-(const ModInt<MOD>& lhs, const long long& rhs) {
	return ModInt<MOD>(lhs) -= rhs;
}
template <long long MOD>
ModInt<MOD> operator-(const long long& lhs, const ModInt<MOD>& rhs) {
	return ModInt<MOD>(lhs) -= rhs;
}

template <long long MOD>
ModInt<MOD> operator*(const ModInt<MOD>& lhs, const ModInt<MOD>& rhs) {
	return ModInt<MOD>(lhs) *= rhs;
}
template <long long MOD>
ModInt<MOD> operator*(const ModInt<MOD>& lhs, const long long& rhs) {
	return ModInt<MOD>(lhs) *= rhs;
}
template <long long MOD>
ModInt<MOD> operator*(const long long& lhs, const ModInt<MOD>& rhs) {
	return ModInt<MOD>(lhs) *= rhs;
}

template <long long MOD>
ModInt<MOD> operator/(const ModInt<MOD>& lhs, const ModInt<MOD>& rhs) {
	return ModInt<MOD>(lhs) /= rhs;
}
template <long long MOD>
ModInt<MOD> operator/(const ModInt<MOD>& lhs, const long long& rhs) {
	return ModInt<MOD>(lhs) /= rhs;
}
template <long long MOD>
ModInt<MOD> operator/(const long long& lhs, const ModInt<MOD>& rhs) {
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

// 繰り返し2乗法
// 計算量 O(logn)
template <typename T>
T pow(T a, int n) {
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

// 二項係数
// 計算量 O(r)
template <typename T>
T comb(long long n, long long r) {
	if(n < r || n < 0 || r < 0) {
		return T(0);
	}
	T ret = 1;
	r = std::min(r, n - r);
	for(int i = 0; i < r; i++) {
		ret *= (n - i);
		ret /= i + 1;
	}
	return ret;
}

int main() {
	int n, a, b;
	cin >> n >> a >> b;
	auto all = pow<ModInt<>>(2, n) - 1;
	cout << all - comb<ModInt<>>(n, a) - comb<ModInt<>>(n, b) << endl;
	return 0;
}