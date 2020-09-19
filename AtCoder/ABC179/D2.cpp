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

class DynamicModInt {
	public:
	long long n;
	long long mod;

	DynamicModInt() : n(0), mod(1000000007) {}
	DynamicModInt(long long n, long long mod)
		: n(n < 0 ? n + mod : n % mod), mod(mod) {}

	long long get() const { return this->n; }
	long long get_mod() const { return this->mod; }

	DynamicModInt pow(int n) const {
		DynamicModInt a = *this;
		DynamicModInt ret(1, this->mod);
		while(n != 0) {
			if(n % 2) {
				ret *= a;
			}
			a *= a;
			n /= 2;
		}
		return ret;
	}

	DynamicModInt inv() const { return this->pow(this->mod - 2); }

	DynamicModInt& operator=(const long long rhs) {
		return *this = DynamicModInt(rhs, this->mod);
	}
	DynamicModInt& operator+=(const DynamicModInt rhs) {
		return *this = DynamicModInt(this->n + rhs.n,
									 std::min(this->mod, rhs.get_mod()));
	}
	DynamicModInt& operator-=(const DynamicModInt rhs) {
		return *this = DynamicModInt(this->n - rhs.n,
									 std::min(this->mod, rhs.get_mod()));
	}
	DynamicModInt& operator*=(const DynamicModInt rhs) {
		return *this = DynamicModInt(this->n * rhs.n,
									 std::min(this->mod, rhs.get_mod()));
	}
	DynamicModInt& operator/=(const DynamicModInt rhs) {
		return *this *= rhs.inv();
	}
	bool operator==(const DynamicModInt rhs) const {
		return this->mod == rhs.get_mod() && this->n == rhs.n;
	}
	bool operator!=(const DynamicModInt rhs) const { return !(*this == rhs); }
};

DynamicModInt operator+(const DynamicModInt& lhs, const DynamicModInt& rhs) {
	return DynamicModInt(lhs) += rhs;
}
DynamicModInt operator+(const DynamicModInt& lhs, const long long& rhs) {
	return DynamicModInt(lhs) += DynamicModInt(rhs, lhs.get_mod());
}
DynamicModInt operator+(const long long& lhs, const DynamicModInt& rhs) {
	return DynamicModInt(lhs, rhs.get_mod()) += rhs;
}

DynamicModInt operator-(const DynamicModInt& lhs, const DynamicModInt& rhs) {
	return DynamicModInt(lhs) -= rhs;
}
DynamicModInt operator-(const DynamicModInt& lhs, const long long& rhs) {
	return DynamicModInt(lhs) -= DynamicModInt(rhs, lhs.get_mod());
}
DynamicModInt operator-(const long long& lhs, const DynamicModInt& rhs) {
	return DynamicModInt(lhs, rhs.get_mod()) -= rhs;
}

DynamicModInt operator*(const DynamicModInt& lhs, const DynamicModInt& rhs) {
	return DynamicModInt(lhs) *= rhs;
}
DynamicModInt operator*(const DynamicModInt& lhs, const long long& rhs) {
	return DynamicModInt(lhs) *= DynamicModInt(rhs, lhs.get_mod());
}
DynamicModInt operator*(const long long& lhs, const DynamicModInt& rhs) {
	return DynamicModInt(lhs, rhs.get_mod()) *= rhs;
}

DynamicModInt operator/(const DynamicModInt& lhs, const DynamicModInt& rhs) {
	return DynamicModInt(lhs) /= rhs;
}
DynamicModInt operator/(const DynamicModInt& lhs, const long long& rhs) {
	return DynamicModInt(lhs) /= DynamicModInt(rhs, lhs.get_mod());
}
DynamicModInt operator/(const long long& lhs, const DynamicModInt& rhs) {
	return DynamicModInt(lhs, rhs.get_mod()) /= rhs;
}

std::ostream& operator<<(std::ostream& os, const DynamicModInt& x) {
	return os << x.n;
}

std::istream& operator>>(std::istream& is, DynamicModInt& x) {
	return is >> x.n;
}

using ModInt = DynamicModInt;

int main() {
	int n, k;
	cin >> n >> k;
	VI l(k), r(k);
	REP(i, k) { cin >> l[i] >> r[i]; }

	vector<ModInt> d(n + 1, ModInt(0, 998244353));
	d[0] = 1;
	d[1] = -1;
	REP(i, n) {
		if(i != 0) {
			d[i] += d[i - 1];
		}
		REP(j, k) {
			if(i + l[j] < n) {
				d[i + l[j]] += d[i];
				d[min(i + r[j] + 1, n)] -= d[i];
			}
		}
	}

	cout << d[n - 1] << endl;
	return 0;
}
