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

// 最大公約数を返す
constexpr long long gcd(long long a, long long b) {
	if(a < b) {
		std::swap(a, b);
	}

	long long r = a % b;
	while(r != 0) {
		a = b;
		b = r;
		r = a % b;
	}

	return b;
}

// 高速素因数分解
class Factor {
	std::vector<int> divisors;

	public:
	Factor(int n) : divisors(n, 0) {
		std::iota(this->divisors.begin(), this->divisors.end(), 0);
		for(int i = 2; i * i < n; i++) {
			if(this->divisors[i] != i) {
				continue;
			}
			this->divisors[i] = i;
			for(int j = i * i; j < n; j += i) {
				if(this->divisors[j] == j) {
					this->divisors[j] = i;
				}
			}
		}
	}

	std::unordered_map<int, int> operator()(int n) {
		std::unordered_map<int, int> factors;
		while(n != 1) {
			int divisor = this->divisors[n];
			while(n % divisor == 0) {
				factors[divisor]++;
				n /= divisor;
			}
		}
		return factors;
	}
};

int main() {
	int n;
	cin >> n;
	VI a(n);
	REP(i, n) { cin >> a[i]; }

	int setwize = a[0];
	REP(i, n - 1) { setwize = gcd(setwize, a[i + 1]); }
	if(setwize != 1) {
		cout << "not coprime" << endl;
		return 0;
	}

	auto factor = Factor(1000010);
	VI count(1000010, 0);
	REP(i, n) {
		auto factors = factor(a[i]);
		EACH(e, factors) {
			int divisor = e.first;
			if(count[divisor]++ != 0) {
				cout << "setwise coprime" << endl;
				return 0;
			}
		}
	}

	cout << "pairwise coprime" << endl;
	return 0;
}
