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
#include <sstream>
#include <string>
#include <tuple>
#include <type_traits>
#include <unordered_map>
#include <utility>
#include <vector>

#define REP(i, n) for(int i = 0, i##_MACRO = (n); i < i##_MACRO; i++)
#define RANGE(i, a, b) for(int i = (a), i##_MACRO = (b); i < i##_MACRO; i++)
#define EACH(e, a) for(auto&& e : a)
#define ALL(a) (a).begin(), (a).end()
#define AALL(a, n) (a), ((a) + (n))
#define FILL(a, n) memset((a), n, sizeof(a))
#define FILLZ(a) FILL(a, 0)
#define INT(x) (static_cast<int>(x))
#define MODNUM (INT(1e9 + 7))
#define MOD(x) ((x) % MODNUM)

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

inline long long mod_pow(long long a, long long n, long long mod) {
	long long ret = 1;
	while(n != 0) {
		if(n % 2) {
			ret = (ret * a) % mod;
		}
		a = (a * a) % mod, n /= 2;
	}
	return ret;
}

// modの逆元
inline long long mod_inv(long long n, long long mod) {
	return mod_pow(n, mod - 2, mod);
}

// エラトステネスの篩
template <typename T>
inline std::vector<T> sieve_of_eratosthenes(T n) {
	std::vector<T> sieve(n, 0);
	for(int i = 2; i < n; i++) {
		sieve[i] = i;
	}
	T i = 2;
	while(i * i < n) {
		if(sieve[i]) {
			for(T j = i * i; j < n; j += i) {
				sieve[j] = 0;
			}
		}
		i++;
	}
	return sieve;
}

// 素数リスト
template <typename T>
inline std::vector<T> prime_list(T n) {
	std::vector<T> primes = sieve_of_eratosthenes(n);
	primes.erase(std::remove(primes.begin(), primes.end(), 0), primes.end());
	return primes;
}

// 素因数分解(素数表を用いる)
inline std::unordered_map<long long, int>
factor(long long n, std::vector<long long>& primes) {
	std::unordered_map<long long, int> factors;
	for(int i = 0; primes[i] * primes[i] <= n; i++) {
		int j = 0;
		while(n % primes[i] == 0) {
			n /= primes[i];
			j++;
		}
		if(j != 0) {
			factors[primes[i]] = j;
		}
	}
	if(n != 1) {
		factors[n] = 1;
	}
	return factors;
}

// vectorの要素すべての最小公倍数をmodで割った余りを返す
template <typename T>
inline long long
mod_lcm(std::vector<T>& v, std::vector<long long>& primes, long long mod) {
	int n = v.size();
	std::unordered_map<long long, int> lcm_factors;
	for(int i = 0; i < n; i++) {
		std::unordered_map<long long, int> factors = factor(v[i], primes);
		for(const std::pair<long long, int>& factor : factors) {
			if(lcm_factors[factor.first] < factor.second) {
				lcm_factors[factor.first] = factor.second;
			}
		}
	}
	long long retval = 1;
	for(const std::pair<long long, int>& factor : lcm_factors) {
		retval = (retval * mod_pow(factor.first, factor.second, mod)) % mod;
	}
	return retval;
}

int main() {
	int n;
	scanf("%d", &n);
	VI a(n);
	REP(i, n) { scanf("%d", &a[i]); }
	vector<ll> primes = prime_list<ll>(1e6 + 100);
	ll lcmall = mod_lcm(a, primes, MODNUM);
	ll result = 0;
	REP(i, n) { result = MOD(result + MOD(lcmall * mod_inv(a[i], MODNUM))); }
	printf("%lld\n", result);
	return 0;
}
