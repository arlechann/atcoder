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
class Counter {
	using iterator = typename std::unordered_map<T, std::size_t>::iterator;
	using const_iterator =
		typename std::unordered_map<T, std::size_t>::const_iterator;

	std::unordered_map<T, std::size_t> m;

	public:
	template <typename U>
	Counter(U& v) {
		for(T& e : v) {
			this->m[e]++;
		}
	}

	std::vector<T> keys() const {
		std::vector<T> v(this->size());
		int i = 0;
		for(const std::pair<T, std::size_t>& e : this->m) {
			v[i++] = e.first;
		}
		return v;
	}

	std::vector<size_t> values() const {
		std::vector<size_t> v(this->size());
		int i = 0;
		for(const std::pair<T, std::size_t>& e : this->m) {
			v[i++] = e.second;
		}
		return v;
	}

	std::size_t size() const { return this->m.size(); }
	iterator begin() { return this->m.begin(); }
	iterator end() { return this->m.end(); }
	const_iterator cbegin() const { return this->m.cbegin(); }
	const_iterator cend() const { return this->m.cend(); }

	const std::size_t& operator[](T a) const& { return this->m[a]; }
	std::size_t& operator[](T a) & { return this->m[a]; }
	std::size_t operator[](T a) const&& { return std::move(this->m[a]); }
};

int main() {
	int n;
	cin >> n;
	VI v(n);
	REP(i, n) { cin >> v[i]; }

	VI even, odd;
	REP(i, n) {
		if(i % 2) {
			even.push_back(v[i]);
		} else {
			odd.push_back(v[i]);
		}
	}
	Counter<int> even_counter(even);
	Counter<int> odd_counter(odd);
	auto even_max = make_pair(0, 0);
	auto even_max2 = make_pair(0, 0);
	auto odd_max = make_pair(0, 0);
	auto odd_max2 = make_pair(0, 0);
	EACH(e, even_counter) {
		pair<int, int> tmp(e.second, e.first);
		if(tmp > even_max) {
			even_max2 = even_max;
			even_max = tmp;
		} else if(tmp > even_max2) {
			even_max2 = tmp;
		}
	}
	EACH(e, odd_counter) {
		pair<int, int> tmp(e.second, e.first);
		if(tmp > odd_max) {
			odd_max2 = odd_max;
			odd_max = tmp;
		} else if(tmp > odd_max2) {
			odd_max2 = tmp;
		}
	}

	if(even_max.second == odd_max.second) {
		cout << n - max(even_max.first + odd_max2.first,
						even_max2.first + odd_max.first)
			 << endl;
	} else {
		cout << n - even_max.first - odd_max.first << endl;
	}
	return 0;
}
