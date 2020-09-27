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

int roundup_pow2(int n) {
	if(!(n & (n - 1))) {
		return n;
	}

	int ret = 1;
	while(n > ret) {
		ret <<= 1;
	}
	return ret;
}

template <typename T, typename U>
class LazySegmentTree {
	using F = std::function<T(T, T)>;
	using G = std::function<T(T, U)>;
	using H = std::function<U(U, U)>;

	std::vector<T> tree;
	std::vector<U> lazy;
	F merge;
	G mapping;
	H composition;
	T id1;
	U id2;
	std::size_t size;

	public:
	LazySegmentTree(const vector<T>& a,
					const F f,
					const T id1,
					const G g,
					const H h,
					const U id2)
		: tree(roundup_pow2(a.size()) * 2 - 1, id1),
		  lazy(roundup_pow2(a.size()) * 2 - 1, id2), merge(f), id1(id1),
		  mapping(g), composition(h), id2(id2), size(roundup_pow2(a.size())) {
		int offset = this->size - 1;
		for(int i = 0; i < a.size(); i++) {
			this->tree[i + offset] = a[i];
		}
		for(int i = offset - 1; i >= 0; i--) {
			this->tree[i] =
				this->merge(this->tree[i * 2 + 1], this->tree[i * 2 + 2]);
		}
	}

	void debug_print() {
		cout << "tree: ";
		REP(i, this->size * 2 - 1) {
			if(i == this->size - 1) {
				cout << "| ";
			}
			cout << "(" << this->tree[i].first << "," << this->tree[i].second
				 << ") ";
		}
		cout << "\nlazy: ";
		REP(i, this->size * 2 - 1) {
			if(i == this->size - 1) {
				cout << "| ";
			}
			cout << this->lazy[i] << " ";
		}
		cout << endl;
	}

	void
	update(const std::size_t left, const std::size_t right, const U value) {
		this->update_impl(left, right, 0, 0, this->size, value);
	}

	T find(const std::size_t left, const std::size_t right) {
		return this->find_impl(left, right, 0, 0, this->size);
	}

	private:
	void update_impl(std::size_t query_left,
					 std::size_t query_right,
					 std::size_t node,
					 std::size_t node_left,
					 std::size_t node_right,
					 U value) {
		this->force(node);
		if(node_right <= query_left || query_right <= node_left) {
			return;
		}
		if(query_left <= node_left && node_right <= query_right) {
			this->lazy[node] = this->composition(this->lazy[node], value);
			this->force(node);
			return;
		}
		this->update_impl(query_left,
						  query_right,
						  node * 2 + 1,
						  node_left,
						  node_left + (node_right - node_left) / 2,
						  value);
		this->update_impl(query_left,
						  query_right,
						  node * 2 + 2,
						  node_left + (node_right - node_left) / 2,
						  node_right,
						  value);
		this->tree[node] =
			this->merge(this->tree[node * 2 + 1], this->tree[node * 2 + 2]);
	}

	T find_impl(size_t query_left,
				size_t query_right,
				size_t node,
				size_t node_left,
				size_t node_right) {
		this->force(node);
		if(node_right <= query_left || query_right <= node_left) {
			return this->id1;
		}
		if(query_left <= node_left && node_right <= query_right) {
			return this->tree[node];
		}

		return this->merge(find_impl(query_left,
									 query_right,
									 node * 2 + 1,
									 node_left,
									 node_left + (node_right - node_left) / 2),
						   find_impl(query_left,
									 query_right,
									 node * 2 + 2,
									 node_left + (node_right - node_left) / 2,
									 node_right));
	}

	void force(std::size_t node) {
		if(this->lazy[node] == this->id2) {
			return;
		}
		if(node * 2 + 1 < this->size * 2 - 1) {
			this->lazy[node * 2 + 1] =
				this->composition(lazy[node * 2 + 1], lazy[node]);
			this->lazy[node * 2 + 2] =
				this->composition(lazy[node * 2 + 2], lazy[node]);
		}
		this->tree[node] = this->mapping(this->tree[node], lazy[node]);
		this->lazy[node] = this->id2;
	}
};

using mint = ModInt<998244353>;

int main() {
	int n, q;
	cin >> n >> q;
	VI l(q), r(q);
	VI d(q);
	REP(i, q) {
		cin >> l[i] >> r[i] >> d[i];
		l[i] = n - l[i] + 1;
		r[i] = n - r[i];
		swap(l[i], r[i]);
	}

	vector<mint> keta(n + 1, 0);
	keta[0] = 1;
	REP(i, n) { keta[i + 1] = keta[i] * 10; }

	vector<mint> repunit(n + 1, 0);
	repunit[0] = 1;
	REP(i, n) { repunit[i + 1] = repunit[i] * 10 + 1; }

	vector<pair<mint, int>> s(n, {1, 1});

	LazySegmentTree<pair<mint, int>, int> lst(
		s,
		[&](pair<mint, int> a, pair<mint, int> b) {
			return make_pair(a.first + b.first * keta[a.second],
							 a.second + b.second);
		},
		{0, 1},
		[&](pair<mint, int> a, int b) {
			return make_pair(b ? b * repunit[a.second - 1] : a.first, a.second);
		},
		[](int a, int b) { return b ? b : a; },
		0);
	// lst.debug_print();
	REP(i, q) {
		lst.update(l[i], r[i], d[i]);
		cout << lst.find(0, n).first << endl;
		// lst.debug_print();
	}
	return 0;
}
