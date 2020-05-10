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

struct LoopInfo {
	ll dist_begin_loop;
	ll loop_begin_node;
	ll loop_size;
	LoopInfo(ll d, ll n, ll s)
		: dist_begin_loop(d), loop_begin_node(n), loop_size(s) {}
};

// loopの始点までの距離, loopの始点, loopのサイズ
LoopInfo find_loop(vector<ll>& a) {
	ll n = a.size();
	vector<ll> dist(n, 0);
	ll node = 0;
	ll prev_node = 0;
	ll i = 0;
	dist[0] = -1;
	while(true) {
		// cout << "node: " << node << endl;
		dist[node] = dist[prev_node] + 1;
		prev_node = node;
		node = a[node] - 1;
		if(dist[node] != 0 || (i != 0 && node == 0)) {
			// cout << "node: " << node << ", prev: " << prev_node
			//	 << ", dist[node]: " << dist[node] << endl;
			return LoopInfo(dist[node], node, dist[prev_node] - dist[node] + 1);
		}
		i++;
	}
}

ll find_node(vector<ll>& a, ll count) {
	ll node = 0;
	REP(i, count) { node = a[node] - 1; }
	return node;
}

int main() {
	ll n, k;
	cin >> n >> k;
	vector<ll> a(n);
	REP(i, n) { cin >> a[i]; }
	LoopInfo li = find_loop(a);
	// cout << "dist: " << li.dist_begin_loop << ", size: " << li.loop_size
	//	 << endl;
	if(k < li.dist_begin_loop) {
		cout << find_node(a, k) + 1 << endl;
	} else {
		cout << find_node(a,
						  li.dist_begin_loop +
							  (k - li.dist_begin_loop) % li.loop_size) +
					1
			 << endl;
	}
	return 0;
}
