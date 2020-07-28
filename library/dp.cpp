#include <vector>
#include <unordered_map>
#include <boost/functional/hash.hpp>

// 最長増加部分列
template <typename T>
std::vector<T> lis(std::vector<T>& v) {
	int n = v.size();
	std::vector<T> dp;
	dp.push_back(v[0]);
	for(auto&& e : v) {
		if(e > *dp.rbegin()) { // >=にすると広義単調増加
			dp.push_back(e);
		} else {
			*std::lower_bound(dp.begin(), dp.end(), e) = e;
		}
	}
	return dp;
}

template <typename T, typename F, typename ...Args>
class DP {
	F rec;
	unordered_map<std::tuple<Args...>, T> dp;

	public:
	DP(F f) : rec(f) {}

	T operator(Args ...args) {
		std::tuple<Args...> t = make_tuple(...args);
		if((T found = this->dp.find(t)) != this->dp.end()) {
			return *found;
		}
		return this->dp[t] = this->rec(...args);
	}
}
