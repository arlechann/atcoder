#include <vector>

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
