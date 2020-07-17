#include <map>
#include <unordered_map>
#include <vector>

template <typename T>
std::unordered_map<T, size_t> group_count(std::vector<T>& v) {
	std::unordered_map<T, size_t> c;
	for(T& e : v) {
		c[e]++;
	}
	return c;
}