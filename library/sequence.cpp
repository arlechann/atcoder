#include <map>
#include <unordered_map>
#include <vector>

template <typename T>
std::unordered_map<size_t, T> group_count(std::vector<T>& v) {
	std::unordered_map<size_t, T> c;
	for(T& e : v) {
		c[e]++;
	}
	return c;
}