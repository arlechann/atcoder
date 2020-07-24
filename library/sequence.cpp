#include <unordered_map>
#include <vector>

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