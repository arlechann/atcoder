#include <unordered_map>
#include <utility>
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

template <typename T>
class RunLengthEncoding {
	using iterator = typename std::vector<std::pair<T, std::size_t>>::iterator;
	using const_iterator =
		typename std::vector<std::pair<T, std::size_t>>::const_iterator;

	std::vector<std::pair<T, size_t>> rle;

	public:
	template <typename U>
	RunLengthEncoding(U seq) {
		T prev = *seq.begin();
		std::size_t count = 0;
		for(T& e : seq) {
			if(e != prev) {
				this->rle.push_back(make_pair(prev, count));
				prev = e;
				count = 0;
			}
			count++;
		}
		this->rle.push_back(make_pair(prev, count));
	}

	std::vector<T> get() const { return this->rle; }

	std::size_t size() const { return this->rle.size(); }
	iterator begin() { return this->rle.begin(); }
	iterator end() { return this->rle.end(); }
	const_iterator cbegin() const { return this->rle.cbegin(); }
	const_iterator cend() const { return this->rle.cend(); }

	const std::pair<T, std::size_t>& operator[](std::size_t i) const& {
		return this->rle[i];
	}
	std::pair<T, std::size_t>& operator[](std::size_t i) & {
		return this->rle[i];
	}
	std::pair<T, std::size_t> operator[](std::size_t i) const&& {
		return std::move(this->rle[i]);
	}
};