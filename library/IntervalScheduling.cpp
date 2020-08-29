#include <algorithm>
#include <vector>

const int INF = 2e9;

template <typename T>
struct Interval {
	T s;
	T e;
	Interval() {}
	Interval(T s, T e) : s(s), e(e) {}
	bool operator<(const Interval<T>& rhs) { return this->e < rhs.e; }
};

// 区間スケジューリング
// O(nlogn)
template <typename T>
class IntervalScheduling {
	std::vector<Interval<T>> intervals;

	public:
	IntervalScheduling(std::vector<Interval<T>> intervals)
		: intervals(intervals) {
		sort(this->intervals.begin(), this->intervals.end());
	}

	std::vector<Interval<T>> operator()(void) {
		std::vector<Interval<T>> schedules;
		T time = -INF;
		for(Interval i : this->intervals) {
			if(i.s >= time) { // 端点の重複を許可
				schedule.push_back(i);
				time = i.e;
			}
		}
		return schedules;
	}
};