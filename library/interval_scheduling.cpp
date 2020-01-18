#include <algorithm>
#include <vector>

const int INF = 2e9;

struct Interval {
	int s;
	int e;
	Interval() {}
	Interval(int s, int e) : s(s), e(e) {}
};

bool operator<(const Interval& lhs, const Interval& rhs) {
	return lhs.e < rhs.e;
}

// 区間スケジューリング
// O(nlogn)
std::vector<Interval> interval_scheduling(std::vector<Interval> intervals) {
	std::vector<Interval> schedule;
	sort(intervals.begin(), intervals.end());
	int time = -INF;
	for(Interval i : intervals) {
		if(i.s >= time) { // 端点の重複を許可
			schedule.push_back(i);
			time = i.e;
		}
	}
	return schedule;
}