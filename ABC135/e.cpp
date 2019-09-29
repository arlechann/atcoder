#include <cstdio>
#include <deque>
#include <queue>
#include <utility>

using namespace std;

int used[200002][200002];
int dirx[4] = {1, 1, -1, -1};
int diry[4] = {1, -1, 1, -1};

int to_index(int n) {
	return n + 100000;
}

int main() {
	int k;
	int x, y;

	scanf("%d", &k);
	scanf("%d%d", &x, &y);

	queue<deque<pair<int, int>>> q;
	auto dq = new deque<pair<int, int>>;
	dq->push_back(make_pair(to_index(0), to_index(0)));
	q.push(dq);
	while(!q.empty()) {
		auto now = q.front();
		q.pop();

		if(now->back()->first == x && now->back().second == y) {
			for(auto ittr = now->begin(); ittr != now->end(); ittr++) {
				printf("%d %d\n", ittr->first, ittr->second);
			}
			return 0;
		}

		for(int i = 0; i <= k; i++) {
			int dx = i;
			int dy = k - i;

			for(int j = 0; j < 4; j++) {
				int nx = now->back().first + dx * dirx[j];
				int ny = now->back().second + dy * dirx[j];
				deque<pair<int, int>>* ndq = new deque();
				ndq = now;
				if(0 <= nx && 200001 < nx && 0 <= ny && ny < 200001) {
					ndq->push_back(make_pair(nx, ny));
					q.push(ndq);
				}
			}
		}
	}

	puts("-1\n");
	return 0;
}