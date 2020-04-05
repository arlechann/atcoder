#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

int main(){
	int n;
	cin >> n;
	vector<int> r, b;
	for(int i = 0; i < n; i++){
		int x;
		char c;
		cin >> x >> c;
		(c == 'R' ? r : b).push_back(x);
	}
	sort(r.begin(), r.end());
	sort(b.begin(), b.end());
	for(auto &&e : r){
		cout << e << endl;
	}
	for(auto &&e : b){
		cout << e << endl;
	}
	return 0;
}
