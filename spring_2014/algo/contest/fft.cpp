#define _CRT_SECURE_NO_DEPRECATE
#define _SCL_SECURE_NO_DEPRECATE
#include <iostream>
#include <vector>
#include <complex>
#include <algorithm>
#include <cmath>

using namespace std;

#define forn(i, n) for(int i = 0; i <(n); ++i)
#define forv(i, a) forn(i, (int)a.size())
const double eps = 1e-9;
const double pi = 3.1415926535897932384626433832795;

typedef complex<double> fft_t;

void fft(vector<fft_t> & a, fft_t w) {
	int n = a.size();

	if (n == 1) {
		return;
	}

	vector<fft_t> a0(n / 2), a1(n / 2);
	forv(i, a) {
		if (i % 2 == 0) {
			a0[i / 2] = a[i];
		}
		else {
			a1[i / 2] = a[i];
		}
	}

	fft(a0, w*w);
	fft(a1, w*w);

	fft_t q = 1;
	forn(i, n) {
		a[i] = a0[i%(n/2)] + q*a1[i%(n/2)];
		q *= w;
	}
}

vector<int> multiply(vector<int> & a, vector<int> & b) {
	vector<fft_t> fa(a.begin(), a.end());
	vector<fft_t> fb(b.begin(), b.end());

	size_t n = 1;
	while (n < max(a.size(), b.size())) n *= 2;
	n *= 2;

	fa.resize(n);
	fb.resize(n);

	double alpha = 2 * pi / n;
	fft_t w(cos(alpha), sin(alpha));

	fft(fa, w);
	fft(fb, w);
	
	forv(i, fa) {
		fa[i] *= fb[i];
	}

	fft(fa, fft_t(cos(-alpha), sin(-alpha)));
	vector<int> res(fa.size());
	forv(i, fa){
		res[i] = (int)(fa[i].real() / fa.size() + 0.5);
	}

	return res;
}

vector<int> fromString(string str) {
	vector<int> res(str.size());
	forv(i, str){
		res[str.size() - i - 1] = str[i] - '0';
	}
	return res;
}

void printVec(vector<int> vec) {
	int realSize = vec.size() - 1;
	while (realSize > 0 && vec[realSize] == 0) --realSize;

	for (int i = realSize; i >= 0; --i) {
		cout << vec[i];
	}
	cout << endl;
}

void doit() {
	string sa, sb;
	cin >> sa >> sb;

	vector<int> a, b;
	a = fromString(sa);
	b = fromString(sb);

	vector<int> res = multiply(a, b);

	forv(i, res) {
		if (i + 1 == res.size()) break;

		res[i + 1] += res[i] / 10;
		res[i] %= 10;
	}

	while (res.back() > 10) {
		int t = res.back() / 10;
		res.back() %= 10;
		res.push_back(t);
	}

	printVec(res);
}

int main() {
#ifdef __ASD__
	freopen("input.txt", "rt", stdin);
	freopen("output.txt", "wt", stdout);
#endif

	int n;

	cin >> n;
	forn(i, n) {
		doit();
	}

	return 0;
}