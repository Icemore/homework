// problem 1521 run 5655669

#include <iostream>
#include <random>

struct node {
	node(int val, int prior, node* left = 0, node* right = 0)
		: val(val), prior(prior), left(left), right(right)
	{
		fix();
	}

	void fix() {
		cnt = 1;

		if (left) cnt += left->cnt;
		if (right) cnt += right->cnt;
	}

	~node() {
		delete left;
		delete right;
	}

	int val;
	int cnt;
	int prior;
	node* left;
	node* right;

private:
	node(node const & other);
	node& operator=(node const & other);
};

int getCnt(node* x) {
	return x ? x->cnt : 0;
}

void split(node* x, int pos, node*& t1, node*& t2) {
	if (!x) {
		t1 = 0;
		t2 = 0;
		return;
	}

	if (pos <= getCnt(x->left)) {
		split(x->left, pos, t1, t2);
		x->left = t2;
		x->fix();
		t2 = x;
	}
	else {
		split(x->right, pos - getCnt(x->left) - 1, t1, t2);
		x->right = t1;
		x->fix();
		t1 = x;
	}
}

node* merge(node* t1, node* t2) {
	if (!t1) return t2;
	if (!t2) return t1;

	if (t1->prior < t2->prior) {
		t2->left = merge(t1, t2->left);
		t2->fix();
		return t2;
	}
	else {
		t1->right = merge(t1->right, t2);
		t1->fix();
		return t1;
	}
}

void addToBack(node*& root, int val) {
	static std::mt19937 gen(234);
	static std::uniform_int_distribution<int> dist;

	node* newNode = new node(val, dist(gen));
	root = merge(root, newNode);
}

int main() {
	int n, k;
	node* root = 0;

	std::cin >> n >> k;

	for (int i = 0; i < n; ++i) {
		addToBack(root, i);
	}

	int cur = 0;
	--k;

	for (int i = 0; i < n; ++i) {
		cur = (cur + k) % root->cnt;
		node* left;
		node* mid;
		node* right;

		split(root, cur, left, right);
		split(right, 1, mid, right);

		std::cout << mid->val + 1 << " ";

		root = merge(left, right);
		delete mid;
	}

	delete root;

	return 0;
}