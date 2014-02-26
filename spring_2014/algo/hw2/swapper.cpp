// http://www.e-olimp.com/solutions/1361392

// default stack is not enough
#pragma comment(linker,"/STACK:256000000")

#include <iostream>
using namespace std;

struct node {
    node(int val, node* left = 0, node* right = 0)
        : val(val), left(left), right(right)
    {
        fix();
    }

    void fix() {
        sum = 0;
        cnt = 0;

        if(left) {
            sum += left->sum;
            cnt += left->cnt;
        }
        if(right) {
            sum += right->sum;
            cnt += right->cnt;
        }
        
        ++cnt;
        sum += val;
    }

    int val;
    long long sum;
    int cnt;
    node* left;
    node* right;
};

node* roots[2];
int n, m, t, x, y;

int getCnt(node* x) {
    return x ? x->cnt : 0;
}

long long getSum(node* x) {
    return x ? x->sum : 0;
}

node* rotateR(node* x) {
    node* y = x->left;

    x->left = y->right;
    y->right = x;
    
    x->fix();
    y->fix();
    
    return y;
}

node* rotateL(node* y) {
    node* x = y->right;

    y->right = x->left;
    x->left = y;

    y->fix();
    x->fix();
    
    return x;
}

node* splay(node* x, int val, int cur, int &mark) {
    if(!x || cur + getCnt(x->left) == val) {
        mark = 0;
        return x;
    }

    if(val < cur + getCnt(x->left)) {
        x->left = splay(x->left, val, cur, mark);
        x = rotateR(x);
        ++mark;

        if(mark > 1) {
            x->right = rotateR(x->right);
            mark = 0;
        }
    }
    else {
        x->right = splay(x->right, val, cur + 1 + getCnt(x->left), mark);
        x = rotateL(x);
        --mark;

        if(mark < -1) {
            x->left = rotateL(x->left);
            mark = 0;
        }
    }

    return x;
}

void splay(node*& x, int pos) {
    int mark = 0;
    x = splay(x, pos, 0, mark);
}

void splitL(node*& x, int pos, node*& l, node*& r) {
    splay(x, pos);
    
    r = x->right;
    l = x;
    x->right = 0;

    x->fix();
}

void splitR(node*& x, int pos, node*& l, node*& r) {
    splay(x, pos);

    l = x->left;
    r = x;
    x->left = 0;

    x->fix();
}

node* merge(node*& l, node*& r) {
    if(!l) return r;

    splay(l, l->cnt - 1);

    l->right = r;
    l->fix();

    return l;
}

void add(node*& x, int val) {
    node* newNode = new node(val);

    if(!x) {
        x = newNode;
    }
    else {
        merge(x, newNode);
    }
}

void splitInterval(node*& x, int l, int r, node*& left, node*& mid, node*& right) {
    node* tmp;

    splitR(x, l, left, tmp);
    splitL(tmp, r - l, mid, right);
}

void mergeInterval(node*& res, node*& left, node*& mid, node*& right) {
    res = merge(left, mid);
    res = merge(res, right);
}

void doit1(node*& even, node*& odd) {
    swap(even, odd);
}

void doit2(node*& even, node*& odd) {
    cout << getSum(even) + getSum(odd) << "\n";
}

void doit(int type, int l, int r) {
    node* left[2] = {roots[0], roots[1]};
    node* right[2]={};
    node* mid[2] = {};

    if((l+1)/2 <= r/2)
        splitInterval(roots[0], (l+1)/2, r/2, left[0], mid[0], right[0]);
    if(r!=0 && l/2 <= (r-1)/2) 
        splitInterval(roots[1], l/2, (r-1)/2, left[1], mid[1], right[1]);

    if(type == 1) {
        doit1(mid[0], mid[1]);
    }
    else {
        doit2(mid[0], mid[1]);
    }

    mergeInterval(roots[0], left[0], mid[0], right[0]);
    mergeInterval(roots[1], left[1], mid[1], right[1]);
}


int main() {
    ios_base::sync_with_stdio(false); cin.tie(0);
    
    for(int tc = 1; ; ++tc) {
        cin >> n >> m;

        if(n == 0 && m == 0) {
            break;
        }

        roots[0] = roots[1] = 0;

        if(tc>1) cout << endl;
        cout << "Swapper " << tc << ":\n";

        for(int i = 0; i < n; ++i) {
            cin >> x;
            
            add(roots[i%2], x);
        }

        for(int i = 0; i < m; ++i) {
            cin >> t >> x >> y;
            --x; --y;

            doit(t, x, y);
        }

    }

    return 0;
}