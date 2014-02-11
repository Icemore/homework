#include <iostream>

struct link
{
    int val;
    link *prev, *next;
};

void sort(link* a)
{
    for(link* cur = a; cur != 0; cur = cur->next)
    {
        link* p = cur;
        link* prev= p->prev;
        while(p != a && prev->val > p->val)
        {
            int tmp = p->val;
            p->val = prev->val;
            prev->val = tmp;

            p = p->prev;
            prev = prev->prev;
        }
    }
}

void print(link *a)
{
    while(a != 0)
    {
        std::cout << a->val << ' ';
        a = a->next;
    }
}

void doit(link* start, link* end)
{
    link cur = {0, 0, 0};
    std::cin >> cur->val;
    
    if(cur->val == 0)
    {
        sort(start);
        print(start);
    }
    else
    {
        end->next = &cur;
        cur.prev = end;

        doit(start, &cur);
    }
}

int main()
{
    

    return 0;
}
