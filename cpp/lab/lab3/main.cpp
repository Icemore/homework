#include <iostream>

void merge(int* p1, int* pe1, int* p2, int* pe2, int *dst)
{
    while(p1 != pe1 && p2 != pe2)
    {
        if(*p1 < *p2)
            *dst++ = *p1++;
        else
            *dst++ = *p2++;
    }

    while(p1 != pe1)
        *dst++ = *p1++;

    while(p2 != pe2)
        *dst++ = *p2++;
}

void mergeSort(int* a, int* pend, int *tmp)
{
    size_t len = pend - a;
    if(len <= 1) return;

    size_t mid = len / 2;

    mergeSort(a, a + mid, tmp);
    mergeSort(a + mid, pend, tmp);

    merge(a, a + mid, a + mid, pend, tmp);
   
    while(a != pend)
        *a++ = *tmp++;
}

int main()
{
    int a[101] = {};
    int tmp[101] = {};
    int *pend = a;

    std::cin >> *pend;
    while(*pend != 0)
    {
        ++pend;
        std::cin >> *pend;
    }
    
    mergeSort(a, pend, tmp);
    for(int *p = a; p != pend; ++p)
        std::cout << *p << ' ';

    return 0;
}
