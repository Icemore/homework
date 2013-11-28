#pragma once

#include <iostream>
#include <cmath>

int gcd(int a, int b)
{
    if(b == 0) return a;
    else return gcd(b, a%b);
}


class Rational
{
public:
    Rational()
        : num(0), den(1)
    {}

    Rational(int num, int den = 1)
        : num(num), den(den)
    {
        
        normalize();
    }

    Rational& operator+=(Rational const & other)
    {
        num = num * other.den + den * other.num;
        den = den * other.den;
        normalize();

        return *this;
    }

    Rational& operator-=(Rational const & other)
    {
        return (*this) += -other;
    }

    Rational& operator*=(Rational const & other)
    {
        num *= other.num;
        den *= other.den;
        normalize();

        return *this;
    }

    Rational& operator/=(Rational const & other)
    {
        return (*this) *= Rational(other.den, other.num); 
    }

    Rational operator-() const
    {
        return Rational(-num, den);
    }

    bool operator<(Rational const & other) const
    {
        return num * other.den < other.num * den;
    }

    bool operator==(Rational const & other) const
    {
        return num == other.num && den == other.den;
    }

    operator double()
    {
        return (double)num / den;
    }

    Rational& operator++()
    {
        return (*this) += 1;
    }

    Rational operator++(int)
    {
       Rational tmp(*this);
       ++(*this);
       return tmp;
    }

friend std::ostream& operator<<(std::ostream &os, Rational const & r1);
friend std::istream& operator>>(std::istream &is, Rational &r1);

private:
    void normalize()
    {
        if(den < 0)
        {
            num *= -1;
            den *= -1;
        }

        int g = gcd(std::abs(num), den);
        num /= g;
        den /= g;
    }

    int num;
    int den;
};

std::ostream& operator<<(std::ostream &os, Rational const & r1)
{
    os << r1.num << '/' << r1.den;
    return os;
}

std::istream& operator>>(std::istream &is, Rational &r1)
{
    char tmp;
    is >> r1.num >> tmp >> r1.den;
    r1.normalize();
    return is;
}

Rational operator+(Rational r1, Rational const & r2)
{
    return r1 +=r2;
}

Rational operator-(Rational r1, Rational const & r2)
{
    return r1 -= r2;
}

Rational operator*(Rational r1, Rational const & r2)
{
    return r1 *= r2;
}

Rational operator/(Rational r1, Rational const & r2)
{
    return r1 /= r2;
}

bool operator>(Rational const & r1, Rational const & r2)
{
    return r2 < r1;
}

bool operator<=(Rational const & r1, Rational const & r2)
{
    return !(r1 > r2);
}

bool operator>=(Rational const & r1, Rational const & r2)
{
    return !(r1 < r2);
}

bool operator!=(Rational const & r1, Rational const & r2)
{
    return !(r1 == r2);
}
