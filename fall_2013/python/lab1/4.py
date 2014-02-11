from math import sqrt

def isprime(test):
    if test == 2: return True
    if test < 2 or test % 2 == 0: return False
    return not any(test % i == 0 for i in range(3, int(sqrt(test)) + 1, 2))

test_num = 2
prime_count = 1

while (prime_count< 10001): 
 test_num = test_num + 1  

 if (isprime(test_num)):
     prime_count += 1

print test_num