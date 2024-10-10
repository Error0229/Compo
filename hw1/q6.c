#include <stdio.h>
int isqrt(int n) {
  int c = 0, s = 1;
  while (s <= n) {
    c++;
    s += 2 * c + 1;
  }
  return c;
}
int main() {
  int n;
  for (n = 0; n <= 20; n++)
    printf("sqrt(%2d) = %2d\n", n, isqrt(n));
  return 0;
}
