#include <stdio.h>
#include <stdlib.h>
#include <string.h>
int main() {
  char str1[15];
  char str2[15];
  int ret;

  strcpy(str1, "cake");
  strcpy(str2, "dog");
  ret = strcmp(str1, str2);
  if (ret > 0)
    printf("cake > dog %d\n", ret);
  else if (ret < 0)
    printf("cake < dog%d\n", ret);
  else
    printf("cake == dog%d\n", ret);
}
