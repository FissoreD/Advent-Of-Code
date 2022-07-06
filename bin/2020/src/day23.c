#include <stdio.h>
#define length 1000001
#define loop_nb 10000000

const int inp[] = {0, 3, 8, 9, 1, 2, 5, 4, 6, 7};
const int inp1[] = {0, 4, 7, 6, 1, 3, 8, 2, 5, 9};

long int l[length];

void readfile()
{
  FILE *f = fopen("input/day23.txt", "r");
  char buff[10];
  int nb = 0;
  fgets(buff, 10, f);
  fclose(f);
  for (int i = 10; i > 0; i--)
    buff[i] = buff[i - 1];
  buff[0] = '0';
  for (int i = 0; i < 10; i++)
    printf("%d", buff[i] - '0');
}

long int initiate_list()
{
  for (long int i = 1; i < length - 1; i++)
  {
    if (i < 10)
    {
      long int index = inp[i];
      long int next = (i + 1 < 10) ? inp[i + 1] : 10;
      l[index] = next;
    }
    else
    {
      l[i] = i + 1;
    }
  }
  long int last = length > 10 ? length - 1 : inp[length - 1];
  l[last] = inp[1];
  return inp[1];
}

long int find_dest(long int current)
{
  long int n1 = l[current];
  long int n2 = l[n1];
  long int n3 = l[n2];
  long int current1 = l[n3];
  long int dest = current - 1;
  while (dest == n1 || dest == n2 || dest == n3 || dest < 1)
  {
    if (dest < 1)
      dest = length;
    dest--;
  }
  l[current] = current1;
  l[n3] = l[dest];
  l[dest] = n1;
  return l[current];
}

long int loop(long int n, long int start)
{
  for (int i = 0; i < loop_nb; i++)
  {
    start = find_dest(start);
  }
  return start;
}

int main(int argc, char const *argv[])
{
  //readfile();
  loop(loop_nb, initiate_list());
  printf("%ld\n", (l[1]) * (l[l[1]]));
  return 0;
}
