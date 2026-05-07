int a[10];

int f(int x)
{
    int i = 0;
    int s = 0;

    while (i < 10) {
        if (x > 0)
            s = s + a[i];
        else
            s = s - a[i];
        i = i + 1;
    }

    return s;
}

