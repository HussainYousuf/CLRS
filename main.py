def insertion_sort(xs: list) -> list:
    for index, elem in enumerate(xs):
        if index < 1:
            continue
        j = index-1
        while j >= 0 and xs[j] > elem:
            xs[j+1] = xs[j]
            j-=1
        xs[j+1] = elem
    return xs