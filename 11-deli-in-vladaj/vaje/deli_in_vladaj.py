###############################################################################
# Želimo definirati pivotiranje na mestu za tabelo [a]. Ker bi želeli
# pivotirati zgolj dele tabele, se omejimo na del tabele, ki se nahaja med
# indeksoma [start] in [end].
#
# Primer: za [start = 0] in [end = 8] tabelo
#
# [10, 4, 5, 15, 11, 2, 17, 0, 18]
#
# preuredimo v
#
# [0, 2, 5, 4, 10, 11, 17, 15, 18]
#
# (Možnih je več različnih rešitev, pomembno je, da je element 10 pivot.)
#
# Sestavi funkcijo [pivot(a, start, end)], ki preuredi tabelo [a] tako, da bo
# element [ a[start] ] postal pivot za del tabele med indeksoma [start] in
# [end]. Funkcija naj vrne indeks, na katerem je po preurejanju pristal pivot.
# Funkcija naj deluje v času O(n), kjer je n dolžina tabele [a].
#
# Primer:
#
#     >>> a = [10, 4, 5, 15, 11, 2, 17, 0, 18]
#     >>> pivot(a, 1, 7)
#     3
#     >>> a
#     [10, 2, 0, 4, 11, 15, 17, 5, 18]
###############################################################################

def pivot(a, start, end):
    l = start + 1
    r = end

    while True:
        # Prvi manjši na desni
        while l <= r and a[r] >= a[start]:
            r -= 1
        
        # Prvi večji na levi
        while l <= r and a[l] < a[start]:
            l += 1
        
        # Zamenjava
        if l <= r:
            temp = a[l]
            a[l] = a[r]
            a[r] = temp
            r -= 1
            l += 1
        else:
            break
    
    # Vstavimo Pivot na pravo mesto
    temp = a[r]
    a[r] = a[start]
    a[start] = temp

    return r
        
a = [10, 4, 5, 15, 11, 2, 17, 0, 18]

###############################################################################
# V tabeli želimo poiskati vrednost k-tega elementa po velikosti.
#
# Primer: Če je
#
#     >>> a = [10, 4, 5, 15, 11, 3, 17, 2, 18]
#
# potem je tretji element po velikosti enak 5, ker so od njega manši elementi
#  2, 3 in 4. Pri tem štejemo indekse od 0 naprej, torej je "ničti" element 2.
#
# Sestavite funkcijo [kth_element(a, k)], ki v tabeli [a] poišče [k]-ti
# element po velikosti. Funkcija sme spremeniti tabelo [a]. Cilj naloge je, da
# jo rešite brez da v celoti uredite tabelo [a].
###############################################################################

def kth_element(a, k):
    p = pivot(a, 0, len(a) - 1)
    if k < p:
        return kth_element(a[0:p], k)
    elif k == p:
        return a[p]
    else:
        return kth_element(a[p+1:], k - p)

###############################################################################
# Tabelo a želimo urediti z algoritmom hitrega urejanja (quicksort).
#
# Napišite funkcijo [quicksort(a)], ki uredi tabelo [a] s pomočjo pivotiranja.
# Poskrbi, da algoritem deluje 'na mestu', torej ne uporablja novih tabel.
#
# Namig: Definirajte pomožno funkcijo [quicksort_part(a, start, end)], ki
#        uredi zgolj del tabele [a].
#
#     >>> a = [10, 4, 5, 15, 11, 3, 17, 2, 18]
#     >>> quicksort(a)
#     [2, 3, 4, 5, 10, 11, 15, 17, 18]
###############################################################################

def quicksort_part(a, start, end):
    if start >= end:
        return
    
    p = pivot(a, start, end)
    quicksort_part(a, start, p - 1)
    quicksort_part(a, p + 1, end)

def quicksort(a):
    quicksort_part(a, 0, len(a) - 1)


###############################################################################
# Če imamo dve urejeni tabeli, potem urejeno združeno tabelo dobimo tako, da
# urejeni tabeli zlijemo. Pri zlivanju vsakič vzamemo manjšega od začetnih
# elementov obeh tabel. Zaradi učinkovitosti ne ustvarjamo nove tabele, ampak
# rezultat zapisujemo v že pripravljeno tabelo (ustrezne dolžine).
# 
# Funkcija naj deluje v času O(n), kjer je n dolžina tarčne tabele.
# 
# Sestavite funkcijo [zlij(target, begin, end, list_1, list_2)], ki v del 
# tabele [target] med start in end zlije tabeli [list_1] in [list_2]. V primeru, 
# da sta elementa v obeh tabelah enaka, naj bo prvi element iz prve tabele.
# 
# Primer:
#  
#     >>> list_1 = [1,3,5,7,10]
#     >>> list_2 = [1,2,3,4,5,6,7]
#     >>> target = [-1 for _ in range(len(list_1) + len(list_2))]
#     >>> zlij(target, 0, len(target), list_1, list_2)
#     >>> target
#     [1,1,2,3,3,4,5,5,6,7,7,10]
#
###############################################################################

def merge(target, begin, end, list_1, list_2):
    i = 0
    j = 0
    while True:
        if begin + i + j == end:
            break

        if j == len(list_2):
            if i == len(list_1):
                break
            target[begin + i + j] = list_1[i]
            i += 1
            continue

        if i == len(list_1):
            if j == len(list_2):
                break
            target[begin + i + j] = list_2[j]
            j += 1
            continue

        if list_1[i] <= list_2[j]:
            target[begin + i + j] = list_1[i]
            i += 1
        else:
            target[begin + i + j] = list_2[j]
            j += 1

list_1 = [1,3,5,7,10]
list_2 = [1,2,3,4,5,6,7]
target = [-1 for i in range(len(list_1) + len(list_2))]
res = [1,1,2,3,3,4,5,5,6,7,7,10]

###############################################################################
# Tabelo želimo urediti z zlivanjem (merge sort). 
# Tabelo razdelimo na polovici, ju rekurzivno uredimo in nato zlijemo z uporabo
# funkcije [zlij].
#
# Namig: prazna tabela in tabela z enim samim elementom sta vedno urejeni.
#
# Napišite funkcijo [mergesort(a)], ki uredi tabelo [a] s pomočjo zlivanja.
# Za razliko od hitrega urejanja tu tabele lahko kopirate, zlivanje pa je 
# potrebno narediti na mestu.
#
# >>> a = [10, 4, 5, 15, 11, 3, 17, 2, 18]
# >>> mergesort(a)
# [2, 3, 4, 5, 10, 11, 15, 17, 18]
###############################################################################

def mergesort_part(a, start, end):
    if (end - start) <= 1:
        return

    n = (end - start) // 2
    mergesort_part(a, start, start + n)
    mergesort_part(a, start + n, end)
    l = a[start : start + n]
    r = a[start + n : end]
    merge(a, start, end, l, r)

def mergesort(a):
    mergesort_part(a, 0, len(a))


a = [10, 4, 5, 15, 11, 3, 17, 2, 18]
res_2 = [2, 3, 4, 5, 10, 11, 15, 17, 18]