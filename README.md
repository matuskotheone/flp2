# Babylonska Veža Implementácia v Jazyku Prolog
Autor: Matus Gazdik (xgazdi04)
2023/2024 
## Popis
Implementacia hry Babylonska veza v jazyku Prolog. Riesenie je implementovane pomocou ITS (Iterative Deepening Search)
algoritmu, ktory najde vzdy optimalne riesenie pre danu poziciu veze.
po precitani zaciatocneho stavu zo vstupu, su zistene dimenzie veze ktora je reprezentovana ako dvojrozmerna matica
dvojic kde prve pismeno znaci riadok a druhe cislo stlpca. Nasledne je pouzita solve_ids , ktora vola pouziva get moves s inkrementujucov sa maximalnou hlbkou prehladavania az kym nie je najdeny cielovy stav alebo maximalna hlbka prehladavania nie je dosiahnuta. Pri kazdej iteracii sa pouziva move ktora pre danu vezu zisti mozne tahy a nasledne sa zavola solve_ids s novymi stavmi. Po najdeni cesty k rieseniu je zavolana funkcia print_solution ktora vypise postupnost tahov k rieseniu.
## Navod na pouzitie
Program je mozne prelozit pomocou prikazu make a spustit pomocou ./flp23-log 
Pre testovanie su prilozene 3 jednoduche testy, ktore su pomenovane test1 - test3 a su spustitelne pomocou prikazu 
```bash
./flp23-log < test1.txt
```

## Obmedzenia
Program je obmedzeny maximalnou hlbkou prehladavania. Pre narocnejsie veze tento program trva dlhsie na vykonanie.
