# ProjektJP
Projekt 1 na języki programowania
## Kompilowanie
Do kompilacji będą potrzebne programy `ocamllex` i `menhir`, które można zainstalować poleceniem `opam install ocamllex menhir`
Aby skompilować wystarczy wpisać komendę `make`. Program wynikowy będzie nosił nazwę `f`.
Aby ponownie skompilować najlepiej wpisać `make clean`, a następnie `make` lub `make install`, które robi te rzeczy automatycznie

## Działanie
### Wejście
Pierwszym i jedynym argumentem programu jest ścieżka do pliku wejściowego. W przypadku nie podania żadnego argumentu domyślnym plikiem będzie `input.f`.

Program ma dwie możliwości działania - redukcję termu lub porównanie termów. 
### Redukcja
Aby zredukować term należy podać pojedynczy term do pliku wejściowego. 
### Porównanie
Aby porównać termy należy podać oba termy w pliku wejściowym oddzielone przecinkiem

### Wynik
Wynik działania programu znajdzie się w pliku `output.f`.
W przypadku redukcji będzie to zredukowany term.
W przypadku porównania pojedyncze słowo `true` lub `false` oznaczające wynik porównania.

## Strukura
Pliki `lexer.mll` i `parser.mly` zawierają odpowiednio lekser i parser. 
Plik `syntax.ml` zawiera typy lambda wyrażeń oraz printery
Plik `support.ml` jest plikiem z TaPLa, do drukowania błędów parsowania.
Plik `core.ml` zawiera całą logikę programu:
* funkcja `unfold` odcukrza term
* funkcja `krivine` jest maszyną Krivine'a do redukcji termów
* funkcja `normalize` iteracyjne normalizuje termy
* funkcja `beta_compare` iteracyjnie (przy użyciu `normalize`) normalizuje termy, przy każdej iteracji sprawdzając, czy już można stwierdzić, że nie są równe
Plik `main.ml` służy do obsługi plików. Uruchamia też całą logikę programu.


