
# FLP - funkcionálny projetk 
#Simplify-bkg

Program odstraňuje zbytočné symboly zo zadanej bezkontextovej gramatiky. 

##Preklad a spustenie

Preklad pomocou make.

Spustenie:
 - prepínač „-i“ : make run
 - prepínač „-1“ : make run1
 - prepínač „-2“ : make run 2  

##Obsah archívu


###simplifi-bkg.hs
Spracováva vstupné argumenty na základe, ktorých spustí prísušný algoritmus. Vypisuje vzniknuté chyby alebo výslednú gramatiku.

###BKG.hs a BKGParser.hs 
Súbor BKG.hs obsahuje datové štruktúry reprezentujúce bezkontextovú gramatiku. Tiež obsajuje štruktúry pre konfiguráciu behu programu.
BKGParser.hs spracováva vstupnú gramatiku do datových štruktúr, s ktorými pracujú algoritmy.

###Logic.hs 
V súbore sú implementvané algoritmy potrebné pre odstránenie zbytočných pravidiel z bezkontextovej gramatiky, podla Algoritmu 4.3 studijný opory TIN . 
    
 
