run:	exe
	./exe

exe:	gantt.pl
	swipl --goal=main --stand_alone=true -o exe -c gantt.pl

clean: 
	rm -rf exe 2>/dev/null

