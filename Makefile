FILE = veza
NAME = flp23-log
ARNAME = flp-log-xgazdi04

.PHONY: all

all: $(FILE)

$(FILE): $(FILE).pl
		swipl -q -g start --stack_limit=16g -o $(NAME) -c $(FILE).pl

clean:
		rm -f $(NAME) $(ARNAME).zip

zip:
		zip $(ARNAME).zip $(FILE).pl README Makefile
