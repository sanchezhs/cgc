CC=gcc
CFLAGS=-Wall -Wextra
LDFLAGS=-lm
SOURCE=main.c
OUTPUT=main

$(OUTPUT): $(SOURCE)
	$(CC) $(CFLAGS) -o $(OUTPUT) $(SOURCE) $(LDFLAGS)

clean:
	rm -rf $(OUTPUT)

