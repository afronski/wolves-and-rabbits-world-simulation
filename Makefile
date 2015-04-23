all:
	erl -make

clean:
	rm -rf ebin/*.beam

.PHONY: all clean
