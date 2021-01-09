.PHONY: build run dev

build: ; stack build

run: ; stack build; stack exec fem-exe

dev: ; ghcid --command 'stack ghci app/Main.hs'
