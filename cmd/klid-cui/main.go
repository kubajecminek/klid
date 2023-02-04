package main

import (
	"log"

	"github.com/jroimartin/gocui"
)

func main() {
	g, err := gocui.NewGui(gocui.OutputNormal)
	if err != nil {
		log.Println("Failed to create a GUI:", err)
		return
	}
	defer g.Close()

	// Set GUI managers
	g.SetManagerFunc(Layout)
	g.InputEsc = true

	// ...key bindings
	setKeyBindings(g)

	g.Highlight = true
	g.SelFgColor = gocui.ColorGreen

	if err := g.MainLoop(); err != nil && err != gocui.ErrQuit {
		// handle error
	}
}

const landingPage = `
## Logo

	    __ __ __    ________
	   / //_// /   /  _/ __ \
	  / ,<  / /    / // / / /
	 / /| |/ /____/ // /_/ /
	/_/ |_/_____/___/_____/

    Autor:   Jakub
    Verze:   0.0.0
    Licence: GPLv3


## Použití

    | Klávesa | Funkce            |
    |---------+-------------------|
    | F1      | Účetní deník      |
    | F2      | Hlavní kniha účtů |
    | F3      | Seznam účtů       |
    | Esc     | Vypnutí programu  |
    | Ctrl+c  | Vypnutí programu  |


Pro kompletní návod navštivte https://www.github.com/1d376/klid/docs/MANUAL.md`
