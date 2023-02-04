package main

import (
	"fmt"

	"github.com/jroimartin/gocui"
)

// Layout ...
func Layout(g *gocui.Gui) error {
	// Get the current terminal size.
	tw, th := g.Size()
	minWidth, minHeight := 5, 5

	// Prevent the application from crashing
	if tw > minWidth && th > minHeight {
		// Main application window
		if v, err := g.SetView("Main", 0, 0, tw-1, th-2); err != nil {
			if err != gocui.ErrUnknownView {
				return err
			}
			g.SetCurrentView("Main")
			if v.Buffer() == "" {
				fmt.Fprint(v, landingPage)
			}
		}
		// Bottom info bar
		if v, err := g.SetView("Info", 0, th-2, tw-(tw/10), th); err != nil {
			if err != gocui.ErrUnknownView {
				return err
			}
			v.Frame = false
			fmt.Fprintln(v, "F1: Deník", "| F2: Hlavní kniha", "| F3: Seznam účtů")
		}
	}
	return nil
}

// inputView is that new window spawned, waiting for you to input something.
func inputView(g *gocui.Gui, v *gocui.View) error {
	// Get the current terminal size.
	tw, th := g.Size()
	// TODO: fix bad title rendering
	// EDIT: this is bug in the library
	const title = "Účet"
	if iv, err := g.SetView("Input", tw/2-30, th/2, tw/2+30, th/2+2); err != nil {
		if err != gocui.ErrUnknownView {
			return err
		}
		iv.Title = title
		iv.Editable = true
		g.Cursor = true
		if _, err := g.SetCurrentView("Input"); err != nil {
			return err
		}
	}
	return nil
}
