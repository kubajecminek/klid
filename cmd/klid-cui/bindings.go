package main

import (
	"log"

	"github.com/jroimartin/gocui"
)

// Binding type improves readability
type Binding struct {
	viewname string
	key      interface{}
	mod      gocui.Modifier
	handler  func(*gocui.Gui, *gocui.View) error
}

// TODO: implement scrolling
var bindings = []Binding{
	{
		viewname: "",
		key:      gocui.KeyCtrlC,
		mod:      gocui.ModNone,
		handler:  quit,
	},
	{
		viewname: "Main",
		key:      gocui.KeyEsc,
		mod:      gocui.ModNone,
		handler:  quit,
	},
	{
		viewname: "Main",
		key:      gocui.KeyF1,
		mod:      gocui.ModNone,
		handler:  journalHandler,
	},
	{
		viewname: "Main",
		key:      gocui.KeyF2,
		mod:      gocui.ModNone,
		handler:  inputView,
	},
	{
		viewname: "Main",
		key:      gocui.KeyF3,
		mod:      gocui.ModNone,
		handler:  accountsHandler,
	},
	{
		viewname: "Input",
		key:      gocui.KeyEsc,
		mod:      gocui.ModNone,
		handler:  deleteView,
	},
	{
		viewname: "Input",
		key:      gocui.KeyEnter,
		mod:      gocui.ModNone,
		handler:  bookHandler,
	},
}

func setKeyBindings(g *gocui.Gui) error {
	for _, bind := range bindings {
		if err := g.SetKeybinding(
			bind.viewname,
			bind.key,
			bind.mod,
			bind.handler,
		); err != nil {
			log.Println("Could not set key binding: ", err)
		}
	}
	return nil
}

func quit(g *gocui.Gui, v *gocui.View) error {
	return gocui.ErrQuit
}
