package main

// MyAppHelpTemplate is default template translated to Czech
var MyAppHelpTemplate = `NÁZEV:
	{{.Name}}{{if .Usage}} - {{.Usage}}{{end}}
 
 POUŽITÍ:
	{{if .UsageText}}{{.UsageText}}{{else}}{{.HelpName}} {{if .VisibleFlags}}[global options]{{end}}{{if .Commands}} command [command options]{{end}} {{if .ArgsUsage}}{{.ArgsUsage}}{{else}}[arguments...]{{end}}{{end}}{{if .Version}}{{if not .HideVersion}}
 
 VERZE:
	{{.Version}}{{end}}{{end}}{{if .Description}}
 
 POPIS:
	{{.Description}}{{end}}{{if len .Authors}}
 
 AUTOR{{with $length := len .Authors}}{{if ne 1 $length}}S{{end}}{{end}}:
	{{range $index, $author := .Authors}}{{if $index}}
	{{end}}{{$author}}{{end}}{{end}}{{if .VisibleCommands}}
 
 PŘÍKAZY:{{range .VisibleCategories}}{{if .Name}}
	{{.Name}}:{{range .VisibleCommands}}
	  {{join .Names ", "}}{{"\t"}}{{.Usage}}{{end}}{{else}}{{range .VisibleCommands}}
	{{join .Names ", "}}{{"\t"}}{{.Usage}}{{end}}{{end}}{{end}}{{end}}{{if .VisibleFlags}}
 
 COPYRIGHT:
	{{.Copyright}}{{end}}
`

// MyCommandHelpTemplate is default template translated to Czech
var MyCommandHelpTemplate = `NÁZEV:
   {{.HelpName}} - {{.Usage}}

POUŽITÍ:
   {{if .UsageText}}{{.UsageText}}{{else}}{{.HelpName}}{{if .VisibleFlags}} [command options]{{end}} {{if .ArgsUsage}}{{.ArgsUsage}}{{else}}[arguments...]{{end}}{{end}}{{if .Category}}

KATEGORIE:
   {{.Category}}{{end}}{{if .Description}}

POPIS:
   {{.Description}}{{end}}{{if .VisibleFlags}}

MOŽNOSTI:
   {{range .VisibleFlags}}{{.}}
   {{end}}{{end}}
`
