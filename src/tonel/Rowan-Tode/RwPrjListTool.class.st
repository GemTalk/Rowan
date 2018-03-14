Class {
	#name : 'RwPrjListTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tode'
}

{ #category : 'man page' }
RwPrjListTool >> manPage [

	^ TDManPage
		commandName: 'list'
		sourceMethod: self class name asString , '>>manPage'
		fromString:
			'NAME
  prj list - List Rowan projects
SYNOPSIS
  prj list
DESCRIPTION
  The `prj list` command brings up a window on the list of Rowan projects. 

EXAMPLES
  prj list 
'
]

{ #category : 'cypress api' }
RwPrjListTool >> openProjectList: topez [

	"the Cypress project list is using class categories and stylized method protocol for package 
	membership. Once tODE supports first class projects and package browsing, this form will no 
	longer be needed ... Cypress/Rowan projects are managed separately from standard Rowan projects"

	| block |
	block := [ Rowan image loadedProjects sortWithBlock: [ :a :b | a name <= b name ] ].
	^ (TDRowanProjectBrowser new
		topez: topez;
		projectBlock: block;
		windowLabel: 'Rowan Projects';
		yourself) open
]
