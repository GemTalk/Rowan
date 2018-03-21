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
	block := [ 
	(Rowan image loadedProjects
		collect: [ :each | 
			TDRowanProjectDefinition new
				projectName: each name;
				yourself ])
		sortWithBlock: [ :a :b | a projectName <= b projectName ] ].
	^ (TDRowanProjectBrowser new
		topez: topez;
		projectBlock: block;
		projectTool: self;
		windowLabel: 'Rowan Projects';
		yourself) open
]

{ #category : 'browsing' }
RwPrjListTool >> projectBrowseProjectPackagesFor: loadedProject [

	self halt
]
