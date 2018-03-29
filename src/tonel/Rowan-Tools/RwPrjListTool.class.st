Class {
	#name : 'RwPrjListTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools'
}

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
