Class {
	#name : 'TDRowanPackageBrowser',
	#superclass : 'TDAbstractRowanToolBuilder',
	#instVars : [
		'packages',
		'packageBlock',
		'selectedPackageName',
		'projectDef'
	],
	#category : 'Rowan-Tode'
}

{ #category : 'menu actions' }
TDRowanPackageBrowser >> addClassMenuAction: listElement selectionIndex: selectionIndex [

	| classDefinitionString packageDef classDefTemplate |
	selectionIndex = 0
		ifTrue: [ ^ false ].
	packageDef := self packages at: selectionIndex.
	classDefTemplate := Rowan projectTools browser
		classCreationTemplateForSubclassOf: 'Object'
		className: 'newClass'
		category: packageDef packageName.
	classDefinitionString := (GsMultiLineTextInteraction
		prompt: 'Define class: '
		template: classDefTemplate) signal.
	classDefinitionString
		ifNil: [ 
			Transcript
				cr;
				show: 'add class aborted...'.
			^ false ].
	listElement topez createClassFrom: classDefinitionString.
	^ #'refreshView'
]

{ #category : 'menu actions' }
TDRowanPackageBrowser >> browsePackageClassesMenuAction: listElement selectionIndex: selectionIndex [

	| packageDef block |
	selectionIndex = 0
		ifTrue: [ ^ false ].
	packageDef := self packages at: selectionIndex.
	block := [ packageDef classDefinitions ].
	^ (listElement topez toolInstanceFor: 'browse')
		browseRowanClassesBlock: block
		windowName: #'rowanClasses'
		location: #'app4'
		label: 'Classes in ' , packageDef packageName
]

{ #category : 'tools' }
TDRowanPackageBrowser >> clientList: miniTool listElement: listElement [

	| labels selectedPackageIndex packageList count |
	theMiniTool := miniTool.
	packages := packageBlock value.
	labels := {}.
	selectedPackageIndex := nil.
	count := 1.
	packages
		do: [ :packageDef | 
			| packageName |
			packageName := packageDef packageName.
			selectedPackageName = packageName
				ifTrue: [ selectedPackageIndex := count ].
			count := count + 1.
			labels
				add:
					{packageName.
					{(TextEmphasis bold)}} ].
	packageList := labels
		collect: [ :ar | 
			| nm textAttributes |
			nm := ar at: 1.
			textAttributes := ar at: 2.

			Text string: nm attributes: textAttributes ].
	^ packageList -> selectedPackageIndex
]

{ #category : 'tools' }
TDRowanPackageBrowser >> itemSelected: miniTool listElement: listElement selectedIndex: index shiftPressed: shiftPressed [

	selectedPackageName := nil.
	(index == 0 or: [ index > self packages size ])
		ifTrue: [ ^ false ].
	self browsePackageClassesMenuAction: listElement selectionIndex: index.
	^ true
]

{ #category : 'menu action specs' }
TDRowanPackageBrowser >> menuActionSpec: miniTool [

	theMiniTool := miniTool.
	^ {#('new package' nil #'newPackageMenuAction:selectionIndex:').
	#('add class to package' nil #'addClassMenuAction:selectionIndex:').
	#('remove package' nil #'removePackageMenuAction:selectionIndex:')} , {#('-')}
		, self standardMenuActionSpec
]

{ #category : 'menu actions' }
TDRowanPackageBrowser >> newPackageMenuAction: listElement selectionIndex: selectionIndex [

	| packageName |
	packageName := (GsTextInteraction
		prompt: 'Enter the package name'
		template: self projectDef projectName , '-Core') signal.
	packageName
		ifNil: [ 
			Transcript
				cr;
				show: 'New package aborted...'.
			^ false ].
	Rowan projectTools browser
		addPackageNamed: packageName
		toProjectNamed: self projectDef projectName.
	^ #'refreshView'
]

{ #category : 'accessing' }
TDRowanPackageBrowser >> packageBlock [

   ^packageBlock

]

{ #category : 'accessing' }
TDRowanPackageBrowser >> packageBlock: anObject [

   packageBlock := anObject

]

{ #category : 'accessing' }
TDRowanPackageBrowser >> packages [

   ^packages

]

{ #category : 'accessing' }
TDRowanPackageBrowser >> packages: anObject [

   packages := anObject

]

{ #category : 'accessing' }
TDRowanPackageBrowser >> projectDef [

	^ projectDef
]

{ #category : 'accessing' }
TDRowanPackageBrowser >> projectDef: anObject [

	projectDef := anObject
]

{ #category : 'menu actions' }
TDRowanPackageBrowser >> removePackageMenuAction: listElement selectionIndex: selectionIndex [

	| classDefinitionString packageDef classDefTemplate |
	selectionIndex = 0
		ifTrue: [ ^ false ].
	packageDef := self packages at: selectionIndex.
	Rowan projectTools browser removePackageNamed: packageDef packageName.
	^ #'refreshView'
]

{ #category : 'accessing' }
TDRowanPackageBrowser >> windowLocation [

	windowLocation ifNil: [ windowLocation := #'app3' ].
	^ windowLocation
]

{ #category : 'accessing' }
TDRowanPackageBrowser >> windowName [

	windowName ifNil: [ windowName := #'rowanPackages' ].
	^ windowName
]
