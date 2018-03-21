Class {
	#name : 'TDRowanPackageBrowser',
	#superclass : 'TDAbstractRowanToolBuilder',
	#instVars : [
		'packages',
		'packageBlock',
		'selectedPackageName'
	],
	#category : 'Rowan-Tode'
}

{ #category : 'menu actions' }
TDRowanPackageBrowser >> browsePackageClassesMenuAction: listElement selectionIndex: selectionIndex [

	| packageDef block |
	selectionIndex = 0
		ifTrue: [ ^ false ].
	packageDef := self packages at: selectionIndex.
	block := [ packageDef classDefinitions ].
	^ (listElement topez toolInstanceFor: 'browse')
		browseClassesBlock: block
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
TDRowanPackageBrowser >> windowLocation [

	windowLocation ifNil: [ windowLocation := #'app3' ].
	^ windowLocation
]

{ #category : 'accessing' }
TDRowanPackageBrowser >> windowName [

	windowName ifNil: [ windowName := #'rowanPackages' ].
	^ windowName
]
