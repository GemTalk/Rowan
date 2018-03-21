Extension { #name : 'TDShellTool' }

{ #category : '*rowan-tode' }
TDShellTool >> browseClassesBlock: classDefsBlock windowName: windowName location: windowLocation label: windowLabel [

	| classDefs spec |
	classDefs := classDefsBlock value.
	classDefs isEmpty
		ifTrue: [ 
			self inform: 'No matching definitions found'.
			^ classDefs ].
	spec := (TDEditorSpec topez: topez editorAspect: #'definitionList')
		windowLabel: windowLabel;
		windowName: windowName;
		windowLocation: windowLocation;
		yourself.
	self options
		at: 'staticList'
		ifPresent: [ :ignored | classDefs editUsing: spec ]
		ifAbsent: [ classDefs editUsing: spec objectBlock: classDefsBlock ].
	^ classDefs
]

{ #category : '*rowan-tode' }
TDShellTool >> browseRowanClassHierarchy: aClass label: windowLabel [

	^ (aClass asTDClassDefinition
		appEnv: self applicationEnvironment;
		yourself)
		editUsing:
			((TDEditorSpec topez: topez editorAspect: #'rowanHierarchyList')
				windowName: #'hierarchy';
				windowLabel: windowLabel;
				yourself)
]
