Class {
	#name : 'TDRowanClassHierarchyListElementBuilder',
	#superclass : 'TDClassicClassHierarchyListElementBuilder',
	#category : 'Rowan-Tode'
}

{ #category : 'accessing' }
TDRowanClassHierarchyListElementBuilder >> classDefinitionList [

	| image |
	image := Rowan image.
	^ super classDefinitionList
		collect: [ :classDef | 
			| rowanClassDef |
			rowanClassDef := TDRowanLoadedClassDefinition new
				className: classDef className.
			(object className = classDef className and: [ object loadedClass notNil ])
				ifTrue: [ 
					image
						loadedClassNamed: classDef className
						ifFound: [ :loadedClass | rowanClassDef loadedClass: loadedClass ]
						ifAbsent: [  ] ].
			rowanClassDef loadedClassExtensions
				addAll:
					(image
						loadedClassExtensionsNamed: classDef className
						ifFound: [ :loadedClassExtensions | loadedClassExtensions ]
						ifAbsent: [ #() ]).
			rowanClassDef ]
]

{ #category : 'tools' }
TDRowanClassHierarchyListElementBuilder >> itemSelected: listElement at: index shiftPressed: shiftPressed [

	| aspect classDef itemAttribute classDefClientElement stonArray buttonBarClientElement classProtocolClientElement |
	index ifNil: [ ^ nil ].
	index > listElement itemCache size
		ifTrue: [ ^ nil ].
	itemAttribute := listElement itemCache at: index.
	classDef := itemAttribute object.
	aspect := itemAttribute aspect.
	TDClassDefinition selectClassNamed: classDef className topez: listElement topez.
	classDefClientElement := classDef
		clientElementUsing:
			((TDEditorSpec topez: listElement topez editorAspect: #'rowanClassDefinition')
				windowName: #'classicClassDef';
				windowLocation: #'classicClassDef';
				yourself).
	buttonBarClientElement := {classDef.
	{{#'instance'.
	#'browseInstanceProtocol:'.
	'instance'.
	'view instance-side methods'.
	true}.
	{#'class'.
	#'browseClassProtocol:'.
	'class'.
	'view class-side methods'.
	false}}}
		clientElementUsing:
			((TDEditorSpec topez: listElement topez editorAspect: #'buttonBar')
				windowName: #'classicClassButtonBar';
				windowLocation: #'classicClassButtonBar';
				yourself).
	classProtocolClientElement := classDef
		clientElementUsing:
			((TDEditorSpec topez: listElement topez editorAspect: #'rowanProtocolList')
				windowName: #'classicProtocolList';
				windowLocation: #'classicProtocol';
				selectionIndex: 1;
				yourself).
	stonArray := {#'editStonElement:'.
	classDefClientElement.
	#'editStonElement:'.
	classProtocolClientElement.
	#'editStonElement:'.
	buttonBarClientElement}.
	(theWindowName ~~ #'hierarchy' or: [ selectedClass ~~ classDef theClass ])
		ifTrue: [ 
			| hierarchyClientElement |
			hierarchyClientElement := classDef
				clientElementUsing:
					((TDEditorSpec topez: listElement topez editorAspect: #'classicHierarchyList')
						windowName: #'hierarchy';
						yourself).
			selectedClass := classDef theClass.
			stonArray
				addAll:
					{#'editStonElement:'.
					hierarchyClientElement} ].
	stonArray addAll: (self enableDisableMenuItems: classDef).
	^ listElement objectSerializer toString: stonArray
]
