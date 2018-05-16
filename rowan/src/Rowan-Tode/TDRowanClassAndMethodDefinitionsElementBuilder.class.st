Class {
	#name : 'TDRowanClassAndMethodDefinitionsElementBuilder',
	#superclass : 'TDClassAndMethodDefinitionsElementBuilder',
	#category : 'Rowan-Tode'
}

{ #category : 'menu actions' }
TDRowanClassAndMethodDefinitionsElementBuilder >> removeClassMenuAction: listElement selectionIndex: selectionIndex [

	| cls |
	cls := self selectedClass: listElement selectionIndex: selectionIndex.
	Rowan projectTools browser removeClassNamed: cls name asString.
	^ #'refreshView'
]
