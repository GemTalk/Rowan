Class {
	#name : 'RwAbstractUnloadedProject',
	#superclass : 'RwAbstractProject',
	#instVars : [
		'resolvedProject'
	],
	#category : 'Rowan-Core'
}

{ #category : 'accessing' }
RwAbstractUnloadedProject >> _resolvedProject [
	^ resolvedProject
]

{ #category : 'accessing' }
RwAbstractUnloadedProject >> gemstoneSetDefaultSymbolDictNameTo: symbolDictName [
	self _resolvedProject gemstoneSetDefaultSymbolDictNameTo: symbolDictName
]

{ #category : 'accessing' }
RwAbstractUnloadedProject >> projectsHome: aProjectHomeReferenceOrString [
	self _resolvedProject projectsHome: aProjectHomeReferenceOrString
]
