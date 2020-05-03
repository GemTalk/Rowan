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
RwAbstractUnloadedProject >> packageNames [
	^self _resolvedProject packageNames
]

{ #category : 'accessing' }
RwAbstractUnloadedProject >> projectsHome [
	^self _resolvedProject projectsHome
]

{ #category : 'accessing' }
RwAbstractUnloadedProject >> projectsHome: aProjectHomeReferenceOrString [
	self _resolvedProject projectsHome: aProjectHomeReferenceOrString
]

{ #category : 'testing' }
RwAbstractUnloadedProject >> useGit [

	^self _resolvedProject useGit
]
