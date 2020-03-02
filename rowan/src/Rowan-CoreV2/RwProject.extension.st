Extension { #name : 'RwProject' }

{ #category : '*rowan-corev2' }
RwProject >> loadedComponents [
	^ self _loadedProject loadedComponentDefinitions
]

{ #category : '*rowan-corev2' }
RwProject >> platformConditionalAttributes [
	"Answer the platformConditionalAttributes used to load the project"

	^ self _loadedProject platformConditionalAttributes
]

{ #category : '*rowan-corev2' }
RwProject >> projectsHome [
	^ self _loadedProject resolvedProject projectsHome
]

{ #category : '*rowan-corev2' }
RwProject >> projectsHome: aFileReference [
	self _loadedProject resolvedProject projectsHome: aFileReference
]

{ #category : '*rowan-corev2' }
RwProject >> topLevelComponentNames [
	^ self _loadedProject resolvedProject _loadSpecification componentNames
]
