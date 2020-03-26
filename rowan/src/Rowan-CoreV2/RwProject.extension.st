Extension { #name : 'RwProject' }

{ #category : '*rowan-corev2' }
RwProject >> _repositoryRoot: aFileReference [
	self _loadedProject resolvedProject repositoryRoot: aFileReference
]

{ #category : '*rowan-corev2' }
RwProject >> componentNames [
	^ self _loadedProject componentNames
]

{ #category : '*rowan-corev2' }
RwProject >> exportLoadSpecification [
	^ self _loadedProject asDefinition exportLoadSpecification
]

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
RwProject >> repositoryRoot [
	^ self _loadedProject repositoryRoot
]

{ #category : '*rowan-corev2' }
RwProject >> repositoryRoot: aFileReferenceOrString [
	| resolvedProject originalRepositoryRoot |
	originalRepositoryRoot := self repositoryRoot.
	self requiredProjects
		do: [ :project | 
			project repositoryRoot = originalRepositoryRoot
				ifTrue: [ 
					"only embedded required projects should have their repository root swapped out"
					project _repositoryRoot: aFileReferenceOrString ] ].
	resolvedProject := self asDefinition.
	resolvedProject repositoryRoot: aFileReferenceOrString.
	^ resolvedProject loadProjectSet
]

{ #category : '*rowan-corev2' }
RwProject >> requiredProjects [
	"return list of dependent project names"

	"https://github.com/GemTalk/Rowan/issues/571 is addressed"

	| requiredProjectNames theComponents |
	theComponents := self loadedComponents. "use loadedComponents, not _loadedProjectDefinition - loadedComponents kept up to date"
	requiredProjectNames := Set new.
	theComponents
		conditionalComponentsStartingWith: self componentNames
		platformConditionalAttributes: self platformConditionalAttributes
		do: [ :aComponent | requiredProjectNames addAll: aComponent projectNames ].
	^ requiredProjectNames asArray
		collect: [ :projectName | Rowan projectNamed: projectName ]
]

{ #category : '*rowan-corev2' }
RwProject >> topLevelComponentNames [
	^ self _loadedProject resolvedProject _loadSpecification componentNames
]
