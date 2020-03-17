Extension { #name : 'RwProjectSpecification' }

{ #category : '*rowan-gemstone-specificationsv1' }
RwProjectSpecification >> defaultSymbolDictName: aString [


	(self platformSpec at: 'gemstone') defaultSymbolDictName: aString
]

{ #category : '*rowan-gemstone-specificationsv1' }
RwProjectSpecification >> initialize [

	super initialize.
	comment := ''.
	configsPath := 'rowan/configs'.
	repoPath := 'rowan/src'.
	specsPath := 'rowan/specs'.
	platformSpec := Dictionary new
		at: 'gemstone' put: RwGemStoneSpecification new;
		yourself.
	repoSpec := RwGitRepositorySpecification new.
	version := self currentVersion.
	defaultConfigurationNames := {'Default'}.
	defaultGroupNames := {'default'}.
]

{ #category : '*rowan-gemstone-specificationsv1' }
RwProjectSpecification >> platformSpec [

   ^platformSpec ifNil: [ 
		platformSpec := Dictionary new
			at: 'gemstone' put: RwGemStoneSpecification new;
			yourself ].

]