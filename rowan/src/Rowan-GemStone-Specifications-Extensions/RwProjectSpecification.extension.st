Extension { #name : 'RowanProjectSpecification' }

{ #category : '*rowan-gemstone-specifications-extensions' }
RwProjectSpecification >> platformSpec [

   ^platformSpec ifNil: [ 
		platformSpec := Dictionary new
			at: 'gemstone' put: RwGemStoneSpecification new;
			yourself ].

]

{ #category : '*rowan-gemstone-specifications-extensions' }
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
	defaultConfigurationNames := #('Default').
	defaultGroupNames := #('default').

]

