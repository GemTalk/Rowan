accessing
version

	| vers |
	vers :=  self versionString.
	vers isEmpty ifTrue: [ ^ nil ].
	^ vers asRwSemanticVersionNumber