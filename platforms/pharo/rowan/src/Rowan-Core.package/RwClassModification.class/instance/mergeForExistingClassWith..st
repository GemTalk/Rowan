merging
mergeForExistingClassWith: anExistingClassModification

	"merge aClassModification modifictions into the receiver"

	anExistingClassModification propertiesModification isEmpty
		ifFalse: [
			self propertiesModification isEmpty
				ifFalse: ["the properties modification for the receiver can be safely ignored" ].
			propertiesModification := anExistingClassModification propertiesModification copy ].
	instanceMethodsModification
		mergeForExistingClassWith: anExistingClassModification instanceMethodsModification.
	classMethodsModification
		mergeForExistingClassWith: anExistingClassModification classMethodsModification.
	after := anExistingClassModification after
