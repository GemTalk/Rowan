merging
mergeForNewClassVersionWith: aClassModification

	"merge aClassModification modifictions into the receiver"

	aClassModification propertiesModification isEmpty
		ifFalse: [ 
			self
				error:
					'internal error - an incoming class modification is not expected to have properties when a new class version is involved' ].
	instanceMethodsModification
		mergeForNewClassVersionWith: aClassModification instanceMethodsModification.
	classMethodsModification
		mergeForNewClassVersionWith: aClassModification classMethodsModification
