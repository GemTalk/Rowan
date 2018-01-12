"
No class-specific documentation for CypGsClassVariableChangePatch, hierarchy is: 
Object
  CypGsPatch
    CypGsClassVariableChangePatch( oldClassDefinition newClassDefinition)

"
Class {
	#name : 'RwGsClassVariableChangePatch',
	#superclass : 'RwGsClassPatch',
	#category : 'Rowan-GemStone'
}

{ #category : 'actions' }
RwGsClassVariableChangePatch >> addToNewClassesByName: aDictionary [

	"noop"

	
]

{ #category : 'actions' }
RwGsClassVariableChangePatch >> createClassFor: aPatchSet [

	"we're using createClassFor: to apply the class variable modifications. not expected to 
		create a new class version"

	| oldClassVersion newClassVersion |
	oldClassVersion := self oldClassVersion.
	newClassVersion := super createClassFor: aPatchSet.
	oldClassVersion == newClassVersion
		ifFalse: [ 
			self
				error:
					'internal error - unexpected new class version for class variable modification' ].
	^ oldClassVersion
]
