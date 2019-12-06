Class {
	#name : 'RwLoadSpecificationV2',
	#superclass : 'RwSpecification',
	#instVars : [
		'specName',
		'projectAlias',
		'comment',
		'componentNames',
		'groupNames'
	],
	#category : 'Rowan-Components'
}

{ #category : 'private' }
RwLoadSpecificationV2 >> _validate [
	"ensure that the data structures within the receiver contain valid information:
		1. platform implementation is responsible for validating platform structures"

	#(#'componentNames' #'groupNames' #'specName')
		do: [ :messageName | 
			(self perform: messageName)
				ifNil: [ 
					Error
						signal:
							'The instance variable ' , messageName asString printString , ' cannot be nil' ] ].
	^ true
]

{ #category : 'accessing' }
RwLoadSpecificationV2 >> comment [
	^ comment ifNil: [ ^ '' ]
]

{ #category : 'accessing' }
RwLoadSpecificationV2 >> comment: aString [
	comment := aString
]

{ #category : 'accessing' }
RwLoadSpecificationV2 >> componentNames [
	^ componentNames
]

{ #category : 'accessing' }
RwLoadSpecificationV2 >> componentNames: anArray [
	componentNames := anArray asArray
]

{ #category : 'accessing' }
RwLoadSpecificationV2 >> groupNames [
	^ groupNames
]

{ #category : 'accessing' }
RwLoadSpecificationV2 >> groupNames: anArray [
	groupNames := anArray asArray
]

{ #category : 'initialization' }
RwLoadSpecificationV2 >> initialize [
	super initialize.
	componentNames := {}.
	groupNames := {'core'.
	'tests'}.
]

{ #category : 'copying' }
RwLoadSpecificationV2 >> postCopy [
	"don't forget to copy any compound state objects"

	super postCopy.
	componentNames := componentNames copy.
	groupNames := groupNames copy
]

{ #category : 'accessing' }
RwLoadSpecificationV2 >> projectAlias [
	"project alias is used as the name of the root directory for the project ... necessary if the project is 
		embedded in another project's git repository or you want use a non-default directory on disk"

	^ projectAlias ifNil: [ self projectName ]
]

{ #category : 'accessing' }
RwLoadSpecificationV2 >> projectAlias: aString [
	"project alias is used as the name of the root directory for the project ... necessary if the project is 
		embedded in another project's git repository or you want use a non-default directory on disk"

	projectAlias := aString
]

{ #category : 'accessing' }
RwLoadSpecificationV2 >> specName [

	^ specName ifNil: [ self projectAlias ]
]

{ #category : 'accessing' }
RwLoadSpecificationV2 >> specName: aString [
	specName := aString
]
