Class {
	#name : 'RwSimpleProjectLoadComponentV2',
	#superclass : 'RwBasicProjectLoadComponentV2',
	#instVars : [
		'condition',
		'groupName',
		'packageNames',
		'componentNames',
		'projectNames'
	],
	#category : 'Rowan-ComponentsV2'
}

{ #category : 'validation' }
RwSimpleProjectLoadComponentV2 >> _validatedPackageNames [
	"answer the validated set of package names"

	^ self packageNames asSet
]

{ #category : 'comparing' }
RwSimpleProjectLoadComponentV2 >> = aRwSimpleProjectLoadComponentV2 [
	(aRwSimpleProjectLoadComponentV2 isKindOf: self class)
		ifFalse: [ ^ false ].
	^ ((((((self name = aRwSimpleProjectLoadComponentV2 name
		and: [ self comment = aRwSimpleProjectLoadComponentV2 comment ])
		and: [ 
			self conditionalPackageMapSpecs
				= aRwSimpleProjectLoadComponentV2 conditionalPackageMapSpecs ])
		and: [ self condition = aRwSimpleProjectLoadComponentV2 condition ])
		and: [ self groupName = aRwSimpleProjectLoadComponentV2 groupName ])
		and: [ self packageNames = aRwSimpleProjectLoadComponentV2 packageNames ])
		and: [ self componentNames = aRwSimpleProjectLoadComponentV2 componentNames ])
		and: [ self projectNames = aRwSimpleProjectLoadComponentV2 projectNames ]
]

{ #category : 'visiting' }
RwSimpleProjectLoadComponentV2 >> acceptVisitor: aVisitor [
	^ aVisitor visitSimpleProjectLoadComponent: self
]

{ #category : 'accessing' }
RwSimpleProjectLoadComponentV2 >> addComponentNamed: aComponentName [ 
	self componentNames add: aComponentName.
	componentNames := componentNames asSet asArray
]

{ #category : 'accessing' }
RwSimpleProjectLoadComponentV2 >> addPackageNames: aPackageNames [
	"add packages to default conditional/group if applicable"

	self packageNames addAll: aPackageNames.
	packageNames := packageNames asSet asArray
]

{ #category : 'accessing' }
RwSimpleProjectLoadComponentV2 >> addProjectNamed: aProjectName [
	self projectNames add: aProjectName.
	projectNames := projectNames asSet asArray
]

{ #category : 'accessing' }
RwSimpleProjectLoadComponentV2 >> componentNames [

	^ componentNames
]

{ #category : 'accessing' }
RwSimpleProjectLoadComponentV2 >> condition [
	^condition
]

{ #category : 'accessing' }
RwSimpleProjectLoadComponentV2 >> condition: object [
	condition := object
]

{ #category : 'accessing' }
RwSimpleProjectLoadComponentV2 >> groupName [
	^groupName
]

{ #category : 'accessing' }
RwSimpleProjectLoadComponentV2 >> groupName: object [
	groupName := object
]

{ #category : 'comparing' }
RwSimpleProjectLoadComponentV2 >> hash [
	| hashValue |
	hashValue := self name hash.
	hashValue := hashValue bitXor: self comment.
	hashValue := hashValue bitXor: self conditionalPackageMapSpecs.
	hashValue := hashValue bitXor: self groupName.
	hashValue := hashValue bitXor: self packageNames.
	hashValue := hashValue bitXor: self componentNames.
	hashValue := hashValue bitXor: self projectNames.
	^ hashValue
]

{ #category : 'initialization' }
RwSimpleProjectLoadComponentV2 >> initialize [
	super initialize.
	condition := 'common'.
	groupName := 'core'.
	packageNames := {}.
	componentNames := {}.
	projectNames := {}
]

{ #category : 'ston' }
RwSimpleProjectLoadComponentV2 >> instVarNamesInOrderForSton [
	^ #(#'name' #'projectName' #'comment' #'condition' #'groupName' #'packageNames' #'componentNames' #'projectNames' #'conditionalPackageMapSpecs')
]

{ #category : 'accessing' }
RwSimpleProjectLoadComponentV2 >> packageNames [

	^ packageNames
]

{ #category : 'accessing' }
RwSimpleProjectLoadComponentV2 >> projectNames [
	^projectNames
]

{ #category : 'accessing' }
RwSimpleProjectLoadComponentV2 >> projectNames: object [
	projectNames := object
]
