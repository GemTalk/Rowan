Class {
	#name : 'RwSimpleProjectLoadComponentV2',
	#superclass : 'RwAbstractSimpleProjectLoadComponentV2',
	#instVars : [
		'packageNames',
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
	^ (super = aRwSimpleProjectLoadComponentV2
		and: [ self packageNames = aRwSimpleProjectLoadComponentV2 packageNames ])
		and: [ self projectNames = aRwSimpleProjectLoadComponentV2 projectNames ]
]

{ #category : 'visiting' }
RwSimpleProjectLoadComponentV2 >> acceptNestedVisitor: aVisitor [

	^ self acceptVisitor: aVisitor
]

{ #category : 'visiting' }
RwSimpleProjectLoadComponentV2 >> acceptVisitor: aVisitor [
	^ aVisitor visitSimpleProjectLoadComponent: self
]

{ #category : 'accessing' }
RwSimpleProjectLoadComponentV2 >> addPackageNames: aPackageNames [
	"add packages to default conditional/group if applicable"

	self packageNames addAll: aPackageNames.
	packageNames := packageNames asSet asArray sort
]

{ #category : 'accessing' }
RwSimpleProjectLoadComponentV2 >> addProjectNamed: aProjectName [
	self projectNames add: aProjectName.
	projectNames := projectNames asSet asArray sort
]

{ #category : 'comparing' }
RwSimpleProjectLoadComponentV2 >> hash [
	| hashValue |
	hashValue := super hash.
	hashValue := hashValue bitXor: self packageNames.
	hashValue := hashValue bitXor: self projectNames.
	^ hashValue
]

{ #category : 'initialization' }
RwSimpleProjectLoadComponentV2 >> initialize [
	super initialize.
	packageNames := {}.
	projectNames := {}
]

{ #category : 'ston' }
RwSimpleProjectLoadComponentV2 >> instVarNamesInOrderForSton [
	^ #(#'name' #'projectName' #'comment' #'condition' #'groupName' #'preloadDoitName' #'postloadDoitName' #'packageNames' #'componentNames' #'projectNames' #'conditionalPackageMapSpecs')
]

{ #category : 'accessing' }
RwSimpleProjectLoadComponentV2 >> packageNames [

	^ packageNames
]

{ #category : 'accessing' }
RwSimpleProjectLoadComponentV2 >> projectNames [
	^projectNames
]
