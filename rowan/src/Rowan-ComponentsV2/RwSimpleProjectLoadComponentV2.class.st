Class {
	#name : 'RwSimpleProjectLoadComponentV2',
	#superclass : 'RwAbstractRowanProjectLoadComponentV2',
	#instVars : [
		'projectNames'
	],
	#category : 'Rowan-ComponentsV2'
}

{ #category : 'ston' }
RwSimpleProjectLoadComponentV2 >> _stonReplacementClass [
	^ RwLoadComponent
]

{ #category : 'validation' }
RwSimpleProjectLoadComponentV2 >> _validatedPackageNames [
	"answer the validated set of package names"

	^ self packageNames asSet
]

{ #category : 'comparing' }
RwSimpleProjectLoadComponentV2 >> = aRwSimpleProjectLoadComponentV2 [
	^ super = aRwSimpleProjectLoadComponentV2
		and: [ self projectNames = aRwSimpleProjectLoadComponentV2 projectNames ]
]

{ #category : 'visiting' }
RwSimpleProjectLoadComponentV2 >> acceptNestedVisitor: aVisitor [

	^ self acceptVisitor: aVisitor
]

{ #category : 'visiting' }
RwSimpleProjectLoadComponentV2 >> acceptVisitor: aVisitor [
	^ aVisitor visitComponent: self
]

{ #category : 'accessing' }
RwSimpleProjectLoadComponentV2 >> addProjectNamed: aProjectName [
	self projectNames add: aProjectName.
	projectNames := projectNames asSet asArray sort
]

{ #category : 'accessing' }
RwSimpleProjectLoadComponentV2 >> conditionalPropertyMatchers [
	| conditionalPropertyMatchers |
	conditionalPropertyMatchers := Dictionary new
		at: {(self _platformPatternMatcherFor: self condition)} put: {};
		yourself.
	^ conditionalPropertyMatchers
]

{ #category : 'comparing' }
RwSimpleProjectLoadComponentV2 >> hash [
	^ super hash bitXor: self projectNames hash
]

{ #category : 'initialization' }
RwSimpleProjectLoadComponentV2 >> initialize [
	super initialize.
	projectNames := {}
]

{ #category : 'ston' }
RwSimpleProjectLoadComponentV2 >> instVarNamesInOrderForSton [
	^ #(#'name' #'projectName' #'condition' #'preloadDoitName' #'postloadDoitName' #'projectNames' #'componentNames' #'packageNames' #'conditionalPackageMapSpecs' #'comment')
]

{ #category : 'accessing' }
RwSimpleProjectLoadComponentV2 >> label [
	^ self basename , ' [' , self condition , ']'
]

{ #category : 'copying' }
RwSimpleProjectLoadComponentV2 >> postCopy [
	super postCopy.
	projectNames := projectNames copy.
]

{ #category : 'accessing' }
RwSimpleProjectLoadComponentV2 >> projectNames [
	^projectNames
]

{ #category : 'accessing' }
RwSimpleProjectLoadComponentV2 >> removeProjectNamed: aProjectName [
	self projectNames remove: aProjectName ifAbsent: [  ]
]
