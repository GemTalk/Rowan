Class {
	#name : 'RwComponent',
	#superclass : 'RwAbstractComponent',
	#category : 'Rowan-ComponentsV2'
}

{ #category : 'validation' }
RwComponent >> _validatedPackageNames [
	"answer the validated set of package names"

	^ self packageNames asSet
]

{ #category : 'comparing' }
RwComponent >> = aRwComponent [
	^ super = aRwComponent
		and: [ self projectNames = aRwComponent projectNames ]
]

{ #category : 'visiting' }
RwComponent >> acceptNestedVisitor: aVisitor [

	^ self acceptVisitor: aVisitor
]

{ #category : 'visiting' }
RwComponent >> acceptVisitor: aVisitor [
	^ aVisitor visitSimpleProjectLoadComponent: self
]

{ #category : 'accessing' }
RwComponent >> addProjectNamed: aProjectName [
	self projectNames add: aProjectName.
	projectNames := projectNames asSet asArray sort
]

{ #category : 'accessing' }
RwComponent >> conditionalPropertyMatchers [
	| conditionalPropertyMatchers |
	conditionalPropertyMatchers := Dictionary new
		at: {(self _platformPatternMatcherFor: self condition)} put: {};
		yourself.
	^ conditionalPropertyMatchers
]

{ #category : 'comparing' }
RwComponent >> hash [
	^ super hash bitXor: self projectNames hash
]

{ #category : 'initialization' }
RwComponent >> initialize [
	super initialize.
	projectNames := {}
]

{ #category : 'ston' }
RwComponent >> instVarNamesInOrderForSton [
	^ #(#'name' #'projectName' #'condition' #'preloadDoitName' #'postloadDoitName' #'projectNames' #'componentNames' #'packageNames' #'conditionalPackageMapSpecs' #'comment')
]

{ #category : 'accessing' }
RwComponent >> projectNames [
	^projectNames
]

{ #category : 'accessing' }
RwComponent >> removeProjectNamed: aProjectName [
	self projectNames remove: aProjectName ifAbsent: [  ]
]
