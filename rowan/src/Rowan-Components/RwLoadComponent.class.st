Class {
	#name : 'RwLoadComponent',
	#superclass : 'RwAbstractActiveComponent',
	#category : 'Rowan-Components'
}

{ #category : 'comparing' }
RwLoadComponent >> = aRwLoadComponent [
	^ super = aRwLoadComponent
		and: [ self projectNames = aRwLoadComponent projectNames ]
]

{ #category : 'visiting' }
RwLoadComponent >> acceptNestedVisitor: aVisitor [

	^ self acceptVisitor: aVisitor
]

{ #category : 'visiting' }
RwLoadComponent >> acceptVisitor: aVisitor [
	^ aVisitor visitComponent: self
]

{ #category : 'accessing' }
RwLoadComponent >> conditionalPropertyMatchers [
	^ Dictionary new
		at: {(RwUnconditionalPlatformAttributeMatcher new)} put: {};
		yourself
]

{ #category : 'comparing' }
RwLoadComponent >> hash [
	^ super hash bitXor: self projectNames hash
]

{ #category : 'ston' }
RwLoadComponent >> instVarNamesInOrderForSton [
	^ #(#'name' #'projectName' #'preloadDoitName' #'postloadDoitName' #'projectNames' #'componentNames' #'packageNames' #'conditionalPackageMapSpecs' #'comment')
]

{ #category : 'accessing' }
RwLoadComponent >> removeProjectNamed: aProjectName [
	self projectNames remove: aProjectName ifAbsent: [  ]
]
