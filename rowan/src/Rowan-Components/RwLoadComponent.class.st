Class {
	#name : 'RwLoadComponent',
	#superclass : 'RwAbstractActiveComponent',
	#category : 'Rowan-Components'
}

{ #category : 'validation' }
RwLoadComponent >> _validatedPackageNames [
	"answer the validated set of package names"

	^ self packageNames asSet
]

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
RwLoadComponent >> addProjectNamed: aProjectName [
	self projectNames add: aProjectName.
	projectNames := projectNames asSet asArray sort
]

{ #category : 'accessing' }
RwLoadComponent >> conditionalPropertyMatchers [
	^ Dictionary new
		at: {(RwUnconditionalPlatformAttributeMatcher new)} put: {};
		yourself
]

{ #category : 'ston' }
RwLoadComponent >> fromSton: stonReader [
	"Decode non-variable classes from a map of their instance variables and values.
	Override to customize and add a matching #toSton: (see implementors)."

	(UserGlobals at: #'USE_NEW_COMPONENT_CLASSES' ifAbsent: [ false ])
		ifTrue: [ 
			self class isVariable
				ifTrue: [ self subclassResponsibility ]
				ifFalse: [ 
					| instanceVariableNames |
					instanceVariableNames := self class allInstVarNames.
					stonReader
						parseMapDo: [ :instVarName :value | 
							(self class == RwLoadComponent and: [ instVarName = #'condition' ])
								ifTrue: [
									"we're skipping the condition instvar, assuming that #fromSton: has been forwarded from RwSimpleProjectLoadComponent 
										and condition instance var isn't supported in RwLoadComponent"
									value ~= 'common'
										ifTrue: [ 
											"if the value is not common, then we'll throw an error, since we should not have top level components with conditions"
											self
												error:
													'condition instance variable is ignored for RwLoadComponent instances is convert component to a subcomponent' ] ]
								ifFalse: [ self instVarAt: (instanceVariableNames indexOf: instVarName asSymbol) put: value ] ] ] ]
		ifFalse: [ super fromSton: stonReader ]
]

{ #category : 'comparing' }
RwLoadComponent >> hash [
	^ super hash bitXor: self projectNames hash
]

{ #category : 'initialization' }
RwLoadComponent >> initialize [
	super initialize.
	projectNames := {}
]

{ #category : 'ston' }
RwLoadComponent >> instVarNamesInOrderForSton [
	^ #(#'name' #'projectName' #'preloadDoitName' #'postloadDoitName' #'projectNames' #'componentNames' #'packageNames' #'conditionalPackageMapSpecs' #'comment')
]

{ #category : 'accessing' }
RwLoadComponent >> projectNames [
	^projectNames
]

{ #category : 'accessing' }
RwLoadComponent >> removeProjectNamed: aProjectName [
	self projectNames remove: aProjectName ifAbsent: [  ]
]
