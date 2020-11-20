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
	^ aVisitor visitComponent: self
]

{ #category : 'accessing' }
RwComponent >> addProjectNamed: aProjectName [
	self projectNames add: aProjectName.
	projectNames := projectNames asSet asArray sort
]

{ #category : 'accessing' }
RwComponent >> conditionalPropertyMatchers [
	^ Dictionary new
		at: {(RwUnconditionalPlatformAttributeMatcher new)} put: {};
		yourself
]

{ #category : 'ston' }
RwComponent >> fromSton: stonReader [
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
							(self class == RwComponent and: [ instVarName = #'condition' ])
								ifTrue: [
									"we're skipping the condition instvar, assuming that #fromSton: has been forwarded from RwSimpleProjectLoadComponent 
										and condition instance var isn't supported in RwComponent"
									value ~= 'common'
										ifTrue: [ 
											"if the value is not common, then we'll throw an error, since we should not have top level components with conditions"
											self
												error:
													'condition instance variable is ignored for RwComponent instances is convert component to a subcomponent' ] ]
								ifFalse: [ self instVarAt: (instanceVariableNames indexOf: instVarName asSymbol) put: value ] ] ] ]
		ifFalse: [ super fromSton: stonReader ]
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
