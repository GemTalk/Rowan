Class {
	#name : 'RwSequencedProjectLoadComponentV2',
	#superclass : 'RwAbstractSimpleProjectLoadComponentV2',
	#category : 'Rowan-ComponentsV2'
}

{ #category : 'validation' }
RwSequencedProjectLoadComponentV2 >> _validatedPackageNames [
	"answer the validated set of package names"

	^ #()
]

{ #category : 'visiting' }
RwSequencedProjectLoadComponentV2 >> acceptNestedVisitor: aVisitor [
	^ self
		error:
			'The sequenced project load component ' , self name printString
				,
					' can only be loaded as a top-level component and cannot be nested within another component."'
]

{ #category : 'visiting' }
RwSequencedProjectLoadComponentV2 >> acceptTopLevelVisitor: aVisitor [
	"the receiver is being visited from an RwSequencedProjectLoadComponent.
		RwSequencedProjectLoadComponents may not be nested. "

	^ self
		error:
			'The sequenced project load component ' , self name printString
				,
					' can only be loaded as a component directly referenced from a load spec."'
]

{ #category : 'visiting' }
RwSequencedProjectLoadComponentV2 >> acceptVisitor: aVisitor [
	^ aVisitor visitSequencedProjectLoadComponent: self
]

{ #category : 'accessing' }
RwSequencedProjectLoadComponentV2 >> addComponentNamed: aComponentName [
	"preserve the load order"

	self componentNames add: aComponentName
]

{ #category : 'accessing' }
RwSequencedProjectLoadComponentV2 >> conditionalPackageMapSpecs [
	^ Dictionary new
]

{ #category : 'accessing' }
RwSequencedProjectLoadComponentV2 >> conditionalPackageMapSpecsAt: key ifAbsent: absentBlock [
	self shouldNotImplement: #'conditionalPackageMapSpecsAt:ifAbsent:'
]

{ #category : 'ston' }
RwSequencedProjectLoadComponentV2 >> instVarNamesInOrderForSton [
	^ #(#'name' #'projectName' #'comment' #'condition' #'groupName' #'preloadDoitName' #'postloadDoitName' #'componentNames')
]
