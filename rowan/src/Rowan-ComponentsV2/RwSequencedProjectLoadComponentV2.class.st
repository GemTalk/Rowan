Class {
	#name : 'RwSequencedProjectLoadComponentV2',
	#superclass : 'RwAbstractSimpleProjectLoadComponentV2',
	#category : 'Rowan-ComponentsV2'
}

{ #category : 'visiting' }
RwSequencedProjectLoadComponentV2 >> acceptVisitor: aVisitor [
	^ aVisitor visitSequencedProjectLoadComponent: self
]

{ #category : 'accessing' }
RwSequencedProjectLoadComponentV2 >> conditionalPackageMapSpecs [
	self shouldNotImplement: #'conditionalPackageMapSpecs'
]

{ #category : 'accessing' }
RwSequencedProjectLoadComponentV2 >> conditionalPackageMapSpecsAt: key ifAbsent: absentBlock [
	self shouldNotImplement: #'conditionalPackageMapSpecsAt:ifAbsent:'
]

{ #category : 'ston' }
RwSequencedProjectLoadComponentV2 >> instVarNamesInOrderForSton [
	^ #(#'name' #'projectName' #'comment' #'condition' #'groupName' #'preloadDoitName' #'postloadDoitName' #'componentNames')
]
