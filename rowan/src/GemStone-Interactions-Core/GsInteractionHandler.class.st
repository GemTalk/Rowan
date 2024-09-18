Class {
	#name : 'GsInteractionHandler',
	#superclass : 'Object',
	#instVars : [
		'choiceBlock',
		'confirmBlock',
		'informBlock',
		'textBlock',
		'multiLineTextBlock',
		'defaultBlock',
		'inspectBlock'
	],
	#category : 'GemStone-Interactions-Core'
}

{ #category : 'accessing' }
GsInteractionHandler >> choiceBlock [
  choiceBlock ifNil: [ ^ self defaultBlock ].
  ^ choiceBlock
]

{ #category : 'accessing' }
GsInteractionHandler >> choiceBlock: anObject [

   "Modify the value of the instance variable 'choiceBlock'."
   choiceBlock := anObject
]

{ #category : 'accessing' }
GsInteractionHandler >> confirmBlock [
  confirmBlock ifNil: [ ^ self defaultBlock ].
  ^ confirmBlock
]

{ #category : 'accessing' }
GsInteractionHandler >> confirmBlock: anObject [

   "Modify the value of the instance variable 'confirmBlock'."
   confirmBlock := anObject
]

{ #category : 'accessing' }
GsInteractionHandler >> defaultBlock [
  defaultBlock
    ifNil: [ 
      defaultBlock := [ :interaction | 
      self
        error:
          'No interactive handler defined for ' , interaction class name asString , ' interaction.' ] ].
  ^ defaultBlock
]

{ #category : 'accessing' }
GsInteractionHandler >> defaultBlock: anObject [

   "Modify the value of the instance variable 'defaultBlock'."
   defaultBlock := anObject
]

{ #category : 'accessing' }
GsInteractionHandler >> informBlock [
  informBlock ifNil: [ ^ self defaultBlock ].
  ^ informBlock
]

{ #category : 'accessing' }
GsInteractionHandler >> informBlock: anObject [

   "Modify the value of the instance variable 'informBlock'."
   informBlock := anObject
]

{ #category : 'accessing' }
GsInteractionHandler >> inspectBlock [
  inspectBlock ifNil: [ ^ self defaultBlock ].
  ^ inspectBlock
]

{ #category : 'accessing' }
GsInteractionHandler >> inspectBlock: anObject [

   "Modify the value of the instance variable 'inspectBlock'."
   inspectBlock := anObject
]

{ #category : 'interactions' }
GsInteractionHandler >> interactWithChoice: interaction [
	interaction interactionsEnabled
		ifFalse: [ ^ interaction ].
	^ self choiceBlock value: interaction
]

{ #category : 'interactions' }
GsInteractionHandler >> interactWithConfirm: interaction [
	interaction interactionsEnabled
		ifFalse: [ ^ false ].
	^ self confirmBlock value: interaction
]

{ #category : 'interactions' }
GsInteractionHandler >> interactWithExplore: interaction [
	interaction interactionsEnabled
		ifFalse: [ ^ false ].
	^ self interactWithInspect: interaction
]

{ #category : 'interactions' }
GsInteractionHandler >> interactWithInform: interaction [
	interaction interactionsEnabled
		ifFalse: [ ^ false ].
	^ self informBlock value: interaction
]

{ #category : 'interactions' }
GsInteractionHandler >> interactWithInspect: interaction [
	interaction interactionsEnabled
		ifFalse: [ ^ interaction theObject ].
	^ self inspectBlock value: interaction
]

{ #category : 'interactions' }
GsInteractionHandler >> interactWithMultiLineText: interaction [
	interaction interactionsEnabled
		ifFalse: [ ^ interaction theObject ].
	^ self multiLineTextBlock value: interaction
]

{ #category : 'interactions' }
GsInteractionHandler >> interactWithText: interaction [
	interaction interactionsEnabled
		ifFalse: [ ^ interaction theObject ].
	^ self textBlock value: interaction
]

{ #category : 'accessing' }
GsInteractionHandler >> multiLineTextBlock [
  multiLineTextBlock ifNil: [ ^ self defaultBlock ].
  ^ multiLineTextBlock
]

{ #category : 'accessing' }
GsInteractionHandler >> multiLineTextBlock: anObject [

   "Modify the value of the instance variable 'multiLineTextBlock'."
   multiLineTextBlock := anObject
]

{ #category : 'accessing' }
GsInteractionHandler >> textBlock [
  textBlock ifNil: [ ^ self defaultBlock ].
  ^ textBlock
]

{ #category : 'accessing' }
GsInteractionHandler >> textBlock: anObject [

   "Modify the value of the instance variable 'textBlock'."
   textBlock := anObject
]
