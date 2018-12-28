Extension { #name : 'RwAbstractTool' }

{ #category : '*rowan-tools-deprecated' }
RwAbstractTool >> validate: aRwSpecification [
	self deprecated: 'RwAbstractTool>>validate: deprecated in Rowan 1.3.0'.
  aRwSpecification specName
    ifNil: [ self error: 'The specification must have a specName specified.' ]
]

{ #category : '*rowan-tools-deprecated' }
RwAbstractTool >> validateForRead: aRwSpecification [
	self deprecated: 'RwAbstractTool>>validateForRead: deprecated in Rowan 1.3.0'.
  aRwSpecification repositoryRootPath
    ifNil: [ 
      self
        error:
          'The repositoryRootPath must be defined in the specification: '
            , aRwSpecification specName printString ]
]

{ #category : '*rowan-tools-deprecated' }
RwAbstractTool >> validateForWrite: aRwSpecification [
	self deprecated: 'RwAbstractTool>>validateForWrite: deprecated in Rowan 1.3.0'.
  aRwSpecification repositoryRootPath
    ifNil: [ 
      self
        error:
          'The repositoryRootPath must be defined in the specification: '
            , aRwSpecification specName printString ]
]
