Extension { #name : 'RwClassDefinition' }

{ #category : '*rowan-cypress' }
RwClassDefinition >> category [
  ^ properties at: 'category'
]

{ #category : '*rowan-cypress' }
RwClassDefinition >> classInstVarNames [
  ^ properties at: 'classinstvars'
]

{ #category : '*rowan-cypress' }
RwClassDefinition >> classVarNames [
  ^ properties at: 'classvars'
]

{ #category : '*rowan-cypress' }
RwClassDefinition >> comment [
  ^ properties at: 'comment'
]

{ #category : '*rowan-cypress' }
RwClassDefinition >> instVarNames [
  ^ properties at: 'instvars'
]

{ #category : '*rowan-cypress' }
RwClassDefinition >> name [
  ^ self key
]

{ #category : '*rowan-cypress' }
RwClassDefinition >> poolDictionaryNames [
  ^ properties at: 'pools'
]

{ #category : '*rowan-cypress' }
RwClassDefinition >> subclassType [
  | type |
  type := properties at: 'type'.
  type = 'normal'
    ifTrue: [ ^ '' ].
  ^ type
]
