Extension { #name : 'CypressMethodDefinition' }

{ #category : '*cypresstonel-core' }
CypressMethodDefinition >> fullClassName [
	
	^ self classIsMeta
		ifFalse: [self className]
		ifTrue: [ self className, ' class' ]

]

{ #category : '*cypresstonel-core' }
CypressMethodDefinition >> isExtensionMethod [
	^ category beginsWith: '*'

]

{ #category : '*cypresstonel-core' }
CypressMethodDefinition >> isMethodDefinition [
  ^ true

]

{ #category : '*cypresstonel-core' }
CypressMethodDefinition >> sortKey [
	^ self className, '.', (self classIsMeta ifTrue: ['meta'] ifFalse: ['nonmeta']), '.', self selector

]
