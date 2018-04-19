Extension { #name : 'CypressMethodDefinition' }

{ #category : '*cypresstonel-core' }
CypressMethodDefinition >> fullClassName [
	
	^ self classIsMeta
		ifFalse: [self className]
		ifTrue: [
			self actualClass isNil
				ifFalse: [self className, ' class']
				ifTrue: [self className, ' classSide']]

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
