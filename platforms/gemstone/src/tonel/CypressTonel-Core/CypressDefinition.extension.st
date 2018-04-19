Extension { #name : 'CypressDefinition' }

{ #category : '*cypresstonel-core' }
CypressDefinition >> <= other [
	^ self sortKey <= other sortKey

]

{ #category : '*cypresstonel-core' }
CypressDefinition >> isClassDefinition [
  ^ false

]

{ #category : '*cypresstonel-core' }
CypressDefinition >> isMethodDefinition [
  ^ false

]

{ #category : '*cypresstonel-core' }
CypressDefinition >> isTraitDefinition [
  ^ false

]

{ #category : '*cypresstonel-core' }
CypressDefinition >> sortKey [
	self subclassResponsibility

]
