Extension { #name : 'CypressObject' }

{ #category : '*cypress-environmental-tools' }
CypressObject >> resolveGlobalNamed: aString lookupSymbolList: lookupSymbolList or: aBlock [
  ^ ((lookupSymbolList resolveSymbol: aString) ifNil: [ ^ aBlock value ]) value

]
