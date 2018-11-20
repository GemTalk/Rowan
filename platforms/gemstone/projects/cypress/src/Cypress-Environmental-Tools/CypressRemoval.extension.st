Extension { #name : 'CypressRemoval' }

{ #category : '*cypress-environmental-tools' }
CypressRemoval >> loadClassDefinition: aSymbolDictionaryName environmentLoader: environmentLoader [
  CypressError
    signal:
      'inappropriate to send #loadClassDefinition:environmentLoader: to a removal operation'

]

{ #category : '*cypress-environmental-tools' }
CypressRemoval >> loadMethodDefinition: lookupSymbolList environmentLoader: environmentLoader [
  CypressError
    signal:
      'inappropriate to send #loadMethodDefinition:environmentLoader: to a removal operation'

]

{ #category : '*cypress-environmental-tools' }
CypressRemoval >> postLoadDefinition: lookupSymbolList environmentId: environmentId [
  CypressError
    signal:
      'inappropriate to send #postLoadDefinition:environmentId: to a removal operation'

]
