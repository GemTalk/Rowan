Extension { #name : 'CypressDefinition' }

{ #category : '*cypress-environmental-tools' }
CypressDefinition >> loadClassDefinition: aSymbolDictionaryName environmentLoader: environmentLoader [
  "default is to do nothing"


]

{ #category : '*cypress-environmental-tools' }
CypressDefinition >> loadMethodDefinition: lookupSymbolList environmentLoader: environmentLoader [
  "default is to do nothing"


]

{ #category : '*cypress-environmental-tools' }
CypressDefinition >> postLoad: lookupSymbolList environmentId: environmentId [
  "noop"


]

{ #category : '*cypress-environmental-tools' }
CypressDefinition >> postLoadOver: aDefinition lookupSymbolList: lookupSymbolList environmentId: environmentId [
  self postLoad: lookupSymbolList environmentId: environmentId

]
