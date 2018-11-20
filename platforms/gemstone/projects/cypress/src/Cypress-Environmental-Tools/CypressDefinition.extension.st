Extension { #name : 'CypressDefinition' }

{ #category : '*cypress-environmental-tools' }
loadClassDefinition: aSymbolDictionaryName environmentLoader: environmentLoader [
  "default is to do nothing"


]

{ #category : '*cypress-environmental-tools' }
loadMethodDefinition: lookupSymbolList environmentLoader: environmentLoader [
  "default is to do nothing"


]

{ #category : '*cypress-environmental-tools' }
postLoad: lookupSymbolList environmentId: environmentId [
  "noop"


]

{ #category : '*cypress-environmental-tools' }
postLoadOver: aDefinition lookupSymbolList: lookupSymbolList environmentId: environmentId [
  self postLoad: lookupSymbolList environmentId: environmentId

]
