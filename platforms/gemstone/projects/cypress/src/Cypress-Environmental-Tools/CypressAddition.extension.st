Extension { #name : 'CypressAddition' }

{ #category : '*cypress-environmental-tools' }
CypressAddition >> loadClassDefinition: aSymbolDictionaryName environmentLoader: environmentLoader [
  self definition
    loadClassDefinition: aSymbolDictionaryName
    environmentLoader: environmentLoader

]

{ #category : '*cypress-environmental-tools' }
CypressAddition >> loadMethodDefinition: lookupSymbolList environmentLoader: environmentLoader [
  self definition
    loadMethodDefinition: lookupSymbolList
    environmentLoader: environmentLoader

]

{ #category : '*cypress-environmental-tools' }
CypressAddition >> postLoadDefinition: lookupSymbolList environmentId: environmentId [
  self definition
    postLoadOver: nil
    lookupSymbolList: lookupSymbolList
    environmentId: environmentId

]
