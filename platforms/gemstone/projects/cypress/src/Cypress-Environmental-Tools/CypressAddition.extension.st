Extension { #name : 'CypressAddition' }

{ #category : '*cypress-environmental-tools' }
loadClassDefinition: aSymbolDictionaryName environmentLoader: environmentLoader [
  self definition
    loadClassDefinition: aSymbolDictionaryName
    environmentLoader: environmentLoader

]

{ #category : '*cypress-environmental-tools' }
loadMethodDefinition: lookupSymbolList environmentLoader: environmentLoader [
  self definition
    loadMethodDefinition: lookupSymbolList
    environmentLoader: environmentLoader

]

{ #category : '*cypress-environmental-tools' }
postLoadDefinition: lookupSymbolList environmentId: environmentId [
  self definition
    postLoadOver: nil
    lookupSymbolList: lookupSymbolList
    environmentId: environmentId

]
