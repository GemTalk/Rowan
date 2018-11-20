Extension { #name : 'CypressModification' }

{ #category : '*cypress-environmental-tools' }
loadClassDefinition: aSymbolDictionaryName environmentLoader: environmentLoader [
  self modification
    loadClassDefinition: aSymbolDictionaryName
    environmentLoader: environmentLoader

]

{ #category : '*cypress-environmental-tools' }
loadMethodDefinition: lookupSymbolList environmentLoader: environmentLoader [
  self modification
    loadMethodDefinition: lookupSymbolList
    environmentLoader: environmentLoader

]

{ #category : '*cypress-environmental-tools' }
method: CypressModification
postLoadDefinition: lookupSymbolList environmentId: environmentId [
  self modification
    postLoadOver: self obsoletion
    lookupSymbolList: lookupSymbolList
    environmentId: environmentId

]
