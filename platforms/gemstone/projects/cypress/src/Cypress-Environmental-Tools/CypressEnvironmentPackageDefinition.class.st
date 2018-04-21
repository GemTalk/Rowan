"
No class-specific documentation for CypressEnvironmentPackageDefinition, hierarchy is: 
Object
  CypressObject
    CypressPackageDefinition( name)
      CypressEnvironmentPackageDefinition( lookupSymbolList)

"
Class {
	#name : 'CypressEnvironmentPackageDefinition',
	#superclass : 'CypressPackageDefinition',
	#instVars : [
		'lookupSymbolList'
	],
	#category : 'Cypress-Environmental-Tools'
}

{ #category : 'accessing' }
CypressEnvironmentPackageDefinition >> lookupSymbolList [

   ^lookupSymbolList

]

{ #category : 'accessing' }
CypressEnvironmentPackageDefinition >> lookupSymbolList: anObject [

   lookupSymbolList := anObject

]

{ #category : 'accessing' }
CypressEnvironmentPackageDefinition >> symbolList [
  lookupSymbolList ifNil: [ ^ super symbolList ].
  ^ self lookupSymbolList

]
