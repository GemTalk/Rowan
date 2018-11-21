Extension { #name : 'CypressMethodDefinition' }

{ #category : '*cypress-environmental-tools' }
CypressMethodDefinition >> loadMethodDefinition: lookupSymbolList environmentLoader: environmentLoader [
  | cls actualCls |
  cls := (lookupSymbolList resolveSymbol: self className) value.
  actualCls := self classIsMeta
    ifTrue: [ cls class ]
    ifFalse: [ cls ].
  actualCls
    compileMethod: self source
    dictionaries: environmentLoader compilationSymbolList
    category: self category
    environmentId: environmentLoader defaultEnvironmentId

]

{ #category : '*cypress-environmental-tools' }
CypressMethodDefinition >> postLoadOver: aDefinition lookupSymbolList: lookupSymbolList environmentId: environmentId [
  super
    postLoadOver: aDefinition
    lookupSymbolList: lookupSymbolList
    environmentId: environmentId.
  (self isInitializer
    and: [ aDefinition isNil or: [ self source ~= aDefinition source ] ])
    ifTrue: [ 
      (self theNonMetaClass: lookupSymbolList)
        perform: #'initialize'
        env: environmentId ]

]

{ #category : '*cypress-environmental-tools' }
CypressMethodDefinition >> theNonMetaClass: lookupSymbolList [
  ^ self
    resolveGlobalNamed: self className
    lookupSymbolList: lookupSymbolList
    or: [  ]

]
