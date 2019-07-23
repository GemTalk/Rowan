Extension { #name : 'RwMethodDefinition' }

{ #category : '*rowan-cypress-definitions' }
RwMethodDefinition class >> fromCypressMethod: cypMethodDef [
  ^ self new
    fromCypressMethod: cypMethodDef;
    yourself
]

{ #category : '*rowan-cypress-definitions' }
RwMethodDefinition >> fromCypressMethod: cypMethodDef [
  properties
    at: 'selector' put: cypMethodDef selector asSymbol;
    at: 'protocol' put: cypMethodDef category;
    yourself.
  self source: cypMethodDef source
]
