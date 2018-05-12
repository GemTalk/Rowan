Extension { #name : 'RwMethodDefinition' }

{ #category : '*rowan-cypress-core' }
RwMethodDefinition >> fromCypressMethod: cypMethodDef [
  properties
    at: 'selector' put: cypMethodDef selector asSymbol;
    at: 'protocol' put: cypMethodDef category;
    yourself.
  self source: cypMethodDef source
]

{ #category : '*rowan-cypress-core' }
RwMethodDefinition class >> fromCypressMethod: cypMethodDef [
  ^ self new
    fromCypressMethod: cypMethodDef;
    yourself
]
