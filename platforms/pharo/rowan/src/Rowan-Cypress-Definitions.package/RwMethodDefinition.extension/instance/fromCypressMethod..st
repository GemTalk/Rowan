*rowan-cypress-definitions
fromCypressMethod: cypMethodDef
  properties
    at: 'selector' put: cypMethodDef selector asSymbol;
    at: 'protocol' put: cypMethodDef category;
    yourself.
  self source: cypMethodDef source