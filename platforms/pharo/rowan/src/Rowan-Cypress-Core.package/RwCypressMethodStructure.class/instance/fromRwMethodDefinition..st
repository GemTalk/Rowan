initialization
fromRwMethodDefinition: methodDefinition
  self selector: methodDefinition selector asString.
  self category: (methodDefinition properties at: 'protocol').
  self source: methodDefinition source