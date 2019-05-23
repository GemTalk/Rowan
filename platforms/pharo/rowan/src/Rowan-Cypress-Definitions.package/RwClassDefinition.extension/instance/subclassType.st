*rowan-cypress-definitions
subclassType
  | type |
  type := properties at: 'type'.
  type = 'normal'
    ifTrue: [ ^ '' ].
  ^ type