initialization
fromSnapshot: snapshot
  | classMap classDefinitions |
  classDefinitions := OrderedCollection new.
  classMap := Dictionary new.
  snapshot
    do: [ :definition | 
      definition
        classDefinition: [ :classDefinition | classDefinitions add: classDefinition ]
        methodDefinition: [ :methodDefinition | 
          (classMap at: methodDefinition className ifAbsentPut: [ Set new ])
            add: methodDefinition ] ].
  self name: self name from: classDefinitions classMap: classMap