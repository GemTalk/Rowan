"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: initialization
"
fromPackage: aCypressPackageDefinition
  | snapshot classMap classDefinitions |
  snapshot := aCypressPackageDefinition snapshot.
  classDefinitions := OrderedCollection new.
  classMap := Dictionary new.
  snapshot definitions
    do: [ :definition | 
      definition
        classDefinition: [ :classDefinition | classDefinitions add: classDefinition ]
        methodDefinition: [ :methodDefinition | 
          (classMap at: methodDefinition className ifAbsentPut: [ Set new ])
            add: methodDefinition ] ].
  self
    name: aCypressPackageDefinition name , self packageExtension
    from: classDefinitions
    classMap: classMap
