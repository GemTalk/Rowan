"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: loading
"
analyzeRemovalOfAdditions
  "if there is an addition and a removal for the same definition, the addition wins ... needed when loading multiple packages and a defintion has been moved from one package to another --- see atomic loads for Metacello"

  | index |
  index := CypressDefinitionIndex
    definitions: (self additions collect: [ :each | each definition ]).
  self removals
    removeAllSuchThat: [ :removal | 
      (index
        definitionLike: removal definition
        ifPresent: [ :additionDefinition | self obsoletions at: additionDefinition description put: removal definition ]
        ifAbsent: [  ]) notNil ]
