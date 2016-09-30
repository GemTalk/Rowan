"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: loading
"
analyzeRemovalOfAdditions
  "if there is an addition and a removal for the same definition, the addition wins ... needed when loading multiple packages and a defintion has been moved from one package to another --- see atomic loads for Metacello"

  | index |
  self halt.
  index := CypressDefinitionIndex definitions: self additions.
  self removals
    removeAllSuchThat: [ :removal | 
      (index
        definitionLike: removal
        ifPresent: [ :addition | 
          self halt.
          self obsoletions at: addition put: removal ]
        ifAbsent: [  ]) notNil ]