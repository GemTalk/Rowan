"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: comparing - private
"
addModification: aCypressModification to: aCollection
  aCypressModification modification
    classDefinition: [ :classDefinition | self addClassModification: aCypressModification to: aCollection ]
    methodDefinition: [ :methodDefinition | self addMethodModification: aCypressModification to: aCollection ]
