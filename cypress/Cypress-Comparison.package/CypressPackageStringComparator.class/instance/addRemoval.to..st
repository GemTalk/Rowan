"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: comparing - private
"
addRemoval: aCypressModification to: aCollection
  aCypressModification definition
    classDefinition: [ :classDefinition | self addClassRemoval: aCypressModification to: aCollection ]
    methodDefinition: [ :methodDefinition | self addMethodRemoval: aCypressModification to: aCollection ]
