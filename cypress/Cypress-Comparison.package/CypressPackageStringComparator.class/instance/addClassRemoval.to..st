"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: comparing - private
"
addClassRemoval: aCypressModification to: aCollection
  aCollection
    add:
      {(aCypressModification definition details).
      #'removal'.
      #'class'.
      (aCypressModification definition classDefinitionString)}
