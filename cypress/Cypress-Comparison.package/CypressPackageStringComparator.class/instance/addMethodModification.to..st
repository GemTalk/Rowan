"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: comparing - private
"
addMethodModification: aCypressModification to: aCollection
  aCollection
    add:
      {(aCypressModification modification details).
      #'modification'.
      #'method'.
      (aCypressModification obsoletion source).
      (aCypressModification modification source)}
