"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: *Cypress-Definitions
"
subclassType
  "Answer a description of the receiver to identify whether it is a regular class,
	 a byte subclass, or an indexable subclass."

  ^ (self isBytes and: [ self superClass isBytes not ])
    ifTrue: [ 'byteSubclass' ]
    ifFalse: [ 
      (self isIndexable and: [ self superClass isIndexable not ])
        ifTrue: [ 'indexableSubclass' ]
        ifFalse: [ '' ] ]