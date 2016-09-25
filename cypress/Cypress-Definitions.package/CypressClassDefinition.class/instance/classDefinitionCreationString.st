"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: accessing
"
classDefinitionCreationString
  ^ String
    streamContents: [ :stream | 
      stream
        nextPutAll: '(CypressClassDefinition';
        lf;
        tab;
        nextPutAll: 'name: ' , self className printString;
        lf;
        tab;
        nextPutAll: 'superclassName: ' , self superclassName printString;
        lf;
        tab;
        nextPutAll: 'category: ' , self category printString;
        lf;
        tab;
        nextPutAll: 'instVarNames: #(' , self instanceVariablesString , ')';
        lf;
        tab;
        nextPutAll:
            'classInstVarNames: #(' , self classInstanceVariablesString , ')';
        lf;
        tab;
        nextPutAll: 'classVarNames: #(' , self classVariablesString , ')';
        lf;
        tab;
        nextPutAll:
            'poolDictionaryNames: #(' , self poolDictionariesString , ')';
        lf;
        tab;
        nextPutAll: 'comment: ' , self comment printString;
        lf;
        tab;
        nextPutAll: 'subclassType: ' , self subclassType printString , ')';
        lf;
        tab;
        tab;
        nextPutAll:
            'loadClassDefinition"optionally loadClassDefinition: <symbolDictionary-name>".';
        yourself ]