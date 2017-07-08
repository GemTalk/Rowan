"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: writing - private
"
fileOutClassDeclaration: classStructure on: aStream
  aStream
    nextPutAll: 'doit';
    lf;
    nextPutAll: '(' , classStructure superclassName;
    lf.
  self
    writeClassTypeMessage: classStructure
    on: aStream
    hasInstanceVariables: [ aStream
        nextPutAll:
            '	instVarNames: #( ' , classStructure instanceVariablesString , ' )';
        lf ].
  aStream
    nextPutAll: '	classVars: #( ' , classStructure classVariablesString , ' )';
    lf;
    nextPutAll:
        '	classInstVars: #( ' , classStructure classInstanceVariablesString , ' )';
    lf;
    nextPutAll: '	poolDictionaries: #()';
    lf;
    nextPutAll: '	inDictionary: Globals';
    lf;
    nextPutAll: '	options: #())';
    lf;
    nextPutAll: '		category: ' , classStructure category printString , ';';
    lf;
    nextPutAll: '		comment: ' , classStructure comment printString , ';';
    lf;
    nextPutAll: '		immediateInvariant.';
    lf;
    nextPutAll: 'true.';
    lf;
    nextPutAll: '%';
    lf;
    lf
