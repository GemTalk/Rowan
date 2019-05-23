*rowan-cypress-kernel
printDefinitionOn: stream

        stream
            nextPutAll: self superclassName;
            space;
            nextPutAll: self classCreationSelector;
            nextPut: $# ;
            nextPutAll: self className;
            cr; tab.
        stream
            nextPutAll: 'instanceVariableNames: ';
            store: self instanceVariablesString;
            cr; tab;
            nextPutAll: 'classVariableNames: ';
            store: self classVariablesString;
            cr; tab;
            nextPutAll: 'poolDictionaries: ';
            store: self poolDictionariesString;
            cr; tab;
            nextPutAll: 'category: ';
            store: self category asString 
