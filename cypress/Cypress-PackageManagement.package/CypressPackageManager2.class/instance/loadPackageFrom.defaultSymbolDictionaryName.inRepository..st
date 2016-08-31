"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: Loading
"
loadPackageFrom: aKnownPackageInformation defaultSymbolDictionaryName: defaultSymbolDictionaryNameOrNil inRepository: aRepository
  | snapshot summary loader |
  snapshot := (aRepository
    readPackageStructureForPackageNamed: aKnownPackageInformation name) snapshot.
  loader := snapshot
    updatePackage:
      (CypressPackageDefinition named: aKnownPackageInformation name)
    defaultSymbolDictionaryName: defaultSymbolDictionaryNameOrNil.
  summary := Dictionary new.
  loader unloadable notEmpty
    ifTrue: [ 
      summary
        at: 'Unloadable'
        put: (loader unloadable collect: [ :each | each printString ]) ].
  loader errors notEmpty
    ifTrue: [ summary at: 'Errors' put: (loader errors collect: [ :each | each printString ]) ].
  loader requirements notEmpty
    ifTrue: [ summary at: 'Missing Requirements' put: loader requirements asArray ].
  ^ summary
