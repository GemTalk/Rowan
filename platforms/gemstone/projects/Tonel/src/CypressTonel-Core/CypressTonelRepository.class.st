"
No class-specific documentation for CypressTonelRepository, hierarchy is: 
Object
  CypressAbstractRepository( url properties readerClass writerClass)
    CypressFileSystemRepository( directoryPath)
      CypressTonelRepository

"
Class {
	#name : 'CypressTonelRepository',
	#superclass : 'CypressFileSystemRepository',
	#category : 'CypressTonel-Core'
}

{ #category : 'updating properties' }
CypressTonelRepository >> codeFormatProperty: aString [
  (aString equalsNoCase: 'Tonel')
    ifFalse: [ ^ super codeFormatProperty: aString ].
  properties at: '_gs_format' put: aString

]

{ #category : 'initializing - private' }
CypressTonelRepository >> initializeReaderAndWriterClasses [
  self isCodeFormatTonel
    ifTrue: [ 
      readerClass := TonelCypressReader.
      writerClass := TonelCypressWriter ]
    ifFalse: [ super initializeReaderAndWriterClasses ]

]

{ #category : 'testing properties' }
CypressTonelRepository >> isCodeFormatTonel [

	^self isCodeFormatProperty: 'Tonel'

]

{ #category : 'accessing properties' }
CypressTonelRepository >> packageExtension [
  ^ properties at: 'packageExtension' ifAbsent: [ '' ]

]

{ #category : 'reading' }
CypressTonelRepository >> readPackageStructureForPackageNamed: packageName [ 
  | reader |
  reader := self reader
    packageName: packageName;
    loadDefinitions;
    yourself.
  ^ CypressSnapshot definitions: reader definitions

]

{ #category : 'reading' }
CypressTonelRepository >> readPackageStructureForPackageNamed: packageName  files: fileNames [
  "Load only those files whose file names end with one of the Strings
   in the Array fileNames."
  | reader |
  reader := self reader
    packageName: packageName;
    loadDefinitionsFromFiles: fileNames  ;
    yourself.
  ^ CypressSnapshot definitions: reader definitions

]

{ #category : 'reading' }
CypressTonelRepository >> readPropertiesFile [
  "noop"


]

{ #category : 'reading' }
CypressTonelRepository >> readPropertiesFile: fileName [
  "noop"


]

{ #category : 'writing' }
CypressTonelRepository >> writePackageStructure: aPackageStructure [
  ^ self writer
    writeSnapshot: aPackageStructure snapshot
    inPackageNamed: aPackageStructure packageName

]

{ #category : 'writing' }
CypressTonelRepository >> writePropertiesFile: fileName [
  "noop"


]
