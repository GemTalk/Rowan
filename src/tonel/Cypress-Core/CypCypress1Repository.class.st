"
No class-specific documentation for CypCypressRepository, hierarchy is: 
Object
  CypRepository( url properties readerClass writerClass)
    CypFileSystemRepository( directoryPath disk)
      CypCypressRepository

"
Class {
	#name : 'CypCypress1Repository',
	#superclass : 'CypFileSystemRepository',
	#category : 'Cypress-Core'
}

{ #category : 'paths' }
CypCypress1Repository >> fullPathForPackageNamed: packageName [

	^directoryPath , packageName , '.package'
]

{ #category : 'reading' }
CypCypress1Repository >> newPackageReader [

	^CypCypress1PackageReader withDisk: disk
]

{ #category : 'accessing' }
CypCypress1Repository >> packageExtension [
	"The extension that labels a directory as containing a package."

	^'.package'
]

{ #category : 'accessing' }
CypCypress1Repository >> propertiesFileName [

	^'properties.ston'
]

{ #category : 'accessing' }
CypCypress1Repository >> propertiesParserClass [
	"TODO: This should be a STON-specific parser"

	^CypressJsonParser
]
