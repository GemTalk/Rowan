"
I am a late-bound reference. I refer to a file or directory in relation to a well-known location on the filesystem, called an origin. When asked to perform concrete operation, I look up the current location of my origin, and resolve my path against it. 

Usage
----------
FileLocator vmDirectory parent pathString
	> '/Applications'


FileLocator desktop.
FileLocator desktop basename.

FileLocator home basename.
FileLocator image.
FileLocator vmBinary asAbsolute pathString 
	>  '/Applications/CogVM.app/Contents/MacOS/CogVM'
FileLocator vmBinary pathString 
	> '/Applications/CogVM.app/Contents/MacOS/CogVM'
		




Implementation
------------------------
origin 
	A symbolic name for base reference I use to resolve myself.

path
	A relative path that is resolved against my origin""
	
	
"
Class {
	#name : 'FileLocator',
	#superclass : 'AbstractFileReference',
	#instVars : [
		'origin',
		'path'
	],
	#category : 'FileSystem-Core'
}

{ #category : 'windows-origins' }
FileLocator class >> A [
	^ self driveNamed: #A 

]

{ #category : 'adding' }
FileLocator class >> addResolver: aResolver [
	self resolver addResolver: aResolver
]

{ #category : 'windows-origins' }
FileLocator class >> B [
	^ self driveNamed: #B

]

{ #category : 'windows-origins' }
FileLocator class >> C [
	^ self driveNamed: #C

]

{ #category : 'origins' }
FileLocator class >> cache [
	^ self origin: #cache
]

{ #category : 'gemstone-origins' }
FileLocator class >> clientWorkingDirectory [
	^ FileSystem client referenceTo: RelativePath new
]

{ #category : 'origins' }
FileLocator class >> cwd [
	^ self workingDirectory
]

{ #category : 'windows-origins' }
FileLocator class >> D [
	^ self driveNamed: #D

]

{ #category : 'gemstone-origins' }
FileLocator class >> dbfScratchDir [
	"Answer the path to the DBF_SCRATCH_DIR"

	^ self origin: #dbfScratchDir
]

{ #category : 'origins' }
FileLocator class >> desktop [
	^ self origin: #desktop
]

{ #category : 'origins' }
FileLocator class >> documents [
	^ self origin: #documents
]

{ #category : 'windows-origins' }
FileLocator class >> driveNamed: driveName [
	^ FileReference fileSystem: (FileSystem disk) path: Path / (driveName, ':')
]

{ #category : 'windows-origins' }
FileLocator class >> E [
	^ self driveNamed: #E

]

{ #category : 'gemstone-origins' }
FileLocator class >> extent: extentIndex [
	"Answer the path to the extent at extentIndex in DBF_EXTENT_NAMES: (System stoneConfigurationReport at: #DBF_EXTENT_NAMES) at: extentIndex"

	^ self origin: ('extent', extentIndex printString) asSymbol
]

{ #category : 'gemstone-origins' }
FileLocator class >> extent1 [
	"Answer the path to the first exent extent"

	^ self origin: #extent1
]

{ #category : 'gemstone-origins' }
FileLocator class >> extent1Directory [

	^ self origin: #extent1Directory
]

{ #category : 'windows-origins' }
FileLocator class >> F [
	^ self driveNamed: #F

]

{ #category : 'flushing' }
FileLocator class >> flushCaches [
	self resolver flushCaches
]

{ #category : 'windows-origins' }
FileLocator class >> G [
	^ self driveNamed: #G

]

{ #category : 'gemstone-origins' }
FileLocator class >> gemLogDirectory [
	"locator for the log directory (parent directory of gemLog)"

	^ self origin: #gemLogDirectory
]

{ #category : 'windows-origins' }
FileLocator class >> H [
	^ self driveNamed: #H

]

{ #category : 'origins' }
FileLocator class >> home [
	^ self origin: #home
]

{ #category : 'windows-origins' }
FileLocator class >> I [
	^ self driveNamed: #I

]

{ #category : 'gemstone-origins' }
FileLocator class >> image [
	"commonly used, so we need to define it, but the closest that GemStone comes is the #dbfScratchDir"

	^ self dbfScratchDir
]

{ #category : 'gemstone-origins' }
FileLocator class >> imageDirectory [
	"commonly used, so we need to define it, but the closest that GemStone comes is the #dbfScratchDir"

	^ self dbfScratchDir
]

{ #category : 'windows-origins' }
FileLocator class >> J [
	^ self driveNamed: #J

]

{ #category : 'windows-origins' }
FileLocator class >> K [
	^ self driveNamed: #K

]

{ #category : 'windows-origins' }
FileLocator class >> L [
	^ self driveNamed: #L

]

{ #category : 'windows-origins' }
FileLocator class >> M [
	^ self driveNamed: #M

]

{ #category : 'windows-origins' }
FileLocator class >> N [
	^ self driveNamed: #N

]

{ #category : 'windows-origins' }
FileLocator class >> O [
	^ self driveNamed: #O

]

{ #category : 'instance creation' }
FileLocator class >> origin: aSymbol [ 
	^ self origin: aSymbol path: Path workingDirectory
]

{ #category : 'instance creation' }
FileLocator class >> origin: aSymbol path: aPath [
	^ self basicNew 
			initializeWithOrigin: aSymbol path: aPath
]

{ #category : 'windows-origins' }
FileLocator class >> P [
	^ self driveNamed: #P

]

{ #category : 'origins' }
FileLocator class >> preferences [
	^ self origin: #preferences
]

{ #category : 'windows-origins' }
FileLocator class >> Q [
	^ self driveNamed: #Q

]

{ #category : 'windows-origins' }
FileLocator class >> R [
	^ self driveNamed: #R

]

{ #category : 'accessing' }
FileLocator class >> resolver [

	^ SessionTemps current at: #FileLocator_Resolver ifAbsentPut: [
		InteractiveResolver new
			addResolver: SystemResolver new;
			addResolver: PlatformResolver forCurrentPlatform;
			yourself ]
]

{ #category : 'origins' }
FileLocator class >> root [
	^ FileSystem disk root
]

{ #category : 'windows-origins' }
FileLocator class >> S [
	^ self driveNamed: #S

]

{ #category : 'gemstone-origins' }
FileLocator class >> serverWorkingDirectory [
	^ FileSystem server referenceTo: RelativePath new
]

{ #category : 'accessing' }
FileLocator class >> supportedOrigins [
	| origins current |
	origins := IdentitySet new.
	current := self resolver.
	[current notNil] whileTrue:
		[origins addAll: current supportedOrigins.
		current := current next].
	^ origins
]

{ #category : 'mac-origins' }
FileLocator class >> systemApplicationSupport [
	^ self origin: #systemApplicationSupport
]

{ #category : 'mac-origins' }
FileLocator class >> systemLibrary [
	^ self origin: #systemLibrary
]

{ #category : 'windows-origins' }
FileLocator class >> T [
	^ self driveNamed: #T

]

{ #category : 'origins' }
FileLocator class >> temp [
	^ self origin: #temp
]

{ #category : 'gemstone-origins' }
FileLocator class >> tranlog [
	"Answer the path to the current tranlog directory"

	^ self origin: #tranlog
]

{ #category : 'windows-origins' }
FileLocator class >> U [
	^ self driveNamed: #U

]

{ #category : 'mac-origins' }
FileLocator class >> userApplicationSupport [
	^ self origin: #userApplicationSupport
]

{ #category : 'unix-origins' }
FileLocator class >> userData [
	^ self origin: #userData
]

{ #category : 'mac-origins' }
FileLocator class >> userLibrary [
	^ self origin: #userLibrary
]

{ #category : 'windows-origins' }
FileLocator class >> V [
	^ self driveNamed: #V

]

{ #category : 'windows-origins' }
FileLocator class >> W [
	^ self driveNamed: #W
]

{ #category : 'origins' }
FileLocator class >> workingDirectory [
	^ FileSystem disk referenceTo: RelativePath new
]

{ #category : 'windows-origins' }
FileLocator class >> X [
	^ self driveNamed: #X

]

{ #category : 'windows-origins' }
FileLocator class >> Y [
	^ self driveNamed: #Y

]

{ #category : 'windows-origins' }
FileLocator class >> Z [
	^ self driveNamed: #Z

]

{ #category : 'comparing' }
FileLocator >> = other [
	^ self species = other species
		and: [origin = other origin
			and: [path = other path]]
]

{ #category : 'accessing' }
FileLocator >> absolutePath [
	"Return the absolute path"
	^ self resolve path
]

{ #category : 'converting' }
FileLocator >> asAbsolute [
	^ self 
]

{ #category : 'converting' }
FileLocator >> asFileReference [
	^ self resolve
]

{ #category : 'converting' }
FileLocator >> asPath [
	"Answer the receiver's path"

	^self resolve asPath
]

{ #category : 'streams-compatibility' }
FileLocator >> binaryReadStream [
	^ self resolve binaryReadStream
]

{ #category : 'streams-compatibility' }
FileLocator >> binaryWriteStream [
	"Answer a binary write stream on the receiver"

	^ self resolve binaryWriteStream
]

{ #category : 'copying' }
FileLocator >> copyWithPath: newPath [
	^ self class origin: origin path: newPath
]

{ #category : 'error handling' }
FileLocator >> doesNotUnderstand: aMessage [
	"Redirect message to the resolved version of this FileLocator.
	If FileReference won't understand the message send a normal DNU."

	| resolved |
	
	resolved := self resolve.
	(resolved respondsTo: aMessage selector)
		ifTrue: [ ^ resolved perform: aMessage selector withArguments: aMessage arguments ].
	
	^ super doesNotUnderstand: aMessage.
]

{ #category : 'accessing' }
FileLocator >> fullPath [
	^ self resolve path
]

{ #category : 'comparing' }
FileLocator >> hash [
	^ origin hash bitXor: path hash
]

{ #category : 'initialize-release' }
FileLocator >> initializeWithOrigin: aSymbol path: aPath [
	self initialize.
	origin := aSymbol.
	path := aPath.
]

{ #category : 'testing' }
FileLocator >> isAbsolute [
	^ true
]

{ #category : 'testing' }
FileLocator >> isRelative [
	^ false
]

{ #category : 'accessing' }
FileLocator >> origin [
	^ origin
]

{ #category : 'accessing' }
FileLocator >> path [
	^ path
]

{ #category : 'printing' }
FileLocator >> printOn: aStream [
	| fs |
	aStream
		nextPut: ${;
		nextPutAll: origin;
		nextPut: $}.
	path isWorkingDirectory
		ifTrue: [ ^ self ].
	fs := self fileSystem.
	aStream nextPut: fs delimiter.
	fs printPath: path on: aStream
]

{ #category : 'streams' }
FileLocator >> readStream [
	^ self resolve readStream 
]

{ #category : 'operations' }
FileLocator >> renameTo: newBasename [
	
	| result |
	result := self resolve renameTo: newBasename.
	path size > 0
		ifTrue: [ path basename: newBasename ]
		ifFalse: [ path := result path ]
]

{ #category : 'navigating' }
FileLocator >> resolve [
	^ (self class resolver resolve: origin) resolve: path
]

{ #category : 'navigating' }
FileLocator >> resolveString: aString [
	| filesystem thePath |
	filesystem := (self class resolver resolve: origin) fileSystem.
	thePath := filesystem pathFromString: aString.
	^ self withPath: (path resolvePath: thePath)
]

{ #category : 'streams' }
FileLocator >> writeStream [
	^ self resolve writeStream 
]
