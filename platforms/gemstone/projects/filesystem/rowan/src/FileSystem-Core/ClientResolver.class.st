Class {
	#name : 'ClientResolver',
	#superclass : 'PlatformResolver',
	#instVars : [
		'resolver'
	],
	#category : 'FileSystem-Core-Base-Resolver'
}

{ #category : 'instance creation' }
ClientResolver class >> forCurrentPlatform [
	^self new
		resolver: (self resolverClassFor: System clientOsName) new;
		yourself
]

{ #category : 'origins' }
ClientResolver >> cache [
	^self isWindowsStoreClient
		ifTrue: [
			"Because Windows does not define any location for putting cache files, we chose to use the preference directory. 
			http://libertyboy.free.fr/computing/reference/envariables/"
			^ self preferences]
		ifFalse: [
			^ self directoryFromEnvVariableNamed: 'XDG_CACHE_HOME' or: [ self home / '.cache' ]]
]

{ #category : 'origins' }
ClientResolver >> desktop [
	self isWindowsStoreClient
		ifTrue: [^ self home / 'Desktop']
		ifFalse: [^ (self xdgUserDir: 'DESKTOP') ifNil: [ self home / 'Desktop' ]]
]

{ #category : 'origins' }
ClientResolver >> documents [

	^self isWindowsStoreClient
		ifTrue: [
			^ self home / 'Documents' ]
		ifFalse: [
			^ (self xdgUserDir: 'DOCUMENTS') ifNil: [ self home / 'Documents' ]]
]

{ #category : 'resolving' }
ClientResolver >> fileSystem [
	^ ClientStore currentFileSystem
]

{ #category : 'origins' }
ClientResolver >> homeEnvironmentVariableName [
	^ self resolver homeEnvironmentVariableName
]

{ #category : 'testing' }
ClientResolver >> isWindowsStoreClient [

	^self resolver class name = #WindowsResolver
]

{ #category : 'origins - mac' }
ClientResolver >> library [
	^ self userLibrary
]

{ #category : 'private' }
ClientResolver >> osEnvironmentVariable: aString [

	^System clientEnvironmentVariable: aString
]

{ #category : 'origins' }
ClientResolver >> preferences [

	^self isWindowsStoreClient
		ifTrue: [ self directoryFromEnvVariableNamed: 'APPDATA' or: [ self home ] ]
		ifFalse: [self directoryFromEnvVariableNamed: 'XDG_CONFIG_HOME' or: [ self home / '.config' ] ]
]

{ #category : 'accessing' }
ClientResolver >> resolver [
	^resolver
]

{ #category : 'accessing' }
ClientResolver >> resolver: aPlatformResolver [
	resolver := aPlatformResolver
]

{ #category : 'resolving' }
ClientResolver >> supportedOrigins [
	^ self resolver supportedOrigins
]

{ #category : 'origins - mac' }
ClientResolver >> systemApplicationSupport [
	^ self systemLibrary / 'Application Support'
]

{ #category : 'origins - mac' }
ClientResolver >> systemLibrary [
	^  FileSystem clientDisk root / 'Library'
]

{ #category : 'origins' }
ClientResolver >> temp [
	^self isWindowsStoreClient
		ifTrue: [
			self directoryFromEnvVariableNamed: 'TEMP' or: [ ClientFileLocator C / 'windows' / 'temp' ] ]
		ifFalse: [
			'/tmp' asClientFileReference]
]

{ #category : 'origins - mac' }
ClientResolver >> userApplicationSupport [
	^self userLibrary / 'Application Support'
]

{ #category : 'origins' }
ClientResolver >> userData [
	^self isWindowsStoreClient
		ifTrue: [self error: 'userData is not supported in Windows']
		ifFalse: [self directoryFromEnvVariableNamed: 'XDG_DATA_HOME' or: [ self home / '.local' / 'share' ] ]
]

{ #category : 'origins - mac' }
ClientResolver >> userLibrary [
	^  self home / 'Library'
]

{ #category : 'helpers' }
ClientResolver >> xdgParseUserDirLine: aStream [
	"Format is XDG_xxx_DIR=""$HOME/yyy"", where yyy is a shell-escaped homedir-relative path, or XDG_xxx_DIR=""/yyy"", where /yyy is an absolute path. No other format is supported."
	| path firstChar |
	(aStream next = $") ifFalse: [ ^ nil ].
	firstChar := aStream next.
	(#($$ $/) includes: firstChar) ifFalse: [ ^ nil ].
	path := firstChar = $$
				ifTrue: [ (aStream next: 5) = 'HOME/' ifFalse: [ ^ nil ].
					       self home / (aStream upTo: $") ]
				ifFalse: [ self resolveString: '/', (aStream upTo: $") ].
	^ path
]

{ #category : 'helpers' }
ClientResolver >> xdgUserDir: userDirName [
	"Read ~/.config/user-dirs.dirs to find the directory of userDirName (e.g., 'DESKTOP')"
	"http://freedesktop.org/wiki/Software/xdg-user-dirs"
	"This file is written by xdg-user-dirs-update If you want to change or add directories, just edit the line you're interested in. All local changes will be retained on the next run Format is XDG_xxx_DIR=""$HOME/yyy"", where yyy is a shell-escaped homedir-relative path, or XDG_xxx_DIR=""/yyy"", where /yyy is an absolute path. No other format is supported."
	| configFile |
	configFile := self preferences / 'user-dirs.dirs'.
	(configFile isFile and: [ configFile isReadable ]) ifFalse: [ ^ nil ].
	configFile readStreamDo: [ :stream | 
		[ stream atEnd ]
			whileFalse: [ 
				((stream peek ~= $#) and: [ (stream upTo: $=) = ('XDG_', userDirName, '_DIR') ])
							ifTrue: [ ^ self xdgParseUserDirLine: stream ]
							ifFalse: [ stream nextLine ] ] ].
	^ nil
]
