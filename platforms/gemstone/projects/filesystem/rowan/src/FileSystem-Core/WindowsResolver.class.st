"
I am an expert on Windows filesystem conventions. I resolve origins according to these conventions.
"
Class {
	#name : 'WindowsResolver',
	#superclass : 'PlatformResolver',
	#category : 'FileSystem-Core-Base-Resolver'
}

{ #category : 'accessing' }
WindowsResolver class >> platformNames [
	^ {'Win32' . 'Windows NT'}
]

{ #category : 'origins' }
WindowsResolver >> cache [
	"Because Windows does not define any location for putting cache files, we chose to use the preference directory. http://libertyboy.free.fr/computing/reference/envariables/"
	^ self preferences
]

{ #category : 'origins' }
WindowsResolver >> desktop [
	^ self home / 'Desktop'
]

{ #category : 'origins' }
WindowsResolver >> documents [
	^ self home / 'Documents'
]

{ #category : 'private' }
WindowsResolver >> homeEnvironmentVariableName [
	^'USERPROFILE'
]

{ #category : 'origins' }
WindowsResolver >> preferences [
	^ self directoryFromEnvVariableNamed: 'APPDATA' or: [ self home ]
]

{ #category : 'origins' }
WindowsResolver >> temp [
	^ self directoryFromEnvVariableNamed: 'TEMP' or: [ FileLocator C / 'windows' / 'temp' ]
]
