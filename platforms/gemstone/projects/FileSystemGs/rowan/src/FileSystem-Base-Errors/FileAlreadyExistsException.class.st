"
I am an exception that notifies that a file already exists.
"
Class {
	#name : 'FileAlreadyExistsException',
	#superclass : 'FileException',
	#instVars : [
		'file'
	],
<<<<<<< HEAD:platforms/gemstone/projects/FileSystemGs/rowan/src/FileSystem-Base-Errors/FileAlreadyExistsException.class.st
	#category : 'FileSystem-Base-Errors'
=======
	#category : 'Files'
>>>>>>> origin/candidateV2.0:platforms/gemstone/projects/FileSystemGs/rowan/src/Files/FileAlreadyExistsException.class.st
}

{ #category : 'instance creation' }
FileAlreadyExistsException class >> signalOnFile: aFile [

	self new
		file: aFile;
		signal
]

{ #category : 'accessing' }
FileAlreadyExistsException >> file [
	^ file
]

{ #category : 'accessing' }
FileAlreadyExistsException >> file: aFile [

	file := aFile
]

{ #category : 'accessing' }
FileAlreadyExistsException >> messageText [

	^ 'File already exists: ', file basename
]
