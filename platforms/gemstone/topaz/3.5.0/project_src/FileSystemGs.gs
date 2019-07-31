! Class Declarations
! Generated file, do not Edit

doit
(ByteArray
	subclass: 'UUID'
	instVarNames: #(  )
	classVars: #( GeneratorClass )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Network-UUID';
		comment: '';
		immediateInvariant.
true.
%

doit
(Error
	subclass: 'AssertionFailure'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Zinc-Character-Encoding-Core';
		comment: '';
		immediateInvariant.
true.
%

doit
(Error
	subclass: 'FileException'
	instVarNames: #( fileName )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-Errors';
		comment: 'Common superclass for exceptions while using file streams';
		immediateInvariant.
true.
%

doit
(FileException
	subclass: 'CannotDeleteFileException'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-Errors';
		comment: 'Notfify when not able to delete';
		immediateInvariant.
true.
%

doit
(FileException
	subclass: 'FileAlreadyExistsException'
	instVarNames: #( file )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-Errors';
		comment: 'I am an exception that notifies that a file already exists.';
		immediateInvariant.
true.
%

doit
(FileException
	subclass: 'FileDoesNotExistException'
	instVarNames: #( readOnly )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-Errors';
		comment: 'I am raised when an operation is attempted on a file that does not exist.

The method used to signal the exception depends on the form the file name exists in:

- For strings: (FileDoesNotExistException file fileName: aString) signal
- For Files: FileDoesNotExistException signalOnFile: aFile
- For FileReferences: FileDoesNotExistException signalWith: aFileReference


Applications that want to offer the user the opportunity to select a different file can use:

	UIManager default fileDoesNotExistsDefaultAction: exception
';
		immediateInvariant.
true.
%

doit
(FileException
	subclass: 'FileReadError'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-Errors';
		comment: '';
		immediateInvariant.
true.
%

doit
(FileException
	subclass: 'FileWriteError'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-Errors';
		comment: 'Notify about an error when trying to attempt to write to a file';
		immediateInvariant.
true.
%

doit
(Error
	subclass: 'FilePosixError'
	instVarNames: #( reference platformErrorNumber errorGroupName sourceReference options )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-Errors';
		comment: 'This is the abstract class for File/Directory Posix Errors.

Instance Vareables:
platformErrorNumber - the platform error number which resulted in this error.
errorGroupName - this gives the context of the error.
sourceReference - this is only used for copy/rename actions - it is the source reference. The target reference is stored in the reference instance variable.
options - if the error had options, this is where the options are stored.';
		immediateInvariant.
true.
%

doit
(FilePosixError
	subclass: 'FileAccessError'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-Errors';
		comment: 'Search permission is denied on a component of the path prefix, or the file exists and the permissions specified by oflag are denied, or the file does not exist and write permission is denied for the parent directory of the file to be created, or O_TRUNC is specified and write permission is denied.';
		immediateInvariant.
true.
%

doit
(FilePosixError
	subclass: 'FileBadFileDescriptorError'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-Errors';
		comment: 'Bad file descriptor (POSIX.1-2001).';
		immediateInvariant.
true.
%

doit
(FilePosixError
	subclass: 'FileBusyError'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-Errors';
		comment: 'Device or resource busy';
		immediateInvariant.
true.
%

doit
(FilePosixError
	subclass: 'FileDeviceNotSameError'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-Errors';
		comment: '';
		immediateInvariant.
true.
%

doit
(FilePosixError
	subclass: 'FileExistsError'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-Errors';
		comment: 'O_CREAT and O_EXCL are set, and the named file exists.';
		immediateInvariant.
true.
%

doit
(FilePosixError
	subclass: 'FileInvalidOptionError'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-Errors';
		comment: 'The value of an file option argument is not valid.';
		immediateInvariant.
true.
%

doit
(FilePosixError
	subclass: 'FileIOError'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-Errors';
		comment: 'I/O error.

The path argument names a STREAMS file and a hangup or error occurred during the open().';
		immediateInvariant.
true.
%

doit
(FilePosixError
	subclass: 'FileIsDirectoryError'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-Errors';
		comment: 'The named file is a directory and oflag includes O_WRONLY or O_RDWR';
		immediateInvariant.
true.
%

doit
(FilePosixError
	subclass: 'FileMaxFilesOpenError'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-Errors';
		comment: 'The maximum allowable number of files is currently open in the system';
		immediateInvariant.
true.
%

doit
(FilePosixError
	subclass: 'FileNameToLongError'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-Errors';
		comment: 'The length of the path argument exceeds {PATH_MAX} or a pathname component is longer than {NAME_MAX}.

This is used with sockets.';
		immediateInvariant.
true.
%

doit
(FilePosixError
	subclass: 'FileNoEntryError'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-Errors';
		comment: 'O_CREAT is not set and the named file does not exist; or O_CREAT is set and either the path prefix does not exist or the path argument points to an empty string.';
		immediateInvariant.
true.
%

doit
(FilePosixError
	subclass: 'FileNoSpaceError'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-Errors';
		comment: 'The directory or file system that would contain the new file cannot be expanded, the file does not exist, and O_CREAT is specified';
		immediateInvariant.
true.
%

doit
(FilePosixError
	subclass: 'FilePermissionDeniedError'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-Errors';
		comment: 'Operation not permitted error. Concerning capability to mount a file system.';
		immediateInvariant.
true.
%

doit
(FilePosixError
	subclass: 'FileReadOnlyFileSystemError'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-Errors';
		comment: '';
		immediateInvariant.
true.
%

doit
(FilePosixError
	subclass: 'FileSynchronizedIONotSupportedError'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-Errors';
		comment: '';
		immediateInvariant.
true.
%

doit
(FilePosixError
	subclass: 'FileTooManySymbolicLinksError'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-Errors';
		comment: 'More than {SYMLOOP_MAX} symbolic links were encountered during resolution of the path argument.';
		immediateInvariant.
true.
%

doit
(Error
	subclass: 'FileSystemError'
	instVarNames: #( reference )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: 'I am an abstract superclass for errors that may occur during filesystem operations.';
		immediateInvariant.
true.
%

doit
(FileSystemError
	subclass: 'DirectoryDoesNotExist'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: 'I am raised when I an operation is attempted inside a directory that does not exist. ';
		immediateInvariant.
true.
%

doit
(FileSystemError
	subclass: 'DirectoryExists'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: 'I am raised on an attempt to create a directory that already exists.';
		immediateInvariant.
true.
%

doit
(FileSystemError
	subclass: 'DirectoryIsNotEmpty'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: 'I am raised on an attempt to delete a directory when is not empty.';
		immediateInvariant.
true.
%

doit
(FileSystemError
	subclass: 'FileDeletionError'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: '';
		immediateInvariant.
true.
%

doit
(FileSystemError
	subclass: 'FileDoesNotExist'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: '';
		immediateInvariant.
true.
%

doit
(FileSystemError
	subclass: 'FileExists'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: 'I am raised on an attempt to create a file or directory over top of an existing file.';
		immediateInvariant.
true.
%

doit
(FileSystemError
	subclass: 'FileOpenError'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: '';
		immediateInvariant.
true.
%

doit
(FileSystemError
	subclass: 'FileOptionFeatureNotSupported'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Disk';
		comment: '';
		immediateInvariant.
true.
%

doit
(FileSystemError
	subclass: 'IllegalName'
	instVarNames: #( name )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: 'I am raised on an attempt to use an illegal file name
';
		immediateInvariant.
true.
%

doit
(FileSystemError
	subclass: 'SubscriptOutOfBounds'
	instVarNames: #( signaler subscript lowerBound upperBound )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: '';
		immediateInvariant.
true.
%

doit
(Error
	subclass: 'ZnByteStringBecameWideString'
	instVarNames: #( byteString wideString )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Zinc-Character-Encoding-Core';
		comment: 'I am ZnByteStringBecameWideString, a resumable Error signalled to indicate that some byteString was changed to a wideString.

Used by ZnUTF8Encoder>>#readInto:startingAt:count:fromStream: to avoid a #becomeForward: when a ByteString automagically changes into a WideString.

Part of Zinc HTTP Components.';
		immediateInvariant.
true.
%

doit
(Error
	subclass: 'ZnCharacterEncodingError'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Zinc-Character-Encoding-Core';
		comment: 'I am ZnCharacterEncodingError.
I am an Error.

I signal when something goes wrong while encoding or decoding characters.

Part of Zinc HTTP Components';
		immediateInvariant.
true.
%

doit
(ZnCharacterEncodingError
	subclass: 'ZnIncomplete'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Zinc-Character-Encoding-Core';
		comment: 'I am ZnIncomplete.
I am a ZnCharacterEncodingError.
I am an Error.

I signal when the binary stream from which a character is read does not contain enough data to form a full character. This typically occurs when the stream is #atEnd, a file is EOF or a network connection is closed - when the end of a stream is reached when more data is expected/needed.

I can be used to ignore wrongly encoded input by resuming me. By default a question mark will be inserted for each problem and decoding will continue. This is not recommended, as faulty input should not be accepted.

Part of Zinc HTTP Components';
		immediateInvariant.
true.
%

doit
(ZnCharacterEncodingError
	subclass: 'ZnInvalidUTF8'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Zinc-Character-Encoding-Core';
		comment: 'I am ZnInvalidUTF8.
I am a ZnCharacterEncodingError.
I am an Error.

I signal when something goes wrong while encoding or decoding UTF8.

I can be used to ignore wrongly encoded input by resuming me. By default a question mark will be inserted for each problem and decoding will continue. This is not recommended, as faulty input should not be accepted.

Part of Zinc HTTP Components';
		immediateInvariant.
true.
%

doit
(Notification
	subclass: 'ResolutionRequest'
	instVarNames: #( origin )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: 'I represent a request for user assistance in resolving an origin. I am a resumable exception that gets raised when there is no way of automatically resolving a particular origin. ';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'AbstractFileAdaptor'
	instVarNames: #( file )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: 'I provide an abstraction layer to adapt the theoretical file implementation in the FileSystem package with the actual file implemenation in the target Smalltalk dialect.

Each Smalltalk dialect can create its own subclass.

To change which file class to use, update the method FileSystem class>>#fileClass. This should be implemented in dialect dependent package.';
		immediateInvariant.
true.
%

doit
(AbstractFileAdaptor
	subclass: 'GsFileAdaptor'
	instVarNames: #( options )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core-GemStone';
		comment: 'I provide an abstraction layer to adapt the theoretical file implementation in the FileSystem package with the actual file implemenation in the target Smalltalk dialect.';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'AbstractFileReference'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: 'I am an abstract superclass for FileLocator and FileReference. By implementing most of the methods on myself most code duplucation between the locator and the reference can be avoided';
		immediateInvariant.
true.
%

doit
(AbstractFileReference
	subclass: 'FileLocator'
	instVarNames: #( origin path )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: 'I am a late-bound reference. I refer to a file or directory in relation to a well-known location on the filesystem, called an origin. When asked to perform concrete operation, I look up the current location of my origin, and resolve my path against it. 

Usage
----------
FileLocator vmDirectory parent pathString
	> ''/Applications''


FileLocator desktop.
FileLocator desktop basename.

FileLocator home basename.
FileLocator image.
FileLocator vmBinary asAbsolute pathString 
	>  ''/Applications/CogVM.app/Contents/MacOS/CogVM''
FileLocator vmBinary pathString 
	> ''/Applications/CogVM.app/Contents/MacOS/CogVM''
		




Implementation
------------------------
origin 
	A symbolic name for base reference I use to resolve myself.

path
	A relative path that is resolved against my origin"
	
	';
		immediateInvariant.
true.
%

doit
(AbstractFileReference
	subclass: 'FileReference'
	instVarNames: #( filesystem path )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: 'I combine a filesystem and path, which is sufficient to refer to a concrete file or directory. I provide methods for navigating my filesystem, performing filesystem operations and opening and closing files.  I am the primary mechanism for working with files and directories. 

| working |
working := FileSystem disk workingDirectory.
working files 

| disk |
disk := FileSystem disk.
disk root.                               	"a reference to the root directory"
disk working.                         	"a reference to the working directory"';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'AbstractStreamSpec'
	instVarNames: #( fileOptions store )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: '';
		immediateInvariant.
true.
%

doit
(AbstractStreamSpec
	subclass: 'BinaryStreamSpec'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: '';
		immediateInvariant.
true.
%

doit
(AbstractStreamSpec
	subclass: 'BufferedStreamSpec'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: '';
		immediateInvariant.
true.
%

doit
(AbstractStreamSpec
	subclass: 'EncodedStreamSpec'
	instVarNames: #( encoding )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: '';
		immediateInvariant.
true.
%

doit
(EncodedStreamSpec
	subclass: 'EncodedBufferedStreamSpec'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: '';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'FastUUIDGenerator'
	instVarNames: #( bits1 bits2 bits3 bits4 random semaphoreForGenerator )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Network-UUID';
		comment: '';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'FileOptions'
	instVarNames: #( openModeFlag mode posixFlags fileType share permissionFlags parent store )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-FileOptions';
		comment: 'This class contains options for a file. The options available are based on the POSIX standards.

This gives a more flexible and nuanced way of defining how a file is to be opened and used. This is following the POSIX file open options. Much of this
information has been garnered from the VA Smalltalk POSIX implmenetion of CfsFielDescriptior and CfsDirectoryDescriptor.

Instance Variables:
openModeFlag -	This contains the access mode of the file - O_RDONLY | O_RDWR | O_WRONLY.
						These modes are represented by the classes FileOpenReadOnlyMode, FileOpenReadWriteMode, and FileOpenWriteOnlyMode.
posixFlags		- 	This contains a collection of the options which are used to further define how the file is opened, created if missing, truncate, append, etc.
						Options contais instances of subclasses of FileCreateOption
fileType			-   This defines the file type. Available types are Binary, Text, Gzip (Currently GemStone only). The former two are only relevant to Windows.
						The difference between Binary and Text is how cr/lf is handled. In Unicode this distinction is non-sensical so Binary is recommended for Unicode files.
share 			-	Indicates how the file is to be shared with others.

API:
modeString	-	Returns the traditional unix mode string. (See table below.) This is implemented to support GemStone''s GsFile file class.
isWritable		-	Returns true/false based on the access mode - O_RDONLY | O_RDWR | O_WRONLY. This is implemen to support Pharo''s File file class.

This table shows the mapping between the file options and the traditional unix string for defining file open mode.

Mode                 File Options (Flags)
------				-----------------------------------------
	r     				O_RDONLY
	w            		O_WRONLY | O_CREAT | O_TRUNC
	a    				O_WRONLY | O_CREAT | O_APPEND
	r+            		O_RDWR
	w+				O_RDWR | O_CREAT | O_TRUNC
	a+				O_RDWR | O_CREAT | O_APPEND

See https://linux.die.net/man/3/fopen for more details on the fopen open modes.';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'FileSystem'
	instVarNames: #( store )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: 'I present a low-level protocol for interacting with filesystems. I hold a reference to
a store (a subinstance of FileSystemStore) which takes care of the details of performing 
file and directory operations on the filesystem I represent. 

I am responsible for resolving all paths that
I pass into my store. My store acts as a factory and offers platform specific actions.


FileSystem instances know two methods that return a Reference object: workingDirectory and root.

FileSystem disk workingDirectory
FileSystem disk root

';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'FileSystemDirectoryEntry'
	instVarNames: #( reference creation modification isDirectory isSymlink size posixPermissions )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: 'I am a cache for metadata about a file or directory. The information I hold is as follows:

reference
	A reference to the file or directory to which my data pertains.
	
creation
	The creation date and time, stored as number seconds since the 
	Smalltalk epoch.
	
modification
	The modification date and time, number seconds since the Smalltalk epoch.
	
isDirectory 
	True if my data pertains to a directory, false if a file.
	
size
	Size in bytes for a file, 0 for a directory.
';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'FileSystemGuide'
	instVarNames: #( visitor work selectChildren )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: 'I am an abstract superclass for objects that fulfill the Guide role in the Guide/Visitor pattern. My subclasses know how to traverse a filesystem in a specific order, "showing" the files and directories they encounter to a visitor.

visitor
	An object that fulfills the Visitor role and implements the visitor protocol.
	
work
	An OrderedCollection, used to keep track of filesystem nodes that have not yet been visited';
		immediateInvariant.
true.
%

doit
(FileSystemGuide
	subclass: 'BreadthFirstGuide'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: 'I traverse the filesystem in breadth-first order. Given this hierarchy:

alpha
	beta
	gamma
delta
	epsilon

I would visit the nodes in the following order:  alpha, delta, beta, gamma, epsilon.

I use my work instance variable as a queue, adding nodes to be visited to the end and retrieving them from the beginning.
';
		immediateInvariant.
true.
%

doit
(FileSystemGuide
	subclass: 'PostorderGuide'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: 'I traverse the filesystem in depth-first post order. Given this hierarchy:

alpha
	beta
	gamma
delta
	epsilon

I would visit the nodes in the following order:  beta, gamma, alpha, epsilon, delta.

I use my work instance variable as a stack. I push messages that cause nodes to be traversed or visited, and execute them in reverse order.';
		immediateInvariant.
true.
%

doit
(FileSystemGuide
	subclass: 'PreorderGuide'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: 'I traverse the filesystem in depth-first pre order. Given this hierarchy:

alpha
	beta
	gamma
delta
	epsilon

I would visit the nodes in the following order: alpha, beta, gamma, delta, epsilon.

I use my work instance variable as a stack. I push nodes to be visited and visit them in reverse order.';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'FileSystemHandle'
	instVarNames: #( reference options )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: 'I am an abstract superclass for file handle implementations. I provide a uniform interface that streams can use for read and write operations on a file regardless of the filesystem. I encapsulate the actual IO primitives.';
		immediateInvariant.
true.
%

doit
(FileSystemHandle
	subclass: 'FileHandle'
	instVarNames: #( id )
	classVars: #( Registry )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Disk';
		comment: 'I provide an interface for doing IO on an open file. I keep an id, which as an opaque identifier used by the FilePlugin primitives. I translate positions from the 1-based indexes used in Smalltalk to the 0-based offsets used by the primitives.

I implement the primitives on my class side.';
		immediateInvariant.
true.
%

doit
(FileSystemHandle
	subclass: 'MemoryHandle'
	instVarNames: #( entry )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Memory';
		comment: 'I provide "primitives" for doing IO on files in a MemoryFileSystem. I delegate most of my actions to the MemoryFile. This way there is only one place needed where the data is stored.';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'FileSystemPermission'
	instVarNames: #( posixPermission )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: 'I''m a set of permissions for a Directory Entry';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'FileSystemResolver'
	instVarNames: #( next )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: 'I am an abstract superclass for objects that can resolve origins into references. Such objects use the Chain of Responsibility pattern, and when unable to resolve a particular origin, delegate that resolution request to the next resolver in the list.

next
	The next resolver in the list, or nil
';
		immediateInvariant.
true.
%

doit
(FileSystemResolver
	subclass: 'InteractiveResolver'
	instVarNames: #( cache )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: 'I resolve origins by consulting the user. I maintain a cache of the user''s responses.';
		immediateInvariant.
true.
%

doit
(FileSystemResolver
	subclass: 'PlatformResolver'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: 'I am an abstract superclass for platform-specific resolvers.';
		immediateInvariant.
true.
%

doit
(PlatformResolver
	subclass: 'ClientResolver'
	instVarNames: #( resolver )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Client-Core';
		comment: '';
		immediateInvariant.
true.
%

doit
(PlatformResolver
	subclass: 'MacOSResolver'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: 'I am an expert on Mac OS X filesystem conventions. I resolve origins according to these conventions.';
		immediateInvariant.
true.
%

doit
(PlatformResolver
	subclass: 'UnixResolver'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: 'I am an expert on Unix filesystem conventions. I resolve origins according to these conventions.';
		immediateInvariant.
true.
%

doit
(PlatformResolver
	subclass: 'WindowsResolver'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: 'I am an expert on Windows filesystem conventions. I resolve origins according to these conventions.';
		immediateInvariant.
true.
%

doit
(FileSystemResolver
	subclass: 'SystemResolver'
	instVarNames: #(  )
	classVars: #( UserLocalDirectory )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: 'I resolve origins that are related to the currently running Smalltalk system, using primitives provided by the VM. ';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'FileSystemStore'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: 'I am an abstract superclass for store implementations. My subclasses provide access to the actual data storage of a particular kind of filesystem. 

The file system can be accessed via
	FileSystem disk 
	FileSystem memory
	
My associated filesystem can be accessed as follows:
      DiskStore currentFileSystem';
		immediateInvariant.
true.
%

doit
(FileSystemStore
	subclass: 'DiskStore'
	instVarNames: #( maxFileNameLength )
	classVars: #( CurrentFS DefaultWorkingDirectory )
	classInstVars: #( PosixValues PosixErrorNumbers )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Disk';
		comment: 'I am an abstract superclass for disk store implementations. My subclasses provide access to the actual data storage of a particular kind of filesystem. 
';
		immediateInvariant.
true.
%

doit
(DiskStore
	subclass: 'ClientStore'
	instVarNames: #( store defaultWorkingDirectory )
	classVars: #(  )
	classInstVars: #( CurrentFS DefaultWorkingDirectory )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Client-Core';
		comment: '';
		immediateInvariant.
true.
%

doit
(DiskStore
	subclass: 'UnixStore'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Disk';
		comment: 'I''m a specific store for Unix file systems';
		immediateInvariant.
true.
%

doit
(UnixStore
	subclass: 'MacStore'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Disk';
		comment: 'I''m a specific store for OSX file systems';
		immediateInvariant.
true.
%

doit
(DiskStore
	subclass: 'WindowsStore'
	instVarNames: #( disk )
	classVars: #( Disks )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Disk';
		comment: 'I''m a specific store for Windows file systems';
		immediateInvariant.
true.
%

doit
(FileSystemStore
	subclass: 'MemoryStore'
	instVarNames: #( root )
	classVars: #( CurrentFS )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Memory';
		comment: 'I''m a specific store for memory file system';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'FileSystemVisitor'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: 'I am an abstract superclass for objects that can perform operations on directory trees. My subclasses implement the visitor protocol, and process filesystem nodes shown to them by guides.';
		immediateInvariant.
true.
%

doit
(FileSystemVisitor
	subclass: 'AbstractEnumerationVisitor'
	instVarNames: #( out block )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: 'I''m an abstract superclass for enumeration operations on directory entries. ';
		immediateInvariant.
true.
%

doit
(AbstractEnumerationVisitor
	subclass: 'CollectVisitor'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: 'I am a visitor that collects objects from the nodes I visit. I take a block similar to those passed to Collection>>collect:. I evaluate the block with DirectoryEntries for the nodes I visit, and collect the objects answered into an array.

I can use any guide, and the objects in the array I produce will reflect the order imposed by the guide.';
		immediateInvariant.
true.
%

doit
(AbstractEnumerationVisitor
	subclass: 'SelectVisitor'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: 'I am a visitor that selects objects from the nodes I visit. I take a block similar to those passed to Collection>>select:. I evaluate the block with DirectoryEntries for the nodes I visit.

I can use any guide, and the objects in the array I produce will reflect the order imposed by the guide.';
		immediateInvariant.
true.
%

doit
(FileSystemVisitor
	subclass: 'CopyVisitor'
	instVarNames: #( source dest )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: 'I create a copy of the directory tree that I visit. I use the PreorderGuide so that I can create directories before creating their contents. ';
		immediateInvariant.
true.
%

doit
(FileSystemVisitor
	subclass: 'DeleteVisitor'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: 'I delete the directory tree that I visit. I use the PostorderGuide so that I can delete files before deleting their containing directories.';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'MemoryFileSystemEntry'
	instVarNames: #( creationTime modificationTime basename )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Memory';
		comment: 'I am an abstract file system entry for a memory file system.
My subclasses should specialize on the kind of file they are.';
		immediateInvariant.
true.
%

doit
(MemoryFileSystemEntry
	subclass: 'MemoryFileSystemDirectory'
	instVarNames: #( entries )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Memory';
		comment: 'I represent a memory file system entry for a directory';
		immediateInvariant.
true.
%

doit
(MemoryFileSystemEntry
	subclass: 'MemoryFileSystemFile'
	instVarNames: #( bytes size closed )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Memory';
		comment: 'I represent a memory file system entry for a regular file';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'MemoryFileWriteStream'
	instVarNames: #( file writeStream stream )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Memory';
		comment: 'A file write stream - but within memory';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'NumberParser'
	instVarNames: #( sourceStream base neg integerPart fractionPart exponent scale nDigits lastNonZero requestor failBlock )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: '';
		immediateInvariant.
true.
%

doit
(Object
	indexableSubclass: 'Path'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Path';
		comment: 'I''m a private and abstract filesystem path, independent of the string representation used to describe paths on a specific filesystem. I provide methods for navigating the filesystem hierarchy and working with absolute and relative paths. I only refer to a concrete file or directory with regard to a specific filesystem. Normally users should not use me directly. 

API instance creation:

- #from: parses the supplied string using the default delimeter
- #from:delimiter: parses the supplied string using the supplied delimiter.
- #/ creates an absolute path from the supplied string
- #* creates a relative path from the supplied string

API path manipulation:

- #/ adds the supplied string to the receiver
';
		immediateInvariant.
true.
%

doit
(Path
	subclass: 'AbsolutePath'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Path';
		comment: 'I represent an absolute path (a position starting from Path root)';
		immediateInvariant.
true.
%

doit
(Path
	subclass: 'RelativePath'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Path';
		comment: 'I represent a relative path (a position starting from Path workingDirectory)';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'PosixErrorGroup'
	instVarNames: #( errorClasses )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-ErrorGroups';
		comment: '';
		immediateInvariant.
true.
%

doit
(PosixErrorGroup
	subclass: 'ChangeDirectoryErrorGroup'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-ErrorGroups';
		comment: '';
		immediateInvariant.
true.
%

doit
(PosixErrorGroup
	subclass: 'ChangeModeErrorGroup'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-ErrorGroups';
		comment: '';
		immediateInvariant.
true.
%

doit
(PosixErrorGroup
	subclass: 'CloseDirectoryErrorGroup'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-ErrorGroups';
		comment: '';
		immediateInvariant.
true.
%

doit
(PosixErrorGroup
	subclass: 'CloseFileErrorGroup'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-ErrorGroups';
		comment: '';
		immediateInvariant.
true.
%

doit
(PosixErrorGroup
	subclass: 'CopyFileErrorGroup'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-ErrorGroups';
		comment: 'POSIX defines valid errors for each file/directory action - ex: Read, Write, Open, etc.

THis is the abstract superclass whose subclasses model the valid error messages for each file/directory action which produce error messages.

The class method #posixNames needs to be defined with posix names of the errors supported by the action.';
		immediateInvariant.
true.
%

doit
(PosixErrorGroup
	subclass: 'FileControlErrorGroup'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-ErrorGroups';
		comment: '';
		immediateInvariant.
true.
%

doit
(PosixErrorGroup
	subclass: 'FileSeekErrorGroup'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-ErrorGroups';
		comment: '';
		immediateInvariant.
true.
%

doit
(PosixErrorGroup
	subclass: 'FileSizeErrorGroup'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-ErrorGroups';
		comment: '';
		immediateInvariant.
true.
%

doit
(PosixErrorGroup
	subclass: 'FlushErrorGroup'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-ErrorGroups';
		comment: '';
		immediateInvariant.
true.
%

doit
(PosixErrorGroup
	subclass: 'FlushFileErrorGroup'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-ErrorGroups';
		comment: '';
		immediateInvariant.
true.
%

doit
(PosixErrorGroup
	subclass: 'GetCurrentWorkingDirectoryErrorGroup'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-ErrorGroups';
		comment: '';
		immediateInvariant.
true.
%

doit
(PosixErrorGroup
	subclass: 'LockFileErrorGroup'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-ErrorGroups';
		comment: '';
		immediateInvariant.
true.
%

doit
(PosixErrorGroup
	subclass: 'MakeDirectoryErrorGroup'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-ErrorGroups';
		comment: '';
		immediateInvariant.
true.
%

doit
(PosixErrorGroup
	subclass: 'OpenDirectoryErrorGroup'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-ErrorGroups';
		comment: '';
		immediateInvariant.
true.
%

doit
(PosixErrorGroup
	subclass: 'OpenFileErrorGroup'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-ErrorGroups';
		comment: '';
		immediateInvariant.
true.
%

doit
(PosixErrorGroup
	subclass: 'ReadDirectoryErrorGroup'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-ErrorGroups';
		comment: '';
		immediateInvariant.
true.
%

doit
(PosixErrorGroup
	subclass: 'ReadFileErrorGroup'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-ErrorGroups';
		comment: '';
		immediateInvariant.
true.
%

doit
(PosixErrorGroup
	subclass: 'RemoveDirectoryErrorGroup'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-ErrorGroups';
		comment: '';
		immediateInvariant.
true.
%

doit
(PosixErrorGroup
	subclass: 'RemoveFileErrorGroup'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-ErrorGroups';
		comment: '';
		immediateInvariant.
true.
%

doit
(PosixErrorGroup
	subclass: 'RenameErrorGroup'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-ErrorGroups';
		comment: '';
		immediateInvariant.
true.
%

doit
(PosixErrorGroup
	subclass: 'RenameFileErrorGroup'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-ErrorGroups';
		comment: '';
		immediateInvariant.
true.
%

doit
(PosixErrorGroup
	subclass: 'TouchFileErrorGroup'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-ErrorGroups';
		comment: 'This is the error group for calls to update the file access and modification times.';
		immediateInvariant.
true.
%

doit
(PosixErrorGroup
	subclass: 'UnlockFileErrorGroup'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-ErrorGroups';
		comment: '';
		immediateInvariant.
true.
%

doit
(PosixErrorGroup
	subclass: 'WritingErrorGroup'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-ErrorGroups';
		comment: '';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'PosixFlag'
	instVarNames: #( parent )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-FileOptions';
		comment: 'This an abstract class. All POSIX flags are to be implemented as subclasses of this class.

Each concrete subclass should implement the following class methods:
- #posixName - the posix name - ex: O_RDONLY.
- #defaultPlatformValue - the default (Linux) value of the flag - ex: 0 for O_RDONLY.
- #windowsValue - optional - if the Windows value is different from the Linux value, implement this method.';
		immediateInvariant.
true.
%

doit
(PosixFlag
	subclass: 'FileControlFlag'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-FileOptions';
		comment: 'These are flags that can be used with fcntl() calls for managing the file descriptor';
		immediateInvariant.
true.
%

doit
(FileControlFlag
	subclass: 'FileCloseOnExecutionFlag'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-FileOptions';
		comment: 'close-on-exec flag';
		immediateInvariant.
true.
%

doit
(FileControlFlag
	subclass: 'FileGetDescriptorFlag'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-FileOptions';
		comment: 'Read the file descriptor flags; arg is ignored.';
		immediateInvariant.
true.
%

doit
(FileControlFlag
	subclass: 'FileGetLockFlag'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-FileOptions';
		comment: 'see https://linux.die.net/man/2/fcntl';
		immediateInvariant.
true.
%

doit
(FileControlFlag
	subclass: 'FileGetStatusFlag'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-FileOptions';
		comment: '';
		immediateInvariant.
true.
%

doit
(FileControlFlag
	subclass: 'FileSetDescriptorFlag'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-FileOptions';
		comment: 'Set the file descriptor flags to the value specified by arg';
		immediateInvariant.
true.
%

doit
(FileControlFlag
	subclass: 'FileSetLockFlag'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-FileOptions';
		comment: 'Acquire a lock (when l_type is F_RDLCK or F_WRLCK) or release a lock (when l_type is F_UNLCK) on the bytes specified by the l_whence, l_start, and l_len fields of lock. If a conflicting lock is held by another process, this call returns -1 and sets errno to EACCES or EAGAIN.';
		immediateInvariant.
true.
%

doit
(FileControlFlag
	subclass: 'FileSetLockWaitFlag'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-FileOptions';
		comment: 'As for F_SETLK, but if a conflicting lock is held on the file, then wait for that lock to be released. If a signal is caught while waiting, then the call is interrupted and (after the signal handler has returned) returns immediately (with return value -1 and errno set to EINTR; see signal(7)).';
		immediateInvariant.
true.
%

doit
(FileControlFlag
	subclass: 'FileSetStatusFlag'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-FileOptions';
		comment: 'Set the file status flags to the value specified by arg.

File access mode (O_RDONLY, O_WRONLY, O_RDWR) and file creation flags (i.e., O_CREAT, O_EXCL, O_NOCTTY, O_TRUNC) in arg are ignored.

On Linux this command can change only the O_APPEND, O_ASYNC, O_DIRECT, O_NOATIME, and O_NONBLOCK flags.

https://linux.die.net/man/2/fcntl';
		immediateInvariant.
true.
%

doit
(FileControlFlag
	subclass: 'FileSetUnlockFlag'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-FileOptions';
		comment: 'Remove our lease from the file.';
		immediateInvariant.
true.
%

doit
(PosixFlag
	subclass: 'FileDirectoryStreamFlag'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-FileOptions';
		comment: 'The subclasses are flags for filtering the items returned from the directory stream';
		immediateInvariant.
true.
%

doit
(FileDirectoryStreamFlag
	subclass: 'FileDirectoryEntryFlag'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-FileOptions';
		comment: 'This flag is used to get directory entries from the directory stream';
		immediateInvariant.
true.
%

doit
(FileDirectoryStreamFlag
	subclass: 'FileRegularFileEntryFlag'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-FileOptions';
		comment: 'This flag is used to get regular file entries from the directory stream';
		immediateInvariant.
true.
%

doit
(FileDirectoryStreamFlag
	subclass: 'FileSpecialFileEntryFlag'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-FileOptions';
		comment: 'The subclasses are flags for filtering the items returned from the directory stream.
Specifying all three flags will result in all entries in the directory stream being returned.';
		immediateInvariant.
true.
%

doit
(PosixFlag
	subclass: 'FileLockFlag'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-FileOptions';
		comment: '';
		immediateInvariant.
true.
%

doit
(FileLockFlag
	subclass: 'FileMandatoryLockFlag'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-FileOptions';
		comment: 'Exclusive MANDATORY lock.

Windows supports this type of lock.';
		immediateInvariant.
true.
%

doit
(FileLockFlag
	subclass: 'FileReadLockFlag'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-FileOptions';
		comment: 'Used to indicate a shared (read) ADVISORY lock when the file is opened.

Linux supports this type of lock.';
		immediateInvariant.
true.
%

doit
(FileLockFlag
	subclass: 'FileUnlockFlag'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-FileOptions';
		comment: 'These are file locking flags for use in Linux';
		immediateInvariant.
true.
%

doit
(FileLockFlag
	subclass: 'FileWriteLockFlag'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-FileOptions';
		comment: 'Used to indicate a exclusive (write) ADVISORY lock when the file is opened.

Linux supports this type of lock.';
		immediateInvariant.
true.
%

doit
(PosixFlag
	subclass: 'FileOpeningFlag'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-FileOptions';
		comment: 'File Creation Flags:
   These are the options (flags) that control various aspects of the behavior of the open() call, as well as options for subsequent I/O operations.
   These flags cant be retrieved or changed.

For more details see: https://linux.die.net/man/3/open';
		immediateInvariant.
true.
%

doit
(FileOpeningFlag
	subclass: 'FileAppendFlag'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-FileOptions';
		comment: 'The following is from http://man7.org/linux/man-pages/man2/open.2.html

O_APPEND
              The file is opened in append mode.  Before each write(2), the
              file offset is positioned at the end of the file, as if with
              lseek(2).  The modification of the file offset and the write
              operation are performed as a single atomic step.

              O_APPEND may lead to corrupted files on NFS filesystems if
              more than one process appends data to a file at once.  This is
              because NFS does not support appending to a file, so the
              client kernel has to simulate it, which can''t be done without
              a race condition.';
		immediateInvariant.
true.
%

doit
(FileOpeningFlag
	subclass: 'FileCloseOnExecFlag'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-FileOptions';
		comment: 'From http://man7.org/linux/man-pages/man2/open.2.html

O_CLOEXEC

Enable the close-on-exec flag for the new file descriptor.
Specifying this flag permits a program to avoid additional
fcntl(2) F_SETFD operations to set the FD_CLOEXEC flag.

Note that the use of this flag is essential in some
multithreaded programs, because using a separate fcntl(2)
F_SETFD operation to set the FD_CLOEXEC flag does not suffice
to avoid race conditions where one thread opens a file
descriptor and attempts to set its close-on-exec flag using
fcntl(2) at the same time as another thread does a fork(2)
plus execve(2).  Depending on the order of execution, the race
may lead to the file descriptor returned by open() being
unintentionally leaked to the program executed by the child
process created by fork(2).  (This kind of race is in
principle possible for any system call that creates a file
descriptor whose close-on-exec flag should be set, and various
other Linux system calls provide an equivalent of the
O_CLOEXEC flag to deal with this problem.)';
		immediateInvariant.
true.
%

doit
(FileOpeningFlag
	subclass: 'FileCreateFlag'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-FileOptions';
		comment: 'O_CREAT

If the file doesnt already exist, it is created as a new, empty file.
This flag is effective even if the file is being opened only for reading.
If we specify O_CREAT, then we must supply a mode argument in the open() call;
otherwise, the permissions of the new file will be set to some random value from the stack.';
		immediateInvariant.
true.
%

doit
(FileOpeningFlag
	subclass: 'FileDirectFlag'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-FileOptions';
		comment: 'O_DIRECT

Try to minimize cache effects of the I/O to and from this file.  In general this will degrade performance, but it is useful in special situations, such as when applications do their own caching.  File I/O is done directly to/from user-space buffers.  The O_DIRECT flag on its own makes an effort to transfer data synchronously, but does not give the guarantees of the O_SYNC flag that data and necessary metadata are transferred.  To guarantee synchronous I/O, O_SYNC must be used in addition to O_DIRECT.

====

Allow file I/O to bypass the buffer cache. The _GNU_SOURCE feature test macro must be defined in order to make this constant definition available from <fcntl.h>.

O_DIRECT alone only promises that the kernel will avoid copying data from user space to kernel space, and will instead write it directly via DMA (Direct memory access; if possible). Data does not go into caches. There is no strict guarantee that the function will return only after all data has been transferred.

O_SYNC guarantees that the call will not return before all data has been transferred to the disk (as far as the OS can tell). This still does not guarantee that the data isn''t somewhere in the harddisk write cache, but it is as much as the OS can guarantee.

O_DIRECT|O_SYNC is the combination of these, i.e. "DMA + guarantee".';
		immediateInvariant.
true.
%

doit
(FileOpeningFlag
	subclass: 'FileDirectoryFlag'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-FileOptions';
		comment: 'O_DIRECTORY

Return an error (errno equals ENOTDIR) if pathname is not a directory.
This flag is an extension designed specifically for implementing opendir().
The _GNU_SOURCE feature test macro must be defined in order to make this constant definition available from <fcntl.h>.

If path resolves to a non-directory file, fail and set errno to [ENOTDIR].';
		immediateInvariant.
true.
%

doit
(FileOpeningFlag
	subclass: 'FileExclusiveFlag'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-FileOptions';
		comment: 'The following is from: http://man7.org/linux/man-pages/man2/open.2.html

O_EXCL   Ensure that this call creates the file: if this flag is
              specified in conjunction with O_CREAT, and pathname already
              exists, then open() fails with the error EEXIST.

              When these two flags are specified, symbolic links are not
              followed: if pathname is a symbolic link, then open() fails
              regardless of where the symbolic link points.

              In general, the behavior of O_EXCL is undefined if it is used
              without O_CREAT.  There is one exception: on Linux 2.6 and
              later, O_EXCL can be used without O_CREAT if pathname refers
              to a block device.  If the block device is in use by the
              system (e.g., mounted), open() fails with the error EBUSY.

              On NFS, O_EXCL is supported only when using NFSv3 or later on
              kernel 2.6 or later.  In NFS environments where O_EXCL support
              is not provided, programs that rely on it for performing
              locking tasks will contain a race condition.  Portable
              programs that want to perform atomic file locking using a
              lockfile, and need to avoid reliance on NFS support for
              O_EXCL, can create a unique file on the same filesystem (e.g.,
              incorporating hostname and PID), and use link(2) to make a
              link to the lockfile.  If link(2) returns 0, the lock is
              successful.  Otherwise, use stat(2) on the unique file to
              check if its link count has increased to 2, in which case the
              lock is also successful.';
		immediateInvariant.
true.
%

doit
(FileOpeningFlag
	subclass: 'FileKeepSymbolicLinksFlag'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-FileOptions';
		comment: 'Dont dereference symbolic links. If path names a symbolic link, fail and set errno to [ELOOP].

Normally, open() dereferences pathname if it is a symbolic link. However, if the O_NOFOLLOW flag is specified, then open() fails (with errno set to ELOOP) if pathname is a symbolic link. This flag is useful, especially in privileged programs, for ensuring that open() doesnt dereference a symbolic link. To expose the definition of this flag from <fcntl.h>, we must define the _GNU_SOURCE feature test macro.';
		immediateInvariant.
true.
%

doit
(FileOpeningFlag
	subclass: 'FileNonBlockFlag'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-FileOptions';
		comment: '';
		immediateInvariant.
true.
%

doit
(FileOpeningFlag
	subclass: 'FileNotControllingTerminalFlag'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-FileOptions';
		comment: 'Dont let pathname become the controlling terminal.

If the file being opened is a terminal device, prevent it from becoming the controlling terminal. If the file being opened is not a terminal, this flag has no effect.';
		immediateInvariant.
true.
%

doit
(FileOpeningFlag
	subclass: 'FileOpenModeFlag'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-FileOptions';
		comment: 'File access mode flags:

The subclasses implement the modes for opening a Posix compliant file.
These modes are mutually exclusive.

In C they can be retrieved using the fcntl() F_GETFL operation';
		immediateInvariant.
true.
%

doit
(FileOpenModeFlag
	subclass: 'FileOpenReadOnlyFlag'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-FileOptions';
		comment: 'Open the file for reading only';
		immediateInvariant.
true.
%

doit
(FileOpenModeFlag
	subclass: 'FileOpenReadWriteFlag'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-FileOptions';
		comment: 'Open the file for both reading and writing';
		immediateInvariant.
true.
%

doit
(FileOpenModeFlag
	subclass: 'FileOpenWriteOnlyFlag'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-FileOptions';
		comment: 'Open the file for writing only';
		immediateInvariant.
true.
%

doit
(FileOpeningFlag
	subclass: 'FileReadSynchronousFlag'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-FileOptions';
		comment: 'Write I/O operations on the file descriptor shall complete as defined by synchronized I/O file integrity completion.';
		immediateInvariant.
true.
%

doit
(FileOpeningFlag
	subclass: 'FileSychronizedDataFlag'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-FileOptions';
		comment: 'Write I/O operations on the file descriptor shall complete as defined by synchronized I/O data integrity completion.

If both the O_SYNC and O_DSYNC flags are set, the effect is as if only the O_SYNC flag was set.';
		immediateInvariant.
true.
%

doit
(FileOpeningFlag
	subclass: 'FileSynchronizedIOFlag'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-FileOptions';
		comment: 'Write I/O operations on the file descriptor shall complete as defined by synchronized I/O file integrity completion.

If both the O_SYNC and O_DSYNC flags are set, the effect is as if only the O_SYNC flag was set.';
		immediateInvariant.
true.
%

doit
(FileOpeningFlag
	subclass: 'FileSynchronizedReadFlag'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-FileOptions';
		comment: 'close-on-exec flag.

if the FD_CLOEXEC bit is 0, the file descriptor will remain open across an execve(2), otherwise it will be closed.';
		immediateInvariant.
true.
%

doit
(FileOpeningFlag
	subclass: 'FileTruncateFlag'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-FileOptions';
		comment: 'If the file already exists and is a regular file, then truncate it to zero length, destroying any existing data.

On Linux, truncation occurs whether the file is being opened for reading or writing (in both cases, we must have write permission on the file). SUSv3 leaves the combination of O_RDONLY and O_TRUNC unspecified, but most other UNIX implementations behave in the same way as Linux.';
		immediateInvariant.
true.
%

doit
(PosixFlag
	subclass: 'FilePermissionFlag'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-FileOptions';
		comment: 'The subclasses of this class define POSIX style permission options.

For Windows, there are only two applicable options: Read and Write.
So one only needs to use FileOwnerReadPermissionFlag, FileOwnerWritePermissionFlag, or FileOwnerAllPermissionFlag with Windows files.
All other PermissionFlags are ignored.';
		immediateInvariant.
true.
%

doit
(FilePermissionFlag
	subclass: 'FileGroupAllPermissionFlag'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-FileOptions';
		comment: 'Specifies permission for users in the file group class either:

    To read, write, and search, if the file is a directory
    To read, write, and execute, for a file other than a directory

This value has the same effect as specifying all three parameters (SIRGRP, SIWGRP, and SIXGRP).

If applied to a WindowsStore, this has no effect.';
		immediateInvariant.
true.
%

doit
(FilePermissionFlag
	subclass: 'FileGroupExecutePermissionFlag'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-FileOptions';
		comment: 'Specifies permission for users in the file group class either:

    To search, if the file is a directory
    To execute the program in the file, for a file other than a directory.

If applied to a WindowsStore, this has no effect.';
		immediateInvariant.
true.
%

doit
(FilePermissionFlag
	subclass: 'FileGroupReadPermissionFlag'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-FileOptions';
		comment: 'Specifies permission for users in the file group class to read the file.

If applied to a WindowsStore, this has no effect.';
		immediateInvariant.
true.
%

doit
(FilePermissionFlag
	subclass: 'FileGroupWritePermissionFlag'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-FileOptions';
		comment: 'Specifies permission for users in the file owner class either:

    To search, if the file is a directory
    To execute the program in the file, for a file other than a directory.

If applied to a WindowsStore, this has no effect.';
		immediateInvariant.
true.
%

doit
(FilePermissionFlag
	subclass: 'FileOtherAllPermissionFlag'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-FileOptions';
		comment: 'Specifies permission for users in the file other class either:

-   To read, write, and search, if the file is a directory
-   To read, write, and execute, for a file other than a directory

This value has the same effect as specifying all three parameters (SIROTH, SIWOTH, and SIXOTH).

If applied to a WindowsStore, this has no effect.';
		immediateInvariant.
true.
%

doit
(FilePermissionFlag
	subclass: 'FileOtherExecutePermissionFlag'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-FileOptions';
		comment: 'Specifies permission for users in the file other class either:

    To search, if the file is a directory
    To execute the program in the file, for a file other than a directory.

If applied to a WindowsStore, this has no effect.';
		immediateInvariant.
true.
%

doit
(FilePermissionFlag
	subclass: 'FileOtherReadPermissionFlag'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-FileOptions';
		comment: 'Specifies permission for users in the file other class to read the file.

If applied to a WindowsStore, this has no effect.';
		immediateInvariant.
true.
%

doit
(FilePermissionFlag
	subclass: 'FileOtherWritePermissionFlag'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-FileOptions';
		comment: 'Specifies permission for users in the file other class to write the file.

If applied to a WindowsStore, this has no effect.';
		immediateInvariant.
true.
%

doit
(FilePermissionFlag
	subclass: 'FileOwnerAllPermissionFlag'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-FileOptions';
		comment: 'Specifies permission for the file owner either:

-    To read, write, and search, if the file is a directory
-    To read, write, and execute, for a file other than a directory

This value has the same effect as specifying all three parameters (SIRUSR, SIWUSR, and SIXUSR)';
		immediateInvariant.
true.
%

doit
(FilePermissionFlag
	subclass: 'FileOwnerExecutePermissionFlag'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-FileOptions';
		comment: 'Specifies permission for users in the file owner class either:

    To search, if the file is a directory
    To execute the program in the file, for a file other than a directory.

In windows this option has no effect.';
		immediateInvariant.
true.
%

doit
(FilePermissionFlag
	subclass: 'FileOwnerReadPermissionFlag'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-FileOptions';
		comment: 'Specifies permission for users in the file owner class either:

    To read the file or directory

For Windows, this returns a 1 for read.';
		immediateInvariant.
true.
%

doit
(FilePermissionFlag
	subclass: 'FileOwnerWritePermissionFlag'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-FileOptions';
		comment: 'Specifies permission for users in the file owner class either:

    To write to the file or directory

For Windows, this returns a 2.';
		immediateInvariant.
true.
%

doit
(PosixFlag
	subclass: 'FilePositioning'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-FileOptions';
		comment: '';
		immediateInvariant.
true.
%

doit
(FilePositioning
	subclass: 'FileSeekCurrentPosition'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-FileOptions';
		comment: 'SEEK_CUR  It moves file pointer position to given location.';
		immediateInvariant.
true.
%

doit
(FilePositioning
	subclass: 'FileSeekEndPosition'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-FileOptions';
		comment: 'SEEK_END  It moves file pointer position to the end of file.';
		immediateInvariant.
true.
%

doit
(FilePositioning
	subclass: 'FileSeekSetPosition'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-FileOptions';
		comment: 'SEEK_SET  It moves file pointer position to the beginning of the file.';
		immediateInvariant.
true.
%

doit
(PosixFlag
	subclass: 'FileShareFlag'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-FileOptions';
		comment: '';
		immediateInvariant.
true.
%

doit
(FileShareFlag
	subclass: 'FileDenyNoneFlag'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-FileOptions';
		comment: 'Other processes can open the file for any access: read-only, write-only, or read-write.';
		immediateInvariant.
true.
%

doit
(FileShareFlag
	subclass: 'FileDenyReadFlag'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-FileOptions';
		comment: 'Other processes can open the file for write-only access, but they cannot open it for read-only or read-write access.';
		immediateInvariant.
true.
%

doit
(FileShareFlag
	subclass: 'FileDenyReadWriteFlag'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-FileOptions';
		comment: 'The current process has exclusive access to the file.
Other processes may not open the file.   It is unspecified whether
the file may be opened by the current process.';
		immediateInvariant.
true.
%

doit
(FileShareFlag
	subclass: 'FileDenyWriteFlag'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-FileOptions';
		comment: 'Other processes can open the file for read-only access, but they cannot open it for write-only or read-write access.';
		immediateInvariant.
true.
%

doit
(PosixFlag
	subclass: 'FileTypeFlag'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-FileOptions';
		comment: 'Abstract class to support file options. These are used when opening the file.';
		immediateInvariant.
true.
%

doit
(FileTypeFlag
	subclass: 'FileBinaryTypeFlag'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-FileOptions';
		comment: 'A binary file is basically any file that is not "line-oriented". Any file where besides the actual written characters and newlines there are other symbols as well.

No CR-LF translation: Explicitly opens the file in binary mode.

This is Windows only';
		immediateInvariant.
true.
%

doit
(FileTypeFlag
	subclass: 'FileGzipTypeFlag'
	instVarNames: #( compressionLevel )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-FileOptions';
		comment: 'GemStone has a gzipped compression option which this class supports.

Gemstone supports levels of: none, 1 and 9.

The main prupose of this class is to ensure the file opening mode string is properly created.';
		immediateInvariant.
true.
%

doit
(FileTypeFlag
	subclass: 'FileTextTypeFlag'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Base-FileOptions';
		comment: 'Used to indicate a exclusive (write) ADVISORY lock when the file is opened.

Take out a write lease. This will cause the caller to be notified when the file is opened for reading or writing or is truncated. A write lease may be placed on a file only if there are no other open file descriptors for the file.

Linux supports this type of lock.';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'ProcessSpecificVariable'
	instVarNames: #( index )
	classVars: #(  )
	classInstVars: #( soleInstance )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Zinc-Character-Encoding-Core';
		comment: '';
		immediateInvariant.
true.
%

doit
(ProcessSpecificVariable
	subclass: 'DynamicVariable'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Zinc-Character-Encoding-Core';
		comment: '';
		immediateInvariant.
true.
%

doit
(DynamicVariable
	subclass: 'ZnDefaultCharacterEncoder'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Zinc-Character-Encoding-Core';
		comment: '';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'Stdio'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: 'I am a facade class to access standard output streams:

- standard input
- standard output
- standard error

I initialize standard streams in a lazy fashion, asking to the VM for the #stdioHandles. I cache those standard streams and release them on shutdown.

The possible scenarios, i.e. for each of stdin, stderr and stdout:

    the stream is attached to a terminal (default on posix platforms, PharoConsole.exe on Windows (see below for Pharo.exe))
    the stream is redirected to a normal file, e.g. pharo pharo.image > output.txt
    the stream is redirected to a pipe, e.g. pharo pharo.image | tee output.txt

To know exactly which kind of stream you are using, you can use File >> #fileDescriptorType: (args are 0 1 2 for the stdios).


Windows Subtleties - This is from Pharo
=====================

If launched as a desktop app (not from the console), Pharo.exe will not be linked to external streams because none is created. To overcome that, the default behavior of this class is to create a normal file for the three stdio. This can be modified to:
	not create a file (execute #useNullStreams)
	use a memory stream (execute #useMemoryStreams).';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'ZnBase64Encoder'
	instVarNames: #( alphabet inverse lineLength lineEnd whitespace )
	classVars: #( DefaultAlphabet DefaultInverse )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Zinc-Character-Encoding-Core';
		comment: 'I am ZnBase64Encoder.

Base64 encoding is a technique to encode binary data as a string of characters that can be safely transported over various protocols. Basically, every 3 bytes are encoded using 4 characters from an alphabet of 64. Each encoded character represents 6 bits.

The most commonly used alphabet is ''ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/''. One or two equal signs (= or ==) are used for padding.

  ZnBase64Encoder new encode: #[0 1 2 3 4 5].
  ZnBase64Encoder new encode: #[10 20]
  ZnBase64Encoder new decode: ''BQQDAgEA''.
  ZnBase64Encoder new decode: ''FAo=''.

The encoded data can optionally be broken into lines. Characters not part of the alphabet are considered as white space and are ignored when inbetween groups of 4 characters.

My #encode: method works from ByteArray to String, while my #decode: method works from String to ByteArray.

Note that to encode a String as Base64, you first have to encode the characters as bytes using a character encoder.

See also http://en.wikipedia.org/wiki/Base64

Part of Zinc HTTP Components.';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'ZnBufferedReadStream'
	instVarNames: #( stream buffer position limit )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Zinc-Character-Encoding-Core';
		comment: 'I am ZnBufferedReadStream.

I wrap another ReadStream and add efficient buffering for the typical access pattern of parsers: sending lots of #next, #peek and #atEnd messages.

By design I do not implement #position and #position: or anything based on that.

I can wrap both binary or character streams and act accordingly.

Part of Zinc HTTP Components.';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'ZnBufferedReadWriteStream'
	instVarNames: #( readStream writeStream lastRead )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Zinc-Character-Encoding-Core';
		comment: 'I am ZnBufferedReadWriteStream.
I wrap a buffered read stream and a buffered write stream on the same file.

I discard my read buffer on writes, and flush my write buffer on reads.
Make sure to always send me #flush or #close when you''re done,
otherwise the last buffer might not yet have been written.
My class side''s #on:do: helps to ensure this.

I can wrap both binary or character streams and act accordingly.

Part of Zinc HTTP Components.';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'ZnBufferedWriteStream'
	instVarNames: #( stream buffer position )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Zinc-Character-Encoding-Core';
		comment: 'I am ZnBufferedWriteStream.
I wrap a write stream and add buffering.

Make sure to always send me #flush or #close when you''re done,
otherwise the last buffer might not yet have been written.
My class side''s #on:do: helps to ensure this.

I can wrap both binary or character streams and act accordingly.

Part of Zinc HTTP Components.';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'ZnCharacterEncoder'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Zinc-Character-Encoding-Core';
		comment: 'I am ZnCharacterEncoder, I encode and decode Character objects to and from a binary stream.
I am an abstract class with following protocol:

#nextFromStream:
#nextPut:toStream:
#encodedByteCountFor:
#backOnStream:

The first two are compatible with TextConverter and subclasses.

I add some convenience methods:

#encodeString:
#decodeBytes:
#encodedByteCountForString:

Contrary to older encoders, I work strictly from strings to bytes and vice versa and I will throw errors instead of silently ignoring them.

I also implement optimized bulk operations:

#next:putAll:startingAt:toStream:
#readInto:startingAt:count:fromStream:

Additionally, I can encode Integer code points to a binary stream as well as read Integer code points from a binary stream. This is in a sense a more fundamental operation that avoids instanciating Character objects.

#nextCodePointFromStream:
#nextPutCodePoint:toStream:
#encodedByteCountForCodePoint:

#decodeAsCodePoints:
#encodeCodePoints:
#encodedByteCountForCodePoints:

Part of Zinc HTTP Components.';
		immediateInvariant.
true.
%

doit
(ZnCharacterEncoder
	subclass: 'ZnNullEncoder'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Zinc-Character-Encoding-Core';
		comment: 'I am ZnNullEncoder, a concrete subclass of ZnCharacterEncoder.
I perform no encoding or decoding at all for all characters with a code value below 256.

Note that in principle I could handle Latin1 (ISO-8859-1) or ASCII, although that is not completely correct. To get maximum efficiency, it remains an option.
	
Part of Zinc HTTP Components.';
		immediateInvariant.
true.
%

doit
(ZnCharacterEncoder
	subclass: 'ZnSimplifiedByteEncoder'
	instVarNames: #( identifier byteToUnicode unicodeToByte strict )
	classVars: #(  )
	classInstVars: #( byteTextConverters )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Zinc-Character-Encoding-Core';
		comment: 'I am ZnSimplifiedByteEncoder, a concrete subclass of ZnCharacterEncoder.
I handle single byte encodings where byte values 0 to 127 map to ASCII and 128 to 255 are a permutation to Unicode characters.

I am like ZnByteEncoder, a subclass of me, but I implement just two mappings, latin1 or iso-8859-1 and ASCII, to conserve memory.';
		immediateInvariant.
true.
%

doit
(ZnSimplifiedByteEncoder
	subclass: 'ZnByteEncoder'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Zinc-Character-Encoding-Core';
		comment: 'I am ZnByteEncoder, a concrete subclass of ZnCharacterEncoder.
I handle single byte encodings where byte values 0 to 127 map to ASCII and 128 to 255 are a permutation to Unicode characters.

I derive my mappings by parsing official unicode.org specifications.

The list of encodings and their names/aliases was taken from http://encoding.spec.whatwg.org/#legacy-single-byte-encodings

I basically support ISO/IEC 8859 1, 2, 3, 4, 5, 6, 7, 8, 10, 13, 14, 15 and 16, Windows Codepages 866, 874, 1250, 1251, 1252, 1253, 1253, 1254, 1255, 1256, 1257, 1258, KOI8 R & U as well as Mac Roman & Cyrillic - each of these with a number of aliases like latin1, latin2, latin3, latin4, latin5, latin6, cyrillic, arabic, greek and hebrew. See #mappingToIdentifiers

Note that most/all of these encodings should be considered legacy, with UTF-8 being the preferred encoding going forward.  

Part of Zinc HTTP Components.';
		immediateInvariant.
true.
%

doit
(ZnCharacterEncoder
	subclass: 'ZnUTFEncoder'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Zinc-Character-Encoding-Core';
		comment: 'I am ZnUTFEncoder. I am a ZnCharacterEncoder. My subclasses deal with the full range of Unicode character code points.';
		immediateInvariant.
true.
%

doit
(ZnUTFEncoder
	subclass: 'ZnEndianSensitiveUTFEncoder'
	instVarNames: #( endianness )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Zinc-Character-Encoding-Core';
		comment: 'I am ZnEndianSensitiveUTFEncoder.
I am a ZnCharacterEncoder.
I add support for UTF encodings that are sensitive to endianness.
The default is big endian.';
		immediateInvariant.
true.
%

doit
(ZnEndianSensitiveUTFEncoder
	subclass: 'ZnUTF16Encoder'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Zinc-Character-Encoding-Core';
		comment: 'I am ZnUTF16Encoder, a concrete subclass of ZnCharacterEncoder.
I implement the variable length UTF-16 encoding and decoding of Unicode according to RFC 2781.

Wikipedia reference http://en.wikipedia.org/wiki/UTF-16

Part of Zinc HTTP Components.';
		immediateInvariant.
true.
%

doit
(ZnEndianSensitiveUTFEncoder
	subclass: 'ZnUTF32Encoder'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Zinc-Character-Encoding-Core';
		comment: 'I am ZnUTF32Encoder, a concrete subclass of ZnCharacterEncoder.
I implement the fixed length UTF-32 encoding and decoding of Unicode according to http://www.unicode.org/versions/Unicode8.0.0/ch03.pdf definitions D90, D99, D100 and D101.

Wikipedia reference http://en.wikipedia.org/wiki/UTF-32

UCS-4 is another name for the same encoding.

Part of Zinc HTTP Components.';
		immediateInvariant.
true.
%

doit
(ZnUTFEncoder
	subclass: 'ZnUTF8Encoder'
	instVarNames: #(  )
	classVars: #( ByteASCIISet ByteUTF8Encoding Default )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Zinc-Character-Encoding-Core';
		comment: 'I am the GemStone/S implementation of ZnUTF8Encoder.
I implement the variable length UTF-8 encoding and decoding of Unicode according to RFC 3629.

Wikipedia reference http://en.wikipedia.org/wiki/UTF-8

Part of Zinc HTTP Components.

I use the ICU library to encode strings and decode ByteArray and Utf8 instances ... ';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'ZnCharacterReadWriteStream'
	instVarNames: #( readStream writeStream )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Zinc-Character-Encoding-Core';
		comment: 'I am a read-write character stream. I am mainly used to open the Pharo source and changes files.';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'ZnEncodedStream'
	instVarNames: #( stream encoder )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Zinc-Character-Encoding-Core';
		comment: 'I am ZnEncodedStream, an abstract support class for read and write streams on an encoded binary stream.

Part of Zinc HTTP Components';
		immediateInvariant.
true.
%

doit
(ZnEncodedStream
	subclass: 'ZnEncodedReadStream'
	instVarNames: #( peeked )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Zinc-Character-Encoding-Core';
		comment: 'I am ZnEncodedReadStream, an abstract support class for read streams on a binary encoded wrapped stream.

Part of Zinc HTTP Components';
		immediateInvariant.
true.
%

doit
(ZnEncodedReadStream
	subclass: 'ZnCharacterReadStream'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Zinc-Character-Encoding-Core';
		comment: 'I am ZnCharacterReadStream.
I wrap another binary ReadStream and use a ZnCharacerEncoder to allow Characters to be read.

I am not positionable, but I do allow a one character peek using a one character internal buffer.

Part of Zinc HTTP Components.';
		immediateInvariant.
true.
%

doit
(ZnEncodedReadStream
	subclass: 'ZnCodePointReadStream'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Zinc-Character-Encoding-Core';
		comment: 'I am ZnCodePointReadStream.
I wrap another binary ReadStream and use a ZnCharacerEncoder to allow Integer code points to be read.

I am not positionable, but I do allow a one code point peek using a one code point internal buffer.

Part of Zinc HTTP Components.';
		immediateInvariant.
true.
%

doit
(ZnEncodedStream
	subclass: 'ZnEncodedWriteStream'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Zinc-Character-Encoding-Core';
		comment: 'I am ZnEncodedWriteStream, an abstract support class for write streams on a binary encoded wrapped stream.

Part of Zinc HTTP Components';
		immediateInvariant.
true.
%

doit
(ZnEncodedWriteStream
	subclass: 'ZnCharacterWriteStream'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Zinc-Character-Encoding-Core';
		comment: 'I am ZnCharacterWriteStream.
I wrap another binary WriteStream and use a ZnCharacerEncoder to allow Characters to be written.

Part of Zinc HTTP Components.';
		immediateInvariant.
true.
%

doit
(ZnEncodedWriteStream
	subclass: 'ZnCodePointWriteStream'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Zinc-Character-Encoding-Core';
		comment: 'I am ZnCodePointWriteStream.
I wrap another binary WriteStream and use a ZnCharacerEncoder to allow Integer code points to be written.

Part of Zinc HTTP Components.';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'ZnEndianessReadWriteStream'
	instVarNames: #( stream )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Zinc-Character-Encoding-Core';
		comment: 'I am a stream decorator that knows how to read and write little endian numbers from my underlying stream.';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'ZnFastLineReader'
	instVarNames: #( readStream cr lf bufferStream )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Zinc-Character-Encoding-Core';
		comment: 'I am ZnFastLineReader, a helper to efficiently read CR, LF or CRLF terminated lines from a character stream.';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'ZnPercentEncoder'
	instVarNames: #( characterEncoder safeSet decodePlusAsSpace )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Zinc-Character-Encoding-Core';
		comment: 'I am ZnPositionableReadStream.
I am polymorphic with (the most used/important methods of) ReadStream and PositionableStream.

I wrap another read stream and store the elements that I read in a sliding circular buffer so that I am able to go back to any position inside that buffer. 

Essentially, I implement #position and #position: to be used to back out of reading ahead.

Note that the size of my buffer limits how far I can go backwards. A SubscriptOutOfBounds exception will be signalled when an attempt is made to go too far backwards.

The index returned by #position should be considered abstract, without concrete meaning, but it is currently implemented as the count of elements read by #next on the wrapped stream. On a simple stream over an in memory collection, that will be equivalent to an integer index into that collection. But on network streams or streams that were already further along, this will no longer be the case.

The most elementary example of my capabilities can be seen in my implementation of #peek. See also the unit tests #testPlainExcursion and #testSearch

Of course, backing out of an excursion is only possible within the window of the buffer size.

Implementation

- stream <ReadStream> the read stream that I wrap and add positioning to
- buffer <String|ByteArray> sliding, circular buffer
- index <PositiveInteger> zero based index into buffer, where next will be stored
- count <PositiveInteger> number of next operations done on wrapped stream
- delta <PositiveInteger> number of positions that I was moved backwards

The real core methods are #next, #atEnd, #position and #position: and are used to implement the rest.

Part of Zinc HTTP Components';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'ZnPositionableReadStream'
	instVarNames: #( stream buffer count index delta )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Zinc-Character-Encoding-Core';
		comment: 'I am ZnPositionableReadStream.
I am polymorphic with (the most used/important methods of) ReadStream and PositionableStream.

I wrap another read stream and store the elements that I read in a sliding circular buffer so that I am able to go back to any position inside that buffer. 

Essentially, I implement #position and #position: to be used to back out of reading ahead.

Note that the size of my buffer limits how far I can go backwards. A SubscriptOutOfBounds exception will be signalled when an attempt is made to go too far backwards.

The index returned by #position should be considered abstract, without concrete meaning, but it is currently implemented as the count of elements read by #next on the wrapped stream. On a simple stream over an in memory collection, that will be equivalent to an integer index into that collection. But on network streams or streams that were already further along, this will no longer be the case.

The most elementary example of my capabilities can be seen in my implementation of #peek. See also the unit tests #testPlainExcursion and #testSearch

Of course, backing out of an excursion is only possible within the window of the buffer size.

Implementation

- stream <ReadStream> the read stream that I wrap and add positioning to
- buffer <String|ByteArray> sliding, circular buffer
- index <PositiveInteger> zero based index into buffer, where next will be stored
- count <PositiveInteger> number of next operations done on wrapped stream
- delta <PositiveInteger> number of positions that I was moved backwards

The real core methods are #next, #atEnd, #position and #position: and are used to implement the rest.

Part of Zinc HTTP Components';
		immediateInvariant.
true.
%

doit
(Stream
	subclass: 'AbstractBinaryFileStream'
	instVarNames: #( terminal )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Streams';
		comment: 'Provides a simple, platform-independent, file stream. I am 
   - binary
   - not buffered
   - provide no encoding/conversions

!Examples of usage

"Creating a file"
stream := (File named: ''asd.txt'' asFileReference fullName) readStream.

"Accessing the stream properties"
stream position.
stream atEnd.

"Writing"
stream nextPut: 17.
stream nextPutAll: ''sdd''.

"Reading"
stream next.
stream next: 2.

"Skipping"
stream skip: 2. 

"reading up to something"
stream upTo: 23.
stream upToAnyOf: #[ 13 30 ].

"peeking"
stream peek.';
		immediateInvariant.
true.
%

doit
(AbstractBinaryFileStream
	subclass: 'BinaryFileStream'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Streams';
		comment: 'I am a concrete subclass of AbstractBinaryFileStream for normal files. Regardless the position of the file, I will make my operarions on my position and then return the file it''s own position.

In addition to my superclass'' API I provide the following methods.

stream upToEnd
"reads the full stream up to the end and returns the contents"';
		immediateInvariant.
true.
%

doit
(AbstractBinaryFileStream
	subclass: 'StdioStream'
	instVarNames: #( peekBuffer )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Streams';
		comment: 'I am a concrete subclass of AbstractBinaryFileStream for stdio streams.

StdioStreams map to one of three types of underlying file: 

- Terminal input/output.
- Piped input/output and named pipes (FIFO files) such as created with the shell pipe character "|".
- A file mounted on the file system.
  This includes all the files that can be opened with a FileReference, including sysfs files such as /proc/cpuinfo and character devices such as /dev/urandom.

The operations that can be performed on these vary, e.g. it is possible to position the stream for a regular file, but not for piped input.  Currently it is up to the user of StdioStream to know which type of input they are dealing with.

As pipes can''t be positioned and FilePlugin doesn''t provide a peek primitive, simulate #peek by reading the next character and holding on to it until it is consumed.

Despite providing both input and output methods, StdioStreams are either read-only or write-only.  Currently it is up to the user to know which type of stream they are dealing with.

Normally instances of StdioStream are not created directly but via Stdio, e.g: 

Stdio stdin.
Stdio stdout.
Stdio stderr.


Instance Variables:

- peekBuffer     <SmallInteger or nil> The next character to be read from the stream or nil.';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: 'AbstractUUIDTest'
	instVarNames: #( currentUuidGeneratorClass )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Network-UUID-Tests';
		comment: '';
		immediateInvariant.
true.
%

doit
(AbstractUUIDTest
	subclass: 'FastUUIDTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Network-UUID-Tests';
		comment: '';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: 'BinaryFileStreamTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Streams';
		comment: '';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: 'BinaryStreamSpecTest'
	instVarNames: #( reference )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Disk';
		comment: '';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: 'BufferedStreamSpecTest'
	instVarNames: #( reference )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Disk';
		comment: '';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: 'DirectoryEntryTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Core';
		comment: 'SUnit tests for FileSystemDirectoryEntry';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: 'EncodedBufferedStreamSpecTest'
	instVarNames: #( reference )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Disk';
		comment: '';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: 'FileLocatorTest'
	instVarNames: #( locator )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Core';
		comment: 'SUnit test for FileLocator';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: 'FileOptionsTest'
	instVarNames: #( stream )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Disk';
		comment: '';
		immediateInvariant.
true.
%

doit
(FileOptionsTest
	subclass: 'FileUnixOptionsTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Disk';
		comment: '';
		immediateInvariant.
true.
%

doit
(FileOptionsTest
	subclass: 'FileWindowsOptionsTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Disk';
		comment: '';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: 'FileReferenceAttributeTests'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Attributes';
		comment: 'Try and test file attribute access from FileReference.

As Pharo doesn''t provide a mechanism to set the attributes, pick a few well known files and make sure they have reasonable attributes, e.g. the VM is executable, it isn''t older than when the classes were first created or newer than today, etc.
';
		immediateInvariant.
true.
%

doit
(TestCase
	indexableSubclass: 'FileReferenceTest'
	instVarNames: #( filesystem )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Core';
		comment: 'SUnit tests for file reference';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: 'FileSystemHandleTest'
	instVarNames: #( filesystem handle reference )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Core';
		comment: 'SUnit tests for FileSystemHandle';
		immediateInvariant.
true.
%

doit
(FileSystemHandleTest
	subclass: 'FileHandleTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Disk';
		comment: 'SUnit tests for file handles, the tests may be found in superclass';
		immediateInvariant.
true.
%

doit
(FileSystemHandleTest
	subclass: 'MemoryHandleTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Memory';
		comment: 'SUnit tests for memory handles, the tests may be found in superclass';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: 'FileSystemResolverTest'
	instVarNames: #( resolver )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Core';
		comment: 'SUnit tests for FileSystemResolver';
		immediateInvariant.
true.
%

doit
(FileSystemResolverTest
	subclass: 'ClientResolverTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Client';
		comment: '';
		immediateInvariant.
true.
%

doit
(FileSystemResolverTest
	subclass: 'InteractiveResolverTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Core';
		comment: 'SUnit tests for InteractiveResolver';
		immediateInvariant.
true.
%

doit
(FileSystemResolverTest
	subclass: 'PlatformResolverTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Core';
		comment: 'SUnit tests for PlatformResolver';
		immediateInvariant.
true.
%

doit
(FileSystemResolverTest
	subclass: 'SystemResolverTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Core';
		comment: 'SUnit tests for SystemResolver';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: 'FileSystemTest'
	instVarNames: #( filesystem toDelete )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Core';
		comment: 'I am an abstract file system test. 
I directly test 
- the public interface of a FileSystem using these methods directly
- the FileSystem in general through the operation methods of the FileReference';
		immediateInvariant.
true.
%

doit
(FileSystemTest
	subclass: 'ClientFileSystemTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Client';
		comment: '';
		immediateInvariant.
true.
%

doit
(FileSystemTest
	subclass: 'DiskFileSystemTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Disk';
		comment: 'SUnit tests for the disk filesystem';
		immediateInvariant.
true.
%

doit
(FileSystemTest
	subclass: 'MemoryFileSystemTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Memory';
		comment: 'SUnit tests for MemoryFileSystem';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: 'FileSystemTreeTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Core';
		comment: 'SUnit tests for a file system tree';
		immediateInvariant.
true.
%

doit
(FileSystemTreeTest
	subclass: 'CopyVisitorTest'
	instVarNames: #( source dest )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Core';
		comment: 'SUnit tests for CopyVisitor';
		immediateInvariant.
true.
%

doit
(FileSystemTreeTest
	subclass: 'SingleTreeTest'
	instVarNames: #( filesystem )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Core';
		comment: 'SUnit tests for a single file system tree (depth one)';
		immediateInvariant.
true.
%

doit
(SingleTreeTest
	subclass: 'AbstractEnumerationVisitorTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Core';
		comment: 'SUnit tests for AbstractEnumerationVisitor';
		immediateInvariant.
true.
%

doit
(AbstractEnumerationVisitorTest
	subclass: 'CollectVisitorTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Core';
		comment: 'SUnit tests for class CollectVisitor';
		immediateInvariant.
true.
%

doit
(AbstractEnumerationVisitorTest
	subclass: 'SelectVisitorTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Core';
		comment: 'SUnit tests for class SelectVistor
';
		immediateInvariant.
true.
%

doit
(SingleTreeTest
	subclass: 'DeleteVisitorTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Core';
		comment: 'SUnit tests for class DeleteVisitor';
		immediateInvariant.
true.
%

doit
(SingleTreeTest
	subclass: 'GuideTest'
	instVarNames: #( guide visited )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Core';
		comment: 'Common superclass for tests of guidance through the filesystem';
		immediateInvariant.
true.
%

doit
(GuideTest
	subclass: 'BreadthFirstGuideTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Core';
		comment: 'SUnit tests for class BreadthFirstGuide';
		immediateInvariant.
true.
%

doit
(GuideTest
	subclass: 'PostorderGuideTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Core';
		comment: 'SUnit tests for class PostorderGuide';
		immediateInvariant.
true.
%

doit
(GuideTest
	subclass: 'PreorderGuideTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Core';
		comment: 'SUnit tests for class PreorderGuide';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: 'FSGemStoneKernelTests'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-GemStone';
		comment: '';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: 'NumberParserTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Core';
		comment: '';
		immediateInvariant.
true.
%

doit
(TestCase
	indexableSubclass: 'PathTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Core';
		comment: 'SUnit tests for file system paths';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: 'PosixErrorGroupTest'
	instVarNames: #( fileReference errorGroup posixErrorClass fileSystem oldFileReference filesToDelete directoriesToDelete )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Base-ErrorGroups';
		comment: '';
		immediateInvariant.
true.
%

doit
(PosixErrorGroupTest
	subclass: 'PosixErrorGroupClientTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Client';
		comment: '';
		immediateInvariant.
true.
%

doit
(PosixErrorGroupTest
	subclass: 'PosixErrorGroupUnixTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Base-ErrorGroups';
		comment: '';
		immediateInvariant.
true.
%

doit
(PosixErrorGroupTest
	subclass: 'PosixErrorGroupWindowsTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Base-ErrorGroups';
		comment: '';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: 'StdioStreamTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Streams';
		comment: '';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: 'StdioTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Disk';
		comment: '';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: 'WindowsStoreTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Disk';
		comment: 'SUnit tests for class WindowsStore';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: 'ZnBase64EncoderTests'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Zinc-Character-Encoding-Tests';
		comment: '';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: 'ZnBufferedReadStreamTests'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Zinc-Character-Encoding-Tests';
		comment: 'I am ZnBufferedReadStreamTests.';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: 'ZnBufferedReadWriteStreamTests'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Zinc-Character-Encoding-Tests';
		comment: '';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: 'ZnBufferedStreamByteTests'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Zinc-Character-Encoding-Tests';
		comment: 'Tests for bufferer read/write streams that are assumed to contain bytes.

Right now, integer encoding/decoding

References

https://en.wikipedia.org/wiki/Endianness
https://en.wikipedia.org/wiki/Two%27s_complement';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: 'ZnBufferedWriteStreamTests'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Zinc-Character-Encoding-Tests';
		comment: '';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: 'ZnCharacterEncoderTests'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Zinc-Character-Encoding-Tests';
		comment: '';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: 'ZnCharacterStreamTests'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Zinc-Character-Encoding-Tests';
		comment: '';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: 'ZnCrPortableWriteStreamTests'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Zinc-Character-Encoding-Tests';
		comment: '';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: 'ZnFastLineReaderTests'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Zinc-Character-Encoding-Tests';
		comment: '';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: 'ZnNewLineWriterStreamTests'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Zinc-Character-Encoding-Tests';
		comment: '';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: 'ZnPercentEncoderTests'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Zinc-Character-Encoding-Tests';
		comment: '';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: 'ZnPositionableReadStreamTests'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Zinc-Character-Encoding-Tests';
		comment: '';
		immediateInvariant.
true.
%

doit
(TestResource
	subclass: 'DiskFileAttributesTestsResources'
	instVarNames: #( file beforeCreationTime afterCreationTime )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Attributes';
		comment: 'DiskFileAttributesTestsResources provides a file with known attributes that can be used to test attribute retrieval in the File System.

DiskFileAttributesTestsResources is currently used by FileReferenceAttributeTests.

    Instance Variables
	afterCreationTime:	The time just after file was created	<DateAndTime>
	beforeCreationTime:	The time just before file was created	<DateAndTime>
	file:					The name of the file (with known attributes)	<FileReference>


    Implementation Points';
		immediateInvariant.
true.
%

doit
(TestResource
	subclass: 'StdioStreamTestResource'
	instVarNames: #( fileReference contents fileStream stdioStream )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Streams';
		comment: '';
		immediateInvariant.
true.
%

doit
(WriteStream
	subclass: 'MemoryWriteStream'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Memory';
		comment: 'I am like a WriteStream but I have more capacities than it.
I am closer to a FileStream so I delegate some operations to my handle (collection instance variable)';
		immediateInvariant.
true.
%

doit
(WriteStream
	subclass: 'ZnCrPortableWriteStream'
	instVarNames: #( stream cr lf previous )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Zinc-Character-Encoding-Core';
		comment: 'I am a write stream wrapping a second stream. Whenever they ask me to write a cr, a lf, or a crlf I''ll instead print a portable new line depending on the platform I''m on.

stream := '''' writeStream.
converter := ZnCrPortableWriteStream on: stream.
converter cr; cr; lf; nextPut: $a.
stream contents';
		immediateInvariant.
true.
%

doit
(WriteStreamPortable
	subclass: 'ZnNewLineWriterStream'
	instVarNames: #( stream cr lf previous lineEnding )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Zinc-Character-Encoding-Core';
		comment: 'I am a write stream wrapping a second stream. Whenever they ask me to write a cr, a lf, or a crlf I''ll instead print a new line depending on a configured convention. By default I use the current platform convention. 

| stream converter |
stream := '''' writeStream.
converter := ZnNewLineWriterStream on: stream.
converter cr; cr; lf; nextPut: $a.
stream contents

A ZnNewLineWriterStream can be configured with the desired line ending convention using the methods 

converter forCr.
converter forLf.
converter forCrLf.
converter forPlatformLineEnding.';
		immediateInvariant.
true.
%

! Class implementation for 'UUID'

!		Class methods for 'UUID'

category: 'instance creation'
classmethod: UUID
fromString: aString
	| object |
	aString size ~= 36 ifTrue: [Error signal].
	object := self nilUUID. 
	object asUUID: aString.
	^object
%

category: 'accessing'
classmethod: UUID
generatorClass
  self _generatorClass ifNil: [ ^ FastUUIDGenerator ].
  ^ self _generatorClass
%

category: 'accessing'
classmethod: UUID
generatorClass: aClass
  GeneratorClass := aClass.
  aClass ifNotNil: [ aClass initialize ]
%

category: 'instance creation'
classmethod: UUID
new
  ^ (self new: 16) initialize
%

category: 'instance creation'
classmethod: UUID
nilUUID
	"Must call basicNew: here because I have a non-trivial initialize method."

	^self basicNew: 16
%

category: 'private'
classmethod: UUID
_generatorClass
  ^ GeneratorClass
%

!		Instance methods for 'UUID'

category: 'comparing'
method: UUID
< aMagnitude
  "Answer whether the receiver is less than the argument."

  self size = aMagnitude size
    ifFalse: [ ^ self size < aMagnitude size ].
  1 to: self size do: [ :i | 
    (self at: i) = (aMagnitude at: i)
      ifFalse: [ ^ (self at: i) < (aMagnitude at: i) ] ].
  ^ false
%

category: 'comparing'
method: UUID
<= uuid
	^ (self = uuid) or: [ self < uuid ]
%

category: 'comparing'
method: UUID
> uuid
	^ uuid < self
%

category: 'comparing'
method: UUID
>= uuid
	^ (self = uuid) or: [ uuid < self ]
%

category: 'converting'
method: UUID
asInteger
  ^ self inject: 0 into: [ :sum :each | sum * 256 + each ]
%

category: 'converting'
method: UUID
asString
	| result data |
	data := String new: 36.
	result := WriteStream on: data.
	1 to: 4 do:[:i| self printHexAt: i to: result].
	result nextPut: $-.
	5 to: 6 do:[:i| self printHexAt: i to: result].
	result nextPut: $-.
	7 to: 8 do:[:i| self printHexAt: i to: result].
	result nextPut: $-.
	9 to: 10 do:[:i| self printHexAt: i to: result].
	result nextPut: $-.
	11 to: 16 do:[:i| self printHexAt: i to: result].
	^data.
%

category: 'converting'
method: UUID
asString36
	"Encode the UUID as a base 36 string using 0-9 and lowercase a-z.
	This is the shortest representation still being able to work as
	filenames etc since it does not depend on case nor characters
	that might cause problems, and it fits into short filenames like on
	the old MacOS HFS filesystem. The check for 36r is to make this code
	work in versions before Squeak 3.8."

	| num candidate |
	num := 0.
	1 to: self size do: [:i | num := num + ((256 raisedTo: i - 1) * (self at: i))].
	candidate := num printStringBase: 36.
	^((candidate beginsWith: '36r')
			ifTrue: [candidate copyFrom: 4 to: candidate size]
			ifFalse: [candidate]) asLowercase
%

category: 'converting'
method: UUID
asUUID: aString
	| stream token byte sz |
	stream := ReadStream on: (aString copyReplaceAll: '-' with: '') asUppercase.
	sz := stream _collection size.
	1 to: sz/2 do: [:i | 
		token := stream next: 2.
		byte := Integer fromHexString: token.
		self at: i put: byte].
	^self
%

category: 'initialization'
method: UUID
initialize
  self _primMakeUUID
%

category: 'testing'
method: UUID
isNilUUID
  1 to: self size do: [ :i | 
    (self at: i) ~= 0
      ifTrue: [ ^ false ] ].
  ^ true
%

category: 'converting'
method: UUID
printHexAt: index to: aStream
	| map v |
	map := '0123456789abcdef'.
	v := self at: index.
	aStream nextPut: (map at: (v bitShift: -4) + 1). 
	aStream nextPut: (map at: (v bitAnd: 15) + 1).
%

category: 'printing'
method: UUID
printOn: aStream
	aStream nextPutAll: 'an UUID('.
	self asString printOn: aStream.
	aStream nextPutAll: ')'
%

category: 'printing'
method: UUID
printString

	^self asString
%

category: 'private'
method: UUID
_primMakeUUID
  self class generatorClass default generateBytes: self forVersion: 4
%

! Class implementation for 'FileException'

!		Class methods for 'FileException'

category: 'exceptioninstantiator'
classmethod: FileException
fileName: aFileName
	^self new fileName: aFileName
%

category: 'instance creation'
classmethod: FileException
signalOnFile: aFile 
	
	(self fileName: aFile basename) signal: aFile name
%

category: 'instance creation'
classmethod: FileException
signalWith: aReference
	"Signal a new instance of the receiver with the supplied reference.
	aReference is something that can be converted to a path, e.g. a String, Path or FileReference"
  | str |
	^(self fileName: (str := aReference asPath pathString)) signal: str
%

!		Instance methods for 'FileException'

category: 'exceptiondescription'
method: FileException
fileName
	^fileName
%

category: 'exceptionbuilder'
method: FileException
fileName: aFileName
	fileName := aFileName
%

category: 'exceptiondescription'
method: FileException
isResumable
	"Determine whether an exception is resumable."

	^true
%

category: 'exceptiondescription'
method: FileException
messageText
	"Return an exception's message text."

	^ messageText isNil
		ifTrue: [ fileName printString ]
		ifFalse: [ messageText ]
%

! Class implementation for 'FileAlreadyExistsException'

!		Class methods for 'FileAlreadyExistsException'

category: 'instance creation'
classmethod: FileAlreadyExistsException
signalOnFile: aFile

	self new
		file: aFile;
		signal: aFile name "use signal: so filename shows up in exception printString"
%

!		Instance methods for 'FileAlreadyExistsException'

category: 'accessing'
method: FileAlreadyExistsException
file
	^ file
%

category: 'accessing'
method: FileAlreadyExistsException
file: aFile

	file := aFile
%

category: 'accessing'
method: FileAlreadyExistsException
messageText

	^ 'File already exists: ', file basename
%

! Class implementation for 'FileDoesNotExistException'

!		Class methods for 'FileDoesNotExistException'

category: 'signalling'
classmethod: FileDoesNotExistException
signalWithFile: aFile writeMode: writeMode

	| ex |
	ex := (self fileName:  ( aFile isString ifTrue: [aFile] ifFalse: [aFile basename])).
	ex
		readOnly: writeMode not;
		signal: ex fileName  "use signal: so file name shows up in  ex printString"
%

!		Instance methods for 'FileDoesNotExistException'

category: 'accessing'
method: FileDoesNotExistException
readOnly
	^readOnly == true
%

category: 'accessing'
method: FileDoesNotExistException
readOnly: aBoolean
	readOnly := aBoolean
%

! Class implementation for 'FilePosixError'

!		Class methods for 'FilePosixError'

category: 'querry'
classmethod: FilePosixError
allConcreteFilePosixErrorClasses
	^self allSubclasses select: [:ea |
		ea subclasses isEmpty and: [(ea class includesSelector: #defaultErrorNumber)]]
%

category: 'querry'
classmethod: FilePosixError
classWithPosixName: aString
	" FilePosixError classWithPosixName: 'EINVAL' "
	^self allSubclasses detect: [:ea |
		(ea class includesSelector: #posixName) and: [ea posixName = aString]] ifNone: []
%

category: 'querry'
classmethod: FilePosixError
classWithPosixNumber: aString
	" (FilePosixError classWithPosixNumber: 18) posixName "
	^self allSubclasses detect: [:ea |
		(ea class includesSelector: #defaultErrorNumber) and: [ea defaultErrorNumber = aString]] ifNone: []
%

category: 'access'
classmethod: FilePosixError
defaultErrorNumber
	self subclassResponsibility
%

category: 'access'
classmethod: FilePosixError
errorDescription
	self subclassResponsibility
%

category: 'querry'
classmethod: FilePosixError
errorForPlatformError: aNumber

	^self
%

category: 'reporting'
classmethod: FilePosixError
errorNumberForSelector: valueSelector
	^(self class canUnderstand: valueSelector)
		ifTrue: [self perform: valueSelector]
		ifFalse: [self defaultErrorNumber]
%

category: 'printing'
classmethod: FilePosixError
posixErrorsReport
	" FilePosixError posixErrorsReport "
	| aStream |
	aStream := WriteStream on: String new.
	self reportOn: aStream indent: 0.
	^aStream contents
%

category: 'access'
classmethod: FilePosixError
posixName
	self subclassResponsibility
%

category: 'querry'
classmethod: FilePosixError
posixNamesWithClasses
	" FilePosixError posixNamesWithClasses"
	| aDictionary |
	aDictionary := Dictionary new.
	self allSubclasses do: [:ea |
		(ea class includesSelector: #posixName) ifTrue: [aDictionary at: ea posixName put: ea] ].
	^aDictionary
%

category: 'instance creation'
classmethod: FilePosixError
reference: anObject

	^self new
		reference: anObject;
		yourself
%

category: 'printing'
classmethod: FilePosixError
reportOn: aStream indent: indent
	| aSize aValue |
	indent timesRepeat: [aStream tab].

	aStream nextPutAll: self name asString.
	(45 - ((indent * 4) + self name asString size)) timesRepeat: [aStream space].

	((self name = #FilePosixError) not and: [self class includesSelector: #posixName])
		ifTrue: [
			aSize := self posixName size.
			aStream nextPutAll: self posixName].
	(15 - (aSize ifNil: [0])) timesRepeat: [aStream space].

	((self name = #FilePosixError) not and: [self class includesSelector: #defaultErrorNumber])
		ifTrue: [aValue := self defaultErrorNumber asString]
		ifFalse: [aValue := (self subclasses isEmpty not) ifTrue: [''] ifFalse: ['value not defined']].
	aSize :=  aValue size.
	(15 - aSize) timesRepeat: [aStream space].
	aStream nextPutAll: aValue asString.

	aStream crlf.

	(self subclasses asSortedCollection: [:a :b | a name <= b name]) 
		do: [:ea | ea reportOn: aStream indent: indent + 1]
%

!		Instance methods for 'FilePosixError'

category: 'accessing'
method: FilePosixError
errorGroupName
	^errorGroupName
%

category: 'accessing'
method: FilePosixError
errorGroupName: aString
	errorGroupName := aString
%

category: 'accessing'
method: FilePosixError
options
	^options
%

category: 'accessing'
method: FilePosixError
options: anObject
	options := anObject
%

category: 'accessing'
method: FilePosixError
platformErrorNumber
	^platformErrorNumber
%

category: 'accessing'
method: FilePosixError
platformErrorNumber: anInteger
	platformErrorNumber := anInteger
%

category: 'printing'
method: FilePosixError
printOn: aStream
	aStream
		nextPutAll: self class name;
		nextPutAll: '(';
		nextPutAll: self class posixName;
		nextPutAll: '-';
		nextPutAll: self errorGroupName;
		nextPutAll: ')'
%

category: 'accessing'
method: FilePosixError
reference
	^reference
%

category: 'accessing'
method: FilePosixError
reference: anObject
	reference := anObject
%

category: 'accessing'
method: FilePosixError
sourceReference
	^sourceReference
%

category: 'accessing'
method: FilePosixError
sourceReference: aFileReference
	sourceReference := aFileReference
%

! Class implementation for 'FileAccessError'

!		Class methods for 'FileAccessError'

category: 'access'
classmethod: FileAccessError
defaultErrorNumber
	^13
%

category: 'access'
classmethod: FileAccessError
errorDescription
	^'Invalid path or access denied'
%

category: 'access'
classmethod: FileAccessError
posixName
	^'EACCES'
%

! Class implementation for 'FileBadFileDescriptorError'

!		Class methods for 'FileBadFileDescriptorError'

category: 'accessing'
classmethod: FileBadFileDescriptorError
defaultErrorNumber
	^9
%

category: 'accessing'
classmethod: FileBadFileDescriptorError
errorDescription
	^'Bad file descriptor'
%

category: 'accessing'
classmethod: FileBadFileDescriptorError
posixName
	^'EBADF'
%

! Class implementation for 'FileBusyError'

!		Class methods for 'FileBusyError'

category: 'accessing'
classmethod: FileBusyError
defaultErrorNumber
	^16
%

category: 'accessing'
classmethod: FileBusyError
errorDescription
	^'Devince or resource busy'
%

category: 'accessing'
classmethod: FileBusyError
posixName
	^'EBUSY'
%

! Class implementation for 'FileDeviceNotSameError'

!		Class methods for 'FileDeviceNotSameError'

category: 'accessing'
classmethod: FileDeviceNotSameError
defaultErrorNumber
	^18
%

category: 'accessing'
classmethod: FileDeviceNotSameError
errorDescription
	^'Invalid cross-device link'
%

category: 'accessing'
classmethod: FileDeviceNotSameError
posixName
	^'EXDEV'
%

! Class implementation for 'FileExistsError'

!		Class methods for 'FileExistsError'

category: 'accessing'
classmethod: FileExistsError
defaultErrorNumber
	^17
%

category: 'accessing'
classmethod: FileExistsError
errorDescription
	^'File exists'
%

category: 'accessing'
classmethod: FileExistsError
posixName
	^'EEXIST'
%

! Class implementation for 'FileInvalidOptionError'

!		Class methods for 'FileInvalidOptionError'

category: 'accessing'
classmethod: FileInvalidOptionError
defaultErrorNumber
	^22
%

category: 'accessing'
classmethod: FileInvalidOptionError
errorDescription
	^'The function is being used incorrectly.  An argument may be invalid.'
%

category: 'accessing'
classmethod: FileInvalidOptionError
posixName
	^'EINVAL'
%

! Class implementation for 'FileIOError'

!		Class methods for 'FileIOError'

category: 'accessing'
classmethod: FileIOError
defaultErrorNumber
	^5
%

category: 'accessing'
classmethod: FileIOError
errorDescription
	^'I/O error'
%

category: 'accessing'
classmethod: FileIOError
posixName
	^'EIO'
%

! Class implementation for 'FileIsDirectoryError'

!		Class methods for 'FileIsDirectoryError'

category: 'accessing'
classmethod: FileIsDirectoryError
defaultErrorNumber
	^21
%

category: 'accessing'
classmethod: FileIsDirectoryError
errorDescription
	^'Is a directory'
%

category: 'accessing'
classmethod: FileIsDirectoryError
posixName
	^'EISDIR'
%

! Class implementation for 'FileMaxFilesOpenError'

!		Class methods for 'FileMaxFilesOpenError'

category: 'accessing'
classmethod: FileMaxFilesOpenError
defaultErrorNumber
	^23
%

category: 'accessing'
classmethod: FileMaxFilesOpenError
errorDescription
	^'Too many open files'
%

category: 'accessing'
classmethod: FileMaxFilesOpenError
posixName
	^'ENFILE'
%

! Class implementation for 'FileNameToLongError'

!		Class methods for 'FileNameToLongError'

category: 'accessing'
classmethod: FileNameToLongError
defaultErrorNumber
	^36
%

category: 'accessing'
classmethod: FileNameToLongError
errorDescription
	^'Name is too long'
%

category: 'accessing'
classmethod: FileNameToLongError
posixName
	^'ENAMETOOLONG'
%

! Class implementation for 'FileNoEntryError'

!		Class methods for 'FileNoEntryError'

category: 'accessing'
classmethod: FileNoEntryError
defaultErrorNumber
	^2
%

category: 'accessing'
classmethod: FileNoEntryError
errorDescription
	^'No such file or directory'
%

category: 'accessing'
classmethod: FileNoEntryError
posixName
	^'ENOENT'
%

! Class implementation for 'FileNoSpaceError'

!		Class methods for 'FileNoSpaceError'

category: 'accessing'
classmethod: FileNoSpaceError
defaultErrorNumber
	^28
%

category: 'accessing'
classmethod: FileNoSpaceError
errorDescription
	^'No space left on device'
%

category: 'accessing'
classmethod: FileNoSpaceError
posixName
	^'ENOSPC'
%

! Class implementation for 'FilePermissionDeniedError'

!		Class methods for 'FilePermissionDeniedError'

category: 'accessing'
classmethod: FilePermissionDeniedError
defaultErrorNumber
	^1
%

category: 'accessing'
classmethod: FilePermissionDeniedError
errorDescription
	^'Permission denied or not owner'
%

category: 'accessing'
classmethod: FilePermissionDeniedError
posixName
	^'EPERM'
%

! Class implementation for 'FileReadOnlyFileSystemError'

!		Class methods for 'FileReadOnlyFileSystemError'

category: 'accessing'
classmethod: FileReadOnlyFileSystemError
defaultErrorNumber
	^30
%

category: 'accessing'
classmethod: FileReadOnlyFileSystemError
errorDescription
	^'Read only file system'
%

category: 'accessing'
classmethod: FileReadOnlyFileSystemError
posixName
	^'EROFS'
%

! Class implementation for 'FileSynchronizedIONotSupportedError'

!		Class methods for 'FileSynchronizedIONotSupportedError'

category: 'accessing'
classmethod: FileSynchronizedIONotSupportedError
defaultErrorNumber
	^4
%

category: 'accessing'
classmethod: FileSynchronizedIONotSupportedError
errorDescription
	^'Interrupted system call'
%

category: 'accessing'
classmethod: FileSynchronizedIONotSupportedError
posixName
	^'EINTR'
%

! Class implementation for 'FileTooManySymbolicLinksError'

!		Class methods for 'FileTooManySymbolicLinksError'

category: 'accessing'
classmethod: FileTooManySymbolicLinksError
defaultErrorNumber
	^40
%

category: 'accessing'
classmethod: FileTooManySymbolicLinksError
errorDescription
	^'Too many levels of symbolic links'
%

category: 'accessing'
classmethod: FileTooManySymbolicLinksError
posixName
	^'ELOOP'
%

! Class implementation for 'FileSystemError'

!		Class methods for 'FileSystemError'

category: 'instance creation'
classmethod: FileSystemError
reference: aReference
	^ self reference: aReference text: aReference printString
%

category: 'instance creation'
classmethod: FileSystemError
reference: aReference text: aString
	^ self basicNew initializeWithReference: aReference text: aString
%

category: 'instance creation'
classmethod: FileSystemError
signalWith: aReference
	^ (self reference: aReference) signal
%

category: 'instance creation'
classmethod: FileSystemError
signalWith: aReference text: aString
	^ (self reference: aReference text: aString) signal
%

!		Instance methods for 'FileSystemError'

category: 'initialize-release'
method: FileSystemError
initializeWithReference: aReference text: aString
	reference := aReference.
	messageText := aString
%

category: 'testing'
method: FileSystemError
isResumable
	^ true
%

category: 'accessing'
method: FileSystemError
reference
	^ reference
%

! Class implementation for 'IllegalName'

!		Class methods for 'IllegalName'

category: 'instance creation'
classmethod: IllegalName
name: aName
	^ self basicNew initializeWithName: aName
%

category: 'instance creation'
classmethod: IllegalName
signalWith: aName
	^ (self name: aName) signal
%

!		Instance methods for 'IllegalName'

category: 'initialization'
method: IllegalName
initializeWithName: aName
	name := aName.
	self messageText: aName
%

category: 'accessing'
method: IllegalName
name
	^ name
%

! Class implementation for 'SubscriptOutOfBounds'

!		Class methods for 'SubscriptOutOfBounds'

category: 'signalling'
classmethod: SubscriptOutOfBounds
signalFor: subscript lowerBound: lowerBound upperBound: upperBound
	^ self 
		signalFor: subscript 
		lowerBound: lowerBound 
		upperBound: upperBound 
		in: nil
%

category: 'signalling'
classmethod: SubscriptOutOfBounds
signalFor: subscript lowerBound: lowerBound upperBound: upperBound in: object
	^ self new
		signaler: object;
		subscript: subscript;
		lowerBound: lowerBound;
		upperBound: upperBound;
		signal
%

category: 'signalling'
classmethod: SubscriptOutOfBounds
standardMessageText
	"Generate a standard textual description"
	
	^ String streamContents: [ :stream |
		self subscript 
			ifNil: [
				stream << 'subscript' ]
			ifNotNil: [ 
				stream print: self subscript ].
		(self lowerBound notNil and: [ self upperBound notNil])
			ifTrue: [
				stream << ' is not between '.
				stream print: self lowerBound.
				stream << ' and '.
				stream print: self upperBound ] ]
%

!		Instance methods for 'SubscriptOutOfBounds'

category: 'accessing'
method: SubscriptOutOfBounds
lowerBound
	^ lowerBound
%

category: 'accessing'
method: SubscriptOutOfBounds
lowerBound: anObject
	lowerBound := anObject
%

category: 'accessing'
method: SubscriptOutOfBounds
messageText
	"Overwritten to initialiaze the message text to a standard text if it has not yet been set"
	
	^ messageText ifNil: [ messageText := self standardMessageText ]
%

category: 'accessing'
method: SubscriptOutOfBounds
signaler
	^ signaler
%

category: 'accessing'
method: SubscriptOutOfBounds
signaler: anObject
	signaler := anObject
%

category: 'printing'
method: SubscriptOutOfBounds
standardMessageText
	"Generate a standard textual description"
	
	^ String streamContents: [ :stream |
		self subscript 
			ifNil: [
				stream << 'subscript' ]
			ifNotNil: [ 
				stream print: self subscript ].
		(self lowerBound notNil and: [ self upperBound notNil])
			ifTrue: [
				stream << ' is not between '.
				stream print: self lowerBound.
				stream << ' and '.
				stream print: self upperBound ] ]
%

category: 'accessing'
method: SubscriptOutOfBounds
subscript
	^ subscript
%

category: 'accessing'
method: SubscriptOutOfBounds
subscript: anObject
	subscript := anObject
%

category: 'accessing'
method: SubscriptOutOfBounds
upperBound
	^ upperBound
%

category: 'accessing'
method: SubscriptOutOfBounds
upperBound: anObject
	upperBound := anObject
%

! Class implementation for 'ZnByteStringBecameWideString'

!		Class methods for 'ZnByteStringBecameWideString'

category: 'convenience'
classmethod: ZnByteStringBecameWideString
convert: byteString
	"Convert byteString to a wideString, signalling a resumable error"
	
	| wideString |
self flag: 'TODO: WideString does not exist in Gemstone. Using String which can grow to a QuadByteString'.
	wideString := String from: byteString.
	self new
		byteString: byteString;
		wideString: wideString;
		signal.
	^ wideString
%

!		Instance methods for 'ZnByteStringBecameWideString'

category: 'convenience'
method: ZnByteStringBecameWideString
becomeForward
	"Switch the identities of byteString and wideString using #becomeForward:.
		byteString becomeForward: wideString
	 GemStone's #become: is a two way become. This is an attempt to mimic a one-way become"
	| a |
	a := byteString copy.
	a become: wideString.
	byteString := a
%

category: 'accessing'
method: ZnByteStringBecameWideString
byteString
	^ byteString
%

category: 'accessing'
method: ZnByteStringBecameWideString
byteString: anObject
	byteString := anObject
%

category: 'testing'
method: ZnByteStringBecameWideString
isResumable
	^ true
%

category: 'accessing'
method: ZnByteStringBecameWideString
wideString
	^ wideString
%

category: 'accessing'
method: ZnByteStringBecameWideString
wideString: anObject
	wideString := anObject
%

! Class implementation for 'ZnCharacterEncodingError'

!		Instance methods for 'ZnCharacterEncodingError'

category: 'private'
method: ZnCharacterEncodingError
defaultResumeValue
	"Answer the value that by default should be returned if the exception is resumed"
	
	^ nil
%

category: 'Handling'
method: ZnCharacterEncodingError
resume

"See  resume:  for documentation ."

 self isResumable ifFalse:[ 
   "cannot resume from a not-resumable Exception."
   ^ self error:'cannot resume from a not-resumable Exception'
 ].
 ^ self _resume: self defaultResumeValue
%

! Class implementation for 'ZnIncomplete'

!		Instance methods for 'ZnIncomplete'

category: 'private'
method: ZnIncomplete
defaultResumeValue
	"$? codePoint"
	
	^ 63
%

category: 'testing'
method: ZnIncomplete
isResumable
	^ true
%

! Class implementation for 'ZnInvalidUTF8'

!		Instance methods for 'ZnInvalidUTF8'

category: 'private'
method: ZnInvalidUTF8
defaultResumeValue
	"$? codePoint"
	
	^ 63
%

category: 'testing'
method: ZnInvalidUTF8
isResumable
	^ true
%

! Class implementation for 'ResolutionRequest'

!		Class methods for 'ResolutionRequest'

category: 'instance creation'
classmethod: ResolutionRequest
for: origin
	^ self new
		origin: origin;
		signal
%

!		Instance methods for 'ResolutionRequest'

category: 'exceptionDescription'
method: ResolutionRequest
defaultAction
"
	| filedir ref |
	filedir := UIManager default chooseDirectory: 'Where is ', origin asString, '?'.
	ref := filedir ifNotNil: [FileSystem disk referenceTo: filedir fullName].
	self resume: ref
"

	self error: 'Not yet implemented'
%

category: 'accessing'
method: ResolutionRequest
origin: aSymbol
	origin := aSymbol
%

! Class implementation for 'AbstractFileAdaptor'

!		Class methods for 'AbstractFileAdaptor'

category: 'accessing'
classmethod: AbstractFileAdaptor
createDirectory: aPathName
	self subclassResponsibility
%

category: 'accessing'
classmethod: AbstractFileAdaptor
currentWorkingDirectoryPath

	self subclassResponsibility
%

category: 'accessing'
classmethod: AbstractFileAdaptor
deleteDirectory: aPathName
	self subclassResponsibility
%

category: 'accessing'
classmethod: AbstractFileAdaptor
deleteFile: aPathName
	self subclassResponsibility
%

category: 'accessing'
classmethod: AbstractFileAdaptor
entriesIn: pathString
	self subclassResponsibility
%

category: 'accessing'
classmethod: AbstractFileAdaptor
isDirectory: aPathName
	self subclassResponsibility
%

category: 'accessing'
classmethod: AbstractFileAdaptor
lookupDirectory: path filename: basename

	self subclassResponsibility
%

category: 'accessing'
classmethod: AbstractFileAdaptor
lookupDirectoryEntry: fullPath
	self subclassResponsibility
%

category: 'instance creation'
classmethod: AbstractFileAdaptor
on: aFile
	^self new
		file: aFile;
		yourself
%

category: 'accessing'
classmethod: AbstractFileAdaptor
open: aFileReference withOptions: aFileOptions
	| aFileAdaptor |
	aFileAdaptor := self openPathName: aFileReference fullName withOptions: aFileOptions.
	aFileAdaptor ifNil: [FileDoesNotExistException signalWith: aFileReference fullName].
	^aFileAdaptor
%

category: 'accessing'
classmethod: AbstractFileAdaptor
openPathName: aPathname withOptions: aFileOptions
	self subclassResponsibility
%

category: 'accessing'
classmethod: AbstractFileAdaptor
rename: oldFileFullName to: newFileFullName
	self subclassResponsibility
%

!		Instance methods for 'AbstractFileAdaptor'

category: 'accessing - file'
method: AbstractFileAdaptor
atEnd
	self subclassResponsibility
%

category: 'accessing'
method: AbstractFileAdaptor
file
	^file
%

category: 'accessing'
method: AbstractFileAdaptor
file: aFile
	file := aFile
%

category: 'accessing - file'
method: AbstractFileAdaptor
fileSize
	self subclassResponsibility
%

category: 'positioning'
method: AbstractFileAdaptor
flush
	self subclassResponsibility
%

category: 'positioning'
method: AbstractFileAdaptor
fullName
	self subclassResponsibility
%

category: 'accessing - file'
method: AbstractFileAdaptor
isClosed

	self subclassResponsibility
%

category: 'accessing - file'
method: AbstractFileAdaptor
isWritable

	self subclassResponsibility
%

category: 'accessing - file'
method: AbstractFileAdaptor
pathString

	^self file pathString
%

category: 'positioning'
method: AbstractFileAdaptor
sizeOf: handle
	self subclassResponsibility
%

category: 'positioning'
method: AbstractFileAdaptor
truncate: anInteger
	self subclassResponsibility
%

category: 'accessing - file'
method: AbstractFileAdaptor
writeFrom: aByteArray startingAt: anIndex count: count
	self subclassResponsibility
%

! Class implementation for 'GsFileAdaptor'

!		Class methods for 'GsFileAdaptor'

category: 'accessing - file'
classmethod: GsFileAdaptor
createDirectory: aPathName
	^GsFile createServerDirectory: aPathName
%

category: 'accessing - file'
classmethod: GsFileAdaptor
currentWorkingDirectoryPath

	^GsFile _expandEnvVariable: 'PWD' isClient:false
%

category: 'accessing - file'
classmethod: GsFileAdaptor
deleteDirectory: aPathName
	^GsFile removeServerDirectory: aPathName
%

category: 'accessing - file'
classmethod: GsFileAdaptor
deleteFile: aPathName
	^GsFile removeServerFile: aPathName
%

category: 'error handling'
classmethod: GsFileAdaptor
doesNotUnderstand: aMessage
	"Redirect message to the resolved version of this GsFile."
	(GsFile respondsTo: aMessage selector)
		ifTrue: [ ^ GsFile perform: aMessage selector withArguments: aMessage arguments ].
	
	^ super doesNotUnderstand: aMessage.
%

category: 'accessing - file'
classmethod: GsFileAdaptor
entriesIn: pathString
	| result |
	result := self _entriesIn: pathString.
	^(result ifNil: [#()])
		reject: [:ea | (ea  endsWith: '.') or: [ea endsWith: '..']]
%

category: 'accessing - file'
classmethod: GsFileAdaptor
isDirectory: aPathName
	^GsFile _isDirectory: aPathName onClient: false
%

category: 'accessing - file'
classmethod: GsFileAdaptor
lookupDirectory: path filename: basename

	| fullPath|
	fullPath := self _fullPath: path filename: basename.
	^self lookupDirectoryEntry: fullPath
%

category: 'accessing - file'
classmethod: GsFileAdaptor
lookupDirectoryEntry: fullPath
	| gsFileStat |
	gsFileStat := GsFile stat: fullPath isLstat: true.
	gsFileStat _isSmallInteger ifTrue: [ ^ nil ].
	^	{
		(Path from: fullPath) basename.
		gsFileStat ctimeUtcSeconds .
		gsFileStat mtimeUtcSeconds .
		gsFileStat isDirectory .
		gsFileStat size.
		gsFileStat mode bitAnd: 8r777. "https://github.com/GemTalk/FileSystemGs/issues/11"
		(gsFileStat mode bitAnd: 8r0120000) = 8r0120000. "https://github.com/GemTalk/FileSystemGs/issues/10"
	}
%

category: 'instance creation'
classmethod: GsFileAdaptor
openPathName: aPathName withOptions: aFileOptions
	"This is the current way of openning a file"
	| aGsFile |
	aGsFile := 
		GsFile 
			open: aPathName 
			mode: aFileOptions mode 
			onClient: aFileOptions isForeignFileSystem 
			withCompression: aFileOptions isGzipped.
	^aGsFile ifNotNil: [(self on: aGsFile) options: aFileOptions]
%

category: 'accessing - file'
classmethod: GsFileAdaptor
rename: oldFileFullName to: newFileFullName
	^(GsFile renameFileOnServer: oldFileFullName to: newFileFullName) = 0 
		ifTrue: [0] 
		ifFalse: [nil]
%

category: 'accessing - file'
classmethod: GsFileAdaptor
serverErrorString
	^GsFile serverErrorString
%

category: 'instance creation'
classmethod: GsFileAdaptor
stderr

	| aGsFile |
	aGsFile := GsFile _getStdFile: 2 onClient: false.
	^aGsFile ifNotNil: [(self on: aGsFile) options: FileOptions newWrite]
%

category: 'instance creation'
classmethod: GsFileAdaptor
stdin

	| aGsFile |
	aGsFile := GsFile _getStdFile: 0 onClient: false.
	^aGsFile ifNotNil: [(self on: aGsFile) options: FileOptions newRead]
%

category: 'instance creation'
classmethod: GsFileAdaptor
stdout

	| aGsFile |
	aGsFile := GsFile _getStdFile: 1 onClient: false.
	^aGsFile ifNotNil: [(self on: aGsFile) options: FileOptions newWrite]
%

category: 'accessing - file'
classmethod: GsFileAdaptor
_entriesIn: pathString
	| result |
	result := GsFile _contentsOfServerDirectory: pathString expandPath: true.
	^result _isArray ifTrue: [result] ifFalse:  [nil]
%

category: 'accessing - file'
classmethod: GsFileAdaptor
_fullPath: path filename: basename
	"If this is a directory, then the path needs to have a slash on the end"
	| fullPath |
	fullPath := path , '/' , basename.
	(self isDirectory: fullPath) == true ifTrue: [^fullPath , '/'].
	^fullPath
%

!		Instance methods for 'GsFileAdaptor'

category: 'accessing - file'
method: GsFileAdaptor
atEnd
	^self file atEnd
%

category: 'accessing - file'
method: GsFileAdaptor
close
	self file close
%

category: 'error handling'
method: GsFileAdaptor
doesNotUnderstand: aMessage
	"Redirect message to the resolved version of this GsFile."
	(self file respondsTo: aMessage selector)
		ifTrue: [ ^ self file perform: aMessage selector withArguments: aMessage arguments ].
	
	^ super doesNotUnderstand: aMessage.
%

category: 'accessing - file'
method: GsFileAdaptor
fileSize
	^self file fileSize
%

category: 'accessing - file'
method: GsFileAdaptor
flush
	self file flush
%

category: 'accessing - file'
method: GsFileAdaptor
fullName
	^self file pathName
%

category: 'accessing - file'
method: GsFileAdaptor
isClosed
	^self isOpen not
%

category: 'accessing - file'
method: GsFileAdaptor
isOpen
	^self file notNil and: [self file isOpen]
%

category: 'testing'
method: GsFileAdaptor
isWritable
	^self options isWritable
%

category: 'accessing - file'
method: GsFileAdaptor
next: count into: buf
	^self file next: count into: buf
%

category: 'accessing - file'
method: GsFileAdaptor
nextPutAll: stringOrByteArray
	^self file nextPutAll: stringOrByteArray
%

category: 'accessing'
method: GsFileAdaptor
options
	^options
%

category: 'accessing'
method: GsFileAdaptor
options: aFileOptions
	options := aFileOptions
%

category: 'accessing - file'
method: GsFileAdaptor
position
	^self file position ifNil: [ self error: 'position error' ]
%

category: 'accessing - file'
method: GsFileAdaptor
position: aPosition
	(self file position: aPosition) ifNil: [ self error: 'position error' ]
%

category: 'accessing - file'
method: GsFileAdaptor
readInto: readBuffer startingAt: startIndex count: count
	^(self file read: count into: readBuffer) ifNil: [FileReadError signal: 'File Read Error']
%

category: 'accessing - file'
method: GsFileAdaptor
sync
	"The handle is always nil. This is here for Pharo compatability"

	"On Unix, this syncs any written or flushed data still in the kernel file
	system buffers to disk. On Windows this and primFlush: do the same thing

	self file sync



	<primitive: 'primitiveFileSync' module: 'FilePlugin'>

"
	self error: 'sync not yet implmented'

	"fsync() failing cannot be ignored"
%

category: 'accessing - file'
method: GsFileAdaptor
truncate: anInteger
	self file position: anInteger
%

category: 'accessing - file'
method: GsFileAdaptor
writeFrom: aByteArray startingAt: anIndex count: count
	^(self nextPutAll: aByteArray) ifNil: [
		(FileWriteError fileName: self fullName) signal: ('File ' , self file name , ' write failed')]
%

! Class implementation for 'AbstractFileReference'

!		Instance methods for 'AbstractFileReference'

category: 'copying'
method: AbstractFileReference
, extension
	^ self resolve, extension
%

category: 'navigating'
method: AbstractFileReference
/ aString
	"aString is either a file or path.  If aString is relative, it is appended to the receiver, if it is absolute, an instance of the receiver with the path is answered"

	^ self withPath: (self path resolvePath: (self fileSystem pathFromString: aString))
%

category: 'comparing'
method: AbstractFileReference
<= other
	^ self path <= other path
%

category: 'accessing'
method: AbstractFileReference
absolutePath
	"Returns the absolute path"
	
	^ self subclassResponsibility
%

category: 'enumerating'
method: AbstractFileReference
allChildren
	"Return all the files and folders recursively nested in the receiver"
	
	^ CollectVisitor breadthFirst: self resolve collect: [:entry | entry reference]
%

category: 'enumerating'
method: AbstractFileReference
allChildrenMatching: aPattern
    "Return all the files and folders recursively nested in the receiver and matching the pattern, aPattern"
    
    ^ self allChildren select: [ :each |  aPattern match: each basename ]
%

category: 'enumerating'
method: AbstractFileReference
allDirectories
	"Return all the directories recursively nested in the receiver."
	^ (SelectVisitor breadthFirst: self resolve select: [:entry | entry isDirectory ])
		collect: [ :each| each reference ]
%

category: 'enumerating'
method: AbstractFileReference
allDirectoriesMatching: aPattern
    "Return all the directories recursively nested in the receiver."
    ^ (SelectVisitor 
			breadthFirst: self resolve 
			select: [:entry | entry isDirectory and: [ aPattern match: entry basename ] ])
        collect: [ :each | each reference ]
%

category: 'enumerating'
method: AbstractFileReference
allEntries
	^ CollectVisitor breadthFirst: self resolve
%

category: 'enumerating'
method: AbstractFileReference
allFiles
	"Return all the files (not directories) recursively nested in the receiver."
	^ (SelectVisitor breadthFirst: self resolve select: [:entry | entry isFile ])
		collect: [ :each| each reference ]
%

category: 'converting'
method: AbstractFileReference
asAbsolute 
	self subclassResponsibility
%

category: 'converting'
method: AbstractFileReference
asFileReference 
	self subclassResponsibility 
%

category: 'converting'
method: AbstractFileReference
asPath
	"Answer the receiver's path"

	self subclassResponsibility 
%

category: 'delegated'
method: AbstractFileReference
asPathWith: anObject
	^ self resolve asPathWith: anObject
%

category: 'resolving'
method: AbstractFileReference
asResolvedBy: anObject
	^ anObject resolveReference: self
%

category: 'accessing'
method: AbstractFileReference
base
	"Returns the base of the basename, i.e. /foo/gloops.taz base is 'gloops'"
	^ self fullPath base
%

category: 'accessing'
method: AbstractFileReference
basename
	"Returns the basename, i.e. /foo/gloops.taz basename is 'gloops.taz'"
	^ self fullPath basename
%

category: 'accessing'
method: AbstractFileReference
basenameWithIndicator
	"Returns the basename with the indicator appended, i.e. /foo/gloops.taz basenameWithIndicator is 'gloops.taz', whereras /foo basenameWithIndicator is 'foo/'" 
	^ self basename, self indicator   
%

category: 'accessing'
method: AbstractFileReference
basenameWithoutExtension
	"Returns the basename, i.e. /foo/gloops.taz basenameWithoutExtension is 'gloops'"
	^ self fullPath basenameWithoutExtension
%

category: 'accessing'
method: AbstractFileReference
basenameWithoutExtension: anExtension
	"Returns the basename without specified extension (if any)
	('/foo/gloops.taz' asFileReference basenameWithoutExtension: 'taz') = 'gloops'
	"
	^ self fullPath basenameWithoutExtension: anExtension
%

category: 'streams-compatibility'
method: AbstractFileReference
binaryReadStream
	^BufferedStreamSpec newRead on: self
%

category: 'streams-compatibility'
method: AbstractFileReference
binaryReadStreamDo: aBlock
	^self stream: self binaryReadStream do: aBlock
%

category: 'streams-compatibility'
method: AbstractFileReference
binaryReadStreamDo: doBlock ifAbsent: absentBlock
	^ self isFile 
		ifTrue: [ self binaryReadStreamDo: doBlock ]
		ifFalse: [ absentBlock value ]
%

category: 'streams-compatibility'
method: AbstractFileReference
binaryReadStreamIfAbsent: absentBlock
	^ self isFile
		ifTrue: [ self binaryReadStream ]
		ifFalse: [ absentBlock value ]
%

category: 'streams'
method: AbstractFileReference
binaryReadWriteStream
	^BufferedStreamSpec newReadWrite on: self
%

category: 'streams'
method: AbstractFileReference
binaryWriteStream
	^BufferedStreamSpec newWrite on: self
%

category: 'streams'
method: AbstractFileReference
binaryWriteStreamDo: aBlock
	^self stream: self binaryWriteStream do: aBlock
%

category: 'streams'
method: AbstractFileReference
binaryWriteStreamDo: doBlock ifPresent: presentBlock
	^ self isFile
		ifTrue: [ presentBlock ]
		ifFalse: [ self binaryWriteStreamDo: doBlock ]
%

category: 'delegated'
method: AbstractFileReference
canonicalize
	"Answer the receiver with references to the current folder (.) and parent folder (..) removed"

	^ self withPath: self resolve path canonicalize
%

category: 'private'
method: AbstractFileReference
childGeneratorBlock: doBlock matching: patterns
	"
	FileSystem workingDirectory filesMatching: '*'
	FileSystem workingDirectory filesMatching: '*.image;*.changes'
	"
	
	| files reference|
	files := Set new.
	reference := self resolve.
	
	(patterns subStrings: ';', String crlf) do: [ :pattern | 
		doBlock value: reference value: [ :basename|
			(pattern match: basename)
				ifTrue: [ files add: (self / basename) ]]].
	
	^files asOrderedCollection
%

category: 'enumerating'
method: AbstractFileReference
childNames
	^ self children collect: [:each | each basename ]
%

category: 'enumerating'
method: AbstractFileReference
children
	"Answer an array containing references to the direct children of this reference."
	| reference |
	reference := self resolve.
	^ (reference fileSystem childNamesAt: reference path) 
		collect: [ :aName | self / aName ]
%

category: 'enumerating'
method: AbstractFileReference
childrenMatching: patterns
	"
	FileSystem disk workingDirectory childrenMatching: '*'
	FileSystem disk workingDirectory childrenMatching: '*.image;*.changes'
	"
	^ self 
		childGeneratorBlock: [:reference : aBlock| 
			reference fileSystem childNamesAt: reference path do: aBlock ]
		matching:  patterns 
%

category: 'comparing'
method: AbstractFileReference
contains: anObject
	"Return true if anObject is in a subfolder of me"
	^ anObject isContainedBy: self resolve
%

category: 'comparing'
method: AbstractFileReference
containsPath: aPath
	^ self fullPath containsPath: aPath
%

category: 'accessing'
method: AbstractFileReference
contents
	self readStreamDo: [ :stream |
		^ stream contents ]
%

category: 'operations'
method: AbstractFileReference
copyAllTo: aResolvable
	"Performs a deep copy of the receiver, to a location specified by the argument. If the receiver is a file, the file will be copied; if a directory, the directory and its contents will be copied recursively. The argument must be a reference that doesn't exist; it will be created by the copy."
	
	CopyVisitor copy: self resolve asAbsolute to: aResolvable resolve
%

category: 'delegated'
method: AbstractFileReference
copyTo: aReference
	^ self resolve copyTo: aReference resolve
%

category: 'copying'
method: AbstractFileReference
copyWithPath: newPath
	self subclassResponsibility
%

category: 'enumerating'
method: AbstractFileReference
createDirectory
	"Verifies that the directory does not exist and only creates if necessary. Do not remove files contained if they exist.If the parents does not exist return an exception"
	self parent exists ifFalse:[DirectoryDoesNotExist signalWith: self parent path].
	^ self resolve ensureCreateDirectory
%

category: 'enumerating'
method: AbstractFileReference
createFile
	"Create if necessary a file for the receiver. If the parent does not exist return an exception"
	self parent exists ifFalse:[DirectoryDoesNotExist signalWith: self parent path].
	self writeStream close.
	
%

category: 'accessing'
method: AbstractFileReference
creationTime
	^ self resolve creationTime 
%

category: 'operations'
method: AbstractFileReference
delete
	"Delete the receiver, does raise an error if it is not present."
	
	^ self resolve delete
%

category: 'operations'
method: AbstractFileReference
deleteAll
	"Delete this directory and all children of it, raise an error if the file does not exist."
	DeleteVisitor delete: self resolve
%

category: 'operations'
method: AbstractFileReference
deleteAllChildren
	"delete all children of the receiver, raise an error if the receiver does not exist"
	
	self children do: [:aReference | aReference deleteAll ]
%

category: 'operations'
method: AbstractFileReference
deleteIfAbsent: aBlock
	"Delete the receiver, when it does not exist evaluate the block"
	self resolve deleteIfAbsent: aBlock
%

category: 'enumerating'
method: AbstractFileReference
directories
	"Return all the directories (by opposition to files) contained in the receiver"

	| reference |
	reference := self resolve.
	^ (reference fileSystem directoryNamesAt: reference path) 
		collect: [ :aName | self withPath: self path / aName ]
%

category: 'enumerating'
method: AbstractFileReference
directoriesMatching: patterns
	"
	FileSystem disk workingDirectory directoriesMatching: '*'
	FileSystem disk workingDirectory directoriesMatching: 'package-cache'
	"
	^ self 
		childGeneratorBlock: [:reference : aBlock| 
			reference fileSystem directoryNamesAt: reference path do: aBlock ]
		matching:  patterns 
%

category: 'enumerating'
method: AbstractFileReference
directoryNames
	^ self directories collect: #basename
%

category: 'operations'
method: AbstractFileReference
ensureCreateDirectory
	"Verifies that the directory does not exist and only creates if necessary. Do not remove files contained if they exist.Creates the parents if needed"
	^ self resolve ensureCreateDirectory
%

category: 'operations'
method: AbstractFileReference
ensureCreateFile
	"Create if necessary a file for the receiver. If the parent does not exist creates it"
	self parent ensureCreateDirectory.
	self writeStream close.
	
%

category: 'operations'
method: AbstractFileReference
ensureDelete
	"Delete the file and does not raise exception if it does not exist contrary to delete.
	However if it is a directory and it has children an error is signaled. If it is required to 
	delete even with children, use #ensureDeleteAll."
	
	self deleteIfAbsent: [].
%

category: 'operations'
method: AbstractFileReference
ensureDeleteAll
	"Delete this directory and all children of it, and does not raise an error if the file does not exist."
	
	self exists ifFalse: [ ^ self  ].
	self deleteAll
	
%

category: 'operations'
method: AbstractFileReference
ensureDeleteAllChildren
	"delete all children of the receiver and does not raise an error if the receiver does not exist"
	
	self exists ifFalse: [ ^ self  ].
	self deleteAllChildren
%

category: 'enumerating'
method: AbstractFileReference
entries
	^ self resolve entries
%

category: 'accessing'
method: AbstractFileReference
entry
	^ self resolve entry
%

category: 'delegated'
method: AbstractFileReference
exists
	"Return true if the file reference exist (e.g., if there is a file on the hard disk pointed by the file reference)
	
	E.g., (if you are on Unix or OSX)
	'/tmp/' asFileReference exists => true
	'/tmp/zorkbar' asFileReference exists => false
	"
	^ self resolve exists
%

category: 'accessing'
method: AbstractFileReference
extension
	
	^ self fullPath extension.
%

category: 'accessing'
method: AbstractFileReference
extensions
	  "#('foo' 'foo.tar' 'foo.tar.gz' 'foo.1.tar' 'foo.1.tar.gz') collect: [:thing| thing extensions] => #(#() #('tar') #('tar' 'gz') #('1' 'tar') #('1' 'tar' 'gz'))"
	
        ^ self fullPath extensions
%

category: 'enumerating'
method: AbstractFileReference
fileNames
	^ self files collect: #basename
%

category: 'enumerating'
method: AbstractFileReference
files
	"Return all the files (as opposed to folders) contained in the receiver"
	
	| reference |
	reference := self resolve.
	^ (reference fileSystem fileNamesAt: reference path) 
		collect: [ :aName | self withPath: self path / aName ]
%

category: 'enumerating'
method: AbstractFileReference
filesMatching: patterns
	"
	FileSystem disk workingDirectory filesMatching: '*'
	FileSystem disk workingDirectory filesMatching: '*.image;*.changes'
	"
	^ self 
		childGeneratorBlock: [:reference : aBlock| 
			reference fileSystem fileNamesAt: reference path do: aBlock ]
		matching:  patterns 
%

category: 'accessing'
method: AbstractFileReference
fileSystem
	^ self resolve fileSystem
%

category: 'delegated'
method: AbstractFileReference
fullName

	^ self resolve fullName
%

category: 'accessing'
method: AbstractFileReference
fullNameWithIndicator
	"Returns the basename with the indicator appended, i.e. /foo/gloops.taz basenameWithIndicator is '/foo/gloops.taz', whereras /foo basenameWithIndicator is '/foo/'" 
	^ self fullName, self indicator   
%

category: 'accessing'
method: AbstractFileReference
fullPath
	"Returns the absolute path, better use absolutePath"
	
	^ self subclassResponsibility
%

category: 'enumerating'
method: AbstractFileReference
glob: aBlock
	^ SelectVisitor breadthFirst: self resolve select: aBlock
%

category: 'testing'
method: AbstractFileReference
hasChildren
	^self resolve hasChildren
%

category: 'testing'
method: AbstractFileReference
hasDirectories
	^self resolve hasDirectories
%

category: 'testing'
method: AbstractFileReference
hasFiles
	^self resolve hasFiles
%

category: 'comparing'
method: AbstractFileReference
hash
	"Hash is reimplemented because #= is reimplemented"
	^ self path  hash
%

category: 'accessing'
method: AbstractFileReference
humanReadableSize
	^ self size humanReadableSIByteSize
%

category: 'testing'
method: AbstractFileReference
ifFile: fBlock ifDirectory: dBlock ifAbsent: aBlock
	^ self isFile
		ifTrue: fBlock
		ifFalse: [
			self isDirectory
				ifTrue: dBlock
				ifFalse: aBlock ]
%

category: 'printing'
method: AbstractFileReference
indicator
	"Returns a string indicating the type of reference:
	- '?' for a non existing reference',
	- '/' for a directory,
	- the empty string for a file."
	"When this framework gets more complete, it is possible to extend this behavior with the following indicators (taken from unix ls utility):
	- '*' for a regular file that is executable
	- '@' for a symbolic link
	- '|' for FIFOs
	- '=' for sockets
	- '>' for doors"
	^ self exists
		ifTrue: [self isDirectory ifTrue: ['/'] ifFalse: ['']  ]
		ifFalse: ['?']
%

category: 'initialization'
method: AbstractFileReference
initialize
%

category: 'testing'
method: AbstractFileReference
isAbsolute 
	self subclassResponsibility 
%

category: 'testing'
method: AbstractFileReference
isChildOf: anObject
	^ self parent = anObject
%

category: 'comparing'
method: AbstractFileReference
isContainedBy: anObject
	"DoubleDispatch helper for #contains:"
	^ anObject containsReference: self resolve
%

category: 'testing'
method: AbstractFileReference
isDirectory
	^ self resolve isDirectory
%

category: 'testing'
method: AbstractFileReference
isFile
	^ self resolve isFile
%

category: 'testing'
method: AbstractFileReference
isReadable
	^ self resolve isReadable
%

category: 'testing'
method: AbstractFileReference
isRelative 
	self subclassResponsibility 
%

category: 'testing'
method: AbstractFileReference
isRoot
	^ self resolve isRoot
%

category: 'testing'
method: AbstractFileReference
isWritable
	^ self resolve isWritable
%

category: 'accessing'
method: AbstractFileReference
item
	^ self
%

category: 'navigating'
method: AbstractFileReference
makeRelative: anObject
	^ anObject relativeToReference: self resolve
%

category: 'accessing'
method: AbstractFileReference
modificationTime
	^ self resolve modificationTime 
%

category: 'operations'
method: AbstractFileReference
moveTo: aReference
	"Move the receiver in the location passed as argument.
	
	(FileSystem disk workingDirectory / 'paf' ) ensureCreateFile.
	(FileSystem disk workingDirectory / 'fooFolder') ensureCreateDirectory. 
	(FileSystem disk workingDirectory / 'paf' ) moveTo: (FileSystem disk workingDirectory / 'fooFolder' / 'paf')
	"
	^ self resolve moveTo: aReference
%

category: 'utilities'
method: AbstractFileReference
nextVersion
	^ self resolve nextVersion
%

category: 'streams'
method: AbstractFileReference
openOptions: aFileOptions
	^ self resolve openOptions: aFileOptions
%

category: 'streams'
method: AbstractFileReference
openWithOptions: aFileOptions
	"This returns an instance of a file"
	^ self resolve openWithOptions: aFileOptions
%

category: 'delegated'
method: AbstractFileReference
parent
	^ self withPath: self resolve path parent
%

category: 'delegated'
method: AbstractFileReference
parentUpTo: aParentDirName
	^ self withPath: (self path parentUpTo: aParentDirName)
%

category: 'accessing'
method: AbstractFileReference
pathSegments
	^ self fullPath segments
%

category: 'delegated'
method: AbstractFileReference
pathString
	^ self resolve pathString
%

category: 'accessing'
method: AbstractFileReference
permissions
	"Return the FileSystemPermission for this node"
	^ self resolve permissions
%

category: 'streams'
method: AbstractFileReference
readRawStream
	^BinaryStreamSpec newRead on: self
%

category: 'streams'
method: AbstractFileReference
readStream
	^EncodedBufferedStreamSpec newRead on: self
%

category: 'streams'
method: AbstractFileReference
readStreamDo: aBlock
	^self stream: self readStream do: aBlock
%

category: 'streams'
method: AbstractFileReference
readStreamDo: doBlock ifAbsent: absentBlock
	^ self isFile 
		ifTrue: [ self readStreamDo: doBlock ]
		ifFalse: [absentBlock value]
%

category: 'streams'
method: AbstractFileReference
readStreamEncoded: anEncoding

	^ (EncodedBufferedStreamSpec newRead encoding: anEncoding) on: self
%

category: 'streams'
method: AbstractFileReference
readStreamEncoded: anEncoding do: aBlock
	^self stream: (self readStreamEncoded: anEncoding) do: aBlock
%

category: 'streams'
method: AbstractFileReference
readStreamIfAbsent: absentBlock
	^ self isFile
		ifTrue: [ self readStream ]
		ifFalse: [absentBlock value]
%

category: 'navigating'
method: AbstractFileReference
relativeTo: landmark
	"Answer a new path relative to landmark."
	
	"parent/child/grandChild relativeTo: parent returns child/grandChild
	(Filesystem disk / 'griffle' / 'plonk' / 'nurp') relativeTo: (Filesystem disk / 'griffle') 
	returns plonk/nurp"

	^ landmark makeRelative: self resolve
%

category: 'navigating'
method: AbstractFileReference
relativeToPath: landmarkPath 
	
	^ self fullPath relativeTo: landmarkPath
%

category: 'navigating'
method: AbstractFileReference
relativeToReference: landmarkReference
	"Return the path of the receiver relative to landmarkReference."
	
	^ self fullPath relativeTo: landmarkReference path
%

category: 'operations'
method: AbstractFileReference
renameTo: newBasename
	self subclassResponsibility
%

category: 'navigating'
method: AbstractFileReference
resolve
	^ self subclassResponsibility 
%

category: 'navigating'
method: AbstractFileReference
resolve: anObject
	^ anObject asResolvedBy: self
%

category: 'navigating'
method: AbstractFileReference
resolvePath: aPath
	^ self withPath: (self path resolvePath: aPath)
%

category: 'navigating'
method: AbstractFileReference
resolveReference: aReference
	^ aReference isAbsolute
		ifTrue: [ aReference ]
		ifFalse: [ self withPath: aReference path ]
%

category: 'navigating'
method: AbstractFileReference
resolveString: aString 
	self subclassResponsibility
%

category: 'accessing'
method: AbstractFileReference
size
	"Return the size of the file in bytes."
	
	^ self resolve size
%

category: 'delegated'
method: AbstractFileReference
store
	"Answer the receiver with references to the current folder (.) and parent folder (..) removed"

	^ self fileSystem store
%

category: 'streams'
method: AbstractFileReference
stream: aStream do: aBlock
	^ [ aBlock value: aStream ] 
		ensure: [ aStream close ]
%

category: 'streams'
method: AbstractFileReference
streamFrom: aStreamSpec
	"Answer a stream on a file"
	^ aStreamSpec on: self
%

category: 'streams'
method: AbstractFileReference
streamFrom: aStreamSpec do: aBlock
	^self stream: (aStreamSpec streamFrom: self) do: aBlock
%

category: 'streams'
method: AbstractFileReference
streamWritable: writable do: aBlock
	^ writable 
		ifTrue: [ self writeStreamDo: aBlock ]
		ifFalse: [ self readStreamDo: aBlock ]
%

category: 'accessing'
method: AbstractFileReference
uri
	"Convert my path into a file:// type url. For odd characters use %20 notation."

	^ self asUrl
%

category: 'navigating'
method: AbstractFileReference
withExtension: aString
	^ self withPath: (self path withExtension: aString)
%

category: 'copying'
method: AbstractFileReference
withoutExtension
  "Returns the new reference based on receiver with fullname without its extension"

  ^  (self parent / self basenameWithoutExtension)
%

category: 'navigating'
method: AbstractFileReference
withPath: newPath
	^ self path == newPath
		ifTrue: [ self ]
		ifFalse: [ self copyWithPath: newPath ]
%

category: 'streams'
method: AbstractFileReference
writeRawStream
	^BinaryStreamSpec newWrite on: self
%

category: 'streams'
method: AbstractFileReference
writeStream
	^ EncodedBufferedStreamSpec newWrite on: self
%

category: 'streams'
method: AbstractFileReference
writeStreamDo: aBlock
	^self stream: self writeStream do: aBlock
%

category: 'streams'
method: AbstractFileReference
writeStreamDo: doBlock ifPresent: presentBlock
	^ self isFile
		ifTrue: [presentBlock value ]
		ifFalse: [ self writeStreamDo: doBlock ]
%

category: 'streams'
method: AbstractFileReference
writeStreamEncoded: anEncoding

	^(EncodedBufferedStreamSpec newWrite encoding: anEncoding) on: self
%

category: 'streams'
method: AbstractFileReference
writeStreamEncoded: anEncoding do: aBlock
	^self stream: (self writeStreamEncoded: anEncoding) do: aBlock
%

category: 'streams'
method: AbstractFileReference
writeStreamIfPresent: presentBlock
	^ self isFile 
		ifTrue: [ presentBlock value ]
		ifFalse: [ self writeStream ]
%

! Class implementation for 'FileLocator'

!		Class methods for 'FileLocator'

category: 'windows-origins'
classmethod: FileLocator
A
	^ self driveNamed: #A 
%

category: 'adding'
classmethod: FileLocator
addResolver: aResolver
	self resolver addResolver: aResolver
%

category: 'windows-origins'
classmethod: FileLocator
B
	^ self driveNamed: #B
%

category: 'windows-origins'
classmethod: FileLocator
C
	^ self driveNamed: #C
%

category: 'origins'
classmethod: FileLocator
cache
	^ self origin: #cache
%

category: 'origins'
classmethod: FileLocator
cwd
	^ self workingDirectory
%

category: 'windows-origins'
classmethod: FileLocator
D
	^ self driveNamed: #D
%

category: 'gemstone-origins'
classmethod: FileLocator
dbfScratchDir
	"Answer the path to the DBF_SCRATCH_DIR"

	^ self origin: #dbfScratchDir
%

category: 'origins'
classmethod: FileLocator
desktop
	^ self origin: #desktop
%

category: 'origins'
classmethod: FileLocator
documents
	^ self origin: #documents
%

category: 'windows-origins'
classmethod: FileLocator
driveNamed: driveName
	^ FileReference fileSystem: (FileSystem disk) path: Path / (driveName, ':')
%

category: 'windows-origins'
classmethod: FileLocator
E
	^ self driveNamed: #E
%

category: 'gemstone-origins'
classmethod: FileLocator
extent1
	"Answer the path to the first exent extent"

	^ self origin: #extent1
%

category: 'gemstone-origins'
classmethod: FileLocator
extent1Directory

	^ self origin: #extent1Directory
%

category: 'gemstone-origins'
classmethod: FileLocator
extent: extentIndex
	"Answer the path to the extent at extentIndex in DBF_EXTENT_NAMES: (System stoneConfigurationReport at: #DBF_EXTENT_NAMES) at: extentIndex"

	^ self origin: ('extent', extentIndex printString) asSymbol
%

category: 'windows-origins'
classmethod: FileLocator
F
	^ self driveNamed: #F
%

category: 'flushing'
classmethod: FileLocator
flushCaches
	self resolver flushCaches
%

category: 'windows-origins'
classmethod: FileLocator
G
	^ self driveNamed: #G
%

category: 'gemstone-origins'
classmethod: FileLocator
gemLogDirectory
	"locator for the log directory (parent directory of gemLog)"

	^ self origin: #gemLogDirectory
%

category: 'windows-origins'
classmethod: FileLocator
H
	^ self driveNamed: #H
%

category: 'origins'
classmethod: FileLocator
home
	^ self origin: #home
%

category: 'windows-origins'
classmethod: FileLocator
I
	^ self driveNamed: #I
%

category: 'gemstone-origins'
classmethod: FileLocator
image
	"commonly used, so we need to define it, but the closest that GemStone comes is the #dbfScratchDir"

	^ self dbfScratchDir
%

category: 'gemstone-origins'
classmethod: FileLocator
imageDirectory
	"commonly used, so we need to define it, but the closest that GemStone comes is the #dbfScratchDir"

	^ self dbfScratchDir
%

category: 'windows-origins'
classmethod: FileLocator
J
	^ self driveNamed: #J
%

category: 'windows-origins'
classmethod: FileLocator
K
	^ self driveNamed: #K
%

category: 'windows-origins'
classmethod: FileLocator
L
	^ self driveNamed: #L
%

category: 'windows-origins'
classmethod: FileLocator
M
	^ self driveNamed: #M
%

category: 'windows-origins'
classmethod: FileLocator
N
	^ self driveNamed: #N
%

category: 'windows-origins'
classmethod: FileLocator
O
	^ self driveNamed: #O
%

category: 'instance creation'
classmethod: FileLocator
origin: aSymbol 
	^ self origin: aSymbol path: Path workingDirectory
%

category: 'instance creation'
classmethod: FileLocator
origin: aSymbol path: aPath
	^ self basicNew 
			initializeWithOrigin: aSymbol path: aPath
%

category: 'windows-origins'
classmethod: FileLocator
P
	^ self driveNamed: #P
%

category: 'origins'
classmethod: FileLocator
preferences
	^ self origin: #preferences
%

category: 'windows-origins'
classmethod: FileLocator
Q
	^ self driveNamed: #Q
%

category: 'windows-origins'
classmethod: FileLocator
R
	^ self driveNamed: #R
%

category: 'accessing'
classmethod: FileLocator
resolver

	^ SessionTemps current at: #FileLocator_Resolver ifAbsentPut: [
		InteractiveResolver new
			addResolver: SystemResolver new;
			addResolver: PlatformResolver forCurrentPlatform;
			yourself ]
%

category: 'origins'
classmethod: FileLocator
root
	^ FileSystem disk root
%

category: 'windows-origins'
classmethod: FileLocator
S
	^ self driveNamed: #S
%

category: 'gemstone-origins'
classmethod: FileLocator
serverWorkingDirectory
	^ FileSystem server referenceTo: RelativePath new
%

category: 'accessing'
classmethod: FileLocator
supportedOrigins
	| origins current |
	origins := IdentitySet new.
	current := self resolver.
	[current notNil] whileTrue:
		[origins addAll: current supportedOrigins.
		current := current next].
	^ origins
%

category: 'mac-origins'
classmethod: FileLocator
systemApplicationSupport
	^ self origin: #systemApplicationSupport
%

category: 'mac-origins'
classmethod: FileLocator
systemLibrary
	^ self origin: #systemLibrary
%

category: 'windows-origins'
classmethod: FileLocator
T
	^ self driveNamed: #T
%

category: 'origins'
classmethod: FileLocator
temp
	^ self origin: #temp
%

category: 'gemstone-origins'
classmethod: FileLocator
tranlog
	"Answer the path to the current tranlog directory"

	^ self origin: #tranlog
%

category: 'windows-origins'
classmethod: FileLocator
U
	^ self driveNamed: #U
%

category: 'mac-origins'
classmethod: FileLocator
userApplicationSupport
	^ self origin: #userApplicationSupport
%

category: 'unix-origins'
classmethod: FileLocator
userData
	^ self origin: #userData
%

category: 'mac-origins'
classmethod: FileLocator
userLibrary
	^ self origin: #userLibrary
%

category: 'windows-origins'
classmethod: FileLocator
V
	^ self driveNamed: #V
%

category: 'windows-origins'
classmethod: FileLocator
W
	^ self driveNamed: #W
%

category: 'origins'
classmethod: FileLocator
workingDirectory
	^ FileSystem disk referenceTo: RelativePath new
%

category: 'windows-origins'
classmethod: FileLocator
X
	^ self driveNamed: #X
%

category: 'windows-origins'
classmethod: FileLocator
Y
	^ self driveNamed: #Y
%

category: 'windows-origins'
classmethod: FileLocator
Z
	^ self driveNamed: #Z
%

!		Instance methods for 'FileLocator'

category: 'comparing'
method: FileLocator
= other
	^ self species = other species
		and: [origin = other origin
			and: [path = other path]]
%

category: 'accessing'
method: FileLocator
absolutePath
	"Return the absolute path"
	^ self resolve path
%

category: 'converting'
method: FileLocator
asAbsolute
	^ self 
%

category: 'converting'
method: FileLocator
asFileReference
	^ self resolve
%

category: 'converting'
method: FileLocator
asPath
	"Answer the receiver's path"

	^self resolve asPath
%

category: 'streams-compatibility'
method: FileLocator
binaryReadStream
	^ self resolve binaryReadStream
%

category: 'streams-compatibility'
method: FileLocator
binaryReadWriteStream
	^ self resolve binaryReadWriteStream
%

category: 'streams-compatibility'
method: FileLocator
binaryWriteStream
	"Answer a binary write stream on the receiver"

	^ self resolve binaryWriteStream
%

category: 'copying'
method: FileLocator
copyWithPath: newPath
	^ self class origin: origin path: newPath
%

category: 'error handling'
method: FileLocator
doesNotUnderstand: aMessage
	"Redirect message to the resolved version of this FileLocator.
	If FileReference won't understand the message send a normal DNU."

	| resolved |
	
	resolved := self resolve.
	(resolved respondsTo: aMessage selector)
		ifTrue: [ ^ resolved perform: aMessage selector withArguments: aMessage arguments ].
	
	^ super doesNotUnderstand: aMessage.
%

category: 'accessing'
method: FileLocator
fullPath
	^ self resolve path
%

category: 'comparing'
method: FileLocator
hash
	^ origin hash bitXor: path hash
%

category: 'initialize-release'
method: FileLocator
initializeWithOrigin: aSymbol path: aPath
	self initialize.
	origin := aSymbol.
	path := aPath.
%

category: 'testing'
method: FileLocator
isAbsolute
	^ true
%

category: 'testing'
method: FileLocator
isRelative
	^ false
%

category: 'accessing'
method: FileLocator
origin
	^ origin
%

category: 'accessing'
method: FileLocator
path
	^ path
%

category: 'printing'
method: FileLocator
printOn: aStream
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
%

category: 'streams'
method: FileLocator
readRawStream
	^ self resolve readRawStream
%

category: 'streams'
method: FileLocator
readStream
	^ self resolve readStream 
%

category: 'operations'
method: FileLocator
renameTo: newBasename
	
	| result |
	result := self resolve renameTo: newBasename.
	path size > 0
		ifTrue: [ path basename: newBasename ]
		ifFalse: [ path := result path ]
%

category: 'navigating'
method: FileLocator
resolve
	^ (self class resolver resolve: origin) resolve: path
%

category: 'navigating'
method: FileLocator
resolveString: aString
	| filesystem thePath |
	filesystem := (self class resolver resolve: origin) fileSystem.
	thePath := filesystem pathFromString: aString.
	^ self withPath: (path resolvePath: thePath)
%

category: 'streams'
method: FileLocator
streamFrom: aStreamSpec
	^self resolve streamFor: aStreamSpec
%

category: 'streams'
method: FileLocator
writeRawStream
	^ self resolve writeRawStream
%

category: 'streams'
method: FileLocator
writeStream
	^ self resolve writeStream 
%

! Class implementation for 'FileReference'

!		Class methods for 'FileReference'

category: 'cross platform'
classmethod: FileReference
/ aString
	"Answer a reference to the argument resolved against the root of the current disk filesystem."
	
	^ FileSystem disk / aString
%

category: 'instance creation'
classmethod: FileReference
fileSystem: aFilesystem path: aPath
	^ self new setFileSystem: aFilesystem path: aPath
%

category: 'instance creation'
classmethod: FileReference
newTempFilePrefix: prefix suffix: suffix
	| tmpDir random fileName |
	
	tmpDir := FileLocator temp asFileReference.
	[ 
		random := UUID new asInteger asString.
		fileName := prefix , random , suffix.
		(tmpDir / fileName) exists ] whileTrue.
	^ tmpDir / fileName
%

!		Instance methods for 'FileReference'

category: 'navigating'
method: FileReference
, extension
	^ self withPath: self path, extension
%

category: 'comparing'
method: FileReference
= other
	"Two FileReferences are considered equal if they refer to the same file / directory.
	As paths can have multiple relative representations, compare the absolute paths."
	"Perform the path comparison last as conversion to absolute paths is relatively expensive"
	^ self species = other species
		and: [self fileSystem = other fileSystem
			and: [self absolutePath = other absolutePath]]
%

category: 'accessing'
method: FileReference
absolutePath
	"Return the absolute of the receiver"
	^ self path isRelative
		ifFalse: [ self path ]
		ifTrue: [ filesystem resolve: self path ]
%

category: 'converting'
method: FileReference
asAbsolute
	"Return the receiver as an absolute file reference."
	
	^ self isAbsolute
		ifTrue: [ self ]
		ifFalse: [ filesystem referenceTo: (filesystem resolve: path) ]
%

category: 'converting'
method: FileReference
asFileReference
	^ self
%

category: 'converting'
method: FileReference
asPath
	"Answer the receivers path"

	^path
%

category: 'comparing'
method: FileReference
containsReference: aReference
	^  aReference fileSystem = filesystem and: [path contains: aReference path]
%

category: 'operations'
method: FileReference
copyTo: aReference
	self isDirectory
		ifTrue: [ aReference ensureCreateDirectory ]
		ifFalse: [ filesystem copy: path toReference: aReference ]
%

category: 'copying'
method: FileReference
copyWithPath: newPath
	^ filesystem referenceTo: newPath
%

category: 'accessing'
method: FileReference
creationTime 
	^ filesystem creationTimeOf: self path
%

category: 'operations'
method: FileReference
delete
	"Deletes the referenced file or directory. If the directory is not empty, 
	raises an error. Use #deleteAll to delete with the children."

	(self isDirectory and:[self hasChildren]) 
		ifTrue:[DirectoryIsNotEmpty signalWith: self].
	filesystem delete: path
%

category: 'operations'
method: FileReference
deleteIfAbsent: aBlock
	self exists 
		ifTrue: [ self delete ]
		ifFalse: aBlock
%

category: 'operations'
method: FileReference
ensureCreateDirectory
	"Create if necessary a directory for the receiver."
	filesystem ensureCreateDirectory: path
	
%

category: 'navigating'
method: FileReference
entries
	"Return the entries (meta data - file description) of the direct children of the receiver"
	^ self fileSystem entriesAt: self path 
%

category: 'accessing'
method: FileReference
entry
	"Return the entry (meta data) describing the receiver."
	^ filesystem entryAt: path
%

category: 'testing'
method: FileReference
exists
	^ filesystem exists: path
%

category: 'accessing'
method: FileReference
fileSystem
	"Return the filesystem to which the receiver belong."
	^ filesystem
%

category: 'accessing'
method: FileReference
fullName
	"Return the full path name of the receiver."
	^ filesystem stringFromPath: (filesystem resolve: path)
%

category: 'accessing'
method: FileReference
fullPath
	^ self path
%

category: 'testing'
method: FileReference
hasChildren
	"Return whether the receiver has any children."
	"FileSystem workingDirectory hasChildren"
	
	^ filesystem hasChildren: path
%

category: 'testing'
method: FileReference
hasDirectories
	"Return whether the receiver has children that are directories."
	"FileSystem workingDirectory hasDirectories"
	
	^ filesystem hasDirectories: path
%

category: 'testing'
method: FileReference
hasFiles
	"Return whether the receiver has children that are files."
	"FileSystem workingDirectory hasFiles"
	
	^ filesystem hasFiles: path
%

category: 'comparing'
method: FileReference
hash
	^ path hash bitXor: filesystem hash
%

category: 'testing'
method: FileReference
isAbsolute
	^ path isAbsolute
%

category: 'testing'
method: FileReference
isDirectory
	^ filesystem isDirectory: path
%

category: 'testing'
method: FileReference
isFile
	^ filesystem isFile: path
%

category: 'testing'
method: FileReference
isReadable
	^ filesystem isReadable: path
%

category: 'testing'
method: FileReference
isRelative
	^ path isRelative
%

category: 'testing'
method: FileReference
isRoot
	^ path isRoot
%

category: 'testing'
method: FileReference
isSymlink 
	^ filesystem isSymlink: path
%

category: 'testing'
method: FileReference
isWritable
	^ filesystem isWritable: path
%

category: 'utilities'
method: FileReference
lastFileFor: baseFileName extension: extension
  "Assumes a file is named using a version number encoded as '.' followed by digits
  preceding the file extension, e.g., games.22.ston
  Answer the file name with the largest number.
  If a version number is not found, raises an error"

  "FileSystem workingDirectory lastFileFor: 'games' extension: 'ston'"

    | files |
    files := self childrenMatching: baseFileName , '.*.' , extension.
    files ifEmpty: [ ^ self error: 'No file with number pattern' ].
    ^ (files asSortedCollection: [ :a :b | a basename < b basename ]) last
%

category: 'accessing'
method: FileReference
modificationTime 
	"Returns the last date of modification of self"
	^ filesystem modificationTimeOf: self path
%

category: 'operations'
method: FileReference
moveTo: aReference
	
	| result |
	result := self fileSystem 
		move: self path
		to: aReference resolve.
	result ifNotNil: [
		self setFileSystem: result fileSystem path: result path ].		
%

category: 'utilities'
method: FileReference
nextNameFor: baseFileName extension: extension
  "Assumes a file name includes a version number encoded as '.' followed by digits 
   preceding the file extension, e.g., games.22.ston
   Increment the version number (of the largest one) and answer the new file name, e.g., games23.ston
   If a version number is not found, set the version to 1 and answer a new file name"

	"(FileSystem workingDirectory nextNameFor: 'games' extension: 'ston') asFileReference ensureCreateFile"

	| files splits version |
	files := self childrenMatching: baseFileName , '.*.' , extension.
	files ifEmpty: [ ^ baseFileName , '.1.' , extension ].
	splits := files
		collect: [ :filename | filename basename splitOn: $. ]
		thenSelect: [ :split | (split at: 1) = baseFileName and: [ split size = 3 ] ].
	splits := splits asSortedCollection: [ :a :b | (a at: 2) asNumber < (b at: 2) asNumber ].
	version := splits isEmpty
		ifTrue: [ 1 ]
		ifFalse: [ (splits last at: 2) asNumber + 1 ].
	^ baseFileName , '.' , version asString , '.' , extension
%

category: 'utilities'
method: FileReference
nextVersion
	"Assumes a file (or folder) name includes a version number encoded as '.' followed by digits 
	preceding the file extension.  Increment the version number and answer the new file name.
	If a version number is not found, return just the file"

	| parent version versionNumbers nameWithoutExtension |
	
	self exists
		ifFalse: [ ^ self ].
	
	parent := self parent.
	nameWithoutExtension := self basename copyUpTo: $..
	
	versionNumbers := parent children 
				select: [ :f| 
					(f basename beginsWith: nameWithoutExtension) ]
				thenCollect: [ :f| 
					Number squeezeNumberOutOfString: (f basename copyLast: (f basename size - nameWithoutExtension size))   ifFail: [ 0 ]].
	
	versionNumbers ifEmpty: [ ^self ].
	
	version := versionNumbers max + 1.
	^ parent / (nameWithoutExtension , '.', version asString) , self extension
%

category: 'operations'
method: FileReference
openOptions: aFileOptions 
	^self fileSystem open: self options: aFileOptions
%

category: 'operations'
method: FileReference
openWithOptions: aFileOptions 
	"This will return an instance of the File or FileAdaptor, depending on installation"
	^filesystem open: self withOptions: aFileOptions
%

category: 'accessing'
method: FileReference
path
	"Return the path internal representation that denotes the receiver in the context of its filesystem. "
	^ path
%

category: 'printing'
method: FileReference
pathString
	"Return the full path name of the receiver."
	
	^ filesystem stringFromPath: (filesystem resolve: path)
%

category: 'accessing'
method: FileReference
permissions
	^ filesystem permissions: self path
%

category: 'printing'
method: FileReference
printOn: aStream
	filesystem forReferencePrintOn: aStream.
	filesystem printPath: path on: aStream
%

category: 'operations'
method: FileReference
renameTo: newBasename
	
	| destinationPath |
	destinationPath := self fileSystem 
		rename: self 
		to: self parent / newBasename.
	
	destinationPath ifNotNil: [
		self 
			setFileSystem: filesystem 
			path: destinationPath ].
	^ self
		
%

category: 'accessing'
method: FileReference
resolve
	^ self
%

category: 'resolving'
method: FileReference
resolvePath: anObject
	^ self withPath: (path resolve: anObject)
%

category: 'resolving'
method: FileReference
resolveReference: aReference
	
	^ (filesystem = aReference fileSystem or: [aReference isRelative])
		ifTrue: [filesystem referenceTo: (path resolvePath: aReference path)]
		ifFalse: [aReference]
%

category: 'resolving'
method: FileReference
resolveString: aString 
	| thePath |
	thePath := filesystem pathFromString: aString.
	^ filesystem referenceTo: (path resolve: thePath)
%

category: 'initialize-release'
method: FileReference
setFileSystem: aFilesystem path: aPath
	filesystem := aFilesystem.
	path := aPath
%

category: 'accessing'
method: FileReference
size
	^ filesystem sizeOf: path
%

! Class implementation for 'AbstractStreamSpec'

!		Class methods for 'AbstractStreamSpec'

category: 'instance creation'
classmethod: AbstractStreamSpec
new
	^self basicNew
		initialize;
		store: (FileSystem disk) store;
		yourself
%

category: 'instance creation'
classmethod: AbstractStreamSpec
newAppend

	^self new
		initializeForAppend;
		yourself
%

category: 'instance creation'
classmethod: AbstractStreamSpec
newRead
	^self new
		initializeForRead;
		yourself
%

category: 'instance creation'
classmethod: AbstractStreamSpec
newReadWrite

	^self new
		initializeForReadWrite;
		yourself
%

category: 'instance creation'
classmethod: AbstractStreamSpec
newWrite

	^self new
		initializeForWrite;
		yourself
%

category: 'instance creation'
classmethod: AbstractStreamSpec
on: aFileReference options: aFileOptions
	"Note: This returns an instance of the stream on the fileReference"
	^self basicNew
		initalizeOn: aFileReference options: aFileOptions
		yourself
%

category: 'instance creation'
classmethod: AbstractStreamSpec
onOptions: aFileOptions
	^self basicNew
		initalizeOnOptions: aFileOptions
		yourself
%

!		Instance methods for 'AbstractStreamSpec'

category: 'private'
method: AbstractStreamSpec
alignStoreWith: aFileReference
	"Stream Spec is built with the local filesystem. If a foreign filesystem
     was used to build the reference, switch to the foreign filesystem"

	aFileReference fileSystem isForeignFileSystem 
		ifTrue: [self store: aFileReference fileSystem store]
%

category: 'file options configuration'
method: AbstractStreamSpec
append
	self fileOptions append
%

category: 'private'
method: AbstractStreamSpec
beUnixStore
	self store: UnixStore createDefault
%

category: 'private'
method: AbstractStreamSpec
beWindowsStore
	self store: WindowsStore createDefault
%

category: 'file type configuration'
method: AbstractStreamSpec
binaryFileType
	self fileOptions binaryFileType
%

category: 'private'
method: AbstractStreamSpec
containedStreamOn: aFileReference
	^(self containedStreamSpec onOptions: self fileOptions) on: aFileReference
%

category: 'configuration'
method: AbstractStreamSpec
containedStreamSpec
	self subclassResponsibility
%

category: 'file options configuration'
method: AbstractStreamSpec
create
	self fileOptions create
%

category: 'file options configuration'
method: AbstractStreamSpec
exclusive
	self fileOptions exclusive
%

category: 'accessing'
method: AbstractStreamSpec
fileOptions
	^fileOptions
%

category: 'accessing'
method: AbstractStreamSpec
fileOptions: aFileOptions
	aFileOptions parent: self.
	fileOptions := aFileOptions
%

category: 'file type configuration'
method: AbstractStreamSpec
fileType
	^self fileOptions fileType
%

category: 'file type configuration'
method: AbstractStreamSpec
gzipHighCompression
	self fileOptions gzipHighCompression
%

category: 'file type configuration'
method: AbstractStreamSpec
gzipLowCompression
	self fileOptions gzipLowCompression
%

category: 'initialization'
method: AbstractStreamSpec
initalizeOn: aFileReference options: aFileOptions
	self initialize.
	self fileOptions: aFileOptions.
	^self on: aFileReference
%

category: 'initialization'
method: AbstractStreamSpec
initalizeOnOptions: aFileOptions
	self initialize.
	self fileOptions: aFileOptions copy.
	self store: (
		aFileOptions parent 
			ifNil: [FileSystem disk] 
			ifNotNil: [:streamSpec | streamSpec store])
%

category: 'initialization'
method: AbstractStreamSpec
initialize
	self fileOptions: FileOptions new
%

category: 'initialization'
method: AbstractStreamSpec
initializeForAppend
	self fileOptions write create append
%

category: 'initialization'
method: AbstractStreamSpec
initializeForRead
	self fileOptions read
%

category: 'initialization'
method: AbstractStreamSpec
initializeForReadWrite
	self fileOptions readWrite
%

category: 'initialization'
method: AbstractStreamSpec
initializeForWrite
	self fileOptions write create truncate
%

category: 'testing'
method: AbstractStreamSpec
isGzipped

	^self fileOptions isGzipped
%

category: 'testing'
method: AbstractStreamSpec
isReadable

	^self fileOptions isReadable
%

category: 'testing'
method: AbstractStreamSpec
isReadWritable

	^self fileOptions isReadWritable
%

category: 'testing'
method: AbstractStreamSpec
isWritable

	^self fileOptions isWritable
%

category: 'printing'
method: AbstractStreamSpec
mode
	^self fileOptions mode
%

category: 'streams'
method: AbstractStreamSpec
on: aFileReference
	self alignStoreWith: aFileReference.
	^ self streamClass on: (self containedStreamOn: aFileReference)
%

category: 'streams'
method: AbstractStreamSpec
on: aFileReference do: aBlock
	| aStream |
	aStream := self on: aFileReference.
	^ [ aBlock value: aStream ]
			ensure: [ aStream close ]
%

category: 'streams'
method: AbstractStreamSpec
on: aFileReference do: aBlock ifAbsent: anAbsentBlock
	^aFileReference isFile
		ifTrue: [self on: aFileReference do: aBlock]
		ifFalse: [anAbsentBlock value]
%

category: 'streams'
method: AbstractStreamSpec
on: aFileReference do: aBlock ifPresent: anAbsentBlock
	^aFileReference isFile
		ifTrue: [anAbsentBlock value]
		ifFalse: [ self on: aFileReference do: aBlock]
%

category: 'file options configuration'
method: AbstractStreamSpec
read
	self fileOptions read
%

category: 'configuration'
method: AbstractStreamSpec
readStreamClass
	self subclassResponsibility
%

category: 'file options configuration'
method: AbstractStreamSpec
readWrite
	self fileOptions readWrite
%

category: 'configuration'
method: AbstractStreamSpec
readWriteStreamClass
	self subclassResponsibility
%

category: 'accessing'
method: AbstractStreamSpec
store
	^store ifNil: [^FileSystem disk store]
%

category: 'accessing'
method: AbstractStreamSpec
store: anObject
	store := anObject
%

category: 'configuration'
method: AbstractStreamSpec
streamClass
	self isReadWritable ifTrue: [^self readWriteStreamClass].
	self isWritable ifTrue: [^self writeStreamClass] .
	^self readStreamClass
%

category: 'file type configuration'
method: AbstractStreamSpec
textFileType
	self fileOptions textFileType
%

category: 'file options configuration'
method: AbstractStreamSpec
truncate
	self fileOptions truncate
%

category: 'file options configuration'
method: AbstractStreamSpec
write
	self fileOptions write
%

category: 'configuration'
method: AbstractStreamSpec
writeStreamClass
	self subclassResponsibility
%

! Class implementation for 'BinaryStreamSpec'

!		Instance methods for 'BinaryStreamSpec'

category: 'configuration'
method: BinaryStreamSpec
containedStreamSpec
	^self error: 'This is the lowest level of nested streams'
%

category: 'streams'
method: BinaryStreamSpec
on: aFileReference
	self alignStoreWith: aFileReference.
	^(aFileReference fileSystem open: aFileReference options: self fileOptions) binaryStream
%

category: 'configuration'
method: BinaryStreamSpec
readStreamClass
	^self error: 'This is the lowest level of nested streams'
%

category: 'configuration'
method: BinaryStreamSpec
readWriteStreamClass
	^self error: 'This is not supported by the BinaryFileStream'
%

category: 'configuration'
method: BinaryStreamSpec
writeStreamClass
	^self error: 'This is the lowest level of nested streams'
%

! Class implementation for 'BufferedStreamSpec'

!		Instance methods for 'BufferedStreamSpec'

category: 'configuration'
method: BufferedStreamSpec
containedStreamSpec
	^BinaryStreamSpec
%

category: 'configuration'
method: BufferedStreamSpec
readStreamClass
	^ZnBufferedReadStream
%

category: 'configuration'
method: BufferedStreamSpec
readWriteStreamClass
	^ZnBufferedReadWriteStream
%

category: 'configuration'
method: BufferedStreamSpec
writeStreamClass
	^ZnBufferedWriteStream
%

! Class implementation for 'EncodedStreamSpec'

!		Instance methods for 'EncodedStreamSpec'

category: 'encoding'
method: EncodedStreamSpec
ascii
	self encoding: 'ascii'
%

category: 'configuration'
method: EncodedStreamSpec
containedStreamSpec
	^BinaryStreamSpec
%

category: 'accessing'
method: EncodedStreamSpec
encoding
	^encoding
%

category: 'accessing'
method: EncodedStreamSpec
encoding: anObject
	encoding := anObject
%

category: 'initialization'
method: EncodedStreamSpec
initialize
	super initialize.
	self utf8
%

category: 'encoding'
method: EncodedStreamSpec
iso88591
	self encoding: 'iso88591'
%

category: 'encoding'
method: EncodedStreamSpec
latin1
	self encoding: 'latin1'
%

category: 'encoding'
method: EncodedStreamSpec
null
	self encoding: 'null'
%

category: 'streams'
method: EncodedStreamSpec
on: aFileReference
	self alignStoreWith: aFileReference.
	^ self streamClass
		on: (self containedStreamOn: aFileReference)
		encoding: self encoding
%

category: 'configuration'
method: EncodedStreamSpec
readStreamClass
	^ZnCharacterReadStream
%

category: 'configuration'
method: EncodedStreamSpec
readWriteStreamClass
	^ZnCharacterReadWriteStream
%

category: 'encoding'
method: EncodedStreamSpec
utf16
	self encoding: 'utf16'
%

category: 'encoding'
method: EncodedStreamSpec
utf32
	self encoding: 'utf32'
%

category: 'encoding'
method: EncodedStreamSpec
utf8
	self encoding: 'utf8'
%

category: 'configuration'
method: EncodedStreamSpec
writeStreamClass
	^ZnCharacterWriteStream
%

! Class implementation for 'EncodedBufferedStreamSpec'

!		Instance methods for 'EncodedBufferedStreamSpec'

category: 'configuration'
method: EncodedBufferedStreamSpec
containedStreamSpec
	^BufferedStreamSpec
%

! Class implementation for 'FastUUIDGenerator'

!		Class methods for 'FastUUIDGenerator'

category: 'accessing'
classmethod: FastUUIDGenerator
default
	^(SessionTemps current at: self _defaultKey ifAbsent: [])
		ifNil: [
			| default |
			default := self new initialize.
      SessionTemps current at: self _defaultKey put: default ]
%

category: 'initialization'
classmethod: FastUUIDGenerator
initialize

	SessionTemps current removeKey: self _defaultKey ifAbsent: []
%

category: 'accessing'
classmethod: FastUUIDGenerator
next
	"Return a UUID using the shared generator"
	
	"self next"
	
	"[ UUIDGenerator next ] bench"
	
	^ self default next
%

category: 'private'
classmethod: FastUUIDGenerator
_defaultKey

	^ #'UUID_DEFAULT'
%

!		Instance methods for 'FastUUIDGenerator'

category: 'instance creation'
method: FastUUIDGenerator
generateBytes: aPlaceHolder forVersion: aVersion
	aVersion = 4 ifTrue: [self generateFieldsVersion4]
		ifFalse: [self error: 'Unsupported version'].
	self placeFields: aPlaceHolder.
%

category: 'instance creation'
method: FastUUIDGenerator
generateFieldsVersion4

	semaphoreForGenerator critical: 
			[bits1 := random integer.
			bits2 := random integer.
			bits3 := random integer.
			bits4 := random integer].
	bits2 := (bits2 bitAnd: 16rFFFF0FFF) bitOr: 16r4000.
	bits3 := (bits3 bitAnd: 16r3FFFFFFF) bitOr: 16r80000000
%

category: 'instance creation'
method: FastUUIDGenerator
initialize

	random := Random new.
	semaphoreForGenerator := Semaphore forMutualExclusion
%

category: 'accessing'
method: FastUUIDGenerator
next

	"Return the next UUID"
	
	| uuid |
	uuid := UUID nilUUID.
	self generateBytes: uuid forVersion: 4.
	^ uuid
%

category: 'instance creation'
method: FastUUIDGenerator
placeFields: aPlaceHolder

	aPlaceHolder
		unsigned32At: 1 put: bits1;
		unsigned32At: 5 put: bits2;
		unsigned32At: 9 put: bits3;
		unsigned32At: 13 put: bits4
%

! Class implementation for 'FileOptions'

!		Class methods for 'FileOptions'

category: 'instance creation'
classmethod: FileOptions
new
	^self basicNew
		initialize;
		yourself
%

category: 'instance creation'
classmethod: FileOptions
newAppend
	^self new
		initializeAppend;
		yourself
%

category: 'instance creation'
classmethod: FileOptions
newRead
	^self new
		initializeRead;
		yourself
%

category: 'instance creation'
classmethod: FileOptions
newReadWrite
	^self new
		initializeReadWrite;
		yourself
%

category: 'instance creation'
classmethod: FileOptions
newWrite
	^self new
		initializeWrite;
		yourself
%

!		Instance methods for 'FileOptions'

category: 'permissions'
method: FileOptions
accessPermissions

	self permissionFlags isEmpty ifTrue: [^#FileSystemPermission default posixPermission].
	^self permissionFlags inject: 0 into: [:total :ea | total bitOr: ea posixFlagValue]
%

category: 'convenience'
method: FileOptions
accessRule
	^self openModeFlag accessRule
%

category: 'posix flags - adding and removing'
method: FileOptions
addAppendFlag
	self removeTruncateFlag.
	self addPosixFlag: FileAppendFlag new
%

category: 'posix flags - adding and removing'
method: FileOptions
addCreateFlag

	self addPosixFlag: FileCreateFlag new
%

category: 'posix flags - adding and removing'
method: FileOptions
addExclusiveFlag

	self addPosixFlag: FileExclusiveFlag new
%

category: 'permissions'
method: FileOptions
addPermissionFlag: aPermissionFlag
	(self hasPermissionsFlag: aPermissionFlag class) ifTrue: [^self].
	aPermissionFlag parent: self.
	self permissionFlags add: aPermissionFlag
%

category: 'posix flags - adding and removing'
method: FileOptions
addPosixFlag: aPosixFlag
	(self hasPosixFlag: aPosixFlag class) ifTrue: [^self].
	aPosixFlag parent: self.
	self posixFlags add: aPosixFlag
%

category: 'posix flags - adding and removing'
method: FileOptions
addTruncateFlag
	self removeAppendFlag.
	self addPosixFlag: FileTruncateFlag new
%

category: 'initialization'
method: FileOptions
alignStoreWith: anObject
	"FileOptions is built with the local filesystem. If a foreign filesystem
     was used to build the reference, switch to the foreign filesystem"

	anObject store isForeignFileSystem 
		ifTrue: [self store: anObject store]
%

category: 'commands'
method: FileOptions
append
	self hasNoReadOrWriteMode ifTrue: [^self signalOpenModeFlagNotDefined].
	self addAppendFlag
%

category: 'posix flags'
method: FileOptions
appendFlag
	^self posixFlagForClass: FileAppendFlag
%

category: 'sharing'
method: FileOptions
beNonShared
	self share: FileDenyReadWriteFlag new
%

category: 'open mode flag'
method: FileOptions
beReadMode
	self openModeFlag: FileOpenReadOnlyFlag new.
%

category: 'sharing'
method: FileOptions
beReadOnlyShared
	self share: FileDenyWriteFlag new
%

category: 'open mode flag'
method: FileOptions
beReadWriteMode
	self openModeFlag: FileOpenReadWriteFlag new.
%

category: 'sharing'
method: FileOptions
beShared
	self share: FileDenyNoneFlag new
%

category: 'open mode flag'
method: FileOptions
beWriteMode
	self openModeFlag: FileOpenWriteOnlyFlag new.
%

category: 'sharing'
method: FileOptions
beWriteOnlyShared
	self share: FileDenyReadFlag new
%

category: 'file type'
method: FileOptions
binaryFileType
	self fileType: FileBinaryTypeFlag new
%

category: 'commands'
method: FileOptions
create
	self hasNoReadOrWriteMode ifTrue: [^self signalOpenModeFlagNotDefined].
	self addCreateFlag
%

category: 'convenience'
method: FileOptions
createRule
	^self fileCreateFlag
		ifNil: [self store class openExistingRule]
		ifNotNil: [:option | option createRule]
%

category: 'commands'
method: FileOptions
exclusive
	self hasNoReadOrWriteMode ifTrue: [^self signalOpenModeFlagNotDefined].
	self addExclusiveFlag
%

category: 'posix flags'
method: FileOptions
fileCreateFlag
	^self posixFlagForClass: FileCreateFlag
%

category: 'posix flags'
method: FileOptions
fileExclusiveFlag
	^self posixFlagForClass: FileExclusiveFlag
%

category: 'posix flags'
method: FileOptions
fileOpenFlagsTotalValue
	^self fileOpenModeValue bitOr: self posixFlagsTotalValue
%

category: 'posix flags'
method: FileOptions
fileOpenModeValue
	^self openModeFlag posixFlagValue
%

category: 'accessing'
method: FileOptions
fileType
	^fileType
%

category: 'accessing'
method: FileOptions
fileType: aFileTypeFlag
	aFileTypeFlag parent: self.
	fileType := aFileTypeFlag
%

category: 'permissions'
method: FileOptions
groupAll
	(self permissionsFlagForClass: FileGroupExecutePermissionFlag) ifNotNil: [:aFlag | self removePermissionFlag: aFlag].
	(self permissionsFlagForClass: FileGroupReadPermissionFlag) ifNotNil: [:aFlag | self removePermissionFlag: aFlag].
	(self permissionsFlagForClass: FileGroupWritePermissionFlag) ifNotNil: [:aFlag | self removePermissionFlag: aFlag].
	self addPermissionFlag: FileGroupAllPermissionFlag new
%

category: 'permissions'
method: FileOptions
groupExecute
	(self hasPermissionsFlag: FileGroupAllPermissionFlag) ifTrue: [^self].
	self addPermissionFlag: FileGroupExecutePermissionFlag new
%

category: 'permissions'
method: FileOptions
groupRead
	(self hasPermissionsFlag: FileGroupAllPermissionFlag) ifTrue: [^self].
	self addPermissionFlag: FileGroupReadPermissionFlag new
%

category: 'permissions'
method: FileOptions
groupWrite
	(self hasPermissionsFlag: FileGroupAllPermissionFlag) ifTrue: [^self].
	self addPermissionFlag: FileGroupWritePermissionFlag new
%

category: 'file type'
method: FileOptions
gzipHighCompression
	self fileType: FileGzipTypeFlag high
%

category: 'file type'
method: FileOptions
gzipLowCompression
	self fileType: FileGzipTypeFlag low
%

category: 'testing'
method: FileOptions
hasExclsiveFlag
	^self hasPosixFlag: FileExclusiveFlag
%

category: 'testing'
method: FileOptions
hasFileAppendFlag
	^self hasPosixFlag: FileAppendFlag
%

category: 'testing'
method: FileOptions
hasFileCreateFlag
	^self hasPosixFlag: FileCreateFlag
%

category: 'testing'
method: FileOptions
hasFileTruncateFlag
	^self hasPosixFlag: FileTruncateFlag
%

category: 'testing'
method: FileOptions
hasNoReadOrWriteMode
	^self openModeFlag isNil
%

category: 'testing'
method: FileOptions
hasPermissionsFlag: anPosixFlagClass
	^(self permissionsFlagForClass: anPosixFlagClass) notNil
%

category: 'testing'
method: FileOptions
hasPosixFlag: anPosixFlagClass
	^(self posixFlagForClass: anPosixFlagClass) notNil
%

category: 'initialization'
method: FileOptions
initialize
	self posixFlags: OrderedCollection new.
	self permissionFlags: OrderedCollection new.
	self beShared
%

category: 'initialization'
method: FileOptions
initializeAppend
	self beWriteMode.
	self addCreateFlag.
	self addAppendFlag
%

category: 'initialization'
method: FileOptions
initializeRead
	self beReadMode
%

category: 'initialization'
method: FileOptions
initializeReadWrite
	self beReadWriteMode
%

category: 'initialization'
method: FileOptions
initializeWrite
	self beWriteMode.
	self addCreateFlag.
	self addTruncateFlag
%

category: 'testing'
method: FileOptions
isForeignFileSystem
	^self store isForeignFileSystem
%

category: 'testing'
method: FileOptions
isGzipped
	^(self fileType  ifNil: [^false]) isGzipped
%

category: 'testing'
method: FileOptions
isReadable
	^self openModeFlag isReadable
%

category: 'testing'
method: FileOptions
isReadWritable
	^self openModeFlag isReadable and: [self openModeFlag isWritable]
%

category: 'testing'
method: FileOptions
isWritable
	^self openModeFlag isWritable
%

category: 'printing'
method: FileOptions
mode
	| aStream |
	aStream := WriteStream on: String new.
	self printOpenModeStringOn: aStream.
	^aStream contents
%

category: 'accessing'
method: FileOptions
openModeFlag
	^openModeFlag
%

category: 'accessing'
method: FileOptions
openModeFlag: aFileOpenMode
	aFileOpenMode parent: self.
	openModeFlag :=  aFileOpenMode
%

category: 'permissions'
method: FileOptions
otherAll
	(self permissionsFlagForClass: FileOtherExecutePermissionFlag) ifNotNil: [:aFlag | self removePermissionFlag: aFlag].
	(self permissionsFlagForClass: FileOtherReadPermissionFlag) ifNotNil: [:aFlag | self removePermissionFlag: aFlag].
	(self permissionsFlagForClass: FileOtherWritePermissionFlag) ifNotNil: [:aFlag | self removePermissionFlag: aFlag].
	self addPermissionFlag: FileOtherAllPermissionFlag new
%

category: 'permissions'
method: FileOptions
otherExecute
	(self hasPermissionsFlag: FileOtherAllPermissionFlag) ifTrue: [^self].
	self addPermissionFlag: FileOtherExecutePermissionFlag new
%

category: 'permissions'
method: FileOptions
otherRead
	(self hasPermissionsFlag: FileOtherAllPermissionFlag) ifTrue: [^self].
	self addPermissionFlag: FileOtherReadPermissionFlag new
%

category: 'permissions'
method: FileOptions
otherWrite
	(self hasPermissionsFlag: FileOtherAllPermissionFlag) ifTrue: [^self].
	self addPermissionFlag: FileOtherWritePermissionFlag new
%

category: 'permissions'
method: FileOptions
ownerAll
	(self permissionsFlagForClass: FileOwnerExecutePermissionFlag) ifNotNil: [:aFlag | self removePermissionFlag: aFlag].
	(self permissionsFlagForClass: FileOwnerReadPermissionFlag) ifNotNil: [:aFlag | self removePermissionFlag: aFlag].
	(self permissionsFlagForClass: FileOwnerWritePermissionFlag) ifNotNil: [:aFlag | self removePermissionFlag: aFlag].
	self addPermissionFlag: FileOwnerAllPermissionFlag new
%

category: 'permissions'
method: FileOptions
ownerExecute
	(self hasPermissionsFlag: FileOwnerAllPermissionFlag) ifTrue: [^self].
	self addPermissionFlag: FileOwnerExecutePermissionFlag new
%

category: 'permissions'
method: FileOptions
ownerRead
	(self hasPermissionsFlag: FileOwnerAllPermissionFlag) ifTrue: [^self].
	self addPermissionFlag: FileOwnerReadPermissionFlag new
%

category: 'permissions'
method: FileOptions
ownerWrite
	(self hasPermissionsFlag: FileOwnerAllPermissionFlag) ifTrue: [^self].
	self addPermissionFlag: FileOwnerWritePermissionFlag new
%

category: 'accessing'
method: FileOptions
parent
	^parent
%

category: 'accessing'
method: FileOptions
parent: anObject
	parent := anObject
%

category: 'accessing'
method: FileOptions
permissionFlags
	^permissionFlags
%

category: 'accessing'
method: FileOptions
permissionFlags: aCollection
	permissionFlags := aCollection
%

category: 'permissions'
method: FileOptions
permissionsFlagForClass: anPermissionFlagClass
	^self permissionFlags detect: [:ea | ea isKindOf: anPermissionFlagClass] ifNone: [nil]
%

category: 'posix flags'
method: FileOptions
posixFlagForClass: anPosixFlagClass
	^self posixFlags detect: [:ea | ea isKindOf: anPosixFlagClass] ifNone: [nil]
%

category: 'accessing'
method: FileOptions
posixFlags
	^posixFlags
%

category: 'accessing'
method: FileOptions
posixFlags: aCollection
	posixFlags := aCollection
%

category: 'posix flags'
method: FileOptions
posixFlagsTotalValue
	^self posixFlags inject: 0 into: [:total :ea | total bitOr: ea posixFlagValue]
%

category: 'printing'
method: FileOptions
printOn: aStream
	super printOn: aStream.
	aStream
		nextPutAll: '(';
		nextPutAll: (self openModeFlag ifNil: ['nil'] ifNotNil: [:f | f posixName ifNil: ['nil']]);
		nextPutAll: ')'
%

category: 'printing'
method: FileOptions
printOpenModeStringOn: aStream

	self openModeFlag printOpenModeStringOn: aStream.
	self fileType ifNotNil: [:ft | ft printOpenModeStringOn: aStream.]
%

category: 'commands'
method: FileOptions
read
	self hasNoReadOrWriteMode ifTrue: [^self initializeRead].
	self isWritable ifTrue: [self beReadWriteMode]
%

category: 'commands'
method: FileOptions
readWrite
	self initializeReadWrite
%

category: 'posix flags - adding and removing'
method: FileOptions
removeAppendFlag
	self appendFlag ifNotNil: [:flag | self removePosixFlag: flag]
%

category: 'permissions'
method: FileOptions
removePermissionFlag: aPermissionFlag
	aPermissionFlag parent: nil.
	self permissionFlags remove: aPermissionFlag ifAbsent: []
%

category: 'posix flags - adding and removing'
method: FileOptions
removePosixFlag: aPosixFlag
	aPosixFlag parent: nil.
	self posixFlags remove: aPosixFlag ifAbsent: []
%

category: 'posix flags - adding and removing'
method: FileOptions
removeTruncateFlag
	self truncateFlag ifNotNil: [:flag | self removePosixFlag: flag]
%

category: 'accessing'
method: FileOptions
share
	^share
%

category: 'accessing'
method: FileOptions
share: aFileShareFlag
	aFileShareFlag parent: self.
	share := aFileShareFlag
%

category: 'convenience'
method: FileOptions
shareRule
	^self share shareRule
%

category: 'errors'
method: FileOptions
signalOpenModeFlagNotDefined

	self error: 'Attempting to add an open flag without first defining the open mode of #read, #write or #readWrite'
%

category: 'accessing'
method: FileOptions
store
	^store ifNil: [
		self parent isNil ifTrue: [^FileSystem disk store].
		^self parent store]
%

category: 'accessing'
method: FileOptions
store: aDiskStore
	store := aDiskStore
%

category: 'file type'
method: FileOptions
textFileType
	self fileType: FileTextTypeFlag new
%

category: 'commands'
method: FileOptions
truncate
	self hasNoReadOrWriteMode ifTrue: [^self signalOpenModeFlagNotDefined].
	self addTruncateFlag
%

category: 'posix flags'
method: FileOptions
truncateFlag
	^self posixFlagForClass: FileTruncateFlag
%

category: 'convenience'
method: FileOptions
truncateRule
	^self truncateFlag
		ifNil: [self store class noTruncateRule]
		ifNotNil: [:option | option truncateRule]
%

category: 'commands'
method: FileOptions
write
	self hasNoReadOrWriteMode ifTrue: [^self initializeWrite].
	self isReadable ifTrue: [self beReadWriteMode]
%

! Class implementation for 'FileSystem'

!		Class methods for 'FileSystem'

category: 'instance creation'
classmethod: FileSystem
store: aStore
	^ self basicNew
		initializeWithStore: aStore;
		yourself
%

!		Instance methods for 'FileSystem'

category: 'navigating'
method: FileSystem
* anObject
	"Return a relative reference."
	
	^ self referenceTo:( Path * anObject)
%

category: 'navigating'
method: FileSystem
/ anObject
	"Return the absolute reference obtained by resolving anObject against the
	root of this filesystem."
	
	^ self root / anObject
%

category: 'comparing'
method: FileSystem
= other
	^ self species = other species and: [self store = other store]
%

category: 'public'
method: FileSystem
binaryReadStreamOn: aResolvable
	"Resolve the argument into an absolute path and open a file handle on the file
	at that path. Ask the handle to give us a read stream for reading the file."

	^ (self open: aResolvable options: FileOptions newRead) binaryStream.
%

category: 'public'
method: FileSystem
binaryWriteStreamOn: aResolvable
	"Resolve the argument into an absolute path and open a file handle on the file
	at that path. Ask the handle to give us a binary write stream for reading the file."

	^ (self open: aResolvable options: FileOptions newWrite) binaryStream.
%

category: 'public'
method: FileSystem
checkName: aString fixErrors: fixErrors
	^ store checkName: aString fixErrors: fixErrors
%

category: 'public-enumerating'
method: FileSystem
childNamesAt: aResolvable
	^ Array streamContents: [ :out | 
		self childNamesAt: aResolvable do: [ :path|
			out nextPut: path ]].
%

category: 'public-enumerating'
method: FileSystem
childNamesAt: aResolvable do: aBlock
	| path |
	path := self resolve: aResolvable.
	store
		directoryAt: path
		ifAbsent: [ store signalDirectoryDoesNotExist: path ]
		nodesDo: [ :entry | 
			aBlock value: (store basenameFromEntry: entry) ]
%

category: 'public-enumerating'
method: FileSystem
childrenAt: aResolvable
	^ Array streamContents: [ :out | 
		self childrenAt: aResolvable do: [ :path|
			out nextPut: path ]].
%

category: 'public-enumerating'
method: FileSystem
childrenAt: aResolvable do: aBlock
	| path |
	path := self resolve: aResolvable.
	store
		directoryAt: path
		ifAbsent: [ store signalDirectoryDoesNotExist: path ]
		nodesDo: [ :entry | 
			aBlock value: path / (store basenameFromEntry: entry) ]
%

category: 'public'
method: FileSystem
close
	store close
%

category: 'public'
method: FileSystem
copy: sourcePath ifAbsent: absentBlock to: destinationPath ifPresent: presentBlock
	"Copy the file referenced as sourcePath to the destination referred as destPath. Perform associate actions in case of problems."

	store
		copy: (self resolve: sourcePath)
		ifAbsent: absentBlock
		to: (self resolve: destinationPath)
		ifPresent: presentBlock
		fileSystem: self
%

category: 'public'
method: FileSystem
copy: sourcePath to: destPath
	"Copy the file referenced as sourcePath to the destination referred as destPath.  
	If there is no file at sourcePath, raise FileDoesNotExist.
	If destPath is a file, raise FileExists."
	
	self
		copy: sourcePath
		ifAbsent: [ store signalFileDoesNotExist: sourcePath ]
		to: destPath
		ifPresent: [ store signalFileExists: destPath ]
%

category: 'public'
method: FileSystem
copy: aPath toReference: destinationReference

	^self = destinationReference fileSystem
		ifTrue: [ self copy: aPath to: destinationReference resolve path ]
		ifFalse: [ self copy: aPath toRemote: destinationReference ]
%

category: 'public'
method: FileSystem
copy: aPath toRemote: destRef
	| inputStream path |
	path := self resolve: aPath.
	[ inputStream := self binaryReadStreamOn: path.
	inputStream ifNil: [ store signalFileDoesNotExist: path ].
	destRef fileSystem copyFrom: inputStream to: destRef path ]
		ensure: [ inputStream ifNotNil: [ inputStream close ] ]
%

category: 'public'
method: FileSystem
copyAndDelete: sourcePath to: destination
	"Copy the file referenced as sourcePath to the destination referred as destPath.  
	If there is no file at sourcePath, raise FileDoesNotExist.
	If destPath is a file, raise FileExists.
	If an error occurs during the operation, try and roll back to the original state."
	
	[
		self copy: sourcePath toReference: destination.
		self delete: sourcePath.
	] on: Error do: [ :error |
		destination delete.
		error signal.
	]
%

category: 'private'
method: FileSystem
copyFrom: inputStream to: destPath
	| buffer out |
	out := nil.
	(self exists: destPath)
		ifTrue: [ store signalFileExists: destPath ].
	^ [ 
	out := self binaryWriteStreamOn: destPath.
	buffer := ByteArray new: 1024.
	[ inputStream atEnd ]
		whileFalse: [ 
			buffer := inputStream nextInto: buffer.
			out nextPutAll: buffer ]] ensure: [ out ifNotNil: [ out close ]]
%

category: 'public'
method: FileSystem
createDirectory: aResolvable
	"Resolve aResolvable into an absolute path, then as the store to create a directory there. 
	The store is expected to raise an exception if it cannot do so."

	^ store createDirectory: (self resolve: aResolvable)
%

category: 'public'
method: FileSystem
creationTimeOf: aResolvable
	"Returns the creation date of aResolvable"

	^ store creationTimeOf: (self resolve: aResolvable)
%

category: 'public'
method: FileSystem
delete: aResolvable
	store delete: (self resolve: aResolvable)
%

category: 'public'
method: FileSystem
delimiter
	"Return path delimiter used by this filesystem."

	^ store delimiter
%

category: 'public-enumerating'
method: FileSystem
directoriesAt: aResolvable
	^ Array streamContents: [ :out | 
		self directoriesAt: aResolvable do: [ :path|
			out nextPut: path ]].
%

category: 'public-enumerating'
method: FileSystem
directoriesAt: aResolvable do: aBlock
	| path |
	path := self resolve: aResolvable.
	store
		directoryAt: path
		ifAbsent: [ store signalDirectoryDoesNotExist: path ]
		directoryNodesDo: [ :entry | 
			aBlock value: path / (store basenameFromEntry: entry) ]
%

category: 'public-enumerating'
method: FileSystem
directoryNamesAt: aResolvable
	^ Array streamContents: [ :out | 
		self directoryNamesAt: aResolvable do: [ :name|
			out nextPut: name ]].
%

category: 'public-enumerating'
method: FileSystem
directoryNamesAt: aResolvable do: aBlock
	| path |
	path := self resolve: aResolvable.
	store
		directoryAt: path
		ifAbsent: [ store signalDirectoryDoesNotExist: path ]
		directoryNodesDo: [ :entry | 
			aBlock value: (store basenameFromEntry: entry) ]
%

category: 'public'
method: FileSystem
ensureCreateDirectory: aResolvable
	"Resolve the argument to an absolute path, then ask the store to make
	sure that all the directories contained in the argument path exist or are created."
	
	store ensureCreateDirectory: (self resolve: aResolvable)
%

category: 'public-enumerating'
method: FileSystem
entriesAt: aResolvable
	^ Array
		streamContents: [ :out | 
			self entriesAt: aResolvable do: [ :entry | out nextPut: entry ]
		]
%

category: 'public-enumerating'
method: FileSystem
entriesAt: aResolvable do: aBlock
	^ self 
		entriesAt: aResolvable 
		ifAbsent: [ store signalDirectoryDoesNotExist: ( self resolve: aResolvable) ]
		do: aBlock 
%

category: 'public-enumerating'
method: FileSystem
entriesAt: aResolvable ifAbsent: absentBlock do: aBlock
	| path entry aFilesystem |
	path := self resolve: aResolvable.
	aFilesystem := self.
	store
		directoryAt: path
		ifAbsent: [ ^ absentBlock value ]
		nodesDo: [ :node | 
			entry := store entryFromNode: node path: path for: aFilesystem.
			aBlock value: entry ]
%

category: 'public'
method: FileSystem
entryAt: aResolvable
	| path |
	path := self resolve: aResolvable.
	^ store
		nodeAt: path
		ifPresent: [ :node | store entryFromNode: node fileSystem: self path: path ]
		ifAbsent: [ store signalFileDoesNotExist: path ]
%

category: 'public'
method: FileSystem
exists: aResolvable
	"Resolve the argument, and answer true if the there is
	a file or directory at that path, false if there is not."
	
	^ store exists: (self resolve: aResolvable)
%

category: 'public'
method: FileSystem
extensionDelimiter
	^ $.
%

category: 'public-enumerating'
method: FileSystem
fileNamesAt: aResolvable
	^ Array streamContents: [ :out | 
		self fileNamesAt: aResolvable do: [ :path|
			out nextPut: path ]].
%

category: 'public-enumerating'
method: FileSystem
fileNamesAt: aResolvable do: aBlock
	| path |
	path := self resolve: aResolvable.
	store
		directoryAt: path
		ifAbsent: [ store signalDirectoryDoesNotExist: path ]
		fileNodesDo: [ :entry | 
			aBlock value: (store basenameFromEntry: entry) ]
%

category: 'public-enumerating'
method: FileSystem
filesAt: aResolvable
	^ Array streamContents: [ :out | 
		self filesAt: aResolvable do: [ :path|
			out nextPut: path ]].
%

category: 'public-enumerating'
method: FileSystem
filesAt: aResolvable do: aBlock
	| path |
	path := self resolve: aResolvable.
	store
		directoryAt: path
		ifAbsent: [ store signalDirectoryDoesNotExist: path ]
		fileNodesDo: [ :entry | 
			aBlock value: path / (store basenameFromEntry: entry) ]
%

category: 'printing'
method: FileSystem
forReferencePrintOn: aStream
	store forReferencePrintOn: aStream
%

category: 'public-testing'
method: FileSystem
hasChildren: aResolvable
	"Returns whether aResolvable has children."
	store directoryAt: (self resolve: aResolvable)
			ifAbsent: [ ^false ]
			nodesDo:  [ :node | ^true ].
	^false
%

category: 'public-testing'
method: FileSystem
hasDirectories: aResolvable
	self
		entriesAt: aResolvable
		ifAbsent: [  ^ false ]
		do: [ :entry | 
			entry isDirectory ifTrue: [ ^true ] ].
	^false
%

category: 'public-testing'
method: FileSystem
hasFiles: aResolvable
	self
		entriesAt: aResolvable
		ifAbsent: [  ^ false ]
		do: [ :entry | entry isFile ifTrue: [ ^true ] ].
	^false
%

category: 'comparing'
method: FileSystem
hash
	^ store hash
%

category: 'initialize-release'
method: FileSystem
initializeWithStore: aStore
	store := aStore
%

category: 'public-testing'
method: FileSystem
isCaseSensitive
	^ self store isCaseSensitive
%

category: 'public-testing'
method: FileSystem
isDirectory: aResolvable
	"Resolve the argument, and answer true if the result refers
	to a directory, false if it refers to a file or doesn't exist."

	^ store isDirectory: (self resolve: aResolvable)
%

category: 'public-testing'
method: FileSystem
isFile: aResolvable
	"Resolve the argument, and answer true if the result refers
	to a file, false if it refers to a directory or doesn't exist."

	^ store isFile: (self resolve: aResolvable)
%

category: 'public-testing'
method: FileSystem
isForeignFileSystem

	^self store ifNil: [false] ifNotNil: [self store isForeignFileSystem]
%

category: 'public-testing'
method: FileSystem
isReadable: aResolvable
	"Resolve the argument, and answer true if the there is
	a file or directory that can be read from."
	
	^ (self permissions: aResolvable) isReadable
%

category: 'public-testing'
method: FileSystem
isSymlink: aResolvable
	"Resolve the argument, and answer true if the result refers
	to a directory, false if it refers to a file or doesn't exist."

	^ store isSymlink: (self resolve: aResolvable)
%

category: 'public-testing'
method: FileSystem
isWritable: aResolvable
	"Resolve the argument, and answer true if the there is
	a file that can be written to or directory that can be changed."
	
	^  (self permissions: aResolvable) isWritable
%

category: 'public'
method: FileSystem
modificationTime: aResolvable
	"Returns the date of the last modification of aResolvable"

	^ store modificationTimeOf: (self resolve: aResolvable)
%

category: 'public'
method: FileSystem
modificationTimeOf: aResolvable
	"Returns the last date of modification of aResolvable"

	^ store modificationTimeOf: (self resolve: aResolvable)
%

category: 'public'
method: FileSystem
move: sourcePath to: destination
	"Move the file /directory referenced as sourcePath to the destination referred as destPath.  
	If there is no file at sourcePath, raise FileDoesNotExist.
	If destPath is a file, raise FileExists.
	If destPath is a directory, move the sourcePath in to the directory"

	| fullDestination |

	destination isFile ifTrue: [ FileExists signalWith: destination ].
	destination isDirectory
		ifTrue: [ fullDestination := destination / sourcePath basename ]
		ifFalse: [ fullDestination := destination ].
	self = destination fileSystem ifTrue: 
	[
		"Ideally we would test whether the source and destination are on the same filesystem from the OSs perspective.
		Since we can't do that, just try rename, and if that fails, copy and delete."
		[ self rename: sourcePath to: fullDestination resolve path ]
			on: Error
			do: [ :error | self copyAndDelete: sourcePath to: fullDestination ].
	] ifFalse:
		[ self copyAndDelete: sourcePath to: fullDestination ].
	^fullDestination
%

category: 'public'
method: FileSystem
open
	"Some kinds of filesystems need to open connections to external resources. Does nothing by default."
	
	store open
%

category: 'public'
method: FileSystem
open: aResolvable options: aFileOptions 
	"Resolve aResolvable into an absolute path, then ask the store to open the file at
	that path using the specified access mode."
	
	| path |
	aFileOptions alignStoreWith: self.
	path := self resolve: aResolvable.
	^ store handleClass 
		open: (FileReference fileSystem: self path: path) 
		options: aFileOptions
%

category: 'public'
method: FileSystem
open: aReference withOptions: aFileOptions 
	"Resolve aResolvable into an absolute path, then ask the store to open the file at
	that path using the specified access mode."

	"This returns an instance of the dialects File or FileAdaptor class - see FileSystem class>>fileClass"
	
	| path |
	path := self resolve: aReference path.
	^ store handleClass 
		open: (FileReference fileSystem: self path: path) 
		withOptions: aFileOptions
%

category: 'private'
method: FileSystem
openStreamDescription: aResolvable options: aFileOptions
	"I am  a helper method to delegate basicOpen:options: to the store.
	 I am called from FileSystemHandle implementations."

	| path |
	
	path := self resolve: aResolvable.
	^ store basicOpen: path options: aFileOptions
%

category: 'converting'
method: FileSystem
pathFromObject: anObject 
	^ anObject asPathWith: self
%

category: 'converting'
method: FileSystem
pathFromString: aString
	^store pathFromString: aString
%

category: 'public'
method: FileSystem
permissions: aResolvable
	"Resolve the argument and return the Permissions for this file or directory "

	^ store permissions: (self resolve: aResolvable)
%

category: 'printing'
method: FileSystem
printOn: aStream
	super printOn: aStream.
	aStream 
		nextPutAll: '('; 
		nextPutAll: (self store ifNil: ['nil'] ifNotNil: [self store printString]); 
		nextPutAll: ')'
%

category: 'printing'
method: FileSystem
printPath: aPath on: aStream
	store printPath: aPath on: aStream
%

category: 'public'
method: FileSystem
readStreamOn: aResolvable
	"Resolve the argument into an absolute path and open a file handle on the file
	at that path. Ask the handle to give us a read stream for reading the file."

	^ (self open: aResolvable options: FileOptions newRead) readStream.
%

category: 'public'
method: FileSystem
referenceTo: aResolvable 
	"Answer a reference to the argument from the context of the receiver filesystem.  	
		Example: Filesystem disk referenceTo: 'plonk.taz'"

	^ FileReference 
		fileSystem: self
		path: (self pathFromObject: aResolvable)
%

category: 'public'
method: FileSystem
rename: sourcePath ifAbsent: aBlock to: destPath ifPresent: pBlock
	"Rename the file referenced as sourcePath to the destination referred as destPath. 
	Perform associate actions in case of problems."
	
	| source destination |
	source := self resolve: sourcePath.
	destination := self resolve: destPath.
	store
		rename: source
		ifAbsent: aBlock
		to: destination
		ifPresent: pBlock
		fileSystem: self.
	^ destination
%

category: 'public'
method: FileSystem
rename: sourcePath to: destName
	"Rename the file referenced as sourcePath to destPath.  Raise exceptions 
	FileExists or FileDoesNotExist  if the operation fails"
	
	^ self
		rename: sourcePath
		ifAbsent: [store signalFileDoesNotExist: sourcePath]
		to: destName
		ifPresent: [store signalFileExists: destName]
%

category: 'public'
method: FileSystem
resolve: aResolvable
	^ aResolvable asResolvedBy: self
%

category: 'navigating'
method: FileSystem
resolvePath: aPath
	"Return a path where the argument is resolved in the context of the
	receiver. The behavior is similar to the one of a command line.
		> cd /a/b/c
		> cd b
		The shell will attempt to make /a/b/c/b the current directory. "

	^ self workingDirectoryPath resolve: aPath
%

category: 'navigating'
method: FileSystem
resolveReference: aReference
	^ aReference fileSystem = self ifTrue:
		[self workingDirectoryPath resolvePath: aReference path]
%

category: 'navigating'
method: FileSystem
resolveString: aString
	"Returns the root of the receiver filesystem, i.e. / on unix"
	
	^ self workingDirectoryPath resolvePath: (store pathFromString: aString)
%

category: 'accessing'
method: FileSystem
root
	"Returns the root of the receiver filesystem, i.e. / on unix"
	^ self referenceTo: Path root
%

category: 'accessing'
method: FileSystem
separator
	"Return path separator used by this filesystem."

	^ store separator
%

category: 'public'
method: FileSystem
sizeOf: aResolvable
	"Resolve the argument and return the size for this file or directory "

	^ store sizeOf: (self resolve: aResolvable)
%

category: 'accessing'
method: FileSystem
store
	^ store
%

category: 'converting'
method: FileSystem
stringFromPath: aPath
	^ store stringFromPath: aPath
%

category: 'accessing'
method: FileSystem
workingDirectory
	"Returns a reference to the directory from where the image was launched"

	^ self referenceTo: self workingDirectoryPath
%

category: 'accessing'
method: FileSystem
workingDirectoryPath
	^ store defaultWorkingDirectory
%

category: 'public'
method: FileSystem
writeStreamOn: aResolvable
	"Open a write stream on the file referred by the argument. It can be a string or a path"

	^ (self open: aResolvable options: FileOptions newWrite) writeStream.
%

! Class implementation for 'FileSystemDirectoryEntry'

!		Class methods for 'FileSystemDirectoryEntry'

category: 'accessing'
classmethod: FileSystemDirectoryEntry
allPosixPermissions 
	^8r777
%

category: 'instance creation'
classmethod: FileSystemDirectoryEntry
fileSystem: aFilesystem path: aPath creation: cTime modification: mTime isDir: aBoolean size: anInteger posixPermissions: posixNumber isSymlink: symlinkBooleam
	"Create a directory entry given a filesystem and a path in such filesystem. In addition, the creation and modification time are required as well as a boolean that indicates whether the entry is a folder or a file and its size."

	^ self 
		reference: (aFilesystem referenceTo: aPath)
		creation: cTime
		modification: mTime
		isDir: aBoolean
		size: anInteger
		posixPermissions: posixNumber
		isSymlink: symlinkBooleam
%

category: 'instance creation'
classmethod: FileSystemDirectoryEntry
reference: ref creation: cTime modification: mTime isDir: aBoolean size: anInteger posixPermissions: posixNumber isSymlink: symlinkBoolean
	"Create a directory entry for the file reference ref, with the creation time, cTime, the modification time, mTime. aBoolean indicates if the entry represents a directory or a file of size given by anInteger"
	
	^ self basicNew
		initializeWithRef: ref
		creation: cTime
		modification: mTime
		isDir: aBoolean
		size: anInteger
		posixPermissions: posixNumber
		isSymlink: symlinkBoolean
%

!		Instance methods for 'FileSystemDirectoryEntry'

category: 'converting'
method: FileSystemDirectoryEntry
asFileReference
	^ reference
%

category: 'accessing'
method: FileSystemDirectoryEntry
basename

	^ reference basename
%

category: 'accessing'
method: FileSystemDirectoryEntry
creation
	^ self creationTime 
%

category: 'accessing'
method: FileSystemDirectoryEntry
creationSeconds
	"Return the creation date and time of the entry receiver in seconds."
		
	^ creation asSeconds
%

category: 'accessing'
method: FileSystemDirectoryEntry
creationTime
	"Return the creation date and time of the entry receiver."
	
	^ creation
%

category: 'delegate'
method: FileSystemDirectoryEntry
extension
	^ reference extension
%

category: 'delegate'
method: FileSystemDirectoryEntry
fullName
	^ reference fullName
%

category: 'initialize-release'
method: FileSystemDirectoryEntry
initializeWithRef: ref 
	creation: cTime 
	modification: mTime 
	isDir: directoryBoolean
	size: bytes 
	posixPermissions: posixNumber 
	isSymlink: symlinkBoolean
	
	reference := ref.
	creation := cTime.
	modification := mTime.
	isDirectory := directoryBoolean.
	size := bytes.
	posixPermissions := posixNumber.
	isSymlink := symlinkBoolean.
%

category: 'testing'
method: FileSystemDirectoryEntry
isDirectory
	"Return whether the receiver is a directory"
	
	^ isDirectory
%

category: 'testing'
method: FileSystemDirectoryEntry
isFile
	"Return whether the receiver is a file"
	
	^ isDirectory not
%

category: 'testing'
method: FileSystemDirectoryEntry
isSymlink 
	^isSymlink
%

category: 'accessing'
method: FileSystemDirectoryEntry
modification
	^ self modificationTime 
%

category: 'accessing'
method: FileSystemDirectoryEntry
modificationSeconds
	"Return the modification date and time of the entry receiver in seconds."
	^ modification
%

category: 'accessing'
method: FileSystemDirectoryEntry
modificationTime
	"Return the modification date and time of the entry receiver."

	^  modification
%

category: 'accessing'
method: FileSystemDirectoryEntry
name
    ^ self basename
%

category: 'delegate'
method: FileSystemDirectoryEntry
pathSegments
	^ reference pathSegments
%

category: 'accessing'
method: FileSystemDirectoryEntry
permissions 
	^self posixPermissions
		ifNotNil: [ FileSystemPermission posixPermissions: self posixPermissions ]
%

category: 'accessing'
method: FileSystemDirectoryEntry
posixPermissions 
	^posixPermissions
%

category: 'printing'
method: FileSystemDirectoryEntry
printOn: aStream

	aStream nextPutAll: 'DirectoryEntry: '.
	reference ifNotNil: [:ref | aStream nextPutAll: reference printString].
%

category: 'delegate'
method: FileSystemDirectoryEntry
readStream
	^ reference readStream
%

category: 'delegate'
method: FileSystemDirectoryEntry
readStreamPortable
	^ reference readStreamPortable
%

category: 'accessing'
method: FileSystemDirectoryEntry
reference
	^ reference
%

category: 'accessing'
method: FileSystemDirectoryEntry
size
	"Returns the receiver size"
	^ size
%

category: 'delegate'
method: FileSystemDirectoryEntry
writeStream
	^ reference writeStream
%

! Class implementation for 'FileSystemGuide'

!		Class methods for 'FileSystemGuide'

category: 'instance creation'
classmethod: FileSystemGuide
for: aVisitor
	^ self basicNew initializeWithVisitor: aVisitor
%

category: 'instance creation'
classmethod: FileSystemGuide
show: aReference to: aVisitor
	^ (self for: aVisitor) show: aReference
%

category: 'instance creation'
classmethod: FileSystemGuide
show: aReference to: aVisitor selecting: aBlock
	^ (self for: aVisitor)
		selectChildren:  aBlock;
		show: aReference
%

!		Instance methods for 'FileSystemGuide'

category: 'initialization'
method: FileSystemGuide
initialize

	work := OrderedCollection new
%

category: 'initialize-release'
method: FileSystemGuide
initializeWithVisitor: aVisitor
	self initialize.
	visitor := aVisitor.
	
%

category: 'removing'
method: FileSystemGuide
pop
	^ work removeLast
%

category: 'adding'
method: FileSystemGuide
push: anObject
	work add: anObject
%

category: 'showing'
method: FileSystemGuide
pushAll: aCollection 
	aCollection do: [ :ea | self push: ea ]
%

category: 'accessing'
method: FileSystemGuide
selectChildren
	^ selectChildren
%

category: 'accessing'
method: FileSystemGuide
selectChildren: aBlock
	"With this block you can control how the guide spreads over directories.
	Example: 
		self selectChildren: [ :parentEntry | parentEntry isSymlink not ].
		This will prevent the outer visitor to see any children of symlinked directories.
		
	Since the guides essentially rearrange the files visited controlling which children
	you see is the main concern of the guide. All the other visiting aspects can be 
	controlled in the visitor."
	selectChildren := aBlock
%

category: 'testing'
method: FileSystemGuide
shouldVisitChildrenOf: anEntry
	^ selectChildren 
		ifNil: [ true ]
		ifNotNil: [ selectChildren cull: anEntry ]
%

category: 'showing'
method: FileSystemGuide
show: aReference
	self subclassResponsibility
%

category: 'accessing'
method: FileSystemGuide
top
	^ work removeFirst
%

category: 'showing'
method: FileSystemGuide
whileNotDoneDo: aBlock
	[ work isEmpty ] whileFalse: [ aBlock value ]
%

! Class implementation for 'BreadthFirstGuide'

!		Instance methods for 'BreadthFirstGuide'

category: 'showing'
method: BreadthFirstGuide
show: aReference 
	self push: aReference entry.
	self whileNotDoneDo: [ self visitNextEntry: self top ]
%

category: 'showing'
method: BreadthFirstGuide
visitNextEntry: entry
		
	entry isFile 
		ifTrue: [ ^ visitor visitFile: entry ].
	
	visitor visitDirectory: entry.
	
	(self shouldVisitChildrenOf: entry)
		ifTrue: [ self pushAll: entry reference entries ].
%

! Class implementation for 'PostorderGuide'

!		Instance methods for 'PostorderGuide'

category: 'showing'
method: PostorderGuide
pushTraverse: aReference 
	self push: (Message 
				selector: #traverse:
				argument: aReference)
%

category: 'showing'
method: PostorderGuide
pushVisit: aReference 
	self push: (Message 
				selector: #visit:
				argument: aReference)
%

category: 'showing'
method: PostorderGuide
show: aReference 
	self pushTraverse: aReference entry.
	self whileNotDoneDo: [ self pop sendTo: self ]
%

category: 'showing'
method: PostorderGuide
traverse: anEntry 
		
	self pushVisit: anEntry.
	
	anEntry isDirectory ifTrue: [ 
		(self shouldVisitChildrenOf: anEntry)
			ifFalse: [ ^ self ].
		anEntry reference entries reverseDo: [ :each | 
			self pushTraverse: each ]]
%

category: 'showing'
method: PostorderGuide
visit: anEntry
	anEntry isDirectory
		ifTrue: [ visitor visitDirectory: anEntry ]
		ifFalse: [ visitor visitFile: anEntry ]
%

! Class implementation for 'PreorderGuide'

!		Instance methods for 'PreorderGuide'

category: 'showing'
method: PreorderGuide
pushAll: aCollection 
	aCollection reverseDo: [ :each | work add: each ]
%

category: 'showing'
method: PreorderGuide
show: aReference
	self push: aReference entry.
	self whileNotDoneDo: [ self visitNextEntry: self pop ]
%

category: 'showing'
method: PreorderGuide
visitNextEntry: entry
	
	entry isFile
		ifTrue: [ visitor visitFile: entry ]
		ifFalse: [ 
			visitor visitDirectory: entry.
			(self shouldVisitChildrenOf: entry)
				ifTrue: [ self pushAll: entry reference entries ]]
%

! Class implementation for 'FileSystemHandle'

!		Class methods for 'FileSystemHandle'

category: 'instance creation'
classmethod: FileSystemHandle
on: aReference options: aFileOptions
	^ self new setReference: aReference options: aFileOptions
%

category: 'instance creation'
classmethod: FileSystemHandle
open: aReference options: aFileOptions
	^ (self on: aReference options: aFileOptions) open
%

category: 'instance creation'
classmethod: FileSystemHandle
open: aReference withOptions: aFileOptions
	^ (self on: aReference options: aFileOptions) open
%

!		Instance methods for 'FileSystemHandle'

category: 'public'
method: FileSystemHandle
at: index
	| buffer |
	buffer := ByteArray new: 1.
	self at: index read: buffer startingAt: 1 count: 1.
	^ buffer at: 1
%

category: 'public'
method: FileSystemHandle
at: index put: anObject
	| buffer |
	buffer := ByteArray with: (anObject isCharacter
		ifTrue: [ anObject codePoint ]
		ifFalse: [ anObject ]).
	self at: index write: buffer startingAt: 1 count: 1.
	
%

category: 'public'
method: FileSystemHandle
at: offset read: buffer startingAt: start count: count
	self subclassResponsibility
%

category: 'public'
method: FileSystemHandle
at: offset write: buffer startingAt: start count: count
	self subclassResponsibility
%

category: 'private'
method: FileSystemHandle
basicOpen
	"get the raw stream description from the filesystem's store"
	^ self fileSystem 
		openStreamDescription: reference path
		options: self options
%

category: 'streams'
method: FileSystemHandle
binaryReadStream
	
	self subclassResponsibility 
%

category: 'streams'
method: FileSystemHandle
binaryWriteStream
	
	self subclassResponsibility 
%

category: 'public'
method: FileSystemHandle
close
	self subclassResponsibility
%

category: 'public'
method: FileSystemHandle
ensureClosed
	reference exists ifTrue: [self close]
%

category: 'accessing'
method: FileSystemHandle
fileSystem
	^ reference fileSystem 
%

category: 'public'
method: FileSystemHandle
flush
	self subclassResponsibility
%

category: 'accessing'
method: FileSystemHandle
fullName
	^ reference fullName
%

category: 'testing'
method: FileSystemHandle
isOpen
	self subclassResponsibility
%

category: 'testing'
method: FileSystemHandle
isWritable

	^ self options isWritable
%

category: 'public'
method: FileSystemHandle
open
	self subclassResponsibility
%

category: 'accessing'
method: FileSystemHandle
options
	^options
%

category: 'accessing'
method: FileSystemHandle
options: aFileOptions
	options := aFileOptions
%

category: 'public'
method: FileSystemHandle
pathString

	^self reference pathString
%

category: 'accessing'
method: FileSystemHandle
reference
	^ reference
%

category: 'accessing'
method: FileSystemHandle
reference: aFileReference
	reference := aFileReference
%

category: 'public'
method: FileSystemHandle
reopen
	self close.
	self open
%

category: 'initialize-release'
method: FileSystemHandle
setReference: aReference options: aFileOptions
	self reference: aReference resolve.
	self options: aFileOptions
%

category: 'public'
method: FileSystemHandle
sync
	self subclassResponsibility
%

category: 'public'
method: FileSystemHandle
truncateTo: anInteger
	self subclassResponsibility
%

! Class implementation for 'FileHandle'

!		Class methods for 'FileHandle'

category: 'accessing'
classmethod: FileHandle
open: aFileReference withOptions: aFileOptions
	"This returns an instance of the dialects File or FileAdaptor class - see FileSystem class>>fileClass"
	^FileSystem fileClass open: aFileReference withOptions: aFileOptions
%

category: 'class initialization'
classmethod: FileHandle
startUp: resuming
	"This functionality is disabled for now, to avoid doing a lot of processing at
	image start up. To reenable, add this class to the start up list."
	
	resuming ifTrue: [self allInstancesDo: [:ea | ea startUp]]
%

!		Instance methods for 'FileHandle'

category: 'public'
method: FileHandle
assureOpen
	"Compatibility method to make the FileHandle Tests pass"
	self isOpen ifFalse: [ self id: self basicOpen ].
%

category: 'public'
method: FileHandle
at: index read: buffer startingAt: start count: count
	
	self flag: 'TODO: remove once FileHandles are really used!'.
	self assureOpen.
	
	^ self 
		setPositionTo: index - 1;
		readInto: buffer startingAt: start count: count
%

category: 'public'
method: FileHandle
at: index write: buffer startingAt: start count: count
	
	self flag: 'TODO: remove once FileHandles are really used!'.
	self assureOpen.
	
	self 
		setPositionTo: index - 1;
		writeFrom: buffer startingAt: start count: count
%

category: 'public'
method: FileHandle
binaryFileStream
	^self binaryStream
%

category: 'public'
method: FileHandle
binaryReadStream
	^self binaryStream
%

category: 'public'
method: FileHandle
binaryStream
	^BinaryFileStream on: (FileSystem fileClass open: reference withOptions: self options)
%

category: 'public'
method: FileHandle
binaryWriteStream
	^self binaryStream
%

category: 'public'
method: FileHandle
close
	self id ifNil: [ ^ self ].
	self id close.
	self id: nil
%

category: 'finalization'
method: FileHandle
finalize
	self close
%

category: 'public'
method: FileHandle
flush
	
	self flag: 'TODO: remove once FileHandles are really used!'.
	self assureOpen.
	
	self id flush
%

category: 'accessing'
method: FileHandle
id
	^id
%

category: 'accessing'
method: FileHandle
id: anObject
	id := anObject
%

category: 'testing'
method: FileHandle
isOpen
	^self id ifNil: [false] ifNotNil: [self id isOpen] 
%

category: 'public'
method: FileHandle
open
	self flag: 'TODO: for now we solely rely on the old FileStreams'.
"	self id: self basicOpen.
	self id ifNil: 	[self signalOpenError]"
%

category: 'public'
method: FileHandle
readInto: byteArray startingAt: startIndex count: count
	"Read up to count bytes of data from this file into the given string or byte array starting at the given index. 
		Answer the number of bytes actually read."

	| buf cnt |
	buf := byteArray species new: count.
	cnt := self id next: count into: buf.
	cnt ifNil: [ 
		self id isNil
			ifTrue: [ ^ self error: 'File is closed' ].
		self error: 'File read failed: ' , (self id class serverErrorString ifNil: [ ' - No error reason found']) ].
	byteArray replaceFrom: startIndex to: startIndex + cnt - 1 with: buf.
	^cnt
%

category: 'public'
method: FileHandle
setPositionTo: anInteger
	"Set this file to the given position."

	| pos |
	pos := self id position: anInteger.
	pos ifNil: [ self error: 'position error' ].
%

category: 'public'
method: FileHandle
signalOpenError
	self reference exists ifFalse: [
		FileDoesNotExistException signalWith: self reference text: 'Attempting to open non existent file - ' , self reference fullName].
	self reference isDirectory ifTrue: [
		FileDoesNotExistException signalWith: self reference text: 'Attempting to open directory - ' , self reference fullName].
	FileOpenError 
		signalWith: self reference 
		text: ('Unable to open'  , self reference fullName)
%

category: 'public'
method: FileHandle
size
	
	self flag: 'TODO: remove once FileHandles are really used!'.
	self assureOpen.
	
	^ self id fileSize
%

category: 'private'
method: FileHandle
startUp
	"This functionality is disabled for now, to avoid doing lots of processing
	on start up."
	
	"We're starting up in a new OS process, so the file id will be invalid.
	Try to reopen the file, but fail silently: just leave the id as nil. #isOpen will
	answer false, and we'll raise an error if anyone tries to do IO."
	
	self basicOpen
%

category: 'public'
method: FileHandle
streamError
	self reference exists
		ifFalse: [FileDoesNotExistException signalWith: self reference].
	self error: 'Unable to open file ' , self reference printString
%

category: 'public'
method: FileHandle
sync
	
	self flag: 'TODO: remove once FileHandles are really used!'.
	self assureOpen.
	"This needs to be implemented in GsFile or this function removed"
	self id sync
%

category: 'public'
method: FileHandle
truncateTo: anInteger
	self id truncate: anInteger
%

category: 'public'
method: FileHandle
writeFrom: stringOrByteArray startingAt: startIndex count: count
	"Write count bytes onto this file from the given string or byte array starting at the given index. 	Answer the number of bytes written."

	| written |
	written := (startIndex = 1 and: [ count = stringOrByteArray size])
		ifTrue: [ self id nextPutAll: stringOrByteArray ]
		ifFalse: [ self id nextPutAll: (stringOrByteArray copyFrom: startIndex to: startIndex + count - 1) ].
	written ifNil: [ self error: 'failed write' ].
	^ written
%

! Class implementation for 'MemoryHandle'

!		Instance methods for 'MemoryHandle'

category: 'public'
method: MemoryHandle
at: index
	^ entry at: index
%

category: 'public'
method: MemoryHandle
at: index put: anObject
	^ entry at: index put: anObject
%

category: 'public'
method: MemoryHandle
at: index read: aCollection startingAt: start count: count 
	^ entry at: index read: aCollection startingAt: start count: count 
%

category: 'public'
method: MemoryHandle
at: first write: aCollection startingAt: start count: count 
	self isWritable ifFalse: [ self primitiveFailed ].
	entry at: first write: aCollection startingAt: start count: count.
%

category: 'streams'
method: MemoryHandle
binaryReadStream

	^ entry binaryReadStream
%

category: 'streams'
method: MemoryHandle
binaryStream
	^self options isWritable 
		ifTrue: [entry binaryWriteStream] 
		ifFalse: [entry binaryReadStream]
%

category: 'streams'
method: MemoryHandle
binaryWriteStream
	
	^ entry binaryWriteStream
%

category: 'public'
method: MemoryHandle
close
	self isOpen ifFalse: [ ^ self ].
	self truncate.
	entry := nil.
%

category: 'stream-protocol'
method: MemoryHandle
copyFrom: from to: position
	^ entry copyFrom: from to: position
%

category: 'public'
method: MemoryHandle
flush
	self truncate
%

category: 'private'
method: MemoryHandle
grow
	entry grow
%

category: 'stream-protocol'
method: MemoryHandle
grownBy: length
	entry grownBy: length
%

category: 'testing'
method: MemoryHandle
isOpen
	^ entry notNil
%

category: 'public'
method: MemoryHandle
open
	entry := self basicOpen.
%

category: 'accessing'
method: MemoryHandle
size
	"return the size for the interna"
	^ entry internalSize
%

category: 'public'
method: MemoryHandle
sync
	self flush
%

category: 'public'
method: MemoryHandle
truncate
	entry truncate
%

category: 'public'
method: MemoryHandle
truncateTo: anInteger
	entry truncateTo: anInteger
%

! Class implementation for 'FileSystemPermission'

!		Class methods for 'FileSystemPermission'

category: 'instance creation'
classmethod: FileSystemPermission
default
	^ self posixPermissions: 8r777
%

category: 'instance creation'
classmethod: FileSystemPermission
new 
	self error: 'Should not be called. Use #posixPermission: instead'
%

category: 'instance creation'
classmethod: FileSystemPermission
posixPermissions: aNumber
	^self basicNew 
		initialize: aNumber;
		yourself
%

!		Instance methods for 'FileSystemPermission'

category: 'comparing'
method: FileSystemPermission
< other
	^ posixPermission < other posixPermission
%

category: 'comparing'
method: FileSystemPermission
<= other
	^ (posixPermission > other posixPermission) not
%

category: 'comparing'
method: FileSystemPermission
> other
	^ other posixPermission < posixPermission
%

category: 'comparing'
method: FileSystemPermission
>= other
	^ other posixPermission <= posixPermission
%

category: 'accessing'
method: FileSystemPermission
groupExecute
	^ self permissionBitAt: 4
%

category: 'accessing'
method: FileSystemPermission
groupRead
	^ self permissionBitAt: 6
%

category: 'accessing'
method: FileSystemPermission
groupWrite
	^ self permissionBitAt: 5
%

category: 'initialization'
method: FileSystemPermission
initialize
%

category: 'initialization'
method: FileSystemPermission
initialize: aNumber
	posixPermission := aNumber.
	self initialize.	
%

category: 'testing'
method: FileSystemPermission
isReadable
	^ self ownerRead
%

category: 'testing'
method: FileSystemPermission
isWritable
	^ self ownerWrite
%

category: 'accessing'
method: FileSystemPermission
otherExecute
	^ self permissionBitAt: 1
%

category: 'accessing'
method: FileSystemPermission
otherRead
	^ self permissionBitAt: 3
%

category: 'accessing'
method: FileSystemPermission
otherWrite
	^ self permissionBitAt: 2
%

category: 'accessing'
method: FileSystemPermission
ownerExecute
	^ self permissionBitAt: 7
%

category: 'accessing'
method: FileSystemPermission
ownerRead
	^ self permissionBitAt: 9
%

category: 'accessing'
method: FileSystemPermission
ownerWrite
	^ self permissionBitAt: 8
%

category: 'accessing'
method: FileSystemPermission
permissionBitAt: bitIndex
	^ (posixPermission bitAt: bitIndex) == 1
%

category: 'private'
method: FileSystemPermission
posixPermission
	^ posixPermission
%

category: 'printing'
method: FileSystemPermission
printOn: aStream
	aStream 
		"Owner"
		nextPut: (self ownerRead ifTrue: [ $r ] ifFalse: [ $- ]);
		nextPut: (self ownerWrite ifTrue: [ $w ] ifFalse: [ $- ]);
		nextPut: (self ownerExecute ifTrue: [ $x ] ifFalse: [ $- ]);
		"Group"
		nextPut: (self groupRead ifTrue: [ $r ] ifFalse: [ $- ]);
		nextPut: (self groupWrite ifTrue: [ $w ] ifFalse: [ $- ]);
		nextPut: (self groupExecute ifTrue: [ $x ] ifFalse: [ $- ]);
		"Other"
		nextPut: (self otherRead ifTrue: [ $r ] ifFalse: [ $- ]);
		nextPut: (self otherWrite ifTrue: [ $w ] ifFalse: [ $- ]);
		nextPut: (self otherExecute ifTrue: [ $x ] ifFalse: [ $- ])
		
%

! Class implementation for 'FileSystemResolver'

!		Class methods for 'FileSystemResolver'

category: 'instance creation'
classmethod: FileSystemResolver
new

	^ self basicNew
		initialize;
		yourself
%

!		Instance methods for 'FileSystemResolver'

category: 'accessing'
method: FileSystemResolver
addResolver: aResolver
	next
		ifNil: [next := aResolver]
		ifNotNil: [next addResolver: aResolver]
%

category: 'resolving'
method: FileSystemResolver
canResolve: aSymbol
	^ self supportedOrigins includes: aSymbol
%

category: 'resolving'
method: FileSystemResolver
fileSystem
	^ FileSystem disk
%

category: 'accessing'
method: FileSystemResolver
flushCaches
	self flushLocalCache.
	next ifNotNil: [next flushCaches]
%

category: 'accessing'
method: FileSystemResolver
flushLocalCache
%

category: 'initialization'
method: FileSystemResolver
initialize
%

category: 'accessing'
method: FileSystemResolver
next
	^ next
%

category: 'resolving'
method: FileSystemResolver
resolve: aSymbol
	^ (self canResolve: aSymbol)
		ifTrue: [self perform: aSymbol]
		ifFalse: [self unknownOrigin: aSymbol]
%

category: 'resolving'
method: FileSystemResolver
resolveString: aString
	| decoded |
	"The argument string is actually a byte array encoded differently on each platform.
	We are transforming it to an image string.
	We assume for now that the string is utf8 encoded."
	decoded := aString decodeFromUTF8 asString.
	^ FileReference 
		fileSystem: self fileSystem 
		path: (self fileSystem pathFromString: decoded)
%

category: 'resolving'
method: FileSystemResolver
resolveString: aString fileSystem: aFileSystem
	| decoded fs |
	"The argument string is actually a byte array encoded differently on each platform.
	We are transforming it to an image string.
	We assume for now that the string is utf8 encoded."
	decoded := aString decodeFromUTF8 asString.
	^ FileReference 
		fileSystem: aFileSystem 
		path: (fs pathFromString: decoded)
%

category: 'resolving'
method: FileSystemResolver
supportedOrigins
	^ #()
%

category: 'resolving'
method: FileSystemResolver
unknownOrigin: aSymbol
	^ next ifNotNil: [next resolve: aSymbol]
%

! Class implementation for 'InteractiveResolver'

!		Instance methods for 'InteractiveResolver'

category: 'accessing'
method: InteractiveResolver
flushLocalCache
	cache := IdentityDictionary new
%

category: 'initialization'
method: InteractiveResolver
initialize
	self flushLocalCache
%

category: 'resolving'
method: InteractiveResolver
resolve: origin
	^ cache at: origin ifAbsent: [self unknownOrigin: origin]
	
%

category: 'resolving'
method: InteractiveResolver
unknownOrigin: origin
	| reference |
	^ (next ifNotNil: [next resolve: origin]) ifNil: 
			[reference := ResolutionRequest for: origin.
			reference ifNotNil: [cache at: origin put: reference]]
%

! Class implementation for 'PlatformResolver'

!		Class methods for 'PlatformResolver'

category: 'instance creation'
classmethod: PlatformResolver
forCurrentPlatform
	^(self resolverClassFor: System osName) new
%

category: 'accessing'
classmethod: PlatformResolver
platformNames
	^ {}
%

category: 'instance creation'
classmethod: PlatformResolver
resolverClassFor: anOsName
	^PlatformResolver allSubclasses detect: [:ea | ea platformNames includes: anOsName]
%

!		Instance methods for 'PlatformResolver'

category: 'origins'
method: PlatformResolver
cache
	"Operating Systems often define standard locations for a personal cache directory. The cache directory is a user-specific non-essential (cached) place where data should be written."
	self subclassResponsibility
%

category: 'private'
method: PlatformResolver
cantFindOriginError
	^ Error signal: 'Can''t find the requested origin' 
%

category: 'origins'
method: PlatformResolver
desktop
	^ self subclassResponsibility
%

category: 'private'
method: PlatformResolver
directoryFromEnvVariableNamed: aString
	^ self directoryFromEnvVariableNamed: aString or: [ self cantFindOriginError ]
%

category: 'private'
method: PlatformResolver
directoryFromEnvVariableNamed: aString or: aBlock
	| envValue |
	envValue := [ self osEnvironmentVariable: aString ]
		on: Error
		do: [ ^ aBlock value ].
	^ envValue isEmptyOrNil
		ifTrue: [ aBlock value ]
		ifFalse: [ self resolveString: envValue ].
%

category: 'origins'
method: PlatformResolver
documents
	^ self subclassResponsibility
%

category: 'origins'
method: PlatformResolver
home
	^ self directoryFromEnvVariableNamed: self homeEnvironmentVariableName
%

category: 'origins'
method: PlatformResolver
homeEnvironmentVariableName
	^ self subclassResponsibility
%

category: 'private'
method: PlatformResolver
osEnvironmentVariable: aString

	^System gemEnvironmentVariable: aString
%

category: 'origins'
method: PlatformResolver
preferences
	^ self subclassResponsibility
%

category: 'resolving'
method: PlatformResolver
supportedOrigins
	^ #(home desktop documents preferences cache temp)
%

category: 'origins'
method: PlatformResolver
temp
	"Where to put files that are not supposed to last long"
	^ self subclassResponsibility 
%

! Class implementation for 'ClientResolver'

!		Class methods for 'ClientResolver'

category: 'instance creation'
classmethod: ClientResolver
forCurrentPlatform
	^self new
		resolver: (self resolverClassFor: System clientOsName) new;
		yourself
%

!		Instance methods for 'ClientResolver'

category: 'origins'
method: ClientResolver
cache
	^ self resolver cache
%

category: 'origins'
method: ClientResolver
desktop
	^ self resolver desktop
%

category: 'origins'
method: ClientResolver
documents
	^ self resolver documents
%

category: 'resolving'
method: ClientResolver
fileSystem
	^ ClientStore currentFileSystem
%

category: 'origins'
method: ClientResolver
homeEnvironmentVariableName
	^ self resolver homeEnvironmentVariableName
%

category: 'private'
method: ClientResolver
osEnvironmentVariable: aString

	^System clientEnvironmentVariable: aString
%

category: 'origins'
method: ClientResolver
preferences
	^ self resolver preferences
%

category: 'accessing'
method: ClientResolver
resolver
	^resolver
%

category: 'accessing'
method: ClientResolver
resolver: aPlatformResolver
	resolver := aPlatformResolver
%

category: 'resolving'
method: ClientResolver
supportedOrigins
	^ self resolver supportedOrigins
%

category: 'origins'
method: ClientResolver
temp
	^ self resolver temp
%

! Class implementation for 'MacOSResolver'

!		Class methods for 'MacOSResolver'

category: 'accessing'
classmethod: MacOSResolver
platformNames
	^  {'Darwin'}
%

!		Instance methods for 'MacOSResolver'

category: 'origins'
method: MacOSResolver
cache
	^ self library / 'Caches'
%

category: 'origins'
method: MacOSResolver
desktop
	^ self home / 'Desktop'
%

category: 'origins'
method: MacOSResolver
documents
	^ self home / 'Documents'
%

category: 'origins'
method: MacOSResolver
homeEnvironmentVariableName
	^'HOME'
%

category: 'origins'
method: MacOSResolver
library
	^ self userLibrary
%

category: 'origins'
method: MacOSResolver
preferences 
	^ self home / 'Library' / 'Preferences'
%

category: 'resolving'
method: MacOSResolver
supportedOrigins
	^ super supportedOrigins , #(userApplicationSupport systemApplicationSupport systemLibrary userLibrary)
%

category: 'origins'
method: MacOSResolver
systemApplicationSupport
	^ self systemLibrary / 'Application Support'
%

category: 'origins'
method: MacOSResolver
systemLibrary
	^  FileSystem disk root / 'Library'
%

category: 'origins'
method: MacOSResolver
temp
	^ '/tmp' asFileReference
%

category: 'origins'
method: MacOSResolver
userApplicationSupport
	^self userLibrary / 'Application Support'
%

category: 'origins'
method: MacOSResolver
userLibrary
	^  self home / 'Library'
%

! Class implementation for 'UnixResolver'

!		Class methods for 'UnixResolver'

category: 'accessing'
classmethod: UnixResolver
platformNames
	^  {'Linux'}
%

!		Instance methods for 'UnixResolver'

category: 'origins'
method: UnixResolver
cache
	"http://standards.freedesktop.org/basedir-spec/basedir-spec-latest.html"

	^ self directoryFromEnvVariableNamed: 'XDG_CACHE_HOME' or: [ self home / '.cache' ]
%

category: 'origins'
method: UnixResolver
desktop
	^ (self xdgUserDir: 'DESKTOP') ifNil: [ self home / 'Desktop' ]
%

category: 'origins'
method: UnixResolver
documents
	^ (self xdgUserDir: 'DOCUMENTS') ifNil: [ self home / 'Documents' ]
%

category: 'origins'
method: UnixResolver
homeEnvironmentVariableName
	^'HOME'
%

category: 'origins'
method: UnixResolver
preferences
	"http://standards.freedesktop.org/basedir-spec/basedir-spec-latest.html"

	^ self directoryFromEnvVariableNamed: 'XDG_CONFIG_HOME' or: [ self home / '.config' ]
%

category: 'resolving'
method: UnixResolver
supportedOrigins
	^ super supportedOrigins , #( userData )
%

category: 'origins'
method: UnixResolver
temp
	^ '/tmp' asFileReference
%

category: 'origins'
method: UnixResolver
userData
	^ self directoryFromEnvVariableNamed: 'XDG_DATA_HOME' or: [ self home / '.local' / 'share' ]
%

category: 'helpers'
method: UnixResolver
xdgParseUserDirLine: aStream
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
%

category: 'helpers'
method: UnixResolver
xdgUserDir: userDirName
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
%

! Class implementation for 'WindowsResolver'

!		Class methods for 'WindowsResolver'

category: 'accessing'
classmethod: WindowsResolver
platformNames
	^ {'Win32' . 'Windows NT'}
%

!		Instance methods for 'WindowsResolver'

category: 'origins'
method: WindowsResolver
cache
	"Because Windows does not define any location for putting cache files, we chose to use the preference directory. http://libertyboy.free.fr/computing/reference/envariables/"
	^ self preferences
%

category: 'origins'
method: WindowsResolver
desktop
	^ self home / 'Desktop'
%

category: 'origins'
method: WindowsResolver
documents
	^ self home / 'Documents'
%

category: 'private'
method: WindowsResolver
homeEnvironmentVariableName
	^'USERPROFILE'
%

category: 'origins'
method: WindowsResolver
preferences
	^ self directoryFromEnvVariableNamed: 'APPDATA' or: [ self home ]
%

category: 'origins'
method: WindowsResolver
temp
	^ self directoryFromEnvVariableNamed: 'TEMP' or: [ FileLocator C / 'windows' / 'temp' ]
%

! Class implementation for 'SystemResolver'

!		Class methods for 'SystemResolver'

category: 'accessing'
classmethod: SystemResolver
defaultLocalDirectoryName
	^ 'pharo-local'
%

category: 'accessing'
classmethod: SystemResolver
userLocalDirectory
	^ UserLocalDirectory ifNil: [ 
		(FileLocator imageDirectory / self defaultLocalDirectoryName) resolve  ]
%

category: 'accessing'
classmethod: SystemResolver
userLocalDirectory: aFileReference
	UserLocalDirectory := aFileReference
%

!		Instance methods for 'SystemResolver'

category: 'origins'
method: SystemResolver
dbfScratchDir
	"Answer the path to the DBF_SCRATCH_DIR"

	^ self  resolveString: (System stoneConfigurationReport at: #DBF_SCRATCH_DIR)
%

category: 'origins'
method: SystemResolver
extent1

	^ self _extent: 1
%

category: 'origins'
method: SystemResolver
extent1Directory

	^ self extent1 parent
%

category: 'origins'
method: SystemResolver
gemLogDirectory
	"Answer the path to the gem log directory"

	^ self  resolveString: (System gemLogPath)
%

category: 'origins'
method: SystemResolver
image

	^ self dbfScratchDir
%

category: 'origins'
method: SystemResolver
imageDirectory

	^ self dbfScratchDir
%

category: 'origins'
method: SystemResolver
rowanProjectsHome

	"Answer the path to $ROWAN_PROJECTS_HOME"

	^ (System gemEnvironmentVariable: 'ROWAN_PROJECTS_HOME')
		ifNil: [ self error: '$ROWAN_PROJECTS_HOME not defined' ]
		ifNotNil: [:str | self  resolveString: str ]
%

category: 'resolving'
method: SystemResolver
supportedOrigins
	^ #(image imageDirectory extent1 extent1Directory tranlog dbfScratchDir workingDirectory rowanProjectsHome )
%

category: 'origins'
method: SystemResolver
tranlog
	"Answer the path to the current tranlog"

	^ self  resolveString: (System stoneConfigurationReport at: #StnCurrentTranLogNames)
%

category: 'private'
method: SystemResolver
_extent: extentIndex
	"Answer the path to the extent at extentIndex in DBF_EXTENT_NAMES: (System stoneConfigurationReport at: #DBF_EXTENT_NAMES) at: extentIndex"

	| extentNames extentName |
	extentNames := System stoneConfigurationReport at: #DBF_EXTENT_NAMES.
	extentNames _isArray
		ifTrue: [  
			extentIndex > extentNames size ifTrue: [  self error: 'Requested extent index (', extentIndex printString, ') exceeds number of extents present in system (', extentNames size printString, ')'  ].
			extentName := extentNames ]
		ifFalse: [ 
			extentIndex ~= 1 ifTrue: [  self error: 'Requested extent index (', extentIndex printString, ') exceeds number of extents present in system (1)'  ].
			extentName := extentNames ].
	^ self  resolveString: extentName
%

! Class implementation for 'FileSystemStore'

!		Class methods for 'FileSystemStore'

category: 'accessing'
classmethod: FileSystemStore
delimiter
	^ self shouldBeImplemented 
%

category: 'accessing'
classmethod: FileSystemStore
isCaseSensitive
	^ self shouldBeImplemented
%

category: 'instance creation'
classmethod: FileSystemStore
new

	^self basicNew
		initialize;
		yourself
%

category: 'accessing'
classmethod: FileSystemStore
osName
	^System osName
%

category: 'accessing'
classmethod: FileSystemStore
separator
	self shouldBeImplemented
%

!		Instance methods for 'FileSystemStore'

category: 'public'
method: FileSystemStore
basenameFromEntry: aNode
	"Used to extract the basename from the low-level representation (node / entry) from the store."
	self subclassResponsibility
%

category: 'abstract'
method: FileSystemStore
basicCreationTimeOf: aNode
	"Used to decide whether the low-level representation (node / entry) from the store is a readable
	file or a directory whose contents can be listed."
	self subclassResponsibility 
%

category: 'abstract'
method: FileSystemStore
basicEntry: directoryEntry path: aPath nodesDo: aBlock
	self subclassResponsibility: #basicEntry:path:nodesDo:
%

category: 'abstract'
method: FileSystemStore
basicIsDirectory: aNode
	"Used to decide whether the low-level representation (node / entry) from the store is a directory.
	This private message should only be called form within the store."
	self subclassResponsibility 
%

category: 'abstract'
method: FileSystemStore
basicIsFile: aNode
	"Used to decide whether the low-level representation (node / entry) from the store is a file.
	This private message should only be called form within the store."
	self subclassResponsibility 
%

category: 'abstract'
method: FileSystemStore
basicIsSymlink: aNode
	^self subclassResponsibility 
	
%

category: 'abstract'
method: FileSystemStore
basicIsWritable: aNode
	"Used to decide whether the low-level representation (node / entry) from the store is 
	a writable file or can be changed."
	self subclassResponsibility 
%

category: 'abstract'
method: FileSystemStore
basicModificationTime: aNode
	"Used to decide whether the low-level representation (node / entry) from the store is a readable
	file or a directory whose contents can be listed."
	self subclassResponsibility 
%

category: 'error signalling'
method: FileSystemStore
basicOpen: aPath  options: aFileOptions
	"open the file at the given path and return an identifier"
	self subclassResponsibility
%

category: 'abstract'
method: FileSystemStore
basicPosixPermissions: aNode
	"Used to get the posix permissions from a low-level filesystem entry / node"
	self subclassResponsibility 
%

category: 'abstract'
method: FileSystemStore
basicSizeOf: aNode
	"Used to get the size of the low-level representation (node / entry) "
	self subclassResponsibility 
%

category: 'public'
method: FileSystemStore
checkName: aString fixErrors: fixErrors
	^ self subclassResponsibility
%

category: 'abstract'
method: FileSystemStore
close
	"Some kinds of filesystems need to open connections to external resources"
%

category: 'private'
method: FileSystemStore
copy: sourcePath ifAbsent: absentBlock to: destinationPath ifPresent: presentBlock fileSystem: aFilesystem

	| buffer out in |
	
	in := nil.
	out := nil.
	buffer := nil.
	[
		in := aFilesystem binaryReadStreamOn: sourcePath.
		in ifNil: [ ^ absentBlock value ].
		
		(self exists: destinationPath)
			ifTrue: [ "cannot overwrite destination"
				^ presentBlock value ].
			
		out := aFilesystem binaryWriteStreamOn: destinationPath.
		buffer := ByteArray new: 1024.
		
		[ in atEnd ]
			whileFalse: [ 
				buffer := in nextInto: buffer.
				out nextPutAll: buffer ]]
	ensure: [ 
		in ifNotNil: [ in close ].
		out ifNotNil: [ out close ]]
%

category: 'abstract'
method: FileSystemStore
createDirectory: aPath
	self subclassResponsibility 
%

category: 'public'
method: FileSystemStore
creationTimeOf: aPath
	"Return the date of creation of the File described by aPath"
	^ self 
		nodeAt: aPath 
		ifPresent: [ :entry | self basicCreationTimeOf: entry ] 
		ifAbsent: [ self signalFileDoesNotExist: aPath ].
	
%

category: 'accessing'
method: FileSystemStore
defaultWorkingDirectory
	^ Path root
%

category: 'abstract'
method: FileSystemStore
delete: aPath
	self subclassResponsibility 
%

category: 'accessing'
method: FileSystemStore
delimiter
	^ self class delimiter
%

category: 'public'
method: FileSystemStore
directoryAt: aPath ifAbsent: absentBlock directoryNodesDo: aBlock
	^ self 
		directoryAt: aPath 
		ifAbsent: absentBlock 
		nodesDo: [ :entry|
			(self basicIsDirectory: entry) 
				ifTrue: [ aBlock value: entry ]].
%

category: 'public'
method: FileSystemStore
directoryAt: aPath ifAbsent: absentBlock fileNodesDo: aBlock
	^ self 
		directoryAt: aPath 
		ifAbsent: absentBlock 
		nodesDo: [ :entry|
			(self basicIsDirectory: entry) 
				ifFalse: [ aBlock value: entry ]].
%

category: 'public'
method: FileSystemStore
directoryAt: aPath ifAbsent: absentBlock nodesDo: aBlock
	^ self
		nodeAt: aPath
		ifPresent: [ :entry | 
			(self basicIsDirectory: entry)
				ifTrue: [ self basicEntry: entry path: aPath nodesDo: aBlock ]
				ifFalse: [ DirectoryDoesNotExist signalWith: aPath ] ]
		ifAbsent: absentBlock
%

category: 'public'
method: FileSystemStore
ensureCreateDirectory: aPath
	(self isDirectory: aPath) ifTrue: [ ^ self ].
	self ensureCreateDirectory: aPath parent.
	self createDirectory: aPath
%

category: 'private'
method: FileSystemStore
entryFromNode: aNode fileSystem: aFilesystem path: aPath
	^ FileSystemDirectoryEntry
		fileSystem: aFilesystem
		path: aPath
		creation: (self basicCreationTimeOf: aNode)
		modification:(self basicModificationTimeOf: aNode)
		isDir: (self basicIsDirectory: aNode)
		size: (self basicSizeOf: aNode)
		posixPermissions: (self basicPosixPermissions: aNode)
		isSymlink: (self basicIsSymlink: aNode)
%

category: 'public'
method: FileSystemStore
entryFromNode: node path: path for: aFileystem
	| entryPath |
	entryPath := path / (self basenameFromEntry: node).
	^ self entryFromNode: node fileSystem: aFileystem path: entryPath
%

category: 'error handling'
method: FileSystemStore
errorClassForErrorNumber: anErrorNumber fileReference: aFileReference errorGroup: anErrorGroup

	^anErrorGroup errorClassForDefaultPlatformError: anErrorNumber fileReference: aFileReference
%

category: 'error handling'
method: FileSystemStore
errorClassForErrorNumber: anErrorNumber fileReference: aFileReference options: fileOptions errorGroup: anErrorGroup

	^anErrorGroup errorClassForDefaultPlatformError: anErrorNumber fileReference: aFileReference options: fileOptions
%

category: 'error handling'
method: FileSystemStore
errorClassForErrorNumber: anErrorNumber newFileReference: aNewFileReference oldFileReference: anOldFileReference errorGroup: anErrorGroup

	^anErrorGroup errorClassForDefaultPlatformError: anErrorNumber newFileReference: aNewFileReference oldFileReference: anOldFileReference
%

category: 'public'
method: FileSystemStore
exists: aPath
	self 
		nodeAt: aPath 
		ifPresent: [ :entry | ^ true ] 
		ifAbsent: [ ^ false ].
	
%

category: 'private'
method: FileSystemStore
filename: aByteString matches: aByteString2
	^ aByteString = aByteString2
%

category: 'initialization'
method: FileSystemStore
initialize
%

category: 'accessing'
method: FileSystemStore
isCaseSensitive
	^ self class isCaseSensitive
%

category: 'public'
method: FileSystemStore
isDirectory: aPath
	aPath isRoot ifTrue: [ ^ true ].
	self 
		nodeAt: aPath 
		ifPresent: [ :entry | ^ self basicIsDirectory: entry ] 
		ifAbsent: [ ^ false ].
	
%

category: 'public'
method: FileSystemStore
isFile: aPath
	"slow solution for big directories! "
	^ self 
		nodeAt: aPath 
		ifPresent: [ :entry | ^ self basicIsFile: entry ] 
		ifAbsent: [ ^ false ]
	
%

category: 'testing'
method: FileSystemStore
isForeignFileSystem
	^ false
%

category: 'public'
method: FileSystemStore
isSymlink: aPath
	aPath isRoot ifTrue: [ ^ true ].
	self 
		nodeAt: aPath 
		ifPresent: [ :entry | ^ self basicIsSymlink: entry ] 
		ifAbsent: [ ^ false ].
	
%

category: 'public'
method: FileSystemStore
isWritable: aPath
	self nodeAt: aPath 
		ifPresent: [ :entry | ^ self basicIsWritable: entry ] 
		ifAbsent: [ ^ false ].
	
%

category: 'public'
method: FileSystemStore
modificationTimeOf: aPath
	"Returns the last date of modification of the File described by aPath"
	^ self 
		nodeAt: aPath 
		ifPresent: [ :entry | self basicModificationTimeOf: entry ] 
		ifAbsent: [ self signalFileDoesNotExist: aPath ].
	
%

category: 'public'
method: FileSystemStore
nodeAt: aPath
	^ self 
		nodeAt: aPath 
		ifPresent: [ :node| node ]
		ifAbsent: [ self error: 'the node ', aPath printString, ' not found.' ]
%

category: 'abstract'
method: FileSystemStore
nodeAt: aPath ifPresent: presentBlock ifAbsent: absentBlock
	self subclassResponsibility: #nodeAt:ifPresent:ifAbsent:
%

category: 'abstract'
method: FileSystemStore
open
	"Some kinds of filesystems need to open connections to external resources"
%

category: 'public'
method: FileSystemStore
openFileStream: path options: aFileOptions
	self subclassResponsibility
%

category: 'converting'
method: FileSystemStore
pathFromString: aString	
	"Use the unix convention by default, since many filesystems are based on it."
	
	^ Path from: aString delimiter: self delimiter
%

category: 'public'
method: FileSystemStore
permissions: aPath
	self nodeAt: aPath 
		ifPresent: [ :entry | ^ FileSystemPermission posixPermissions: (self basicPosixPermissions: entry) ] 
		ifAbsent: [ ^ FileSystemPermission default ].
	
%

category: 'converting'
method: FileSystemStore
printPath: aPath on: out
	"Use the unix convention by default, since it's the most common."
	
	aPath isAbsolute ifTrue: [ out nextPut: self delimiter ].
	^ aPath printOn: out delimiter: self delimiter
%

category: 'private'
method: FileSystemStore
rename: sourcePath ifAbsent: absentBlock to: destinationPath ifPresent: presentBlock fileSystem: anFSFilesystem

	| result |
	(self exists: destinationPath) ifTrue: [ ^ presentBlock value ].
	(self exists: sourcePath) ifFalse: [ ^ absentBlock value ].
	result := self rename: sourcePath to: destinationPath.
	result ifNil: [ self primitiveFailed ].
	^ self.
%

category: 'abstract'
method: FileSystemStore
rename: sourcePath to: destinationPath
	self subclassResponsibility 
%

category: 'accessing'
method: FileSystemStore
separator
	^ self class separator
%

category: 'error signalling'
method: FileSystemStore
signalDirectoryDoesNotExist: aPath
	^ DirectoryDoesNotExist signalWith: aPath
%

category: 'error signalling'
method: FileSystemStore
signalDirectoryExists: aPath
	^ DirectoryExists signalWith: aPath
%

category: 'error signalling'
method: FileSystemStore
signalFileDoesNotExist: aPath
	^ FileDoesNotExistException
		signalWith: aPath
%

category: 'error signalling'
method: FileSystemStore
signalFileExists: aPath
	^ FileExists signalWith: aPath
%

category: 'public'
method: FileSystemStore
sizeOf: aPath
	"Return the size of the File described by aPath"
	^ self 
		nodeAt: aPath 
		ifPresent: [ :entry | self basicSizeOf: entry ] 
		ifAbsent: [ self signalFileDoesNotExist: aPath ]
	
%

category: 'converting'
method: FileSystemStore
stringFromPath: aPath
	^ String streamContents: [ :out | 
		self printPath: aPath on: out ]
%

! Class implementation for 'DiskStore'

!		Class methods for 'DiskStore'

category: 'current'
classmethod: DiskStore
activeClass
	^DiskStore activeClassFor: self osName
%

category: 'current'
classmethod: DiskStore
activeClassFor: anOsName
	self allSubclasses do: [:ea | 
		(ea platformNames includes: anOsName) ifTrue: [^ ea] ].
	^ self
%

category: 'class initialization'
classmethod: DiskStore
allConcreteFilePosixErrorClasses
	^FilePosixError allConcreteFilePosixErrorClasses
%

category: 'class initialization'
classmethod: DiskStore
allConcretePosixFlagClasses
	^PosixFlag allConcretePosixFlagClasses
%

category: 'current'
classmethod: DiskStore
createDefault
	^ self new
%

category: 'create rules'
classmethod: DiskStore
createNewRule
	FileOptionFeatureNotSupported signal: 'createNewRule'
%

category: 'current'
classmethod: DiskStore
current
	^ self currentFileSystem store
%

category: 'current'
classmethod: DiskStore
currentFileSystem
	^ CurrentFS ifNil: [
		CurrentFS := FileSystem store: self activeClass createDefault]
%

category: 'accessing'
classmethod: DiskStore
defaultWorkingDirectory
	"Ask the VM for the default working directory.
	Clients should normally use the instance side method which caches this value."

	DefaultWorkingDirectory ifNil: [DefaultWorkingDirectory := FileSystem fileClass currentWorkingDirectoryPath].
	^(Path from: DefaultWorkingDirectory delimiter: self delimiter).
%

category: 'public'
classmethod: DiskStore
delimiter
	^ self current delimiter
%

category: 'share rules'
classmethod: DiskStore
denyNoneShareRule
	FileOptionFeatureNotSupported signal: 'denyNoneShareRule'
%

category: 'share rules'
classmethod: DiskStore
denyReadShareRule
	FileOptionFeatureNotSupported signal: 'denyReadShareRule'
%

category: 'share rules'
classmethod: DiskStore
denyReadWriteShareRule
	FileOptionFeatureNotSupported signal: 'denyReadWriteShareRule'
%

category: 'share rules'
classmethod: DiskStore
denyWriteShareRule
	FileOptionFeatureNotSupported signal: 'denyWriteShareRule'
%

category: 'class initialization'
classmethod: DiskStore
initialize
	"DiskStore initialize"
	self initializePosixFlagValues.
	self initializePosixErrorValues.
	self allSubclasses do: [:ea | ea initialize]
%

category: 'class initialization'
classmethod: DiskStore
initializeDefaultWorkingDirectory
	^DefaultWorkingDirectory := self defaultWorkingDirectory
%

category: 'class initialization'
classmethod: DiskStore
initializePosixErrorValues
	" DiskStore initializePosixErrorValues"
	| errorNumberSelector |
	errorNumberSelector := ((self name copyFrom: 1 to: self name size - 5) , 'ErrorNumber') asValidSelector.
	self posixErrorNumbers: Dictionary new. 
	self allConcreteFilePosixErrorClasses do: [:eaClass |  
			self posixErrorNumbers at: eaClass posixName put: (eaClass errorNumberForSelector: errorNumberSelector)]
%

category: 'class initialization'
classmethod: DiskStore
initializePosixFlagValues
	" DiskStore initializePosixFlagValues "
	| valueSelector |
	valueSelector := ((self name copyFrom: 1 to: self name size - 5) , 'Value') asValidSelector.
	self posixValues: Dictionary new. 
	self allConcretePosixFlagClasses do: [:eaClass |  
			self posixValues at: eaClass posixName put: (eaClass valueForSelector: valueSelector)]
%

category: 'public'
classmethod: DiskStore
maxFileNameLength
	self subclassResponsibility 
%

category: 'create rules'
classmethod: DiskStore
noTruncateRule
	FileOptionFeatureNotSupported signal: 'noTruncateRule'
%

category: 'create rules'
classmethod: DiskStore
openAlwaysRule
	FileOptionFeatureNotSupported signal: 'openAlwaysRule'
%

category: 'create rules'
classmethod: DiskStore
openExistingRule
	FileOptionFeatureNotSupported signal: 'openExistingRule'
%

category: 'posix'
classmethod: DiskStore
optionPlatformValueFor: aPosixName

	^self posixValues at: aPosixName ifAbsent: []
%

category: 'current'
classmethod: DiskStore
platformNames
	^{}
%

category: 'accessing'
classmethod: DiskStore
posixErrorNumbers
	^PosixErrorNumbers
%

category: 'accessing'
classmethod: DiskStore
posixErrorNumbers: aCollection
	PosixErrorNumbers := aCollection
%

category: 'accessing'
classmethod: DiskStore
posixValues
	^PosixValues
%

category: 'accessing'
classmethod: DiskStore
posixValues: aCollection
	PosixValues := aCollection
%

category: 'access rules'
classmethod: DiskStore
readOnlyAccessRule
	FileOptionFeatureNotSupported signal: 'readOnlyAccessRule'
%

category: 'access rules'
classmethod: DiskStore
readWriteAccessRule
	FileOptionFeatureNotSupported signal: 'readWriteAccessRule'
%

category: 'current'
classmethod: DiskStore
reset
	self posixValues: nil.
	CurrentFS := nil.
	DefaultWorkingDirectory := nil
%

category: 'system startup'
classmethod: DiskStore
shutDown: quitting
	"Force to detect filesystem after image restart"
	self reset
%

category: 'system startup'
classmethod: DiskStore
startUp: resuming
	resuming 
		ifTrue: [ self reset ].
	DefaultWorkingDirectory := self defaultWorkingDirectory.
%

category: 'create rules'
classmethod: DiskStore
truncateExistingRule
	FileOptionFeatureNotSupported signal: 'truncateExistingRule'
%

category: 'access rules'
classmethod: DiskStore
writeOnlyAccessRule
	FileOptionFeatureNotSupported signal: 'readWriteAccessRule'
%

!		Instance methods for 'DiskStore'

category: 'comparing'
method: DiskStore
= other
	^ self species = other species
%

category: 'private'
method: DiskStore
basenameFromEntry: entry
	^ entry at: 1
%

category: 'private'
method: DiskStore
basicCreationTimeOf: anEntry
	" the entry contains the seconds since the squeak epoch in local time"
	| dt |
	dt := DateAndTime posixSeconds:  (anEntry at: 2)  offset: (Duration seconds: 0).
	dt offset: (Duration seconds: (dt currentTimeZone transitionAtUTC: dt) offsetFromUTC).
	^ dt
%

category: 'private'
method: DiskStore
basicEntriesIn: aPath
	^FileSystem fileClass entriesIn: aPath
%

category: 'private'
method: DiskStore
basicEntry: ignored path: aPath nodesDo: aBlock

	| pathString |
		
	pathString := self stringFromPath: aPath.
	(self basicEntriesIn: pathString) do: [:eaEntry |
		aBlock value: (FileSystem fileClass lookupDirectoryEntry: eaEntry)]
%

category: 'public'
method: DiskStore
basicEntryAt: aPath
	| path basename |
	
	path := self stringFromPath: aPath parent.
	basename := aPath basename.
	
	^ (FileSystem fileClass lookupDirectory: path filename: basename)
		ifNil: [ #badDirectoryPath ].
%

category: 'private'
method: DiskStore
basicIsDirectory: anEntry

	^anEntry at: 4
%

category: 'private'
method: DiskStore
basicIsFile: anEntry
	^ (anEntry at: 4) not
%

category: 'private'
method: DiskStore
basicIsSymlink: anEntry
	^(anEntry size >= 7)
		ifTrue: [ anEntry at: 7 ]
		ifFalse: [ false ]
%

category: 'private'
method: DiskStore
basicModificationTimeOf: anEntry
	" the entry contains the seconds since the squeak epoch in local time"

	| dt |
	dt := DateAndTime posixSeconds:  (anEntry at: 3) offset: (Duration seconds: 0).
	dt offset: (Duration seconds: (dt currentTimeZone transitionAtUTC: dt) offsetFromUTC).
	^ dt
%

category: 'public'
method: DiskStore
basicOpen: aPath options: aFileOptions

	| string |
	string := self stringFromPath: aPath.
	^FileSystem fileClass openPathName: string withOptions: aFileOptions
%

category: 'private'
method: DiskStore
basicPosixPermissions: anEntry
	^ (anEntry size >= 6)
		ifTrue: [ anEntry at: 6 ]
		ifFalse: [ nil ].
%

category: 'private'
method: DiskStore
basicSizeOf: anEntry
	^ (anEntry at: 5)
%

category: 'public'
method: DiskStore
checkName: aFileName fixErrors: fixErrors
	"Check a string aFileName for validity as a file name. Answer the original file name if it is valid. If the name is not valid (e.g., it is too long or contains illegal characters) and fixing is false, raise an error. If fixing is true, fix the name (usually by truncating and/or tranforming characters), and answer the corrected name. The default behavior is just to truncate the name to the maximum length for this platform. Subclasses can do any kind of checking and correction appropriate for their platform."
	
	| maxLength |
	aFileName size = 0 ifTrue: [self error: 'zero length file name'].
	maxLength := self maxFileNameLength.
	aFileName size > maxLength ifTrue: [
		fixErrors
			ifTrue: [^ aFileName contractTo: maxLength]
			ifFalse: [self error: 'file name is too long']].
	^ aFileName
%

category: 'public'
method: DiskStore
createDirectory: path
	"Create a directory for the argument path. 
	If the path refers to an existing file, raise FileExists.
	If the path refers to an existing directory, raise DirectoryExists.
	If the parent directory of the path does not exist, raise DirectoryDoesNotExist"

	| parent pathString result |
	pathString := self stringFromPath: path.
	result := self _createDirectory: pathString.
	result
		ifNil: [ 
			parent := path parent.
			(self exists: path)
				ifTrue: [ 
					(self isFile: path)
						ifTrue: [ self signalFileExists: path ]
						ifFalse: [ self signalDirectoryExists: path ] ].
			(self isDirectory: parent)
				ifFalse: [ ^ self signalDirectoryDoesNotExist: parent ].
			self primitiveFailed ].
	^ self
%

category: 'accessing'
method: DiskStore
defaultWorkingDirectory
	"Answer the default working directory, which is defined as the directory where the image resides."
 
	DefaultWorkingDirectory  ifNil: [self class initializeDefaultWorkingDirectory].
	^DefaultWorkingDirectory
%

category: 'public'
method: DiskStore
delete: path
	| pathString |
	
	(self exists: path) 
		ifFalse: [ ^ FileDoesNotExistException signalWith: path ].
		
	pathString := self stringFromPath: path.
	
	(self isDirectory: path)
		ifTrue: [ self deleteDirectory: pathString ]
		ifFalse: [ self deleteFile: pathString ]
%

category: 'public'
method: DiskStore
deleteDirectory: pathString
	(self _deleteDirectory: pathString)
		ifNil: [ self error: 'Error deleting directory ', pathString printString, ' :: ', FileSystem fileClass serverErrorString ]
%

category: 'public'
method: DiskStore
deleteFile: pathString
	(self _deleteFile: pathString)
		ifNil: [ FileDeletionError 
					signalWith: pathString  
					text: 'Could not delete file ' , pathString , '. Check the file is not open.' ]
%

category: 'printing'
method: DiskStore
forReferencePrintOn: aStream
	aStream nextPutAll: 'File @ '
%

category: 'accessing'
method: DiskStore
handleClass
	^ FileHandle
%

category: 'comparing'
method: DiskStore
hash
	^ self species hash
%

category: 'initialization'
method: DiskStore
initialize
	super initialize.
	maxFileNameLength := 255
%

category: 'public'
method: DiskStore
isDirectory: aPath
	| entry |
	
	aPath isRoot ifTrue: [ ^ true ].
	
	entry := self  basicEntryAt: aPath.
	
	^ entry == #badDirectoryPath 
		ifTrue: [  false ]
		ifFalse: [ self basicIsDirectory: entry ].
	
%

category: 'testing'
method: DiskStore
isDiskFileSystem
	^ true
%

category: 'public'
method: DiskStore
isFile: aPath
	| entry |
	
	aPath isRoot ifTrue: [ ^ false ].
	
	entry := self  basicEntryAt: aPath.
	
	^ entry == #badDirectoryPath 
		ifTrue: [  false ]
		ifFalse: [ self basicIsFile: entry ].
	
%

category: 'public'
method: DiskStore
isReadable: aPath
	(self exists: aPath) ifFalse: [ ^ false ].
	self flag: 'TODO: we need a decent primitive for this...'.
	(self basicOpen: aPath options: FileOptions newRead ) 
		ifNotNil: [ :id|
			id close.
			^ true].
	^ false
%

category: 'public'
method: DiskStore
isSymlink: aPath
	| entry |
	
	aPath isRoot ifTrue: [ ^false ].
	
	entry := self  basicEntryAt: aPath.
	
	^ entry == #badDirectoryPath 
		ifTrue: [  false ]
		ifFalse: [ self basicIsSymlink: entry ].
	
%

category: 'public'
method: DiskStore
isWritable: aPath
	(self exists: aPath) ifFalse: [ ^ false ].
	self flag: 'TODO: we need a decent primitive for this...'.
	(self basicOpen: aPath options: FileOptions newWrite) 
		ifNotNil: [ :id|
			id close.
			^ true].
	^ false
%

category: 'public'
method: DiskStore
maxFileNameLength
	^ maxFileNameLength
%

category: 'private'
method: DiskStore
nodeAt: aPath ifPresent: presentBlock ifAbsent: absentBlock
	
	| entry |
	
	aPath isRoot ifTrue: [ ^ presentBlock value: self rootNode ].
	
	entry := self basicEntryAt: aPath.
	
	^ entry == #badDirectoryPath 
		ifTrue: absentBlock
		ifFalse: [
			entry at: 1 put: aPath basename.
			presentBlock value: entry ].
%

category: 'posix'
method: DiskStore
optionPlatformValueFor: aPosixName

	^self class optionPlatformValueFor: aPosixName
%

category: 'public'
method: DiskStore
rename: sourcePath to: destinationPath 
	"Rename the file of the given name to the new name. Fail if there is no file of the old name 
	or if there is an existing file with the new name."

	| sourcePathString targetPathString |
	sourcePathString := self stringFromPath: sourcePath.
	targetPathString := self stringFromPath: destinationPath.
	^self _rename: sourcePathString to: targetPathString
%

category: 'private'
method: DiskStore
rootNode
	^ #('' 0 0 true 0 8r555)
%

category: 'accessing'
method: DiskStore
workingDirectoryEnvironmentVariableName
	^'PWD'
%

category: 'public'
method: DiskStore
_createDirectory: path
	^FileSystem fileClass createDirectory: path.
%

category: 'public'
method: DiskStore
_deleteDirectory: pathString
	^FileSystem fileClass deleteDirectory: pathString
%

category: 'public'
method: DiskStore
_deleteFile: pathString
	^FileSystem fileClass deleteFile: pathString
%

category: 'public'
method: DiskStore
_rename: sourcePathString to: targetPathString
	^FileSystem fileClass rename: sourcePathString to: targetPathString
%

! Class implementation for 'ClientStore'

!		Class methods for 'ClientStore'

category: 'current'
classmethod: ClientStore
currentFileSystem
	self currentFS ifNil: [self initializeClientFS].
	^self currentFS
%

category: 'accessing'
classmethod: ClientStore
currentFS
	^CurrentFS
%

category: 'accessing'
classmethod: ClientStore
currentFS: aFileSystem
	CurrentFS := aFileSystem
%

category: 'accessing'
classmethod: ClientStore
defaultWorkingDirectory
	^self currentFs store defaultWorkingDirectory
%

category: 'class initialization'
classmethod: ClientStore
initialize
%

category: 'class initialization'
classmethod: ClientStore
initializeClientFS

	self currentFS: (FileSystem store: ClientStore new)
%

category: 'accessing'
classmethod: ClientStore
osName
	^System clientOsName
%

category: 'current'
classmethod: ClientStore
reset
	self initializeClientFS
%

!		Instance methods for 'ClientStore'

category: 'private'
method: ClientStore
basicCreationTimeOf: anEntry
	" the entry contains the seconds since the squeak epoch in local time"
	| dt |
	dt := DateAndTime posixSeconds:  ((anEntry at: 2) ifNil: [0]) offset: (Duration seconds: 0).
	dt offset: (Duration seconds: (dt currentTimeZone transitionAtUTC: dt) offsetFromUTC).
	^ dt
%

category: 'private'
method: ClientStore
basicEntriesIn: aPath
	^self error: 'Not implemented for the client'
%

category: 'public'
method: ClientStore
basicEntryAt: aPath
	| anEntryType |
	"This is required becuase GemStone's lookup method only works on the server"
	anEntryType := self fileTypeOf: aPath.
	(anEntryType  isNil or: [anEntryType  > 1]) ifTrue: [^#badDirectoryPath].
	^{
		aPath basename.
		nil.
		nil.
		anEntryType = 1.
		nil.
		nil.
		nil }
%

category: 'private'
method: ClientStore
basicIsSymlink: anEntry
	^(anEntry size >= 7)
		ifTrue: [ (anEntry at: 7) ifNil: [false] ]
		ifFalse: [ false ]
%

category: 'private'
method: ClientStore
basicModificationTimeOf: anEntry
	" the entry contains the seconds since the squeak epoch in local time"

	| dt |
	dt := DateAndTime posixSeconds:  ((anEntry at: 3) ifNil: [0]) offset: (Duration seconds: 0).
	dt offset: (Duration seconds: (dt currentTimeZone transitionAtUTC: dt) offsetFromUTC).
	^ dt
%

category: 'accessing'
method: ClientStore
defaultWorkingDirectory
	^defaultWorkingDirectory
%

category: 'accessing'
method: ClientStore
defaultWorkingDirectory: aPath
	defaultWorkingDirectory := aPath
%

category: 'accessing'
method: ClientStore
defaultWorkingDirectoryString
	"Answer the default working directory, which is defined as the directory where the image resides."
	^System clientEnvironmentVariable: self store workingDirectoryEnvironmentVariableName
%

category: 'wip'
method: ClientStore
delimiter
	"Use the unix convention by default, since many filesystems are based on it."
	
	^ self store delimiter
%

category: 'error handling'
method: ClientStore
errorClassForErrorNumber: anErrorNumber fileReference: aFileReference errorGroup: anErrorGroup

	^self store errorClassForErrorNumber: anErrorNumber fileReference: aFileReference errorGroup: anErrorGroup
%

category: 'error handling'
method: ClientStore
errorClassForErrorNumber: anErrorNumber fileReference: aFileReference options: fileOptions errorGroup: anErrorGroup

	^self store errorClassForErrorNumber: anErrorNumber fileReference: aFileReference options: fileOptions errorGroup: anErrorGroup
%

category: 'error handling'
method: ClientStore
errorClassForErrorNumber: anErrorNumber newFileReference: aNewFileReference oldFileReference: anOldFileReference errorGroup: anErrorGroup

	^anErrorGroup errorClassForDefaultPlatformError: anErrorNumber newFileReference: aNewFileReference oldFileReference: anOldFileReference
%

category: 'public'
method: ClientStore
exists: aPath
	| result |
	result := self fileTypeOf: aPath.
	^result notNil and: [result < 2]
%

category: 'private'
method: ClientStore
fileTypeOf: aPath
	^FileSystem fileClass _fileKind: (self store stringFromPath: aPath) onClient: true
%

category: 'initialize'
method: ClientStore
initialize
	super initialize.
	self store: self class activeClass new.
	self defaultWorkingDirectory: (Path from: self defaultWorkingDirectoryString delimiter: self delimiter)
%

category: 'testing'
method: ClientStore
isForeignFileSystem
	^true
%

category: 'testing'
method: ClientStore
isWindowsStoreClient
	^self store class name = #WindowsStore
%

category: 'converting'
method: ClientStore
pathFromString: aString	

	^self store pathFromString: aString
%

category: 'converting'
method: ClientStore
printPath: aPath on: out
	
	^self store printPath: aPath on: out
%

category: 'accessing'
method: ClientStore
store
	^store
%

category: 'accessing'
method: ClientStore
store: aDiskStore
	store := aDiskStore
%

category: 'private'
method: ClientStore
_createDirectory: path
	^FileSystem fileClass createClientDirectory: path
%

category: 'private'
method: ClientStore
_deleteDirectory: pathString
	^FileSystem fileClass deleteClientDirectory: pathString
%

category: 'private'
method: ClientStore
_deleteFile: pathString
	^FileSystem fileClass deleteClientFile: pathString
%

category: 'private'
method: ClientStore
_rename: sourcePathString to: targetPathString
	^FileSystem fileClass renameOnClient: sourcePathString to: targetPathString
%

! Class implementation for 'UnixStore'

!		Class methods for 'UnixStore'

category: 'public'
classmethod: UnixStore
delimiter
	^ $/
%

category: 'public'
classmethod: UnixStore
isCaseSensitive

	^ true
%

category: 'public'
classmethod: UnixStore
maxFileNameLength

	^ 255
%

category: 'public'
classmethod: UnixStore
platformNames
	^ {'Linux'}
%

category: 'public'
classmethod: UnixStore
separator 
	^ $:
%

!		Instance methods for 'UnixStore'

category: 'public'
method: UnixStore
checkName: aFileName fixErrors: fixing
	"Check if the file name contains any invalid characters"
	| fName |
	fName := super checkName: aFileName fixErrors: fixing.
	
	(fName includes: self delimiter) ifFalse:
		[^fName].
	
	fixing ifFalse: [self error:'Invalid file name'].
	
	^ fName copyReplaceAll: self delimiter asString with: '#'
%

! Class implementation for 'MacStore'

!		Class methods for 'MacStore'

category: 'public'
classmethod: MacStore
isCaseSensitive

	^ true
%

category: 'public'
classmethod: MacStore
platformNames
	^ {'Darwin'}
%

! Class implementation for 'WindowsStore'

!		Class methods for 'WindowsStore'

category: 'create rules'
classmethod: WindowsStore
createNewRule
	^1
%

category: 'accessing'
classmethod: WindowsStore
delimiter
	^ $\
%

category: 'share rules'
classmethod: WindowsStore
denyNoneShareRule
	^3
%

category: 'share rules'
classmethod: WindowsStore
denyReadShareRule
	^2
%

category: 'share rules'
classmethod: WindowsStore
denyReadWriteShareRule
	^0
%

category: 'share rules'
classmethod: WindowsStore
denyWriteShareRule
	^1
%

category: 'file constants'
classmethod: WindowsStore
fileAttributeNormal
	^128
%

category: 'file constants'
classmethod: WindowsStore
invalidHandleValue
	"The value is returned if a file handle was not created"
	^4294967295
%

category: 'accessing'
classmethod: WindowsStore
isCaseSensitive
	^ false
%

category: 'accessing'
classmethod: WindowsStore
maxFileNameLength
	self flag: 'TODO: more tests needed here!'.
	^ 255
%

category: 'create rules'
classmethod: WindowsStore
noTruncateRule
	^nil
%

category: 'create rules'
classmethod: WindowsStore
openAlwaysRule
	^4
%

category: 'create rules'
classmethod: WindowsStore
openExistingRule
	^3
%

category: 'accessing'
classmethod: WindowsStore
platformNames
	^ {'Win32' . 'Windows NT'}
%

category: 'access rules'
classmethod: WindowsStore
readOnlyAccessRule
	^2147483648
%

category: 'access rules'
classmethod: WindowsStore
readWriteAccessRule
	^self readOnlyAccessRule bitOr: self writeOnlyAccessRule
%

category: 'accessing'
classmethod: WindowsStore
separator 
	^ $\
%

category: 'create rules'
classmethod: WindowsStore
truncateExistingRule
	^5
%

category: 'access rules'
classmethod: WindowsStore
writeOnlyAccessRule
	^1073741824
%

!		Instance methods for 'WindowsStore'

category: 'public'
method: WindowsStore
checkName: aFileName fixErrors: fixing
	"Check if the file name contains any invalid characters"
	
	| fName badChars |
	fName := super checkName: aFileName fixErrors: fixing.
	badChars := (#( $: $< $> $| $/ $\ $? $* $"), ((0 to: 31) collect: [:n | n asCharacter])) asSet.
	
	(fName includesAnyOf: badChars)
		ifFalse: [^ fName].
	
	fixing ifFalse: [^self error: 'filename contains invalid characters'].
	
	^ fName collect:
		[:char | (badChars includes: char) 
				ifTrue: [$#] 
				ifFalse: [char]]
%

category: 'converting'
method: WindowsStore
currentDisk
	^ disk ifNil: [  disk := FileSystem workingDirectory path segments first ]
%

category: 'error handling'
method: WindowsStore
errorClassForErrorNumber: anErrorNumber fileReference: aFileReference errorGroup: anErrorGroup

	^anErrorGroup errorClassForWindowsPlatformError: anErrorNumber fileReference: aFileReference
%

category: 'error handling'
method: WindowsStore
errorClassForErrorNumber: anErrorNumber fileReference: aFileReference options: fileOptions errorGroup: anErrorGroup

	^anErrorGroup errorClassForWindowsPlatformError: anErrorNumber fileReference: aFileReference options: fileOptions
%

category: 'error handling'
method: WindowsStore
errorClassForErrorNumber: anErrorNumber newFileReference: aNewFileReference oldFileReference: anOldFileReference errorGroup: anErrorGroup

	^anErrorGroup errorClassForWindowsPlatformError: anErrorNumber newFileReference: aNewFileReference oldFileReference: anOldFileReference
%

category: 'converting'
method: WindowsStore
pathFromString: aString
	"Need to distinguish '' and '/' , so tag latter with invalid character ':'  "
	| normalized pathClass pathElements |
	normalized := aString copy replaceAll: UnixStore delimiter with: self delimiter.
	pathElements := self delimiter split: normalized.
	pathClass := (Path isAbsoluteWindowsPath: normalized)
		ifTrue: [ (normalized = self delimiter asString) ifTrue: [ pathElements := { ':' } ].
			AbsolutePath ]
		ifFalse: [ self stripDrive: pathElements.
			RelativePath ].
	^pathClass withAll: pathElements
%

category: 'converting'
method: WindowsStore
printPath: aPath on: aStream
	| hasDrive |
	aPath isRoot
		ifTrue: [ ^ self ].	"effectively Windows root is empty string"
	aPath isWorkingDirectory
		ifTrue: [ ^ aPath printOn: aStream delimiter: self delimiter ].
	aPath isRelative
		ifTrue: [ ^ aPath printOn: aStream delimiter: self delimiter ].
	aPath segments first first = $:
		ifTrue: [ ^ aStream nextPut: self delimiter ].	"as tagged in #pathFromString:  "
	hasDrive := aPath segments first second = $:.
	(hasDrive not )
		ifTrue: [ aStream nextPut: self delimiter ].
	aPath printOn: aStream delimiter: self delimiter.
	(hasDrive and: [ aPath segments size = 1 ])
		ifTrue: [ aStream nextPut: self delimiter ]
%

category: 'converting'
method: WindowsStore
stripDrive: pathElements
	pathElements ifNotEmpty: [ pathElements at: 1 put: ( ($: split: pathElements first) last)  ]
%

category: 'accessing'
method: WindowsStore
workingDirectoryEnvironmentVariableName
	^'USERPROFILE'
%

! Class implementation for 'MemoryStore'

!		Class methods for 'MemoryStore'

category: 'current'
classmethod: MemoryStore
currentFileSystem
	^ CurrentFS ifNil: [ CurrentFS := FileSystem store: MemoryStore new ]
%

category: 'public'
classmethod: MemoryStore
delimiter
	^ $/
%

category: 'public'
classmethod: MemoryStore
isCaseSensitive
	^ true
%

category: 'class initialization'
classmethod: MemoryStore
reset
	CurrentFS := nil
%

category: 'public'
classmethod: MemoryStore
separator 
	^ $:
%

category: 'system startup'
classmethod: MemoryStore
startUp
	self reset
%

!		Instance methods for 'MemoryStore'

category: 'public'
method: MemoryStore
basenameFromEntry: aMemoryFileSystemEntry
	^ aMemoryFileSystemEntry basename
%

category: 'private'
method: MemoryStore
basicCreationTimeOf: aMemoryFileSystemEntry
	"Returns the creation date of aMemoryFileSystemEntry"
	^ aMemoryFileSystemEntry creationTime
%

category: 'private'
method: MemoryStore
basicEntry: entry nodesDo: aBlock
	entry fileEntriesDo: aBlock
%

category: 'private'
method: MemoryStore
basicEntry: directoryEntry path: aPath nodesDo: aBlock
	directoryEntry fileEntriesDo: aBlock
%

category: 'private'
method: MemoryStore
basicIsDirectory: aMemoryFileSystemEntry
	^ aMemoryFileSystemEntry isDirectory
%

category: 'private'
method: MemoryStore
basicIsFile: aMemoryFileSystemEntry
	^ aMemoryFileSystemEntry isFile
%

category: 'private'
method: MemoryStore
basicIsSymlink: aNode
	^false
%

category: 'private'
method: MemoryStore
basicModificationTimeOf: aMemoryFileSystemEntry
	"Return the basic modification time of aMemoryFileSystemEntry"
	^ aMemoryFileSystemEntry modificationTime
%

category: 'private'
method: MemoryStore
basicOpen: path options: aFileOptions
	^ self
		nodeAt: path
		ifPresent: [ :aMemoryFileSystemEntry | 
			aMemoryFileSystemEntry
				basicOpen;
				yourself ]
		ifAbsent: [ (aFileOptions isWritable or: [aFileOptions hasFileCreateFlag])
				ifTrue: [ self createFile: path ] 
				ifFalse: [ self signalFileDoesNotExist: path ] ]
%

category: 'private'
method: MemoryStore
basicPosixPermissions: anEntry
	^ 8r777
%

category: 'private'
method: MemoryStore
basicSizeOf: aMemoryFileSystemEntry
	"Return the basic size of aMemoryFileSystemEntry"
	^ aMemoryFileSystemEntry fileSize
%

category: 'public'
method: MemoryStore
checkName: aString fixErrors: fixErrors
	aString ifEmpty: [ self error: 'zero length file name' ].
	^ aString
%

category: 'private'
method: MemoryStore
copy: sourcePath ifAbsent: absentBlock to: destinationPath ifPresent: presentBlock fileSystem: aFilesystem
        | sourceNode destinationNode |

        sourceNode := self
                nodeAt: sourcePath
                ifPresent: [ :source | source ]
                ifAbsent: [ ^ absentBlock value].

        sourceNode isDirectory
                ifTrue: [ ^ absentBlock value ].

        destinationNode := self
                nodeAt: destinationPath parent
                ifPresent: [ :destination |  destination ]
                ifAbsent: [ ^ self signalDirectoryDoesNotExist: destinationPath parent ].

        destinationNode isFile
                ifTrue: [ self signalDirectoryDoesNotExist: destinationPath parent ].

        (destinationNode fileEntriesIncludes: destinationPath basename)
                ifTrue: [ "cannot overwrite existing file"^ presentBlock value ].

        destinationNode
                fileEntryAt: destinationPath basename
                put: (sourceNode copy
                                        basename: destinationPath basename;
                                        yourself)
%

category: 'public'
method: MemoryStore
createDirectory: path
	| parent |
	parent := path parent.
	^ self
		nodeAt: parent
		ifPresent: [ :entry | 
			entry
				fileEntryAt: path basename
				ifPresent: [ :node | 
					node isDirectory
						ifTrue: [ self signalDirectoryExists: path ]
						ifFalse: [ self signalFileExists: path ] ].
			entry ensureCreateDirectory: path basename  ]
		ifAbsent: [ self signalDirectoryDoesNotExist: parent ]
%

category: 'private'
method: MemoryStore
createFile: aPath
	^ self
		nodeAt: aPath parent
		ifPresent: [ :entry | 
			entry isDirectory
				ifTrue: [ entry ensureCreateFile: aPath basename ]]
		ifAbsent: [ self signalDirectoryDoesNotExist: aPath parent ]
%

category: 'public'
method: MemoryStore
delete: path
	self 
		nodeAt: path parent
		ifPresent: [ :dict | 
			dict fileEntryRemove: path basename ifAbsent: [ FileDoesNotExistException signalWith: path ]] 
		ifAbsent: [ DirectoryDoesNotExist signalWith: path parent ]
%

category: 'printing'
method: MemoryStore
forReferencePrintOn: aStream
	aStream nextPutAll: 'memory://'
%

category: 'accessing'
method: MemoryStore
handleClass
	^ MemoryHandle 
%

category: 'initialization'
method: MemoryStore
initialize 
	root := MemoryFileSystemDirectory new
%

category: 'testing'
method: MemoryStore
isMemoryFileSystem
	^ true
%

category: 'private'
method: MemoryStore
nodeAt: aPath ifPresent: presentBlock ifAbsent: absentBlock
	| current |
	aPath isRoot
		ifTrue: [ ^ presentBlock value: self root ].
	current := self root.
	aPath
		do: [ :segment | 
			current isDirectory
				ifTrue: [ current := current fileEntryAt: segment ifAbsent: [ ^ absentBlock value ] ]
				ifFalse: [ ^ absentBlock value ] ].
	^ presentBlock value: current
%

category: 'public'
method: MemoryStore
openFileStream: path options: aFileOptions

	| entry |
	entry := self basicOpen: path options: aFileOptions.
	^ aFileOptions isWritable
		ifTrue: [ entry writeStream ]
		ifFalse: [ entry readStreamPortable ]
%

category: 'public'
method: MemoryStore
rename: sourcePath to: destinationPath
	| sourceEntry destinationParentEntry newName |
	
	sourceEntry := self nodeAt: sourcePath.
	newName := destinationPath basename.
	
	destinationParentEntry := self nodeAt: destinationPath parent.
	
	destinationParentEntry isDirectory
		ifFalse: [ Error signal: 'Copy destination has to be a directory' ].
	destinationParentEntry 
		fileEntryAt: newName
		ifPresent: [ Error signal: 'Destination file exists already' ].
		
	destinationParentEntry 
		fileEntryAt: newName
		put: sourceEntry.
	sourceEntry basename: newName.
	
	
	(self nodeAt: sourcePath parent)
		fileEntryRemove: sourcePath basename 
	
%

category: 'private'
method: MemoryStore
replaceFile: path in: aBlock
	^ self
		nodeAt: path parent
		ifPresent: [ :entry | | old new |
			entry isDirectory
				ifFalse: [ self signalFileDoesNotExist: path ].
			old := entry fileEntryAt: path basename ifAbsent: [ self signalFileDoesNotExist: path ].
			new := aBlock value: old.
			entry fileEntryAt: path basename put: new ]
		ifAbsent: [ self signalFileDoesNotExist: path ]
%

category: 'accessing'
method: MemoryStore
root
	^ root
%

! Class implementation for 'FileSystemVisitor'

!		Instance methods for 'FileSystemVisitor'

category: 'initialization'
method: FileSystemVisitor
initialize
%

category: 'visiting'
method: FileSystemVisitor
visitDirectory: aReference
	^ self visitReference: aReference
%

category: 'visiting'
method: FileSystemVisitor
visitFile: aReference
	^ self visitReference: aReference
%

category: 'visiting'
method: FileSystemVisitor
visitReference: aReference
%

! Class implementation for 'AbstractEnumerationVisitor'

!		Instance methods for 'AbstractEnumerationVisitor'

category: 'visiting'
method: AbstractEnumerationVisitor
breadthFirst: aReference
	^ self visit: aReference with: (BreadthFirstGuide for: self)
%

category: 'initialization'
method: AbstractEnumerationVisitor
initializeWithBlock: aBlock
	self initialize.
	block := aBlock
%

category: 'visiting'
method: AbstractEnumerationVisitor
postorder: aReference
	^ self visit: aReference with: (PostorderGuide for: self)
%

category: 'visiting'
method: AbstractEnumerationVisitor
preorder: aReference
	^ self visit: aReference with: (PreorderGuide for: self)
%

category: 'visiting'
method: AbstractEnumerationVisitor
visit: aReference with: aGuide
	out := (Array new: 10) writeStreamPortable.
	aGuide show: aReference.
	^ out contents
%

category: 'visiting'
method: AbstractEnumerationVisitor
visitReference: anEntry
	self subclassResponsibility
%

! Class implementation for 'CollectVisitor'

!		Class methods for 'CollectVisitor'

category: 'instance creation'
classmethod: CollectVisitor
breadthFirst: aReference
	^ self breadthFirst: aReference collect: [:entry | entry]
%

category: 'instance creation'
classmethod: CollectVisitor
breadthFirst: aReference collect: aBlock
	^ (self collect: aBlock) breadthFirst: aReference
%

category: 'instance creation'
classmethod: CollectVisitor
collect: aBlock
	^ self basicNew initializeWithBlock: aBlock
%

category: 'instance creation'
classmethod: CollectVisitor
postorder: aReference
	^ self postorder: aReference collect: [:entry | entry]
%

category: 'instance creation'
classmethod: CollectVisitor
postorder: aReference collect: aBlock
	^ (self collect: aBlock) postorder: aReference
%

category: 'instance creation'
classmethod: CollectVisitor
preorder: aReference
	^ self preorder: aReference collect: [:entry | entry]
%

category: 'instance creation'
classmethod: CollectVisitor
preorder: aReference collect: aBlock
	^ (self collect: aBlock) preorder: aReference
%

!		Instance methods for 'CollectVisitor'

category: 'visiting'
method: CollectVisitor
visitReference: anEntry
	out nextPut: (block value: anEntry)
%

! Class implementation for 'SelectVisitor'

!		Class methods for 'SelectVisitor'

category: 'instance creation'
classmethod: SelectVisitor
breadthFirst: aReference
	^ self breadthFirst: aReference select: [:entry | true]
%

category: 'instance creation'
classmethod: SelectVisitor
breadthFirst: aReference select: aBlock
	^ (self select: aBlock) breadthFirst: aReference
%

category: 'instance creation'
classmethod: SelectVisitor
postorder: aReference
	^ self postorder: aReference select: [:entry | true]
%

category: 'instance creation'
classmethod: SelectVisitor
postorder: aReference select: aBlock
	^ (self select: aBlock) postorder: aReference
%

category: 'instance creation'
classmethod: SelectVisitor
preorder: aReference
	^ self preorder: aReference select: [:entry | true]
%

category: 'instance creation'
classmethod: SelectVisitor
preorder: aReference select: aBlock
	^ (self select: aBlock) preorder: aReference
%

category: 'instance creation'
classmethod: SelectVisitor
select: aBlock
	^ self basicNew initializeWithBlock: aBlock
%

!		Instance methods for 'SelectVisitor'

category: 'visiting'
method: SelectVisitor
visitReference: anEntry
	(block value: anEntry) 
		ifTrue: [ out nextPut: anEntry ]
%

! Class implementation for 'CopyVisitor'

!		Class methods for 'CopyVisitor'

category: 'instance creation'
classmethod: CopyVisitor
copy: source to: dest
	(self from: source to: dest) visit
%

category: 'instance creation'
classmethod: CopyVisitor
from: srcReference to: dstReference
	^ self basicNew
		initializeWithSource: srcReference 
		dest: dstReference
%

!		Instance methods for 'CopyVisitor'

category: 'visiting'
method: CopyVisitor
copyDirectory: aReference
	| directory |
	directory := dest resolve: (aReference relativeTo: source).
	directory createDirectory
%

category: 'visiting'
method: CopyVisitor
copyFile: aReference
	| copy |
	copy := dest resolve: (aReference relativeTo: source).
	aReference copyTo: copy
%

category: 'initialize-release'
method: CopyVisitor
initializeWithSource: srcReference dest: dstReference
	self initialize.
	source := srcReference.
	dest := dstReference
%

category: 'visiting'
method: CopyVisitor
visit
	(PreorderGuide for: self) 
		show: source
%

category: 'visiting'
method: CopyVisitor
visitDirectory: anEntry
	| reference |
	reference := anEntry reference.
	reference = source
		ifTrue: [dest ensureCreateDirectory]
		ifFalse: [self copyDirectory: reference]
%

category: 'visiting'
method: CopyVisitor
visitFile: anEntry
	| reference |
	reference := anEntry reference.
	reference = source
		ifTrue: [source copyTo: dest]
		ifFalse: [self copyFile: reference]
%

! Class implementation for 'DeleteVisitor'

!		Class methods for 'DeleteVisitor'

category: 'instance creation'
classmethod: DeleteVisitor
delete: aReference
	^ self new visit: aReference
%

!		Instance methods for 'DeleteVisitor'

category: 'visiting'
method: DeleteVisitor
visit: aReference
	PostorderGuide 
		show: aReference 
		to: self 
		selecting: [ :entry | entry isSymlink not ]
%

category: 'visiting'
method: DeleteVisitor
visitReference: anEntry
	anEntry reference delete
%

! Class implementation for 'MemoryFileSystemEntry'

!		Class methods for 'MemoryFileSystemEntry'

category: 'instance creation'
classmethod: MemoryFileSystemEntry
named: aFileName
	^ self new
		basename: aFileName;
		yourself
%

category: 'instance creation'
classmethod: MemoryFileSystemEntry
new

	^ self basicNew
		initialize;
		yourself
%

!		Instance methods for 'MemoryFileSystemEntry'

category: 'accessing'
method: MemoryFileSystemEntry
basename
	^ basename
%

category: 'accessing'
method: MemoryFileSystemEntry
basename: aString
	basename := aString
%

category: 'accessing'
method: MemoryFileSystemEntry
creationTime
	
	^ creationTime
%

category: 'accessing'
method: MemoryFileSystemEntry
fileSize
	self subclassResponsibility 
%

category: 'initialization'
method: MemoryFileSystemEntry
initialize 
	creationTime := modificationTime := DateAndTime now.
%

category: 'testing'
method: MemoryFileSystemEntry
isDirectory
	self subclassResponsibility
%

category: 'testing'
method: MemoryFileSystemEntry
isFile
	^ self isDirectory not
%

category: 'accessing'
method: MemoryFileSystemEntry
modificationTime
	^ modificationTime
%

category: 'accessing'
method: MemoryFileSystemEntry
modificationTime: anObject
	
	modificationTime := anObject
%

category: 'private'
method: MemoryFileSystemEntry
modified
	modificationTime := DateAndTime now.
%

! Class implementation for 'MemoryFileSystemDirectory'

!		Instance methods for 'MemoryFileSystemDirectory'

category: 'creation'
method: MemoryFileSystemDirectory
ensureCreateDirectory: aDirectoryName
	^ self 
		fileEntryAt: aDirectoryName
		put: (self class named: aDirectoryName)
%

category: 'creation'
method: MemoryFileSystemDirectory
ensureCreateFile: aFileName
	^ self 
		fileEntryAt: aFileName 
		put: (MemoryFileSystemFile named: aFileName)
%

category: 'accessing'
method: MemoryFileSystemDirectory
entries
	^ entries
%

category: 'enumeration'
method: MemoryFileSystemDirectory
fileEntriesDo: aBlock
	
	entries keys sort do: [ :fileName|
		aBlock value: (entries at: fileName)].
%

category: 'accessing'
method: MemoryFileSystemDirectory
fileEntriesIncludes: aFileName
	^ entries includesKey: aFileName
%

category: 'accessing'
method: MemoryFileSystemDirectory
fileEntryAt: aFileName
	^ entries at: aFileName
%

category: 'accessing'
method: MemoryFileSystemDirectory
fileEntryAt: aFileName ifAbsent: aBlock
	^ entries at: aFileName ifAbsent: aBlock
%

category: 'accessing'
method: MemoryFileSystemDirectory
fileEntryAt: aFileName ifPresent: aBlock
	^ (entries at: aFileName ifAbsent: [])
			ifNotNil: [:entry | aBlock value: entry ]
%

category: 'accessing'
method: MemoryFileSystemDirectory
fileEntryAt: aFileName put: anEntry
	^ entries 
		at: aFileName 
		ifAbsentPut: [
			self modified.
			anEntry ]
%

category: 'accessing'
method: MemoryFileSystemDirectory
fileEntryRemove: aFileName
	^ self fileEntryRemove: aFileName ifAbsent: [ FileDoesNotExistException signalWith: aFileName ]
%

category: 'accessing'
method: MemoryFileSystemDirectory
fileEntryRemove: aFileName ifAbsent: absentBlock
	| deletedEntry |
	deletedEntry := entries removeKey: aFileName ifAbsent: [ ^ absentBlock value ].
	modificationTime := DateAndTime now.
	^ deletedEntry
%

category: 'accessing'
method: MemoryFileSystemDirectory
fileSize
	^ 0
%

category: 'initialization'
method: MemoryFileSystemDirectory
initialize
	super initialize.
	entries := Dictionary new.
%

category: 'testing'
method: MemoryFileSystemDirectory
isDirectory
	^ true
%

! Class implementation for 'MemoryFileSystemFile'

!		Instance methods for 'MemoryFileSystemFile'

category: 'stream-protocol'
method: MemoryFileSystemFile
at: index
	^ bytes at: index
%

category: 'stream-protocol'
method: MemoryFileSystemFile
at: index put: anObject
	index > bytes size
		ifTrue: [ self grow ].
	bytes
		at: index
		put:
			(anObject isCharacter
				ifTrue: [ anObject codePoint ]
				ifFalse: [ anObject ]).
	size := size max: index.
	self modified
%

category: 'stream-protocol'
method: MemoryFileSystemFile
at: index read: aCollection startingAt: start count: count 
	| max stop |
	max := size - index + 1 min: count.
	stop := start + max - 1.
	aCollection 
		replaceFrom: start
		to: stop
		with: bytes
		startingAt: index.
	^ stop - start + 1
%

category: 'stream-protocol'
method: MemoryFileSystemFile
at: first write: aCollection startingAt: start count: count
	| last |
	last := first + count - 1.
	last > bytes size
		ifTrue: [ self grownBy: last - size ].
	bytes
		replaceFrom: first
		to: last
		with: aCollection
		startingAt: start.
	size := last.
	self modified
%

category: 'open/close'
method: MemoryFileSystemFile
basicOpen
	closed := false
%

category: 'streams-compatibility'
method: MemoryFileSystemFile
binaryReadStream
	^ ReadStreamPortable on: self bytes from: 1 to: size
%

category: 'streams'
method: MemoryFileSystemFile
binaryWriteStream
	^ MemoryFileWriteStream on: self
%

category: 'accessing'
method: MemoryFileSystemFile
bytes
	^ bytes
%

category: 'open/close'
method: MemoryFileSystemFile
close
	 closed := true
%

category: 'testing'
method: MemoryFileSystemFile
closed
	^ closed
%

category: 'stream-protocol'
method: MemoryFileSystemFile
copyFrom: from to: position
	^ bytes copyFrom: from to: position
%

category: 'accessing'
method: MemoryFileSystemFile
fileSize
	^ size
%

category: 'stream-protocol'
method: MemoryFileSystemFile
grow
	self grownBy: self sizeIncrement
%

category: 'accessing'
method: MemoryFileSystemFile
grownBy: length
	bytes := bytes grownBy: length.
	self modified
%

category: 'initialization'
method: MemoryFileSystemFile
initialize
	super initialize.
	bytes := ByteArray new.
	size := 0.
	closed := false
%

category: 'accessing'
method: MemoryFileSystemFile
internalSize
	^ bytes size
%

category: 'testing'
method: MemoryFileSystemFile
isDirectory
	^ false
%

category: 'stream-protocol'
method: MemoryFileSystemFile
readStream
	^ ReadStream on: self bytes asString from: 1 to: size
%

category: 'accessing'
method: MemoryFileSystemFile
size

	^ size
%

category: 'accessing'
method: MemoryFileSystemFile
sizeIncrement
	^ (bytes size min: 20) max: 1024
%

category: 'accessing'
method: MemoryFileSystemFile
truncate
	self truncateTo: size
%

category: 'accessing'
method: MemoryFileSystemFile
truncateTo: aSize
	bytes size = aSize
		ifFalse: [ bytes := bytes size < aSize
				ifTrue: [ (ByteArray new: aSize)
						replaceFrom: 1
						to: bytes size
						with: bytes
						startingAt: 1 ]
				ifFalse: [ bytes copyFrom: 1 to: aSize ] ].
	size := bytes size.
	self modified
%

category: 'private'
method: MemoryFileSystemFile
updateContents: aCollection
	bytes := aCollection.
	self updateSize: aCollection size
%

category: 'private'
method: MemoryFileSystemFile
updateSize: newSize
	size := newSize.
	self modified
%

category: 'stream-protocol'
method: MemoryFileSystemFile
writeStreamDo: aBlock
	ByteArray
		streamContents: [ :aStream | 
			aBlock value: aStream.
			bytes := aStream contents ].
	self updateSize: bytes size
%

! Class implementation for 'MemoryFileWriteStream'

!		Class methods for 'MemoryFileWriteStream'

category: 'instance creation'
classmethod: MemoryFileWriteStream
on: aFile

	^ self new
		file: aFile;
		yourself
%

!		Instance methods for 'MemoryFileWriteStream'

category: 'opening-closing'
method: MemoryFileWriteStream
close
	self stream close.
	file close
%

category: 'opening-closing'
method: MemoryFileWriteStream
closed
	^ file closed
%

category: 'accessing'
method: MemoryFileWriteStream
file: aMemoryFileSystemFile 
	file := aMemoryFileSystemFile
%

category: 'writing'
method: MemoryFileWriteStream
flush
	file updateContents: self stream contents
%

category: 'testing'
method: MemoryFileWriteStream
isBinary
	^ self stream isBinary
%

category: 'writing'
method: MemoryFileWriteStream
nextPut: aCollection
	^ self stream nextPut: aCollection
%

category: 'writing'
method: MemoryFileWriteStream
nextPutAll: aCollection
	^ self stream nextPutAll: aCollection
%

category: 'positioning'
method: MemoryFileWriteStream
position
	^ self stream position
%

category: 'positioning'
method: MemoryFileWriteStream
setToEnd
	^ self stream setToEnd
%

category: 'accessing'
method: MemoryFileWriteStream
size
	^ file size
%

category: 'accessing'
method: MemoryFileWriteStream
stream
	^ stream ifNil: [ stream := WriteStreamPortable on: file bytes from: 1 to: file size ]
%

! Class implementation for 'NumberParser'

!		Class methods for 'NumberParser'

category: 'testing'
classmethod: NumberParser
isNumber: aStringOrStream

	| stream |
	stream := aStringOrStream readStreamPortable.
	self parse: stream onError: [ ^ false ].
	^ stream atEnd
%

category: 'instance creation'
classmethod: NumberParser
on: aStringOrStream
	^self new on: aStringOrStream
%

category: 'instance creation'
classmethod: NumberParser
parse: aStringOrStream 
	^self new
		on: aStringOrStream;
		nextNumber
%

category: 'instance creation'
classmethod: NumberParser
parse: aStringOrStream onError: failBlock 
	^(self new)
		on: aStringOrStream;
		failBlock: failBlock;
		nextNumber
%

category: 'instance creation'
classmethod: NumberParser
squeezeNumberOutOfString: stringOrStream
	"Try and find a number in this string. First, look if the string 
	starts with a number. Then, see if it ends with a number. Then,
	remove a character from the front and see if the remaining 
	string makes a number. Repeat the process until no characters
	are left or the number has been found. As soon as a number is
	found, it is returned. Otherwise, the method fails."
	^ self squeezeNumberOutOfString: stringOrStream onError: [self error: 'Reading a number failed']
%

category: 'instance creation'
classmethod: NumberParser
squeezeNumberOutOfString: stringOrStream onError: errorBlock
	"Try and find a number in this string. First, look if the string 
	starts with a number. Then, see if it ends with a number. Then,
	remove a character from the front and see if the remaining 
	string makes a number. Repeat the process until no characters
	are left or the number has been found. As soon as a number is
	found, it is returned. Otherwise, the method fails."
	
	| string |

	string := stringOrStream.
				
	stringOrStream size timesRepeat: [
		(self parse: string onError: [ nil ])
			ifNotNil: [ :result| ^ result ].
		string := string allButFirst ].
	
	^ errorBlock value
%

!		Instance methods for 'NumberParser'

category: 'accessing'
method: NumberParser
allowPlusSign
	"return a boolean indicating if plus sign is allowed or not"

	^false
%

category: 'accessing'
method: NumberParser
allowPlusSignInExponent
	"return a boolean indicating if plus sign is allowed or not in exponent"

	^self allowPlusSign
%

category: 'error'
method: NumberParser
expected: aString 
	| errorString |
	errorString := aString , ' expected'.
	requestor isNil
		ifFalse: [requestor
				notify: errorString
				at: sourceStream position + 1
				in: sourceStream].
	failBlock ifNotNil: [^failBlock cull: errorString cull: sourceStream position + 1].
	self error: 'Reading a number failed: ' , errorString
%

category: 'accessing'
method: NumberParser
exponentLetters
	"answer the list of possible exponents for Numbers.
	Note: this parser will not honour precision attached to the exponent.
	different exponent do not lead to different precisions.
	only IEEE 754 floating point numbers will be created"
	
	^'edq'
%

category: 'error'
method: NumberParser
fail
	failBlock ifNotNil: [^failBlock value].
	self error: 'Reading a number failed'
%

category: 'accessing'
method: NumberParser
failBlock: aBlockOrNil
	failBlock := aBlockOrNil
%

category: 'parsing-private'
method: NumberParser
makeFloatFromMantissa: m exponent: k base: aRadix 
	"Convert infinite precision arithmetic into Floating point.
	This alogrithm rely on correct IEEE rounding mode
	being implemented in Integer>>asFloat and Fraction>>asFloat"

	^(k positive
		ifTrue: [m * (aRadix raisedToInteger: k)]
		ifFalse: [Fraction numerator: m denominator: (aRadix raisedToInteger: k negated)]) asFloat
%

category: 'parsing-private'
method: NumberParser
makeIntegerOrScaledIntegerOrFloat
	"at this point, there is no digit, nor fractionPart.
	maybe it can be a scaled decimal with fraction omitted...
	integer with negative exponent can be floats not fractions"
	
	neg
		ifTrue: [integerPart := integerPart negated].
	self readExponent ifTrue:
		["Check that the result is an integer, otherwise answer a Float.  Fractions are /not/ valid literals."
		 (exponent >= 0 or: [(integerPart * (base raisedToInteger: exponent)) _isInteger]) ifFalse:
			[base := base asFloat].
		^integerPart * (base raisedToInteger: exponent)].
	(self readScaleWithDefaultNumberOfDigits: 0)
		ifTrue: [^integerPart asScaledDecimal: scale].
	^ integerPart
%

category: 'parsing-private'
method: NumberParser
makeScaledDecimalWithNumberOfNonZeroFractionDigits: numberOfNonZeroFractionDigits andNumberOfTrailingZeroInFractionPart: numberOfTrailingZeroInFractionPart
	"at this point integerPart fractionPart and scale have been read out (in inst var).
	Form a ScaledDecimal.
	Care of eliminating trailing zeroes from the fractionPart"
	
	| decimalMultiplier decimalFraction |
	decimalMultiplier := base raisedToInteger: numberOfNonZeroFractionDigits.
	decimalFraction := integerPart * decimalMultiplier + (fractionPart // (base raisedTo: numberOfTrailingZeroInFractionPart)) / decimalMultiplier.
	neg
		ifTrue: [decimalFraction := decimalFraction negated].
	^decimalFraction asScaledDecimal: scale
%

category: 'parsing-large int'
method: NumberParser
nextElementaryLargeIntegerBase: aRadix
	"Form an unsigned integer with incoming digits from sourceStream.
	Return this integer, or zero if no digits found.
	Stop reading if end of digits or if a LargeInteger is formed.
	Count the number of digits and the position of lastNonZero digit and store them in instVar."

	| value digit char |
	value := 0.
	nDigits := 0.
	lastNonZero := 0.
	[value isLarge or: [(char := sourceStream next) isNil
		or: [digit := char numberParserDigitalValue.
			(0 > digit or: [digit >= aRadix])
				and: [sourceStream skip: -1.
					true]]]]
		whileFalse: [
			nDigits := nDigits + 1.
			0 = digit
				ifFalse: [lastNonZero := nDigits].
			value := value * aRadix + digit].
	^value
%

category: 'parsing-public'
method: NumberParser
nextFraction
	| numerator denominator |
	numerator := self nextInteger.
	(sourceStream peekFor: $/) ifFalse: [^numerator].
	denominator := self nextInteger.
	^numerator / denominator
%

category: 'parsing-public'
method: NumberParser
nextInteger
	"Main method for reading an Integer.
	This won't try to read a Float nor a ScaledDecimal."
	
	base := 10.
	neg := self peekSignIsMinus.
	integerPart := self nextUnsignedIntegerOrNilBase: base.
	integerPart ifNil: [self fail].
	(sourceStream peekFor: $r)
		ifTrue: ["<base>r<integer>"
			(base := integerPart) < 2
				ifTrue: ["A radix currently need to be greater than 1, ungobble the r and return the integer part"
					sourceStream skip: -1.
					^neg
						ifTrue: [base negated]
						ifFalse: [base]].
			self peekSignIsMinus ifTrue: [neg := neg not].
			integerPart := self nextUnsignedIntegerOrNilBase: base.
			integerPart ifNil: [self fail]].
	neg
		ifTrue: [integerPart := integerPart negated].
	self readExponent
		ifTrue: [^integerPart * (base raisedToInteger: exponent)].
	^ integerPart
%

category: 'parsing-public'
method: NumberParser
nextIntegerBase: aRadix
	"Form an integer with following digits.
	Fail if no digit found"
	
	| isNeg value |
	isNeg := self peekSignIsMinus.
	value := self nextUnsignedIntegerBase: aRadix.
	^isNeg
		ifTrue: [value negated]
		ifFalse: [value]
%

category: 'parsing-public'
method: NumberParser
nextIntegerBase: aRadix ifFail: aBlock
	"Form an integer with optional sign and following digits from sourceStream."
	
	| isNeg value |
	isNeg := self peekSignIsMinus.
	value := self nextUnsignedIntegerOrNilBase: aRadix.
	value ifNil: [^aBlock value].
	^isNeg
		ifTrue: [value negated]
		ifFalse: [value]
%

category: 'parsing-large int'
method: NumberParser
nextLargeIntegerBase: aRadix nPackets: nPackets 
	"Form a Large integer with incoming digits from sourceStream.
	Return this integer, or zero if no digits found.
	Stop reading when no more digits or when nPackets elementary LargeInteger have been encountered.
	Count the number of digits and the lastNonZero digit and store them in instVar"
	
	| high nDigitsHigh lastNonZeroHigh low nDigitsLow halfPackets |
	halfPackets := nPackets bitShift: -1.
	halfPackets = 0 ifTrue: [^self nextElementaryLargeIntegerBase: aRadix].
	high := self nextLargeIntegerBase: aRadix nPackets: halfPackets.
	high isLarge ifFalse: [^high].
	nDigitsHigh := nDigits.
	lastNonZeroHigh := lastNonZero.
	low := self nextLargeIntegerBase: aRadix nPackets: halfPackets.
	nDigitsLow := nDigits.
	nDigits := nDigitsHigh + nDigitsLow.
	lastNonZero := lastNonZero = 0
		ifTrue: [lastNonZeroHigh]
		ifFalse: [lastNonZero + nDigitsHigh].
	^high * (aRadix raisedToInteger: nDigitsLow) + low
%

category: 'parsing-public'
method: NumberParser
nextNumber
	"main method for reading a number.
	This one can read Float Integer and ScaledDecimal"
	
	| numberOfTrailingZeroInIntegerPart |
	base := 10.
	neg := self peekSignIsMinus.
	integerPart := self nextUnsignedIntegerOrNilBase: base.
	integerPart ifNil: [
		"This is not a regular number beginning with a digit
		It is time to check for exceptional condition NaN and Infinity"
		^self readNamedFloatOrFail].
	numberOfTrailingZeroInIntegerPart := nDigits - lastNonZero.
	(sourceStream peekFor: $r)
		ifTrue: ["<base>r<integer>"
			(base := integerPart) < 2
				ifTrue: [
					sourceStream skip: -1.
					^ self expected: 'an integer greater than 1 as valid radix'].
			self peekSignIsMinus
				ifTrue: [neg := neg not].
			integerPart := self nextUnsignedIntegerBase: base.
			numberOfTrailingZeroInIntegerPart := nDigits - lastNonZero].
	^ (sourceStream peekFor: $.)
		ifTrue: [self readNumberWithFractionPartNumberOfTrailingZeroInIntegerPart: numberOfTrailingZeroInIntegerPart]
		ifFalse: [self makeIntegerOrScaledIntegerOrFloat]
%

category: 'parsing-public'
method: NumberParser
nextNumberBase: b
	"Method for reading a number without radix prefix.
	This one can read Float Integer and ScaledDecimal"
	
	| numberOfTrailingZeroInIntegerPart |
	base := b.
	neg := sourceStream peekFor: $-.
	integerPart := self nextUnsignedIntegerOrNilBase: base.
	integerPart ifNil: [
		"This is not a regular number beginning with a digit
		It is time to check for exceptional condition NaN and Infinity"
		^self readNamedFloatOrFail].
	numberOfTrailingZeroInIntegerPart := nDigits - lastNonZero.
	^ (sourceStream peekFor: $.)
		ifTrue: [self readNumberWithFractionPartNumberOfTrailingZeroInIntegerPart: numberOfTrailingZeroInIntegerPart]
		ifFalse: [self makeIntegerOrScaledIntegerOrFloat]
%

category: 'parsing-public'
method: NumberParser
nextScaledDecimal
	"Main method for reading a (scaled) decimal number.
	Good Gracious, do not accept a decimal in another base than 10!
	In other words, do not accept radix notation like 2r1.1, even not 10r5.3
	Do not accept exponent notation neither, like 1.0e-3"
	
	| numberOfNonZeroFractionDigits numberOfTrailingZeroInFractionPart |
	base := 10.
	neg := sourceStream peekFor: $-.
	integerPart := self nextUnsignedIntegerBase: base.
	(sourceStream peekFor: $.)
		ifTrue: [fractionPart := self nextUnsignedIntegerOrNilBase: base.
			fractionPart ifNil: ["Oops, the decimal point seems not part of this number"
							sourceStream skip: -1.
							^ neg
								ifTrue: [integerPart negated asScaledDecimal: 0]
								ifFalse: [integerPart asScaledDecimal: 0]].
			numberOfNonZeroFractionDigits := lastNonZero.
			numberOfTrailingZeroInFractionPart := nDigits - lastNonZero.
			(self readScaleWithDefaultNumberOfDigits: nDigits)
				ifFalse: ["No scale were provided. use number of digits after decimal point as scale"
					scale := nDigits].
			^self makeScaledDecimalWithNumberOfNonZeroFractionDigits: numberOfNonZeroFractionDigits andNumberOfTrailingZeroInFractionPart: numberOfTrailingZeroInFractionPart].
	self readScaleWithDefaultNumberOfDigits: 0.
	neg	ifTrue: [integerPart := integerPart negated].
	^integerPart asScaledDecimal: scale
%

category: 'parsing-public'
method: NumberParser
nextUnsignedIntegerBase: aRadix 
	"Form an unsigned integer with incoming digits from sourceStream.
	Fail if no digit found.
	Count the number of digits and the lastNonZero digit and store int in instVar "
	
	| value |
	value := self nextUnsignedIntegerOrNilBase: aRadix.
	value ifNil: [^self expected: ('a digit between 0 and ' copyWith: (Character digitValue: aRadix - 1))].
	^value
%

category: 'parsing-public'
method: NumberParser
nextUnsignedIntegerBase: aRadix ifFail: errorBlock
	"Form an unsigned integer with incoming digits from sourceStream.
	Answer this integer, or execute errorBlock if no digit found.
	Count the number of digits and the position of lastNonZero digit and store them in instVar"
	
	| value |
	value := self nextUnsignedIntegerOrNilBase: aRadix.
	value ifNil: [^errorBlock value].
	^value
%

category: 'parsing-public'
method: NumberParser
nextUnsignedIntegerOrNilBase: aRadix
	"Form an unsigned integer with incoming digits from sourceStream.
	Answer this integer, or nil if no digit found.
	Count the number of digits and the position of lastNonZero digit and store them in instVar"
	
	| nPackets high nDigitsHigh lastNonZeroHigh low |
	"read no more digits than one elementary LargeInteger"
	high :=  self nextElementaryLargeIntegerBase: aRadix.
	nDigits = 0 ifTrue: [^nil].
	
	"Not enough digits to form a LargeInteger, stop iteration"
	high isLarge ifFalse: [^high].

	"We now have to engage arithmetic with LargeInteger
	Decompose the integer in a high and low packets of growing size:"
	nPackets := 1.
	nDigitsHigh := nDigits.
	lastNonZeroHigh := lastNonZero.
	[
	low := self nextLargeIntegerBase: aRadix nPackets: nPackets .
	high := high * (aRadix raisedToInteger: nDigits) + low.
	lastNonZero = 0 ifFalse: [lastNonZeroHigh := lastNonZero + nDigitsHigh].
	nDigitsHigh := nDigitsHigh + nDigits.
	low isLarge]
		whileTrue: [nPackets := nPackets * 2].

	nDigits := nDigitsHigh.
	lastNonZero := lastNonZeroHigh.
	^high
%

category: 'initialize-release'
method: NumberParser
on: aStringOrStream 
	sourceStream := aStringOrStream isString 
		ifTrue: [ aStringOrStream readStreamPortable ]
		ifFalse: [ aStringOrStream ].
	base := 10.
	neg := false.
	integerPart := fractionPart := exponent := scale := 0.
	requestor := failBlock := nil
%

category: 'parsing-private'
method: NumberParser
peekSignIsMinus
	"Peek an optional sign from sourceStream.
	Answer true if it is minus sign"

	| isMinus |
	isMinus := sourceStream peekFor: $-.
	isMinus ifFalse: [self allowPlusSign ifTrue: [sourceStream peekFor: $+]].
	^isMinus
%

category: 'parsing-private'
method: NumberParser
readExponent
	"read the exponent if any (stored in instVar).
	Answer true if found, answer false if none.
	If exponent letter is not followed by a digit,
	this is not considered as an error.
	Exponent are always read in base 10."

	| eneg epos |
	exponent := 0.
	sourceStream atEnd
		ifTrue: [ ^ false ].
	(self exponentLetters includes: sourceStream peek)
		ifFalse: [ ^ false ].
	sourceStream next.
	eneg := sourceStream peekFor: $-.
	epos := eneg not
		and: [ self allowPlusSignInExponent and: [ sourceStream peekFor: $+ ] ].
	exponent := self nextUnsignedIntegerOrNilBase: 10.
	exponent
		ifNil:
			[ "Oops, there was no digit after the exponent letter.Ungobble the letter"
			exponent := 0.
			sourceStream
				skip:
					((eneg or: [ epos ])
						ifTrue: [ -2 ]
						ifFalse: [ -1 ]).
			^ false ].
	eneg
		ifTrue: [ exponent := exponent negated ].
	exponent abs > 1023
		ifTrue:
			[ "Number literals should have a maximun of 1023 and a minumum of -1022 as exponent, in exponential notation"
			self fail ].
	^ true
%

category: 'parsing-private'
method: NumberParser
readNamedFloatOrFail
	"This method is used when there is no digit encountered:
	It try and read a named Float NaN or Infinity.
	Negative sign for -Infinity has been read before sending this method, and is indicated in the neg inst.var.
	Fail if no named Float is found"
		
	neg ifFalse: [(sourceStream nextMatchAll: 'NaN')
			ifTrue: [^ Float nan]].
	(sourceStream nextMatchAll: 'Infinity')
		ifTrue: [^ neg
			ifTrue: [Float infinity negated]
			ifFalse: [Float infinity]].
	^self expected: 'a digit between 0 and ' , (String with: (Character digitValue: base - 1))
%

category: 'parsing-private'
method: NumberParser
readNumberWithFractionPartNumberOfTrailingZeroInIntegerPart: numberOfTrailingZeroInIntegerPart
	"at this stage, sign integerPart and a decimal point have been read.
	try and form a number with a fractionPart"
	
	| numberOfNonZeroFractionDigits numberOfTrailingZeroInFractionPart mantissa value |
	fractionPart := self nextUnsignedIntegerOrNilBase: base.
	fractionPart ifNil: ["No fractionPart found,ungobble the decimal point and return the integerPart"
					sourceStream skip: -1.
					^ neg
						ifTrue: [integerPart negated]
						ifFalse: [integerPart]].
	numberOfNonZeroFractionDigits := lastNonZero.
	numberOfTrailingZeroInFractionPart := nDigits - lastNonZero.
	self readExponent
		ifFalse: [(self readScaleWithDefaultNumberOfDigits: nDigits)
				ifTrue: [^self makeScaledDecimalWithNumberOfNonZeroFractionDigits: numberOfNonZeroFractionDigits
					andNumberOfTrailingZeroInFractionPart: numberOfTrailingZeroInFractionPart]].

	fractionPart = 0
		ifTrue: [mantissa := integerPart
						// (base raisedToInteger: numberOfTrailingZeroInIntegerPart).
			exponent := exponent + numberOfTrailingZeroInIntegerPart]
		ifFalse: [mantissa := integerPart
						* (base raisedToInteger: numberOfNonZeroFractionDigits) + (fractionPart // (base raisedToInteger: numberOfTrailingZeroInFractionPart)).
			exponent := exponent - numberOfNonZeroFractionDigits].

	value := self makeFloatFromMantissa: mantissa exponent: exponent base: base.
	^ neg
		ifTrue: [value = 0
				ifTrue: [Float negativeZero]
				ifFalse: [value negated]]
		ifFalse: [value]
%

category: 'parsing-private'
method: NumberParser
readScaleWithDefaultNumberOfDigits: anInteger
	"Read the scale if any and store it into scale instance Variable.
	Answer true if found, answer false if none.
	The scale is specified by letter s, optionnally followed by a positive integer in base 10.
	If no integer is specified, that means using as many digits as provided after the fraction separator, as provided by parameter anInteger.
	A letter s followed by another letter is not considered as a scale specification, because it could be part of a message."

	scale := 0.
	sourceStream atEnd
		ifTrue: [ ^ false ].
	(sourceStream peekFor: $s)
		ifFalse: [ ^ false ].
	scale := self nextUnsignedIntegerOrNilBase: 10.
	scale
		ifNil: [ 
			scale := anInteger.
			(sourceStream peek ifNil: [ false ] ifNotNil: [ :nextChar | nextChar isLetter ])
				ifTrue: [ 
					sourceStream skip: -1.	"ungobble the s"
					^ false ]
				ifFalse: [ ^ true ] ].
	^ true
%

category: 'accessing'
method: NumberParser
requestor: anObjectOrNil
	requestor := anObjectOrNil
%

! Class implementation for 'Path'

!		Class methods for 'Path'

category: 'instance creation'
classmethod: Path
* aString
	"Answer a relative path with aString as its sole segment. For example,
	Path * 'griffle' will produce the same result as ./griffle in a unix shell.
	The selector #* was chosen for it's visual similarity to $."
	
	"Note: aString is not parsed, so supplying a string like '/griffle/plonk'
	will not create an absolute path."
	
	^ RelativePath with: aString
%

category: 'instance creation'
classmethod: Path
/ aString
	"Answer an absolute path with aString as it's sole segment. The selector
	was chosen to allow path construction with Smalltalk syntax, which 
	neverthelesss resembles paths as they appear in a unix shell. Eg.
	Path / 'griffle' / 'plonk'."
	
	aString isEmptyOrNil 
		ifTrue: [ Error signal: 'Path element cannot be empty or nil'].
	^ AbsolutePath with: aString
%

category: 'private'
classmethod: Path
addElement: element to: result
	element = '..'
		ifTrue: [ ^ self addParentElementTo: result ].
	element = ''
		ifTrue: [ ^ self addEmptyElementTo: result ].
	element = '.'
		ifFalse: [ result add: element ]
%

category: 'private'
classmethod: Path
addEmptyElementTo: result
	result isEmpty ifTrue: [result add: '']
		
%

category: 'private'
classmethod: Path
addParentElementTo: result
	(result isEmpty or: [ result last = '..' ])
		ifTrue: [ result add: '..' ]
		ifFalse: [ result removeLast ]
		
%

category: 'private'
classmethod: Path
canonicalizeElements: aCollection
	| result |
	result := OrderedCollection new.
	aCollection do: [ :element |
		self addElement: element to: result].
	^ result
%

category: 'encodings'
classmethod: Path
delimeter
	"Internal path delimeter"
	
	^$/
%

category: 'encodings'
classmethod: Path
extensionDelimiter
	"Return the extension delimiter character."
	^ $.
%

category: 'instance creation'
classmethod: Path
from: aString
	"Answer an instance of the receiver with the supplied path using the default delimiter"

	^ self from: aString delimiter: self delimeter
%

category: 'instance creation'
classmethod: Path
from: aString delimiter: aDelimiterCharacter 
	"Answer a path composed of several elements delimited by aCharacter"
	| pathClass splitPathElements |
	aString isEmpty
		ifTrue: [ ^ self root ].

	aString first = $$
		ifTrue: [
			| pathElements envVarString envVarElement |
			"GemStone paths are allowed to start with an environment variable"
			pathElements := aDelimiterCharacter split: aString.
			envVarElement := (pathElements at: 1) .
			envVarString := (System gemEnvironmentVariable: (envVarElement copyFrom: 2 to: envVarElement size)) decodeFromUTF8 asString.
			pathClass :=  ((self isAbsolutePath: envVarString delimiter: aDelimiterCharacter) or: 
									[self isAbsoluteWindowsPath: envVarString]) 
				ifTrue: [ AbsolutePath ]
				ifFalse:[ RelativePath ].
			splitPathElements :=  (aDelimiterCharacter split: envVarString) , (pathElements copyFrom: 2 to: pathElements size) ]
		ifFalse: [ 	
			pathClass :=  ((self isAbsolutePath: aString delimiter: aDelimiterCharacter) or: 
									[self isAbsoluteWindowsPath: aString]) 
				ifTrue: [ AbsolutePath ]
				ifFalse:[ RelativePath ] .
			splitPathElements := aDelimiterCharacter split: aString. ].
	
	^ pathClass withAll: splitPathElements
%

category: 'private'
classmethod: Path
isAbsolutePath: aString delimiter: aCharacter
	"Answer a boolean indicating whether the supplied path is considered absolute"

	^aString first = aCharacter
%

category: 'private'
classmethod: Path
isAbsoluteUnixPath: aString
	^aString first = $/ 
%

category: 'private'
classmethod: Path
isAbsoluteWindowsPath: aString
	aString ifEmpty: [ ^ false ].
	(aString first = $\) ifTrue: [ ^ true ]. "e.g. \file"
	^ ((aString size > 2) and: [ aString first isLetter ])
		ifTrue: [ (aString second = $:) and: [aString third = $\] ] "e.g. C:\..."
		ifFalse: [ false ]
%

category: 'instance creation'
classmethod: Path
parent
	"Answer a path that resolves to the parent of the current
	working directory. This is similar to .. in unix, but doesn't
	rely on actual hardlinks being present in the filesystem."

	^ RelativePath with: '..'
%

category: 'instance creation'
classmethod: Path
parents: anInteger
	| path |
	path := self new: anInteger.
	1 to: anInteger do: [:i | path at: i put: '..'].
	^ path
%

category: 'private'
classmethod: Path
removeRedundantSegments: segments
	"Remove redundant elements ('.' and '') from the supplied segments"

	^segments select:
		[ :segment | segment notEmpty and: [
				segment ~= (segment isUnicodeString ifTrue: ['.' asUnicodeString] ifFalse: ['.'])] ]
%

category: 'instance creation'
classmethod: Path
root
	"Answer the root path - ie, / on unix"
	
	^ AbsolutePath new
%

category: 'private'
classmethod: Path
with: aString
	"Answer an instance of the receiver representing the supplied string.
	This should only be sent to one of the concrete subclasses, e.g. AbsolutePath and RelativePath"
	| inst parsedCollection |

	parsedCollection := self delimeter split: aString.
	parsedCollection := self removeRedundantSegments: parsedCollection.
	inst := self new: parsedCollection size.
	parsedCollection withIndexDo: [:segment :index |
		inst at: index put: segment].
	^ inst
%

category: 'private'
classmethod: Path
withAll: aCollection
	"Answer an instance of the receiver representing the supplied collection of strings.
	This should only be sent to one of the concrete subclasses, e.g. AbsolutePath and RelativePath"
	| inst parsedCollection |

	parsedCollection := OrderedCollection new: (aCollection size max: 10).
	aCollection do: [ :each |
		parsedCollection addAll: (self delimeter split: each) ].
	parsedCollection := self removeRedundantSegments: parsedCollection.
	inst := self new: parsedCollection size.
	parsedCollection withIndexDo: [:segment :index |
		inst at: index put: segment].
	^ inst
%

category: 'instance creation'
classmethod: Path
workingDirectory
	"Answer a path that will always resolve to the current working directory."
	
	^ RelativePath new
%

!		Instance methods for 'Path'

category: 'navigating'
method: Path
, extension 
	^ self withName: self basename extension: extension
%

category: 'navigating'
method: Path
/ aString
	"aString is either a file or path.  If aString is relative, it is appended to the receiver, if it is absolute, an instance of the receiver with the path is answered"

	aString isEmptyOrNil 
		ifTrue: [ Error signal: 'Path element cannot be empty or nil'].

	^self resolvePath: (self class from: aString)
%

category: 'comparing'
method: Path
<= other
	^ self fullName <= other fullName
%

category: 'comparing'
method: Path
= other
	^ self species = other species
		and: [self size = other size
			and: [(1 to: self size) allSatisfy: [:i | (self at: i) = (other at: i)]]]
%

category: 'converting'
method: Path
asPath
	^ self
%

category: 'converting'
method: Path
asPathWith: anObject
	^ self
%

category: 'navigating'
method: Path
asResolvedBy: anObject
	^ anObject resolvePath: self
%

category: 'accessing'
method: Path
base
	^self basenameWithoutExtension 
%

category: 'accessing'
method: Path
basename
	"Returns the base of the basename, 
		i.e. 
		/foo/gloops.taz basename is 'gloops.taz'
		If empty, it is the emptyPathString"

	self isEmpty ifTrue: 
		[ ^ self emptyPathString ].
	^ self at: self size
%

category: 'accessing'
method: Path
basename: newBasename
	"change the basename"
	self size == 0
		"the root node"
		ifTrue: [ ^ Error signal: '0 length Path, cannot change basename'].
	self at: self size put: newBasename
%

category: 'accessing'
method: Path
basenameWithoutExtension
	"Returns the base of the basename but without its extension, 
		i.e. 
		/foo/gloops.taz basenameWithoutExtension is 'gloops'
		/ basenameWithoutExtension is '/'"
	
	^ self basename copyUpToLast: self extensionDelimiter
%

category: 'accessing'
method: Path
basenameWithoutExtension: anExtension
	"Returns the basename without specified extension (if any)
	('/foo/gloops.taz' asPath basenameWithoutExtension: 'taz') = 'gloops'
	"
	| extensionWithDelimiter |

	extensionWithDelimiter := anExtension copyWithFirst: self extensionDelimiter.
	(self basename endsWith: extensionWithDelimiter)
		ifTrue: [^ self basename allButLast: extensionWithDelimiter size]
		ifFalse: [ ^ self basename ]
%

category: 'navigating'
method: Path
canonicalize
	"Answer the receiver with references to the current folder (.) and parent folder (..) removed"
	
	^self class withAll: (self class canonicalizeElements: self segments).
%

category: 'comparing'
method: Path
contains: anObject
	"Return true if anObject is in a subfolder of me"
	^ anObject isContainedBy: self
%

category: 'comparing'
method: Path
containsPath: aPath
	self size < aPath size ifFalse: [^ false].
	1	to: self size 
		do: [:i | (self at: i) = (aPath at: i) ifFalse: [^ false]].
	^ true
%

category: 'comparing'
method: Path
containsReference: aReference
	^ false
%

category: 'private'
method: Path
copyFrom: aPath
	| size |
	size := aPath size min: self size.
	1 to: size do: [:i | self at: i put: (aPath at: i)].
	
%

category: 'accessing'
method: Path
delimiter
	^ $/
%

category: 'enumerating'
method: Path
do: aBlock 
	1 
		to: self size
		do: 
			[ :index || segment |
			segment := self at: index.
			segment isEmpty ifFalse: [ aBlock value: segment ] ]
%

category: 'accessing'
method: Path
emptyPathString
	"Answer the string representing an empty (size = 0) instance of the receiver"

	^self delimiter asString
%

category: 'accessing'
method: Path
extension
	"Return the extension of path basename i.e., /foo/gloops.taz extension is 'taz'"
	
	^ self basename copyAfterLast: self extensionDelimiter
%

category: 'accessing'
method: Path
extensionDelimiter
	^ self class extensionDelimiter
%

category: 'accessing'
method: Path
extensions
	"Return the extensions of the receiver in order of appearance"
	"(Path from: '/foo/bar.tar.gz') extensions"
	^ (self extensionDelimiter split: self basename) allButFirst
%

category: 'accessing'
method: Path
fullName
	"Return the fullName of the receiver."
	
	^ self pathString
%

category: 'comparing'
method: Path
hash

"Returns a numeric hash key for the receiver."

| mySize interval hashValue |

(mySize := self size) == 0
  ifTrue: [ ^15243 ].

"Choose an interval so that we sample at most 5 elements of the receiver"
interval := ((mySize - 1) // 4) max: 1.

hashValue := 4459.
1 to: mySize by: interval do: [ :i | | anElement |
  anElement := self at: i.
  hashValue := (hashValue bitShift: -1) bitXor: anElement hash.
  ].

^ hashValue abs
%

category: 'testing'
method: Path
isAbsolute
	self subclassResponsibility 
%

category: 'private'
method: Path
isAllParents
	1 to: self size do: [:i | (self at: i) = '..' ifFalse: [^ false]].
	^ true
%

category: 'comparing'
method: Path
isChildOf: anObject
	^ self parent = anObject
%

category: 'comparing'
method: Path
isContainedBy: anObject
	"DoubleDispatch helper for #contains:"
	^ anObject containsPath: self
%

category: 'testing'
method: Path
isEmpty
	^ self size = 0
%

category: 'testing'
method: Path
isRelative
	^ self isAbsolute not
%

category: 'testing'
method: Path
isRoot
	self subclassResponsibility 
%

category: 'testing'
method: Path
isWorkingDirectory
	^ self size = 0
%

category: 'private'
method: Path
lengthOfStemWith: aPath
	| limit index |
	limit := self size min: aPath size.
	index := 1.
	[index <= limit and: [(self at: index) = (aPath at: index)]] 
		whileTrue: [index := index + 1].
	^ index - 1
%

category: 'navigating'
method: Path
makeRelative: anObject
	^ anObject relativeToPath: self
%

category: 'navigating'
method: Path
parent
	| size parent |
	self isRoot ifTrue: [^ self].
	self isAllParents ifTrue: [^ self / '..'].
	
	size := self size - 1.
	parent := self class new: size.
	1 to: size do: [:i | parent at: i put: (self at: i)].
	^ parent
%

category: 'navigating'
method: Path
parentUpTo: aParentDirName
	"Answers the path of the parent dir with name aParentDirName or root if not found."

	self withParents
		reverseDo: [ :dir | 
			dir basename = aParentDirName
				ifTrue: [ ^ dir ] ].
	^ Path root
%

category: 'printing'
method: Path
pathString
	"Return a string containing the path elements of the receiver, without the 'Path *' part"

	"((FileSystem workingDirectory / 'book-result' / 'W01-Welcome')
		relativeToReference: FileSystem workingDirectory) pathString
	>>> 'book-result/W01-Welcome'"

	^String streamContents: [ :stream | 
		self printPathOn: stream delimiter: self delimiter ]
%

category: 'printing'
method: Path
printOn: aStream 
	self printOn: aStream delimiter: self delimiter.
%

category: 'printing'
method: Path
printOn: aStream delimiter: aCharacter
	(1 to: self size)
		do: [:index | aStream nextPutAll: (self at: index)]
		separatedBy: [aStream nextPut: aCharacter]
%

category: 'printing'
method: Path
printPathOn: aStream
	"Print the receiver's path on aStream (without 'Path' prepended) using the default delimiter"
	"String streamContents: [ :str| 
		((FileSystem workingDirectory / 'book-result' / 'W01-Welcome') 
			relativeToReference: FileSystem workingDirectory) printPathOn: str]
	>>> 'book-result/W01-Welcome'"

	self printPathOn: aStream delimiter: self delimiter.
%

category: 'printing'
method: Path
printPathOn: aStream delimiter: aCharacter
	"Print the receiver's path on aStream (without 'Path' prepended)"
	"String streamContents: [ :str| 
		((FileSystem workingDirectory / 'book-result' / 'W01-Welcome')
			relativeToReference: FileSystem workingDirectory) printPathOn: str delimiter: $|]
	>>> 'book-result|W01-Welcome'"

	(1 to: self size)
		do: [:index | aStream nextPutAll: (self at: index)]
		separatedBy: [aStream nextPut: aCharacter]
%

category: 'printing'
method: Path
printWithDelimiter: aCharacter
	^ String streamContents: [:out | self printOn: out delimiter: aCharacter]
%

category: 'navigating'
method: Path
relativeTo: anObject
	^ anObject makeRelative: self
%

category: 'navigating'
method: Path
relativeToPath: aPath
	"Return the receiver as relative to the argument aPath"

	"(Path / 'griffle' / 'plonk' / 'nurp') 
		relativeToPath: (Path / 'griffle') 
			returns  plonk/nurp"

	| prefix relative |
	prefix := self lengthOfStemWith: aPath.
	relative := RelativePath parents: aPath size - prefix.
	prefix + 1 to: self size do: [ :i | relative := relative / (self at: i) ].
	^ relative
%

category: 'navigating'
method: Path
resolve
	^ self
%

category: 'navigating'
method: Path
resolve: anObject
	"Return a path in which the argument has been interpreted in the context of the receiver. Different 
	argument types have different resolution semantics, so we use double dispatch to resolve them correctly."
	
	^ anObject asResolvedBy: self
%

category: 'navigating'
method: Path
resolvePath: aPath
	"Answers a path created by resolving the argument against the receiver.
	If the argument is abolute answer the argument itself. Otherwise, concatenate the
	two paths."

	| elements |

	aPath isAbsolute ifTrue: [^ aPath].
	elements := Array new: self size + aPath size.

	1 to: self size do: [:i | elements at: i put: (self at: i)].
	1 to: aPath size do: [:i | elements at: self size + i put: (aPath at: i)].
	
	^ self class withAll: elements
%

category: 'navigating'
method: Path
resolveReference: aReference
	^ aReference
%

category: 'navigating'
method: Path
resolveString: aString
	"Treat strings as relative paths with a single element."
	
	^ self / aString
%

category: 'accessing'
method: Path
segments
	"return an array with all the path segements separated"
	| segments |
	
	segments := OrderedCollection new.
	
	self do: [ :part|
		segments add: part
	].
	
	^ segments asArray 
%

category: 'navigating'
method: Path
withExtension: extension 
	| basename name |
	basename := self basename.
	^ (basename endsWith: extension) 
		ifTrue: [ self ]
		ifFalse: 
			[name := basename copyUpToLast: self extensionDelimiter.
			self withName: name extension: extension]
%

category: 'private'
method: Path
withName: name extension: extension
	| basename |
	basename :=String streamContents:
		[:out |
		out nextPutAll: name.
		out nextPut: self extensionDelimiter.
		out nextPutAll: extension].
	^ self copy 
		at: self size put: basename;
		yourself
%

category: 'enumerating'
method: Path
withParents
	| paths |
	paths := OrderedCollection new.
	1 to: self size -1 do: [ :index | paths add: ((self class new: index) copyFrom: self) ].
	paths add: self.
	
	^ paths
%

! Class implementation for 'AbsolutePath'

!		Class methods for 'AbsolutePath'

category: 'as yet unclassified'
classmethod: AbsolutePath
addEmptyElementTo: result
%

category: 'as yet unclassified'
classmethod: AbsolutePath
from: aString delimiter: aDelimiterCharater
	aString = '/'
		ifTrue: [ ^ self root ].
		
	^ super from: aString delimiter: aDelimiterCharater
%

!		Instance methods for 'AbsolutePath'

category: 'printing'
method: AbsolutePath
asString
  "used by topaz stack display"
  | str sz |
  str := '/' copy .
  1 to: (sz := self size) - 1 do:[:j | str addAll: (self at: j) ; add: $/ ].
  str add: (self at: sz ).
  ^ str
%

category: 'testing'
method: AbsolutePath
isAbsolute
	^ true
%

category: 'testing'
method: AbsolutePath
isRoot
	^ self size = 0
%

category: 'printing'
method: AbsolutePath
printOn: aStream
	aStream nextPutAll: 'Path'.
	self isRoot
		ifTrue: [aStream nextPutAll: ' root']
		ifFalse:
			[1 to: self size do:
				[:i |
				aStream 
					nextPutAll: ' / ''';
				 	nextPutAll: (self at: i);
					nextPut: $']]
%

category: 'printing'
method: AbsolutePath
printPathOn: aStream delimiter: aCharacter
	"Print the path elements of the receiver, without the 'Path *' part"

	aStream nextPut: aCharacter.
	super printPathOn: aStream delimiter: aCharacter
%

category: 'enumerating'
method: AbsolutePath
withParents
	^ super withParents addFirst: (Path root); yourself
%

! Class implementation for 'RelativePath'

!		Instance methods for 'RelativePath'

category: 'accessing'
method: RelativePath
emptyPathString
	"Answer the string representing an empty (size = 0) instance of the receiver.
	For a relative path, this is the current directory"

	^'.'
%

category: 'testing'
method: RelativePath
isAbsolute
	^ false
%

category: 'testing'
method: RelativePath
isRoot
	^ false
%

category: 'printing'
method: RelativePath
printOn: aStream
	aStream nextPutAll: 'Path '.
	self isWorkingDirectory
		ifTrue: [aStream nextPutAll: 'workingDirectory']
		ifFalse: 
			[aStream 
				nextPutAll: '* ''';
				nextPutAll: (self at: 1) asString;
				nextPut: $'.
			2 to: self size do:
				[:i |
				aStream
					nextPutAll: ' / ''';
					nextPutAll: (self at: i);
					nextPut: $']]
					
			
%

category: 'printing'
method: RelativePath
printOn: aStream delimiter: aCharacter
	self isWorkingDirectory ifTrue: [aStream nextPut: $.. ^ self].
	super printOn: aStream delimiter: aCharacter
%

! Class implementation for 'PosixErrorGroup'

!		Class methods for 'PosixErrorGroup'

category: 'constants'
classmethod: PosixErrorGroup
ioFileErrorClass
	^FileIOError
%

category: 'instance creation'
classmethod: PosixErrorGroup
new
	^self basicNew
		initialize;
		yourself
%

category: 'constants'
classmethod: PosixErrorGroup
posixErrorNames
	^#()
%

!		Instance methods for 'PosixErrorGroup'

category: 'accessing'
method: PosixErrorGroup
errorClasses
	^errorClasses
%

category: 'accessing'
method: PosixErrorGroup
errorClasses: aCollection
	errorClasses := aCollection
%

category: 'errors - default'
method: PosixErrorGroup
errorClassForDefaultPlatformError: anErrorNumber fileReference: aFileReference

	^self errorClassForErrorNumber: anErrorNumber
%

category: 'errors - default'
method: PosixErrorGroup
errorClassForDefaultPlatformError: anErrorNumber fileReference: aFileReference options: aFileOptions

	^self errorClassForErrorNumber: anErrorNumber
%

category: 'errors - default'
method: PosixErrorGroup
errorClassForDefaultPlatformError: anErrorNumber newFileReference: aNewFileReference oldFileReference: anOldFileReference

	^self errorClassForErrorNumber: anErrorNumber
%

category: 'private'
method: PosixErrorGroup
errorClassForErrorNumber: anErrorNumber
	^self errorClasses detect: [:ea | ea defaultErrorNumber = anErrorNumber] ifNone: [self error: 'Unexpected Posix Error was discoverd']
%

category: 'api'
method: PosixErrorGroup
errorClassForErrorNumber: anErrorNumber fileReference: aFileReference

	^aFileReference store errorClassForErrorNumber: anErrorNumber fileReference: aFileReference errorGroup: self
%

category: 'api'
method: PosixErrorGroup
errorClassForErrorNumber: anErrorNumber fileReference: aFileReference options: aFileOptions

	^aFileReference store errorClassForErrorNumber: anErrorNumber fileReference: aFileReference  options: aFileOptions errorGroup: self
%

category: 'api'
method: PosixErrorGroup
errorClassForErrorNumber: anErrorNumber newFileReference: aNewFileReference oldFileReference: anOldFileReference

	^aNewFileReference store errorClassForErrorNumber: anErrorNumber newFileReference: aNewFileReference oldFileReference: anOldFileReference errorGroup: self
%

category: 'errors - windows'
method: PosixErrorGroup
errorClassForWindowsPlatformError: anErrorNumber fileReference: aFileReference

	self subclassResponsibility
%

category: 'errors - windows'
method: PosixErrorGroup
errorClassForWindowsPlatformError: anErrorNumber fileReference: aFileReference options: fileOptions

	self subclassResponsibility
%

category: 'errors - windows'
method: PosixErrorGroup
errorClassForWindowsPlatformError: anErrorNumber newFileReference: aNewFileReference oldFileReference: anOldFileReference

	self subclassResponsibility
%

category: 'private'
method: PosixErrorGroup
errorClassWithPosixName: aPosixName
	^self errorClasses detect: [:ea | ea posixName = aPosixName] ifNone: [nil]
%

category: 'api'
method: PosixErrorGroup
errorForNumber: anErrorNumber fileReference: aFileReference
	| aFilePosixErrorClass |
	aFilePosixErrorClass := self errorClassForErrorNumber: anErrorNumber fileReference: aFileReference.
	^(aFilePosixErrorClass reference: aFileReference)
		platformErrorNumber: anErrorNumber;
		errorGroupName: self class name asString;
		yourself
%

category: 'api'
method: PosixErrorGroup
errorForNumber: anErrorNumber fileReference: aFileReference options: aFileOptions
	| aFilePosixErrorClass |
	aFilePosixErrorClass := self errorClassForErrorNumber: anErrorNumber fileReference: aFileReference options: aFileOptions.
	^(aFilePosixErrorClass reference: aFileReference)
		platformErrorNumber: anErrorNumber;
		errorGroupName: self class name asString;
		options: aFileOptions
%

category: 'api'
method: PosixErrorGroup
errorForNumber: anErrorNumber newFileReference: aNewFileReference oldFileReference: anOldFileReference
	| aFilePosixErrorClass |
	aFilePosixErrorClass := self errorClassForErrorNumber: anErrorNumber newFileReference: aNewFileReference oldFileReference: anOldFileReference.
	^(aFilePosixErrorClass reference: aNewFileReference)
		platformErrorNumber: anErrorNumber;
		sourceReference: anOldFileReference;
		errorGroupName: self class name asString;
		yourself
%

category: 'initialization'
method: PosixErrorGroup
initialize
	self initializeErrorClasses
%

category: 'initialization'
method: PosixErrorGroup
initializeErrorClasses
	errorClasses := OrderedCollection new.
	self class posixErrorNames do: [:ea | self errorClasses add: (FilePosixError classWithPosixName: ea)]
%

category: 'constants'
method: PosixErrorGroup
ioFileErrorClass
	^self class ioFileErrorClass
%

category: 'private'
method: PosixErrorGroup
posixErrorNamed: aPosixName
	^self errorClassWithPosixName: aPosixName
%

category: 'constants'
method: PosixErrorGroup
posixErrorNames
	^self class posixErrorNames
%

category: 'initialization'
method: PosixErrorGroup
selector: aSelector errorNames: posixErrorNames

	self selector: aSelector.
	posixErrorNames collect: [:ea | self errorClasses add: (FilePosixError classWithPosixName: ea)]
%

category: 'api'
method: PosixErrorGroup
signalErrorNumber: anErrorNumber fileReference: aFileReference
	(self errorForNumber: anErrorNumber fileReference: aFileReference) signal
%

category: 'api'
method: PosixErrorGroup
signalErrorNumber: anErrorNumber fileReference: aFileReference options: aFileOptions
	(self errorForNumber: anErrorNumber fileReference: aFileReference options: aFileOptions) signal
%

category: 'api'
method: PosixErrorGroup
signalErrorNumber: anErrorNumber newFileReference: aNewFileReference oldFileReference: anOldFileReference
	(self errorForNumber: anErrorNumber  newFileReference: aNewFileReference oldFileReference: anOldFileReference) signal
%

! Class implementation for 'ChangeDirectoryErrorGroup'

!		Class methods for 'ChangeDirectoryErrorGroup'

category: 'constants'
classmethod: ChangeDirectoryErrorGroup
posixErrorNames
	^#('EACCES' 'ENOENT' 'EIO')
%

!		Instance methods for 'ChangeDirectoryErrorGroup'

category: 'errors - windows'
method: ChangeDirectoryErrorGroup
errorClassForWindowsPlatformError: anErrorNumber fileReference: aFileReference

	((#(161 2 3 5) includes: anErrorNumber) and: [aFileReference isDirectory not])
		ifTrue: [^self posixErrorNamed: 'ENOENT'].
	((#(0 3 123 267 87) includes: anErrorNumber) and: [aFileReference isDirectory])
		ifTrue: [^self posixErrorNamed: 'EACCES'].
	^self ioFileErrorClass
%

category: 'constants'
method: ChangeDirectoryErrorGroup
posixErrorNames
	^#('EACCES' 'ENOENT' 'EIO')
%

! Class implementation for 'ChangeModeErrorGroup'

!		Class methods for 'ChangeModeErrorGroup'

category: 'constants'
classmethod: ChangeModeErrorGroup
posixErrorNames
	^#('EACCES' 'ENOENT' 'EPERM' 'EROFS'  'EIO')
%

!		Instance methods for 'ChangeModeErrorGroup'

category: 'errors - windows'
method: ChangeModeErrorGroup
errorClassForWindowsPlatformError: anErrorNumber fileReference: aFileReference
	"#isInUse - needs to be implemented - ^(CfsStat stat: path) isCfsError not -
		Answer true iff the name specified by path is in use (i.e. it is a file or directory).
		Otherwise, answer false."
	(anErrorNumber = 2 and:[aFileReference exists not]) ifTrue: [^self posixErrorNamed: 'ENOENT'].
	anErrorNumber = 123 ifTrue: [^self posixErrorNamed: 'EACCES'].
	anErrorNumber = 19 ifTrue: [^self posixErrorNamed: 'EROFS'].
	^self ioFileErrorClass
%

! Class implementation for 'CloseDirectoryErrorGroup'

!		Class methods for 'CloseDirectoryErrorGroup'

category: 'constants'
classmethod: CloseDirectoryErrorGroup
posixErrorNames
	^#('EBADF'  'EIO')
%

!		Instance methods for 'CloseDirectoryErrorGroup'

category: 'errors - windows'
method: CloseDirectoryErrorGroup
errorClassForWindowsPlatformError: anErrorNumber fileReference: aFileReference

	anErrorNumber = 6 ifTrue: [^self posixErrorNamed: 'EBADF'].
	^self ioFileErrorClass
%

! Class implementation for 'CloseFileErrorGroup'

!		Class methods for 'CloseFileErrorGroup'

category: 'constants'
classmethod: CloseFileErrorGroup
posixErrorNames
	^#('EBADF'  'EIO')
%

!		Instance methods for 'CloseFileErrorGroup'

category: 'errors - windows'
method: CloseFileErrorGroup
errorClassForWindowsPlatformError: anErrorNumber fileReference: aFileReference

	anErrorNumber = 6 ifTrue: [^self posixErrorNamed: 'EBADF'].
	^self ioFileErrorClass
%

! Class implementation for 'CopyFileErrorGroup'

!		Class methods for 'CopyFileErrorGroup'

category: 'constants'
classmethod: CopyFileErrorGroup
posixErrorNames
	^#('EACCES' 'EBUSY' 'EEXIST' 'EIO' 'EISDIR' 'ENOENT' 'EROFS'  'EIO')
%

!		Instance methods for 'CopyFileErrorGroup'

category: 'errors - windows'
method: CopyFileErrorGroup
errorClassForWindowsPlatformError: anErrorNumber newFileReference: aNewFileReference oldFileReference: anOldFileReference

	(#(3 21) includes: anErrorNumber) ifTrue: [^self posixErrorNamed: 'EACCES'].
	anErrorNumber = 5 ifTrue: [
		aNewFileReference isDirectory ifTrue: [^self posixErrorNamed: 'EEXIST'].
		aNewFileReference isFile ifTrue: [^self posixErrorNamed: 'EEXIST'].
		anOldFileReference exists ifFalse: [^self posixErrorNamed: 'ENOENT'].
		anOldFileReference isFile
			ifTrue: [^self posixErrorNamed: 'EACCES']
			ifFalse: [^self posixErrorNamed: 'EISDIR']. ].
	(#(16 32 33) includes: anErrorNumber) ifTrue: [^self posixErrorNamed: 'EBUSY'].
	anErrorNumber = 19 ifTrue: [^self posixErrorNamed: 'EROFS'].
	anErrorNumber = 80 ifTrue: [^self posixErrorNamed: 'EEXIST'].
	anErrorNumber = 87 ifTrue: [
		^anOldFileReference exists not
			ifTrue: [self posixErrorNamed: 'ENOENT']
			ifFalse: [self posixErrorNamed: 'EACCES']].
	anErrorNumber = 123 ifTrue: [^self posixErrorNamed: 'EACCES'].
	anErrorNumber = 161 ifTrue: [^self posixErrorNamed: 'ENOENT'].
	anErrorNumber = 183 ifTrue: [^self posixErrorNamed: 'EEXIST'].
	^self ioFileErrorClass
%

! Class implementation for 'FileControlErrorGroup'

!		Class methods for 'FileControlErrorGroup'

category: 'constants'
classmethod: FileControlErrorGroup
posixErrorNames
	^#('EACCES' 'EBADF' 'EINVAL'  'EIO')
%

!		Instance methods for 'FileControlErrorGroup'

category: 'errors - windows'
method: FileControlErrorGroup
errorClassForWindowsPlatformError: anErrorNumber fileReference: aFileReference

	anErrorNumber = 5 ifTrue: [ ^self posixErrorNamed: 'EACCES' ].
	anErrorNumber = 6 ifTrue: [ ^self posixErrorNamed: 'EBADF' ].
	anErrorNumber = 7 ifTrue: [ ^self posixErrorNamed: 'EINVAL' ].
	^self ioFileErrorClass
%

! Class implementation for 'FileSeekErrorGroup'

!		Class methods for 'FileSeekErrorGroup'

category: 'constants'
classmethod: FileSeekErrorGroup
posixErrorNames
	^#('EBADF' 'EINVAL'  'EIO')
%

!		Instance methods for 'FileSeekErrorGroup'

category: 'errors - windows'
method: FileSeekErrorGroup
errorClassForWindowsPlatformError: anErrorNumber fileReference: aFileReference

	anErrorNumber = 6 ifTrue: [^self posixErrorNamed: 'EBADF'].
	(#(87 131) includes: anErrorNumber) ifTrue: [^self posixErrorNamed: 'EINVAL'].
	^self ioFileErrorClass
%

! Class implementation for 'FileSizeErrorGroup'

!		Class methods for 'FileSizeErrorGroup'

category: 'constants'
classmethod: FileSizeErrorGroup
posixErrorNames
	^#('EBADF'  'EIO')
%

!		Instance methods for 'FileSizeErrorGroup'

category: 'api'
method: FileSizeErrorGroup
errorClassForWindowsPlatformError: anErrorNumber fileReference: aFileReference

	anErrorNumber = 6 ifTrue: [^self posixErrorNamed: 'EBADF'].
	^self ioFileErrorClass
%

! Class implementation for 'FlushErrorGroup'

!		Class methods for 'FlushErrorGroup'

category: 'constants'
classmethod: FlushErrorGroup
posixErrorNames
	^#('EACCES' 'EBADF'  'EIO')
%

!		Instance methods for 'FlushErrorGroup'

category: 'errors - windows'
method: FlushErrorGroup
errorClassForWindowsPlatformError: anErrorNumber fileReference: aFileReference

	anErrorNumber = 5 ifTrue: [^self posixErrorNamed: 'EACCES'].
	anErrorNumber = 6 ifTrue: [^self posixErrorNamed: 'EBADF'].
	^self ioFileErrorClass
%

! Class implementation for 'FlushFileErrorGroup'

!		Class methods for 'FlushFileErrorGroup'

category: 'constants'
classmethod: FlushFileErrorGroup
posixErrorNames
	^#('EACCES' 'EBADF'  'EIO')
%

!		Instance methods for 'FlushFileErrorGroup'

category: 'errors - windows'
method: FlushFileErrorGroup
errorClassForWindowsPlatformError: anErrorNumber fileReference: aFileReference

	anErrorNumber = 5 ifTrue: [^self posixErrorNamed: 'EACCES'].
	anErrorNumber = 6 ifTrue: [^self posixErrorNamed: 'EBADF'].
	^self ioFileErrorClass
%

! Class implementation for 'GetCurrentWorkingDirectoryErrorGroup'

!		Instance methods for 'GetCurrentWorkingDirectoryErrorGroup'

category: 'errors - windows'
method: GetCurrentWorkingDirectoryErrorGroup
errorClassForWindowsPlatformError: anErrorNumber fileReference: aFileReference

	^self ioFileErrorClass
%

! Class implementation for 'LockFileErrorGroup'

!		Class methods for 'LockFileErrorGroup'

category: 'constants'
classmethod: LockFileErrorGroup
posixErrorNames
	^#('EACCES' 'EBADF' 'EBUSY' 'EINVAL' 'EROFS'  'EIO')
%

!		Instance methods for 'LockFileErrorGroup'

category: 'errors - windows'
method: LockFileErrorGroup
errorClassForWindowsPlatformError: anErrorNumber fileReference: aFileReference

	anErrorNumber = 1 ifTrue: [^self posixErrorNamed: 'EINVAL'].
	anErrorNumber = 5 ifTrue: [^self posixErrorNamed: 'EACCES'].
	anErrorNumber = 6 ifTrue: [^self posixErrorNamed: 'EBADF'].
	anErrorNumber = 19 ifTrue: [^self posixErrorNamed: 'EROFS'].
	(#(32 33) includes: anErrorNumber) ifTrue: [^self posixErrorNamed: 'EBUSY'].
	^self ioFileErrorClass
%

! Class implementation for 'MakeDirectoryErrorGroup'

!		Class methods for 'MakeDirectoryErrorGroup'

category: 'constants'
classmethod: MakeDirectoryErrorGroup
posixErrorNames
	^#('EACCES' 'EEXIST' 'ENOSPC' 'EROFS'  'EIO')
%

!		Instance methods for 'MakeDirectoryErrorGroup'

category: 'errors - windows'
method: MakeDirectoryErrorGroup
errorClassForWindowsPlatformError: anErrorNumber fileReference: aFileReference

	anErrorNumber = 3 ifTrue: [
		aFileReference exists
			ifTrue: [^self posixErrorNamed: 'EEXIST']
			ifFalse: [^self posixErrorNamed: 'EACCES'] ].
	anErrorNumber = 5 ifTrue: [
		aFileReference exists
			ifTrue: [^self posixErrorNamed: 'EEXIST']
			ifFalse: [^self posixErrorNamed: 'EACCES'] ].
	anErrorNumber = 19 ifTrue: [^self posixErrorNamed: 'EROFS'].
	anErrorNumber = 123 ifTrue: [^self posixErrorNamed: 'EACCES'].
	anErrorNumber = 161 ifTrue: [^self posixErrorNamed: 'EACCES'].
	anErrorNumber = 183 ifTrue: [^self posixErrorNamed: 'EEXIST'].
	^self ioFileErrorClass
%

! Class implementation for 'OpenDirectoryErrorGroup'

!		Class methods for 'OpenDirectoryErrorGroup'

category: 'constants'
classmethod: OpenDirectoryErrorGroup
posixErrorNames
	^#('EACCES' 'EINVAL' 'ENFILE' 'ENOENT'  'EIO')
%

!		Instance methods for 'OpenDirectoryErrorGroup'

category: 'errors - windows'
method: OpenDirectoryErrorGroup
errorClassForWindowsPlatformError: anErrorNumber fileReference: aFileReference

	anErrorNumber = 2 ifTrue: [
		aFileReference exists ifFalse: [^self posixErrorNamed: 'ENOENT'] ].
	(#(3 5 123 267) includes: anErrorNumber) ifTrue: [^self posixErrorNamed: 'EACCES'].
	anErrorNumber = 84 ifTrue: [^self posixErrorNamed: 'ENFILE'].
	anErrorNumber = 87 ifTrue: [^self posixErrorNamed: 'EINVAL'].
	anErrorNumber = 161 ifTrue: [^self posixErrorNamed: 'ENOENT'].

	^self ioFileErrorClass
%

! Class implementation for 'OpenFileErrorGroup'

!		Class methods for 'OpenFileErrorGroup'

category: 'constants'
classmethod: OpenFileErrorGroup
posixErrorNames
	^#('EACCES' 'EBUSY' 'EEXIST' 'EINVAL' 'ENFILE' 'ENOENT' 'ENOSPC' 'EROFS'  'EIO')
%

!		Instance methods for 'OpenFileErrorGroup'

category: 'errors - windows'
method: OpenFileErrorGroup
errorClassForWindowsPlatformError: anErrorNumber fileReference: aFileReference options: fileOptions

	anErrorNumber = 2 ifTrue: [
		aFileReference isDirectory
			ifTrue: [^self posixErrorNamed: 'EACCES']
			ifFalse: [^self posixErrorNamed: 'ENOENT']].
	(#(3 123 161) includes: anErrorNumber) ifTrue: [^self posixErrorNamed: 'EACCES'].
	anErrorNumber = 4 ifTrue: [^self posixErrorNamed: 'ENFILE'].
	(#(5 87) includes: anErrorNumber) ifTrue: [
		aFileReference isDirectory ifTrue: [^self posixErrorNamed: 'EACCES'].
		fileOptions hasFileTruncateFlag ifTrue: [^self posixErrorNamed: 'EINVAL'].
		fileOptions hasFileCreateFlag ifTrue: [^self posixErrorNamed: 'EACCES'].
		aFileReference isFile ifTrue: [^self posixErrorNamed: 'EEXIST'].
		^self posixErrorNamed: 'EINVAL'].
	anErrorNumber = 19 ifTrue: [^self posixErrorNamed: 'EROFS'].
	(#(32 33) includes: anErrorNumber) ifTrue: [^self posixErrorNamed: 'EBUSY'].
	anErrorNumber = 36 ifTrue: [^self posixErrorNamed: 'ENFILE']. "no more buffers available. Too many in use"
	anErrorNumber = 80   "file exists"
		ifTrue: [
			fileOptions hasExclsiveFlag
				ifTrue: [^self posixErrorNamed: 'EACCES']
				ifFalse: [^self posixErrorNamed: 'EEXIST'] ].
	^self ioFileErrorClass
%

! Class implementation for 'ReadDirectoryErrorGroup'

!		Class methods for 'ReadDirectoryErrorGroup'

category: 'constants'
classmethod: ReadDirectoryErrorGroup
posixErrorNames
	^#('EACCES' 'EBADF'  'EIO')
%

!		Instance methods for 'ReadDirectoryErrorGroup'

category: 'api'
method: ReadDirectoryErrorGroup
errorClassForWindowsPlatformError: anErrorNumber fileReference: aFileReference

	anErrorNumber = 3 ifTrue: [^self posixErrorNamed: 'EACCES'].
	anErrorNumber = 7 ifTrue: [^self posixErrorNamed: 'EINVAL'].
	^self ioFileErrorClass
%

! Class implementation for 'ReadFileErrorGroup'

!		Class methods for 'ReadFileErrorGroup'

category: 'constants'
classmethod: ReadFileErrorGroup
posixErrorNames
	^#('EACCES' 'EBADF' 'EBUSY' 'EINVAL'  'EIO')
%

!		Instance methods for 'ReadFileErrorGroup'

category: 'errors - windows'
method: ReadFileErrorGroup
errorClassForWindowsPlatformError: anErrorNumber fileReference: aFileReference

	(#(5 112) includes: anErrorNumber) ifTrue: [^self posixErrorNamed: 'EACCES'].
	anErrorNumber = 6 ifTrue: [^self posixErrorNamed: 'EBADF'].
	anErrorNumber = 7 ifTrue: [^self posixErrorNamed: 'EINVAL'].
	anErrorNumber = 33 ifTrue: [^self posixErrorNamed: 'EBUSY'].
	^self ioFileErrorClass
%

! Class implementation for 'RemoveDirectoryErrorGroup'

!		Class methods for 'RemoveDirectoryErrorGroup'

category: 'constants'
classmethod: RemoveDirectoryErrorGroup
posixErrorNames
	^#('EACCES' 'EBUSY' 'EEXIST' 'EINVAL' 'ENOENT' 'EROFS'  'EIO')
%

!		Instance methods for 'RemoveDirectoryErrorGroup'

category: 'errors - windows'
method: RemoveDirectoryErrorGroup
errorClassForWindowsPlatformError: anErrorNumber fileReference: aDirectoryReference

	anErrorNumber = 2 ifTrue: [^self posixErrorNamed: 'ENOENT'].
	anErrorNumber = 3 ifTrue: [
		aDirectoryReference exists not ifTrue: [^self posixErrorNamed: 'ENOENT'].
		aDirectoryReference entries isEmpty ifTrue: [^self posixErrorNamed: 'EEXIST'].
		^self posixErrorNamed: 'EACCES'].
	anErrorNumber = 5 ifTrue: [
		aDirectoryReference exists not ifTrue: [^self posixErrorNamed: 'ENOENT'].
		aDirectoryReference entries isEmpty ifTrue: [^self posixErrorNamed: 'EEXIST'].
		^self posixErrorNamed: 'EACCES'].
	anErrorNumber = 16 ifTrue: [^self posixErrorNamed: 'EBUSY'].
	anErrorNumber = 19 ifTrue: [^self posixErrorNamed: 'EROFS'].
	anErrorNumber = 123 ifTrue: [^self posixErrorNamed: 'EACCES'].
	anErrorNumber = 145 ifTrue: [^self posixErrorNamed: 'EEXIST'].
	anErrorNumber = 161 ifTrue: [^self posixErrorNamed: 'ENOENT'].
	anErrorNumber = 267 ifTrue: [^self posixErrorNamed: 'ENOENT'].
	^self ioFileErrorClass
%

! Class implementation for 'RemoveFileErrorGroup'

!		Class methods for 'RemoveFileErrorGroup'

category: 'constants'
classmethod: RemoveFileErrorGroup
posixErrorNames
	^#('EACCES' 'EBUSY' 'EEXIST' 'EINVAL' 'EISDIR' 'ENOENT' 'EPERM' 'EROFS'  'EIO')
%

!		Instance methods for 'RemoveFileErrorGroup'

category: 'errors - windows'
method: RemoveFileErrorGroup
errorClassForWindowsPlatformError: anErrorNumber fileReference: aFileReference

	anErrorNumber = 2 ifTrue: [
		aFileReference isDirectory
			ifTrue: [^self posixErrorNamed: 'EISDIR']
			ifFalse: [^self posixErrorNamed: 'ENOENT']].
	anErrorNumber = 3 ifTrue: [^self posixErrorNamed: 'EACCES'].
	anErrorNumber = 5 ifTrue: [
		aFileReference isDirectory
			ifTrue: [^self posixErrorNamed: 'EISDIR']
			ifFalse: [^self posixErrorNamed: 'EACCES']].
	anErrorNumber = 19 ifTrue: [^self posixErrorNamed: 'EROFS'].
	anErrorNumber = 123 ifTrue: [^self posixErrorNamed: 'EACCES'].
	anErrorNumber = 145 ifTrue: [^self posixErrorNamed: 'EEXIST'].
	anErrorNumber = 161 ifTrue: [^self posixErrorNamed: 'ENOENT'].
	^self ioFileErrorClass
%

! Class implementation for 'RenameErrorGroup'

!		Class methods for 'RenameErrorGroup'

category: 'constants'
classmethod: RenameErrorGroup
posixErrorNames
	^#('EACCES' 'EBUSY' 'EEXIST' 'EINVAL' 'ENOENT' 'ENOSPC' 'EROFS' 'EXDEV'  'EIO')
%

!		Instance methods for 'RenameErrorGroup'

category: 'errors - windows'
method: RenameErrorGroup
errorClassForWindowsPlatformError: anErrorNumber newFileReference: aNewFileReference oldFileReference: anOldFileReference

	anErrorNumber = 2 ifTrue: [
		aNewFileReference isDirectory
			ifTrue: [ ^self posixErrorNamed: 'EISDIR' ]
			ifFalse: [ ^self posixErrorNamed: 'ENOENT' ] ].
	anErrorNumber = 5 ifTrue: [
		aNewFileReference isDirectory
			ifTrue: [ ^self posixErrorNamed: 'EISDIR' ]
			ifFalse: [ ^self posixErrorNamed: 'EACCES' ] ].
	( #( 3 123 ) includes: anErrorNumber ) ifTrue: [ ^self posixErrorNamed: 'EACCES' ].
	anErrorNumber =  19 ifTrue: [ ^self posixErrorNamed: 'EROFS' ].
	anErrorNumber = 145 ifTrue: [ ^self posixErrorNamed: 'EEXIST' ].
	anErrorNumber = 161 ifTrue: [ ^self posixErrorNamed: 'ENOENT' ].
	^self ioFileErrorClass
%

! Class implementation for 'RenameFileErrorGroup'

!		Class methods for 'RenameFileErrorGroup'

category: 'constants'
classmethod: RenameFileErrorGroup
posixErrorNames
	^#('EACCES' 'EBUSY' 'EEXIST' 'EINVAL' 'ENOENT' 'ENOSPC' 'EROFS' 'EXDEV'  'EIO')
%

!		Instance methods for 'RenameFileErrorGroup'

category: 'errors - windows'
method: RenameFileErrorGroup
errorClassForWindowsPlatformError: anErrorNumber newFileReference: aNewFileReference oldFileReference: anOldFileReference

	anErrorNumber = 2 ifTrue: [
		aNewFileReference isDirectory
			ifTrue: [ ^self posixErrorNamed: 'EISDIR' ]
			ifFalse: [ ^self posixErrorNamed: 'ENOENT' ] ].
	anErrorNumber = 5 ifTrue: [
		aNewFileReference isDirectory
			ifTrue: [ ^self posixErrorNamed: 'EISDIR' ]
			ifFalse: [ ^self posixErrorNamed: 'EACCES' ] ].
	( #( 3 123 ) includes: anErrorNumber ) ifTrue: [ ^self posixErrorNamed: 'EACCES' ].
	anErrorNumber =  19 ifTrue: [ ^self posixErrorNamed: 'EROFS' ].
	anErrorNumber = 145 ifTrue: [ ^self posixErrorNamed: 'EEXIST' ].
	anErrorNumber = 161 ifTrue: [ ^self posixErrorNamed: 'ENOENT' ].
	anErrorNumber = 183 ifTrue: [ ^self posixErrorNamed: 'EEXIST' ].
	^self ioFileErrorClass
%

! Class implementation for 'TouchFileErrorGroup'

!		Class methods for 'TouchFileErrorGroup'

category: 'constants'
classmethod: TouchFileErrorGroup
posixErrorNames
	^#('EACCES' 'ENFILE' 'ENOENT' 'EROFS'  'EIO')
%

!		Instance methods for 'TouchFileErrorGroup'

category: 'api'
method: TouchFileErrorGroup
errorClassForWindowsPlatformError: anErrorNumber fileReference: aFileReference

	(#(2 3 161) includes: anErrorNumber) ifTrue: [^self posixErrorNamed: 'ENOENT'].
	anErrorNumber = 4 ifTrue: [^self posixErrorNamed: 'ENFILE'].
	(#(5 123) includes: anErrorNumber) ifTrue: [^self posixErrorNamed: 'EACCES'].
	anErrorNumber = 19 ifTrue: [^self posixErrorNamed: 'EROFS'].
	^self ioFileErrorClass
%

! Class implementation for 'UnlockFileErrorGroup'

!		Class methods for 'UnlockFileErrorGroup'

category: 'constants'
classmethod: UnlockFileErrorGroup
posixErrorNames
	^#('EACCES' 'EBADF' 'EINVAL' 'EROFS' 'EBUSY'  'EIO')
%

!		Instance methods for 'UnlockFileErrorGroup'

category: 'errors - windows'
method: UnlockFileErrorGroup
errorClassForWindowsPlatformError: anErrorNumber fileReference: aFileReference

	(#(1 158) includes: anErrorNumber) ifTrue: [^self posixErrorNamed: 'EINVAL'].
	anErrorNumber = 5 ifTrue: [^self posixErrorNamed: 'EACCES'].
	anErrorNumber = 6 ifTrue: [^self posixErrorNamed: 'EBADF'].
	anErrorNumber = 19 ifTrue: [^self posixErrorNamed: 'EROFS'].
	(#(32 33) includes: anErrorNumber) ifTrue: [^self posixErrorNamed: 'EBUSY'].
	^self ioFileErrorClass
%

! Class implementation for 'WritingErrorGroup'

!		Class methods for 'WritingErrorGroup'

category: 'constants'
classmethod: WritingErrorGroup
posixErrorNames
	^#('EACCES' 'EBADF' 'EBUSY' 'EINVAL' 'ENOSPC' 'EROFS'  'EIO')
%

!		Instance methods for 'WritingErrorGroup'

category: 'errors - windows'
method: WritingErrorGroup
errorClassForWindowsPlatformError: anErrorNumber fileReference: aFileReference

	(#(5 112) includes: anErrorNumber) ifTrue: [^self posixErrorNamed: 'EACCES'].
	anErrorNumber = 6 ifTrue: [^self posixErrorNamed: 'EBADF'].
	anErrorNumber = 19 ifTrue: [^self posixErrorNamed: 'EROFS'].
	anErrorNumber = 33 ifTrue: [^self posixErrorNamed: 'EBUSY'].
	^self ioFileErrorClass
%

! Class implementation for 'PosixFlag'

!		Class methods for 'PosixFlag'

category: 'querry'
classmethod: PosixFlag
allConcretePosixFlagClasses
	^self allSubclasses select: [:ea | ea subclasses isEmpty and: [(ea class includesSelector: #defaultPlatformValue)]]
%

category: 'accessing'
classmethod: PosixFlag
defaultPlatformValue
	^self error: 'Not implmented'
%

category: 'printing'
classmethod: PosixFlag
posixFlagsReport
	" PosixFlag posixFlagsReport "
	| aStream |
	aStream := WriteStream on: String new.
	self reportOn: aStream indent: 0.
	^aStream contents
%

category: 'accessing'
classmethod: PosixFlag
posixName
	self subclassResponsibility
%

category: 'querry'
classmethod: PosixFlag
posixNamesWithClasses
	" PosixFlag posixNamesWithClasses"
	| aDictionary |
	aDictionary := Dictionary new.
	self allSubclasses do: [:ea |
		(ea class includesSelector: #posixName) ifTrue: [aDictionary at: ea posixName put: ea] ].
	^aDictionary
%

category: 'private'
classmethod: PosixFlag
reportOn: aStream indent: indent
	| aSize aValue |
	indent timesRepeat: [aStream tab].

	aStream nextPutAll: self name asString.
	"(45 - ((indent * 4) + self name asString size)) timesRepeat: [aStream space]."
	aStream tab.

	((self name = #PosixFlag) not and: [self class includesSelector: #posixName])
		ifTrue: [
			aSize := self posixName size.
			aStream nextPutAll: self posixName].
	"(15 - (aSize ifNil: [0])) timesRepeat: [aStream space]."
	aStream tab.

	((self name = #PosixFlag) not and: [self class includesSelector: #defaultPlatformValue])
		ifTrue: [aValue := self defaultPlatformValue asString]
		ifFalse: [aValue := (self subclasses isEmpty not) ifTrue: [''] ifFalse: ['value not defined']].
	aSize :=  aValue size.
	"(15 - aSize) timesRepeat: [aStream space]."
	aStream tab.
	aStream nextPutAll: aValue asString.

	((self name = #PosixFlag) not and: [self class includesSelector: #windowsValue])
		ifTrue: [
			aSize := self windowsValue asString size.
			"(15 - aSize) timesRepeat: [aStream space]."
			aStream tab.
			aStream nextPutAll: self windowsValue asString].

	aStream crlf.

	self subclasses do: [:ea | ea reportOn: aStream indent: indent + 1]
%

category: 'querry'
classmethod: PosixFlag
valueForSelector: valueSelector
	^(self class canUnderstand: valueSelector)
		ifTrue: [self perform: valueSelector]
		ifFalse: [self defaultPlatformValue]
%

!		Instance methods for 'PosixFlag'

category: 'accessing'
method: PosixFlag
parent
	^parent
%

category: 'accessing'
method: PosixFlag
parent: anObject
	parent := anObject
%

category: 'querry'
method: PosixFlag
posixFlagValue
	^self store optionPlatformValueFor: self class posixName
%

category: 'querry'
method: PosixFlag
posixName
	^self class posixName
%

category: 'querry'
method: PosixFlag
store
	^self parent store
%

! Class implementation for 'FileCloseOnExecutionFlag'

!		Class methods for 'FileCloseOnExecutionFlag'

category: 'access'
classmethod: FileCloseOnExecutionFlag
defaultPlatformValue
	^1
%

category: 'access'
classmethod: FileCloseOnExecutionFlag
posixName
	^'FD_CLOEXEC'
%

! Class implementation for 'FileGetDescriptorFlag'

!		Class methods for 'FileGetDescriptorFlag'

category: 'accessing'
classmethod: FileGetDescriptorFlag
defaultPlatformValue
	^1
%

category: 'accessing'
classmethod: FileGetDescriptorFlag
posixName
	^'F_GETFD'
%

! Class implementation for 'FileGetLockFlag'

!		Class methods for 'FileGetLockFlag'

category: 'access'
classmethod: FileGetLockFlag
defaultPlatformValue
	^14
%

category: 'access'
classmethod: FileGetLockFlag
posixName
	^'F_GETLK'
%

! Class implementation for 'FileGetStatusFlag'

!		Class methods for 'FileGetStatusFlag'

category: 'access'
classmethod: FileGetStatusFlag
defaultPlatformValue
	^3
%

category: 'access'
classmethod: FileGetStatusFlag
posixName
	^'F_GETFL'
%

! Class implementation for 'FileSetDescriptorFlag'

!		Class methods for 'FileSetDescriptorFlag'

category: 'accessing'
classmethod: FileSetDescriptorFlag
defaultPlatformValue
	^2
%

category: 'accessing'
classmethod: FileSetDescriptorFlag
posixName
	^'F_SETFD'
%

! Class implementation for 'FileSetLockFlag'

!		Class methods for 'FileSetLockFlag'

category: 'accessing'
classmethod: FileSetLockFlag
defaultPlatformValue
	^6
%

category: 'accessing'
classmethod: FileSetLockFlag
posixName
	^'F_SETLK'
%

! Class implementation for 'FileSetLockWaitFlag'

!		Class methods for 'FileSetLockWaitFlag'

category: 'accessing'
classmethod: FileSetLockWaitFlag
defaultPlatformValue
	^7
%

category: 'accessing'
classmethod: FileSetLockWaitFlag
posixName
	^'F_SETLKW'
%

! Class implementation for 'FileSetStatusFlag'

!		Class methods for 'FileSetStatusFlag'

category: 'accessing'
classmethod: FileSetStatusFlag
defaultPlatformValue
	^4
%

category: 'accessing'
classmethod: FileSetStatusFlag
posixName
	^'F_SETFL'
%

! Class implementation for 'FileSetUnlockFlag'

!		Class methods for 'FileSetUnlockFlag'

category: 'accessing'
classmethod: FileSetUnlockFlag
defaultPlatformValue
	^3
%

category: 'accessing'
classmethod: FileSetUnlockFlag
posixName
	^'F_UNLCK'
%

! Class implementation for 'FileDirectoryEntryFlag'

!		Class methods for 'FileDirectoryEntryFlag'

category: 'accessing'
classmethod: FileDirectoryEntryFlag
defaultPlatformValue
	^2
%

category: 'accessing'
classmethod: FileDirectoryEntryFlag
posixName
	^'F_DIR'
%

category: 'accessing'
classmethod: FileDirectoryEntryFlag
windowsValue
	^16r100
%

! Class implementation for 'FileRegularFileEntryFlag'

!		Class methods for 'FileRegularFileEntryFlag'

category: 'accessing'
classmethod: FileRegularFileEntryFlag
defaultPlatformValue
	^1
%

category: 'accessing'
classmethod: FileRegularFileEntryFlag
posixName
	^'F_REG'
%

category: 'accessing'
classmethod: FileRegularFileEntryFlag
windowsValue
	^16r010
%

! Class implementation for 'FileSpecialFileEntryFlag'

!		Class methods for 'FileSpecialFileEntryFlag'

category: 'accessing'
classmethod: FileSpecialFileEntryFlag
defaultPlatformValue
	^4
%

category: 'accessing'
classmethod: FileSpecialFileEntryFlag
posixName
	^'F_SPECIAL'
%

category: 'accessing'
classmethod: FileSpecialFileEntryFlag
windowsValue
	^16r006
%

! Class implementation for 'FileMandatoryLockFlag'

!		Class methods for 'FileMandatoryLockFlag'

category: 'accessing'
classmethod: FileMandatoryLockFlag
defaultPlatformValue
	^0
%

category: 'accessing'
classmethod: FileMandatoryLockFlag
posixName
	^'F_MDLCK'
%

! Class implementation for 'FileReadLockFlag'

!		Class methods for 'FileReadLockFlag'

category: 'accessing'
classmethod: FileReadLockFlag
defaultPlatformValue
	^1
%

category: 'accessing'
classmethod: FileReadLockFlag
posixName
	^'F_RDLCK'
%

! Class implementation for 'FileUnlockFlag'

!		Class methods for 'FileUnlockFlag'

category: 'accessing'
classmethod: FileUnlockFlag
defaultPlatformValue
	^3
%

category: 'accessing'
classmethod: FileUnlockFlag
posixName
	^'F_UNLCK'
%

! Class implementation for 'FileWriteLockFlag'

!		Class methods for 'FileWriteLockFlag'

category: 'accessing'
classmethod: FileWriteLockFlag
defaultPlatformValue
	^2
%

category: 'accessing'
classmethod: FileWriteLockFlag
posixName
	^'F_WRLCK'
%

! Class implementation for 'FileAppendFlag'

!		Class methods for 'FileAppendFlag'

category: 'accessing'
classmethod: FileAppendFlag
defaultPlatformValue
	^16r00400
%

category: 'accessing'
classmethod: FileAppendFlag
posixName
	^'O_APPEND'
%

category: 'accessing'
classmethod: FileAppendFlag
windowsValue
	^16r10000
%

! Class implementation for 'FileCloseOnExecFlag'

!		Class methods for 'FileCloseOnExecFlag'

category: 'accessing'
classmethod: FileCloseOnExecFlag
posixName
	^'O_CLOEXEC'
%

! Class implementation for 'FileCreateFlag'

!		Class methods for 'FileCreateFlag'

category: 'accessing'
classmethod: FileCreateFlag
defaultPlatformValue
	^16r00040
%

category: 'accessing'
classmethod: FileCreateFlag
posixName
	^'O_CREAT'
%

category: 'accessing'
classmethod: FileCreateFlag
windowsValue
	^16r20000
%

!		Instance methods for 'FileCreateFlag'

category: 'convenience'
method: FileCreateFlag
createRule
	^self parent fileExclusiveFlag ifNil: [self store class openAlwaysRule] ifNotNil: [:option | self store class createNewRule]
%

! Class implementation for 'FileDirectFlag'

!		Class methods for 'FileDirectFlag'

category: 'accessing'
classmethod: FileDirectFlag
posixName
	^'O_DIRECT'
%

! Class implementation for 'FileDirectoryFlag'

!		Class methods for 'FileDirectoryFlag'

category: 'accessing'
classmethod: FileDirectoryFlag
posixName
	^'O_DIRECTORY'
%

! Class implementation for 'FileExclusiveFlag'

!		Class methods for 'FileExclusiveFlag'

category: 'accessing'
classmethod: FileExclusiveFlag
defaultPlatformValue
	^16r0080
%

category: 'accessing'
classmethod: FileExclusiveFlag
posixName
	^'O_EXCL'
%

category: 'accessing'
classmethod: FileExclusiveFlag
windowsValue
	^16r40000
%

! Class implementation for 'FileKeepSymbolicLinksFlag'

!		Class methods for 'FileKeepSymbolicLinksFlag'

category: 'accessing'
classmethod: FileKeepSymbolicLinksFlag
posixName
	^'O_NOFOLLOW'
%

! Class implementation for 'FileNonBlockFlag'

!		Class methods for 'FileNonBlockFlag'

category: 'accessing'
classmethod: FileNonBlockFlag
defaultPlatformValue
	^16r00080
%

category: 'accessing'
classmethod: FileNonBlockFlag
posixName
	^'O_NONBLOCK'
%

! Class implementation for 'FileNotControllingTerminalFlag'

!		Class methods for 'FileNotControllingTerminalFlag'

category: 'accessing'
classmethod: FileNotControllingTerminalFlag
posixName
	^'O_NOCTTY'
%

! Class implementation for 'FileOpenModeFlag'

!		Instance methods for 'FileOpenModeFlag'

category: 'convenience'
method: FileOpenModeFlag
accessRule
	self subclassResponsibility
%

category: 'testing'
method: FileOpenModeFlag
isReadable
	^false
%

category: 'testing'
method: FileOpenModeFlag
isWritable
	^false
%

category: 'printing'
method: FileOpenModeFlag
printOpenModeStringOn: aStream

	aStream nextPutAll: self mode
%

! Class implementation for 'FileOpenReadOnlyFlag'

!		Class methods for 'FileOpenReadOnlyFlag'

category: 'accessing'
classmethod: FileOpenReadOnlyFlag
defaultPlatformValue
	^16r00000
%

category: 'accessing'
classmethod: FileOpenReadOnlyFlag
posixName
	^'O_RDONLY'
%

!		Instance methods for 'FileOpenReadOnlyFlag'

category: 'convenience'
method: FileOpenReadOnlyFlag
accessRule
	^self store class readOnlyAccessRule
%

category: 'testing'
method: FileOpenReadOnlyFlag
isReadable
	^true
%

category: 'accessing'
method: FileOpenReadOnlyFlag
mode
	^'r'
%

! Class implementation for 'FileOpenReadWriteFlag'

!		Class methods for 'FileOpenReadWriteFlag'

category: 'accessing'
classmethod: FileOpenReadWriteFlag
defaultPlatformValue
	^16r00002
%

category: 'accessing'
classmethod: FileOpenReadWriteFlag
posixName
	^'O_RDWR'
%

!		Instance methods for 'FileOpenReadWriteFlag'

category: 'convenience'
method: FileOpenReadWriteFlag
accessRule
	^self store class readWriteAccessRule
%

category: 'testing'
method: FileOpenReadWriteFlag
isReadable
	^true
%

category: 'testing'
method: FileOpenReadWriteFlag
isWritable
	^true
%

category: 'accessing'
method: FileOpenReadWriteFlag
mode
	"Truncate and Append are mutually exclusive"
	self parent hasFileTruncateFlag ifTrue: [^'w+'].
	self parent hasFileAppendFlag ifTrue: [^'a+'].
	^'r+'
%

! Class implementation for 'FileOpenWriteOnlyFlag'

!		Class methods for 'FileOpenWriteOnlyFlag'

category: 'accessing'
classmethod: FileOpenWriteOnlyFlag
defaultPlatformValue
	^16r00001
%

category: 'accessing'
classmethod: FileOpenWriteOnlyFlag
posixName
	^'O_WRONLY'
%

!		Instance methods for 'FileOpenWriteOnlyFlag'

category: 'convenience'
method: FileOpenWriteOnlyFlag
accessRule
	^self store class writeOnlyAccessRule
%

category: 'testing'
method: FileOpenWriteOnlyFlag
isWritable
	^true
%

category: 'accessing'
method: FileOpenWriteOnlyFlag
mode
	^self parent hasFileAppendFlag
		ifTrue: ['a']
		ifFalse: ['w']
%

! Class implementation for 'FileReadSynchronousFlag'

!		Class methods for 'FileReadSynchronousFlag'

category: 'accessing'
classmethod: FileReadSynchronousFlag
posixName
	^'O_RSYNC'
%

! Class implementation for 'FileSychronizedDataFlag'

!		Class methods for 'FileSychronizedDataFlag'

category: 'accessing'
classmethod: FileSychronizedDataFlag
posixName
	^'O_DSYNC'
%

! Class implementation for 'FileSynchronizedIOFlag'

!		Class methods for 'FileSynchronizedIOFlag'

category: 'accessing'
classmethod: FileSynchronizedIOFlag
posixName
	^'O_SYNC'
%

! Class implementation for 'FileSynchronizedReadFlag'

!		Class methods for 'FileSynchronizedReadFlag'

category: 'accessing'
classmethod: FileSynchronizedReadFlag
posixName
	^'O_RSYNC'
%

! Class implementation for 'FileTruncateFlag'

!		Class methods for 'FileTruncateFlag'

category: 'accessing'
classmethod: FileTruncateFlag
defaultPlatformValue
	^16r00200
%

category: 'accessing'
classmethod: FileTruncateFlag
posixName
	^'O_TRUNC'
%

category: 'accessing'
classmethod: FileTruncateFlag
windowsValue
	^16r80000
%

!		Instance methods for 'FileTruncateFlag'

category: 'convenience'
method: FileTruncateFlag
truncateRule
	^self store class truncateExistingRule
%

! Class implementation for 'FileGroupAllPermissionFlag'

!		Class methods for 'FileGroupAllPermissionFlag'

category: 'access'
classmethod: FileGroupAllPermissionFlag
defaultPlatformValue
	^16r38
%

category: 'access'
classmethod: FileGroupAllPermissionFlag
posixName
	^'SIRWXG'
%

category: 'access'
classmethod: FileGroupAllPermissionFlag
windowsValue
	^0
%

! Class implementation for 'FileGroupExecutePermissionFlag'

!		Class methods for 'FileGroupExecutePermissionFlag'

category: 'access'
classmethod: FileGroupExecutePermissionFlag
defaultPlatformValue
	^16r008
%

category: 'access'
classmethod: FileGroupExecutePermissionFlag
posixName
	^'SIXGRP'
%

category: 'access'
classmethod: FileGroupExecutePermissionFlag
windowsValue
	^0
%

! Class implementation for 'FileGroupReadPermissionFlag'

!		Class methods for 'FileGroupReadPermissionFlag'

category: 'access'
classmethod: FileGroupReadPermissionFlag
defaultPlatformValue
	^16r020
%

category: 'access'
classmethod: FileGroupReadPermissionFlag
posixName
	^'SIRGRP'
%

category: 'access'
classmethod: FileGroupReadPermissionFlag
windowsValue
	^0
%

! Class implementation for 'FileGroupWritePermissionFlag'

!		Class methods for 'FileGroupWritePermissionFlag'

category: 'access'
classmethod: FileGroupWritePermissionFlag
defaultPlatformValue
	^16r010
%

category: 'access'
classmethod: FileGroupWritePermissionFlag
posixName
	^'SIWGRP'
%

category: 'access'
classmethod: FileGroupWritePermissionFlag
windowsValue
	^0
%

! Class implementation for 'FileOtherAllPermissionFlag'

!		Class methods for 'FileOtherAllPermissionFlag'

category: 'accessing'
classmethod: FileOtherAllPermissionFlag
defaultPlatformValue
	^16r007
%

category: 'accessing'
classmethod: FileOtherAllPermissionFlag
posixName
	^'SIRWXO'
%

category: 'accessing'
classmethod: FileOtherAllPermissionFlag
windowsValue
	^0
%

! Class implementation for 'FileOtherExecutePermissionFlag'

!		Class methods for 'FileOtherExecutePermissionFlag'

category: 'accessing'
classmethod: FileOtherExecutePermissionFlag
defaultPlatformValue
	^16r001
%

category: 'accessing'
classmethod: FileOtherExecutePermissionFlag
posixName
	^'SIXOTH'
%

category: 'accessing'
classmethod: FileOtherExecutePermissionFlag
windowsValue
	^0
%

! Class implementation for 'FileOtherReadPermissionFlag'

!		Class methods for 'FileOtherReadPermissionFlag'

category: 'accessing'
classmethod: FileOtherReadPermissionFlag
defaultPlatformValue
	^16r004
%

category: 'accessing'
classmethod: FileOtherReadPermissionFlag
posixName
	^'SIROTH'
%

category: 'accessing'
classmethod: FileOtherReadPermissionFlag
windowsValue
	^0
%

! Class implementation for 'FileOtherWritePermissionFlag'

!		Class methods for 'FileOtherWritePermissionFlag'

category: 'accessing'
classmethod: FileOtherWritePermissionFlag
defaultPlatformValue
	^16r002
%

category: 'accessing'
classmethod: FileOtherWritePermissionFlag
posixName
	^'SIWOTH'
%

category: 'accessing'
classmethod: FileOtherWritePermissionFlag
windowsValue
	^0
%

! Class implementation for 'FileOwnerAllPermissionFlag'

!		Class methods for 'FileOwnerAllPermissionFlag'

category: 'accessing'
classmethod: FileOwnerAllPermissionFlag
defaultPlatformValue
	^16r1C0
%

category: 'accessing'
classmethod: FileOwnerAllPermissionFlag
posixName
	^'SIRWXU'
%

category: 'accessing'
classmethod: FileOwnerAllPermissionFlag
windowsValue
	^3
%

! Class implementation for 'FileOwnerExecutePermissionFlag'

!		Class methods for 'FileOwnerExecutePermissionFlag'

category: 'accessing'
classmethod: FileOwnerExecutePermissionFlag
defaultPlatformValue
	^16r40
%

category: 'accessing'
classmethod: FileOwnerExecutePermissionFlag
posixName
	^'SIXUSR'
%

category: 'accessing'
classmethod: FileOwnerExecutePermissionFlag
windowsValue
	^0
%

! Class implementation for 'FileOwnerReadPermissionFlag'

!		Class methods for 'FileOwnerReadPermissionFlag'

category: 'accessing'
classmethod: FileOwnerReadPermissionFlag
defaultPlatformValue
	^16r100
%

category: 'accessing'
classmethod: FileOwnerReadPermissionFlag
posixName
	^'SIRUSR'
%

category: 'accessing'
classmethod: FileOwnerReadPermissionFlag
windowsValue
	^1
%

! Class implementation for 'FileOwnerWritePermissionFlag'

!		Class methods for 'FileOwnerWritePermissionFlag'

category: 'accessing'
classmethod: FileOwnerWritePermissionFlag
defaultPlatformValue
	^16r80
%

category: 'accessing'
classmethod: FileOwnerWritePermissionFlag
posixName
	^'SIWUSR'
%

category: 'accessing'
classmethod: FileOwnerWritePermissionFlag
windowsValue
	^2
%

! Class implementation for 'FileSeekCurrentPosition'

!		Class methods for 'FileSeekCurrentPosition'

category: 'accessing'
classmethod: FileSeekCurrentPosition
defaultPlatformValue
	^1
%

category: 'accessing'
classmethod: FileSeekCurrentPosition
posixName
	^'SEEK_CUR'
%

! Class implementation for 'FileSeekEndPosition'

!		Class methods for 'FileSeekEndPosition'

category: 'accessing'
classmethod: FileSeekEndPosition
defaultPlatformValue
	^2
%

category: 'accessing'
classmethod: FileSeekEndPosition
posixName
	^'SEEK_END'
%

! Class implementation for 'FileSeekSetPosition'

!		Class methods for 'FileSeekSetPosition'

category: 'accessing'
classmethod: FileSeekSetPosition
defaultPlatformValue
	^0
%

category: 'accessing'
classmethod: FileSeekSetPosition
posixName
	^'SEEK_SET'
%

! Class implementation for 'FileDenyNoneFlag'

!		Class methods for 'FileDenyNoneFlag'

category: 'access'
classmethod: FileDenyNoneFlag
defaultPlatformValue
	^16r00000
%

category: 'access'
classmethod: FileDenyNoneFlag
posixName
	^'O_DENYNONE'
%

category: 'access'
classmethod: FileDenyNoneFlag
windowsValue
	^16r00040
%

!		Instance methods for 'FileDenyNoneFlag'

category: 'convenience'
method: FileDenyNoneFlag
shareRule
	^self store class denyNoneShareRule
%

! Class implementation for 'FileDenyReadFlag'

!		Class methods for 'FileDenyReadFlag'

category: 'access'
classmethod: FileDenyReadFlag
defaultPlatformValue
	^16r00001
%

category: 'access'
classmethod: FileDenyReadFlag
posixName
	^'O_DENYRD'
%

category: 'access'
classmethod: FileDenyReadFlag
windowsValue
	^16r00030
%

!		Instance methods for 'FileDenyReadFlag'

category: 'convenience'
method: FileDenyReadFlag
shareRule
	^self store class denyReadShareRule
%

! Class implementation for 'FileDenyReadWriteFlag'

!		Class methods for 'FileDenyReadWriteFlag'

category: 'access'
classmethod: FileDenyReadWriteFlag
defaultPlatformValue
	^16r00002
%

category: 'access'
classmethod: FileDenyReadWriteFlag
posixName
	^'O_DENYRDWR'
%

category: 'access'
classmethod: FileDenyReadWriteFlag
windowsValue
	^16r00010
%

!		Instance methods for 'FileDenyReadWriteFlag'

category: 'convenience'
method: FileDenyReadWriteFlag
shareRule
	^self store class denyReadWriteShareRule
%

! Class implementation for 'FileDenyWriteFlag'

!		Class methods for 'FileDenyWriteFlag'

category: 'access'
classmethod: FileDenyWriteFlag
defaultPlatformValue
	^16r00003
%

category: 'access'
classmethod: FileDenyWriteFlag
posixName
	^'O_DENYWR'
%

category: 'access'
classmethod: FileDenyWriteFlag
windowsValue
	^16r00020
%

!		Instance methods for 'FileDenyWriteFlag'

category: 'convenience'
method: FileDenyWriteFlag
shareRule
	^self store class denyWriteShareRule
%

! Class implementation for 'FileTypeFlag'

!		Instance methods for 'FileTypeFlag'

category: 'testing'
method: FileTypeFlag
isGzipped
	^false
%

category: 'printing'
method: FileTypeFlag
printOpenModeStringOn: aStream
%

! Class implementation for 'FileBinaryTypeFlag'

!		Class methods for 'FileBinaryTypeFlag'

category: 'accessing'
classmethod: FileBinaryTypeFlag
defaultPlatformValue
	^0
%

category: 'accessing'
classmethod: FileBinaryTypeFlag
posixName
	^'O_BINARY'
%

category: 'accessing'
classmethod: FileBinaryTypeFlag
windowsValue
	^1
%

!		Instance methods for 'FileBinaryTypeFlag'

category: 'printing'
method: FileBinaryTypeFlag
printOpenModeStringOn: aStream

	aStream nextPutAll: 'b'
%

! Class implementation for 'FileGzipTypeFlag'

!		Class methods for 'FileGzipTypeFlag'

category: 'accessing'
classmethod: FileGzipTypeFlag
defaultPlatformValue
	^0
%

category: 'instance creation'
classmethod: FileGzipTypeFlag
high
	^self new
		compressionLevel: '9';
		yourself
%

category: 'instance creation'
classmethod: FileGzipTypeFlag
low
	^self new
		compressionLevel: '1';
		yourself
%

category: 'instance creation'
classmethod: FileGzipTypeFlag
none
	^self new
%

category: 'accessing'
classmethod: FileGzipTypeFlag
posixName
	^'O_GZIP'
%

!		Instance methods for 'FileGzipTypeFlag'

category: 'accessing'
method: FileGzipTypeFlag
compressionLevel
	^compressionLevel ifNil: ['']
%

category: 'accessing'
method: FileGzipTypeFlag
compressionLevel: aString
	compressionLevel := aString
%

category: 'testing'
method: FileGzipTypeFlag
isGzipped
	^self compressionLevel = '1' or: [self compressionLevel = '9' ]
%

category: 'printing'
method: FileGzipTypeFlag
printOpenModeStringOn: aStream
	aStream nextPutAll: self compressionLevel
%

! Class implementation for 'FileTextTypeFlag'

!		Class methods for 'FileTextTypeFlag'

category: 'accessing'
classmethod: FileTextTypeFlag
defaultPlatformValue
	^0
%

category: 'accessing'
classmethod: FileTextTypeFlag
posixName
	^'O_TEXT'
%

! Class implementation for 'ProcessSpecificVariable'

!		Class methods for 'ProcessSpecificVariable'

category: 'ps - session temps'
classmethod: ProcessSpecificVariable
allocatePSKey: aPSVariable

	"Add a new process-specific key. 
	If an object already registered as a key, answer its index,
	if object is not registered, first search for an empty slot for insertion and if not found, grow an array to add new object"

	| anArray index |
	self psKeysSemaphore critical: [
		anArray := self psKeys.
		index := anArray indexOf: aPSVariable.
		index = 0 ifTrue: [
			anArray addLast: aPSVariable. 
			index := anArray size] ].

	aPSVariable isInheritable ifTrue: [ 
		(self inheritablePSKeys includes: index) ifFalse: [self inheritablePSKeys add: index]].

	^index
%

category: 'ps - session temps'
classmethod: ProcessSpecificVariable
inheritablePSKeys
	"In Pharo, this is class variable in Process"
	^SessionTemps current at: #InheritablePSKeys ifAbsentPut: [Array new]
%

category: 'class initialization'
classmethod: ProcessSpecificVariable
initialize
	self resetSoleInstance
%

category: 'testing'
classmethod: ProcessSpecificVariable
isInheritable
	^false
%

category: 'instance creation'
classmethod: ProcessSpecificVariable
new
	| instance |
	instance := super new.
	instance index: (self allocatePSKey: instance).
	^ instance
%

category: 'ps - session temps'
classmethod: ProcessSpecificVariable
psKeys
	"In Pharo, this is class variable in Process"
	^SessionTemps current at: #PSKeys ifAbsentPut: [Array new]
%

category: 'ps - session temps'
classmethod: ProcessSpecificVariable
psKeysSemaphore
	"In Pharo, this is class variable in Process"
	^SessionTemps current at: #PSKeysSemaphore ifAbsentPut: [Semaphore forMutualExclusion]
%

category: 'class initialization'
classmethod: ProcessSpecificVariable
resetAllSoleInstances
	" ProcessSpecificVariable resetAllSoleInstances "
	self resetSoleInstance.
	self resetPsKeys.
	self resetInheritablePSKeys.
	self allSubclasses do: [:ea | ea resetSoleInstance]
%

category: 'ps - session temps'
classmethod: ProcessSpecificVariable
resetInheritablePSKeys
	^SessionTemps current at: #InheritablePSKeys put: [Array new]
%

category: 'ps - session temps'
classmethod: ProcessSpecificVariable
resetPsKeys
	^SessionTemps current at: #PSKeys put: Array new
%

category: 'class initialization'
classmethod: ProcessSpecificVariable
resetSoleInstance
	soleInstance := nil.
%

category: 'accessing'
classmethod: ProcessSpecificVariable
soleInstance
	^ soleInstance ifNil: [ soleInstance := self new ]
%

category: 'accessing'
classmethod: ProcessSpecificVariable
value
	"Answer the current value for this variable in the current context."
	^ self soleInstance value
%

!		Instance methods for 'ProcessSpecificVariable'

category: 'accessing'
method: ProcessSpecificVariable
default
	"Answer the default value for the variable. The default for the default value is nil."
	^nil
%

category: 'accessing'
method: ProcessSpecificVariable
index
	^index
%

category: 'accessing'
method: ProcessSpecificVariable
index: anInteger
	index := anInteger
%

category: 'inheriting'
method: ProcessSpecificVariable
isInheritable
	^self class isInheritable
%

category: 'process'
method: ProcessSpecificVariable
psValueAt: anIndex
	"In Pharo, this logic is in Process"
	^self _psValueAt: anIndex inProcess: Processor activeProcess
%

category: 'process'
method: ProcessSpecificVariable
psValueAt: anIndex put: anObject
	"In Pharo, this logic is in Process"
	self _psValueAt: anIndex put: anObject inProcess: Processor activeProcess
%

category: 'accessing'
method: ProcessSpecificVariable
value
	"This is a hack. There may need to be something implemented to tie a value to a process in the future."

	^(self psValueAt: index) ifNil: [ self default ]
%

category: 'accessing'
method: ProcessSpecificVariable
valueOrNil
	"a faster version, which doesn't using ifAbsent: to avoid using block closure"

	^self psValueAt: index
%

category: 'process - private'
method: ProcessSpecificVariable
_growArrayInProcess: aProcess

	| currentSize cacheSize  |
	currentSize := (self _psArrayInProcess: aProcess) size.
	cacheSize := self class psKeys size.
	currentSize < cacheSize ifTrue: [
		self 
			_psArray: ((self _psArrayInProcess: aProcess) grownBy: cacheSize - currentSize) 
			inProcess: aProcess].
%

category: 'process - private'
method: ProcessSpecificVariable
_psArray: anArray inProcess: aProcess 

	aProcess environmentAt: #ProcessSpecificVariables put: anArray
%

category: 'process - private'
method: ProcessSpecificVariable
_psArrayInProcess: aProcess

	^aProcess 
			environmentAt: #ProcessSpecificVariables 
			ifAbsent: [aProcess environmentAt: #ProcessSpecificVariables put: (Array new: self class psKeys size)]
%

category: 'process - private'
method: ProcessSpecificVariable
_psValueAt: anIndex inProcess: aProcess

	^(aProcess environmentAt: #ProcessSpecificVariables ifAbsent: [nil]) 
		ifNotNil: [:a | a at: index ifAbsent: [] ]
%

category: 'process - private'
method: ProcessSpecificVariable
_psValueAt: anIndex put: anObject inProcess: aProcess

	self _growArrayInProcess: aProcess.
	^(self _psArrayInProcess: aProcess) at: anIndex put: anObject
%

! Class implementation for 'DynamicVariable'

!		Class methods for 'DynamicVariable'

category: 'accessing'
classmethod: DynamicVariable
value: anObject during: aBlock

	^ self soleInstance value: anObject during: aBlock
%

!		Instance methods for 'DynamicVariable'

category: 'accessing'
method: DynamicVariable
value: anObject during: aBlock

"	The following is the Pharo code "

	| oldValue |
	"oldValue will be nil if the variable has not yet been set"
	oldValue := self psValueAt: index.
	^ [ self
			psValueAt: index
			put: anObject.
		aBlock value ] ensure: [ 
			self
				psValueAt: index
				put: oldValue ]
%

! Class implementation for 'ZnDefaultCharacterEncoder'

!		Instance methods for 'ZnDefaultCharacterEncoder'

category: 'accessing'
method: ZnDefaultCharacterEncoder
default
	^ ZnCharacterEncoder utf8
%

! Class implementation for 'Stdio'

!		Class methods for 'Stdio'

category: 'initialize'
classmethod: Stdio
cleanStdioHandles
	#(stdin stdout stderr) do: [:ea | SessionTemps current removeKey: ea ifAbsent: []]
%

category: 'accessing'
classmethod: Stdio
createReadStreamBlock
	^ SessionTemps current at: #StdioCreateReadStreamBlock ifAbsentPut: [ self useDefaultStreams]
%

category: 'accessing'
classmethod: Stdio
createReadStreamBlock: aBlock
	SessionTemps current at: #StdioCreateReadStreamBlock put: aBlock.
	self cleanStdioHandles
%

category: 'accessing'
classmethod: Stdio
createWriteStreamBlock
	^ SessionTemps current at: #StdioCreateWriteStreamBlock ifAbsentPut: [ self useDefaultStreams]
%

category: 'accessing'
classmethod: Stdio
createWriteStreamBlock: aBlock
	SessionTemps current at: #StdioCreateWriteStreamBlock put: aBlock.
	self cleanStdioHandles
%

category: 'accessing'
classmethod: Stdio
stderr
	^self stdioWriteStreamFor: #stderr
%

category: 'accessing'
classmethod: Stdio
stdin
	^self stdioReadStreamFor: #stdin
%

category: 'stdio'
classmethod: Stdio
stdioReadStreamFor: aSymbol
	SessionTemps current at: #StdioCreateReadStreamBlock ifAbsent: [self useDefaultStreams].
	^SessionTemps current at: aSymbol ifAbsentPut: [
		self createReadStreamBlock value: aSymbol]
%

category: 'stdio'
classmethod: Stdio
stdioWriteStreamFor: aSymbol
	SessionTemps current at: #StdioCreateWriteStreamBlock ifAbsent: [self useDefaultStreams].
	^SessionTemps current at: aSymbol ifAbsentPut: [
		self createWriteStreamBlock value: aSymbol]
%

category: 'accessing'
classmethod: Stdio
stdout
	^self stdioWriteStreamFor: #stdout
%

category: 'stdio'
classmethod: Stdio
useDefaultStreams
	<script>
	| aBlock |
	aBlock := [ :aName | StdioStream on: (FileSystem fileClass perform: aName asSymbol) ].
	self createWriteStreamBlock: aBlock.
	self createReadStreamBlock: aBlock
%

category: 'stdio'
classmethod: Stdio
useMemoryStreams
	<script>
	self createWriteStreamBlock: 
		[ :aName | (FileSystem memory / aName asString) writeStream ].
	self createReadStreamBlock: 
		[ :aName | 
			EncodedBufferedStreamSpec newRead create on: (FileSystem memory / aName asString)  ].
%

! Class implementation for 'ZnBase64Encoder'

!		Class methods for 'ZnBase64Encoder'

category: 'class initialization'
classmethod: ZnBase64Encoder
defaultAlphabet
	^DefaultAlphabet
%

category: 'class initialization'
classmethod: ZnBase64Encoder
defaultInverse
	^DefaultInverse
%

category: 'class initialization'
classmethod: ZnBase64Encoder
initialize
	DefaultAlphabet := String withAll: ($A to: $Z) , ($a to: $z) , ($0 to: $9) , #($+ $/).
	DefaultInverse := Array new: 128.
	0 to: 127 do: [ :each | 
		| offset |
		offset := DefaultAlphabet indexOf: each asCharacter ifAbsent: [ nil ].
		DefaultInverse at: each + 1 put: (offset ifNotNil: [ offset - 1 ]) ]
%

category: 'instance creation'
classmethod: ZnBase64Encoder
new
	^self basicNew
		initialize;
		yourself
%

category: 'class initialization'
classmethod: ZnBase64Encoder
reset
	DefaultAlphabet := nil.
	DefaultInverse := nil
%

!		Instance methods for 'ZnBase64Encoder'

category: 'accessing'
method: ZnBase64Encoder
alphabet
	"Return the alphabet that I am using to encode byte values"
	
	^ alphabet
%

category: 'initialization'
method: ZnBase64Encoder
alphabet: string
	"Set the alphabet to use to string, containing 64 characters to represent 64 byte values.
	I automatically compute the inverse used for fast decoding."
	
	self assert: string size = 64 description: '64 characters are needed for a Base64 alphabet'.
	alphabet := string.
	inverse := Array new: 128.
	0 to: 127 do: [ :each | 
		| offset |
		offset := alphabet indexOf: each asCharacter ifAbsent: [ nil ].
		inverse at: each + 1 put: (offset ifNotNil: [ offset - 1 ]) ]
%

category: 'initialization'
method: ZnBase64Encoder
breakLines
	"Configure me to break lines and insert newlines every 76 characters while encoding"

	self breakLinesAt: 76
%

category: 'initialization'
method: ZnBase64Encoder
breakLinesAt: length
	"Configure me to break lines at lenth, a multiple of 4, and insert newlines"

	self assert: (length \\ 4) = 0 description: 'line length should be a multiple of 4'.
	lineLength := length.
	lineEnd ifNil: [ self lineEndConvention: #crlf ]
%

category: 'private'
method: ZnBase64Encoder
byteCountFor: string	
	| n byteCount |
	"This assumes there are no line breaks in string"
	n := string size.
	byteCount := n // 4 * 3.
	(n > 1 and: [ (string at: n) = $= ])
		ifTrue: [ 
			(n > 2 and: [ (string at: n - 1) = $= ])
				ifTrue: [ byteCount := byteCount - 2 ]
				ifFalse: [ byteCount := byteCount - 1 ] ].
	^ byteCount
%

category: 'private'
method: ZnBase64Encoder
characterCountFor: bytes
	| n characterCount | 
	n := bytes size.
	characterCount := n // 3 + (n \\ 3) sign * 4.
	^ lineLength isNil
		ifTrue: [ characterCount ]
		ifFalse: [ characterCount + ((characterCount // lineLength ) * lineEnd size) ]
%

category: 'private'
method: ZnBase64Encoder
characterForValue: value
	^ alphabet at: value + 1
%

category: 'converting'
method: ZnBase64Encoder
decode: string
	"Decode a Base64 encoded string and return the resulting byte array"
	
	^ ByteArray 
		new: (self byteCountFor: string)
		streamContents: [ :byteStream | 
			self decode: string readStream to: byteStream ]
%

category: 'private'
method: ZnBase64Encoder
decode: char1 and: char2 and: char3 and: char4 to: stream
	| v1 v2 v3 v4 |
	v1 := self valueForCharacter: char1.
	v2 := self valueForCharacter: char2.
	stream nextPut: (v1 bitShift: 2) + (v2 bitShift: -4).
	char3 == $=
		ifFalse: [ 
			v3 := self valueForCharacter: char3.
			stream nextPut: ((v2 bitAnd: 2r1111) bitShift: 4) + (v3 bitShift: -2).
			char4 == $=
				ifFalse: [ 
						v4 := self valueForCharacter: char4.
						stream nextPut: ((v3 bitAnd: 2r11) bitShift: 6) + v4 ] ]
%

category: 'converting'
method: ZnBase64Encoder
decode: stringStream to: byteStream
	| char1 char2 char3 char4 |
	[ stringStream atEnd ] whileFalse: [ 
		self skipWhitespace: stringStream.
		stringStream atEnd ifTrue: [ ^ self ].
		char1 := stringStream next.
		char2 := stringStream next.
		char3 := stringStream next.
		char4 := stringStream next.
		char1 isNil | char2 isNil | char3 isNil | char4 isNil
			ifTrue: [ ZnCharacterEncodingError signal: 'Illegal Base64 input' ].
		self decode: char1 and: char2 and: char3 and: char4 to: byteStream ]
%

category: 'converting'
method: ZnBase64Encoder
encode: byteArray
	"Encode byteArray using Base64 encoding and return the resulting string"
	
	^ String 
		new: (self characterCountFor: byteArray) 
		streamContents: [ :stringStream | 
			self encode: byteArray readStream to: stringStream ]
%

category: 'private'
method: ZnBase64Encoder
encode: byte1 and: byte2 and: byte3 to: stream
	stream nextPut: (self characterForValue: (byte1 bitShift: -2)).
	byte2
		ifNil: [ 
			stream nextPut: (self characterForValue: ((byte1 bitAnd: 2r11) bitShift: 4)).
			stream nextPutAll: '==' ]
		ifNotNil: [ 
			stream nextPut: (self characterForValue: (((byte1 bitAnd: 2r11) bitShift: 4) + (byte2 bitShift: -4))).
			byte3
				ifNil: [ 
					stream nextPut: (self characterForValue: ((byte2 bitAnd: 2r1111) bitShift: 2)).
					stream nextPut: $= ]
				ifNotNil: [ 
					stream nextPut: (self characterForValue: (((byte2 bitAnd: 2r1111) bitShift: 2) + (byte3 bitShift: -6))).
					stream nextPut: (self characterForValue: (byte3 bitAnd: 2r111111)) ] ]
%

category: 'converting'
method: ZnBase64Encoder
encode: byteStream to: stringStream
	| byte1 byte2 byte3 count |
	lineLength
		ifNil: [ 
			[ byteStream atEnd ] whileFalse: [ 
				byte1 := byteStream next.
				byte2 := byteStream next.
				byte3 := byteStream next.
				self encode: byte1 and: byte2 and: byte3 to: stringStream ] ]
		ifNotNil: [ 
			count := 0.
			[ byteStream atEnd ] whileFalse: [ 
				byte1 := byteStream next.
				byte2 := byteStream next.
				byte3 := byteStream next.
				self encode: byte1 and: byte2 and: byte3 to: stringStream.
				(count := count + 4) = lineLength
					ifTrue: [ 
						stringStream nextPutAll: lineEnd.
						count := 0 ] ] ]
%

category: 'initialization'
method: ZnBase64Encoder
initialize
	alphabet := DefaultAlphabet.
	inverse := DefaultInverse.
	self whitespace: #any
%

category: 'private'
method: ZnBase64Encoder
isLegalCharacter: character
	"Return true when character is part of my alphabet"

	| code |
	^ (code := character asciiValue) < 128 
			and: [ (inverse at: code + 1) notNil ]
%

category: 'private'
method: ZnBase64Encoder
isWhitespaceCharacter: character
	"Return true when character should be considered whitespace"

	whitespace 
		ifNil: [ "No whitespace allowed" 
			^ false ].
	whitespace = #separator
		ifTrue: [ "Only separators are considered whitespace" 
			^ character isSeparator ].
	whitespace = #any
		ifTrue: [ "All non-legal (non-alphabet) characters are considered whitespace" 
			^ (self isLegalCharacter: character) not ].
	^ false
%

category: 'initialization'
method: ZnBase64Encoder
lineEndConvention: symbol
	"Set the end of line convention to be used.
	Either #cr, #lf or #crlf (the default)."
	
	self assert: (#(cr lf crlf) includes: symbol).
	lineEnd := String perform: symbol
%

category: 'private'
method: ZnBase64Encoder
skipWhitespace: stream
	[ stream atEnd not and: [ (self isWhitespaceCharacter: stream peek) ] ] 
		whileTrue: [ stream next ]
%

category: 'private'
method: ZnBase64Encoder
valueForCharacter: char
	| code |
	(code := char asciiValue) < 128
		ifTrue: [ 
			(inverse at: code + 1)
				ifNotNil: [ :byteValue | ^ byteValue ] ].
	ZnCharacterEncodingError signal: 'Illegal Base64 input'
%

category: 'initialization'
method: ZnBase64Encoder
whitespace: mode
	"Set the whitespace mode: 
	nil is no whitespace allowed, 
	#separator is CR, LF, FF, SPACE, TAB allowed,
	#any is all non-alphabet characters allowed (the default)"
	
	self assert: (#(nil separator any) includes: mode).
	whitespace := mode
%

! Class implementation for 'ZnBufferedReadStream'

!		Class methods for 'ZnBufferedReadStream'

category: 'instance creation'
classmethod: ZnBufferedReadStream
new

	^ self basicNew
		initialize;
		yourself
%

category: 'instance creation'
classmethod: ZnBufferedReadStream
on: readStream
	^ self new
		on: readStream;
		yourself
%

category: 'convenience'
classmethod: ZnBufferedReadStream
on: readStream do: block
	"Execute block with as argument a ZnBufferedReadStream on readStream.
	Return the value of block."

	^ block value: (self on: readStream)
%

!		Instance methods for 'ZnBufferedReadStream'

category: 'testing'
method: ZnBufferedReadStream
atEnd
	^ position > limit and: [ stream atEnd ]
	
%

category: 'initialize-release'
method: ZnBufferedReadStream
close
	stream close
%

category: 'testing'
method: ZnBufferedReadStream
closed
	^ stream closed
%

category: 'accessing'
method: ZnBufferedReadStream
collectionSpecies
	^ stream isBinary
		ifTrue: [ ByteArray ]
		ifFalse: [ String ]
%

category: 'accessing'
method: ZnBufferedReadStream
contents
	
	^ self upToEnd
%

category: 'accessing'
method: ZnBufferedReadStream
defaultBufferSize
	^ 2 raisedToInteger: 16
%

category: 'private'
method: ZnBufferedReadStream
discardBuffer
	limit := 0.
	position := 1
%

category: 'initialization'
method: ZnBufferedReadStream
initialize

	position := 1.
	limit := 0
%

category: 'accessing-bytes'
method: ZnBufferedReadStream
int16
	^ self nextIntegerOfSize: 2 signed: true bigEndian: true 
%

category: 'accessing-bytes'
method: ZnBufferedReadStream
int32
	^ self nextIntegerOfSize: 4 signed: true bigEndian: true
%

category: 'accessing-bytes'
method: ZnBufferedReadStream
int8
	^ self nextIntegerOfSize: 1 signed: true bigEndian: true 
%

category: 'accessing'
method: ZnBufferedReadStream
isBinary
	^ stream isBinary
%

category: 'testing'
method: ZnBufferedReadStream
isStream
	^ true
%

category: 'accessing'
method: ZnBufferedReadStream
next
	"Return the next element and move over it"
	
	position > limit
		ifTrue: [ self nextBuffer ].
	^ position <= limit
		ifTrue: [ 
			| char |
			char := buffer at: position.
			position := position + 1.
			char ]
		ifFalse: [ nil ]
%

category: 'accessing'
method: ZnBufferedReadStream
next: requestedCount 
	"Read requestedCount elements and return them as a collection.
	If less are available, a smaller collection will be returned."

	^ self 
		next: requestedCount 
		into: (self collectionSpecies new: requestedCount)
%

category: 'accessing'
method: ZnBufferedReadStream
next: requestedCount into: collection
	"Read requestedCount elements into collection,
	returning a copy if less elements are available"
	
	^ self 
		next: requestedCount 
		into: collection 
		startingAt: 1   
%

category: 'accessing'
method: ZnBufferedReadStream
next: requestedCount into: collection startingAt: offset
	"Read requestedCount elements into collection starting at offset,
	returning a copy if less elements are available"
	
	| read |
	read := self 
		readInto: collection 
		startingAt: offset 
		count: requestedCount.
	^ read = requestedCount 
		ifTrue: [ collection ]
		ifFalse: [ collection copyFrom: 1 to: offset + read - 1 ]     
%

category: 'private'
method: ZnBufferedReadStream
nextBuffer
	stream atEnd ifTrue: [ ^ self ].
	limit := stream readInto: buffer startingAt: 1 count: buffer size.
	position := 1
%

category: 'accessing-bytes'
method: ZnBufferedReadStream
nextInt32
	^ self nextIntegerOfSize: 4 signed: true bigEndian: true
%

category: 'accessing-bytes'
method: ZnBufferedReadStream
nextIntegerOfSize: numberOfBytes signed: signed bigEndian: bigEndian
	"Assuming the receiver is a stream of bytes, read the next integer of size numberOfBytes.
	If bigEndian is true, use network byte order, most significant byte first, 
	else use little endian order, least significant byte first.
	If signed is true, interpret as a two-complement signed value, 
	else interpret as a plain unsigned value."
	
	| value |
	value := 0.
	bigEndian
		ifTrue: [ 
			(numberOfBytes - 1) * 8 to: 0 by: -8 do: [ :shift |
				value := value + (self next bitShift: shift) ] ]
		ifFalse: [ 
			0 to: (numberOfBytes - 1) * 8 by: 8 do: [ :shift |
				value := value + (self next bitShift: shift) ] ].
	^ (signed and: [ (value bitAt: numberOfBytes * 8) = 1 ])
		ifTrue: [ value - (1 << (numberOfBytes * 8)) ]
		ifFalse: [ value ]
%

category: 'accessing'
method: ZnBufferedReadStream
nextInto: collection
	"Read the next elements of the receiver into collection,
	returning a copy if less elements are available"
	
	^ self
		next: collection size
		into: collection
%

category: 'accessing-bytes'
method: ZnBufferedReadStream
nextLittleEndianNumber: numberOfBytes
	^ self nextIntegerOfSize: numberOfBytes signed: false bigEndian: false
%

category: 'accessing-bytes'
method: ZnBufferedReadStream
nextNumber: numberOfBytes
	^ self nextIntegerOfSize: numberOfBytes signed: false bigEndian: true
%

category: 'accessing-bytes'
method: ZnBufferedReadStream
nextWord
	^ self nextIntegerOfSize: 2 signed: false bigEndian: true
%

category: 'initialize-release'
method: ZnBufferedReadStream
on: readStream
	stream := readStream.
	self sizeBuffer: self defaultBufferSize
%

category: 'accessing'
method: ZnBufferedReadStream
peek
	"Return the next element but do not move over it"
	
	position > limit
		ifTrue: [ self nextBuffer ].
	^ position <= limit
		ifTrue: [ buffer at: position ]
		ifFalse: [ nil ]
%

category: 'accessing'
method: ZnBufferedReadStream
peek: count
  self peek .
  ^ buffer copyFrom: position to: (position + count min: limit)
%

category: 'accessing'
method: ZnBufferedReadStream
peekFor: object
	"Answer false and do not move over the next element if it is not equal to object, or if the receiver is at the end. 
	Answer true and move over the next element when it is equal to object."

	^ self peek = object
		ifTrue: [ 
			self next.
			true ]
		ifFalse: [ false ]
%

category: 'accessing'
method: ZnBufferedReadStream
position
	
	"If the buffer advanced, we need to check the original stream position, minus what we have read.
	The -1 is because the buffer is base 1"
	^ stream position - limit + position - 1
%

category: 'accessing'
method: ZnBufferedReadStream
position: anInteger 
	
	| bufferEnd bufferStart |
	bufferEnd := stream position.
	bufferStart := bufferEnd - limit.
	(anInteger between: bufferStart and: bufferEnd)
		ifTrue: [ position := anInteger - bufferStart + 1 ]
		ifFalse: [ 
			"We reset the buffer and update the position in the underlying stream"
			limit := 0.
			position := 1.
			stream position: anInteger ]
%

category: 'private'
method: ZnBufferedReadStream
readFromBufferInto: collection startingAt: offset count: requestedCount
	"Read up to requestedCount elements into collection starting at offset,
	from my buffer, answering the number of elements read.
	There could be fewer elements buffered."

	| read |
	read := 0.
	position <= limit
		ifTrue: [ read := limit - position + 1 min: requestedCount.
			collection
				replaceFrom: offset
				to: offset + read - 1
				with: buffer
				startingAt: position.
			position := position + read ].
	^ read
%

category: 'accessing'
method: ZnBufferedReadStream
readInto: collection startingAt: offset count: requestedCount
	"Read requestedCount elements into collection starting at offset,
	answering the number of elements read, there could be fewer elements available."

	| countRead countYetToRead |
	"First, read from elements already in my buffer."
	countRead := self readFromBufferInto: collection startingAt: offset count: requestedCount.
	countYetToRead := requestedCount - countRead.
	countYetToRead > 0
		ifTrue: [ "See if there are more elements to be read from the underlying stream"
			| newOffset |
			newOffset := offset + countRead.
			(self shouldBufferReadOfCount: countYetToRead)
				ifTrue: [ self nextBuffer.
					position > limit ifTrue: [ ^ countRead ].
					limit > 0
						ifTrue:
							[ countRead := countRead + (self readInto: collection startingAt: newOffset count: countYetToRead) ] ]
				ifFalse:
					[ countRead := countRead + (stream readInto: collection startingAt: newOffset count: countYetToRead) ] ].
	^ countRead
%

category: 'accessing'
method: ZnBufferedReadStream
setToEnd
	
	stream setToEnd
%

category: 'private'
method: ZnBufferedReadStream
shouldBufferReadOfCount: elementCount
	"For larger read requests, buffering fails to give an advantage."

	^ elementCount < (buffer size / 2)
%

category: 'accessing'
method: ZnBufferedReadStream
size

	^ stream size
%

category: 'initialize-release'
method: ZnBufferedReadStream
sizeBuffer: size
	buffer := self collectionSpecies new: size
%

category: 'accessing'
method: ZnBufferedReadStream
skip: count
	"Skip over count elements.
	This could be further optimzed."
	
	count timesRepeat: [ self next ]
%

category: 'accessing-bytes'
method: ZnBufferedReadStream
uint16
	^ self nextIntegerOfSize: 2 signed: false bigEndian: true 
%

category: 'accessing-bytes'
method: ZnBufferedReadStream
uint32
	^ self nextIntegerOfSize: 4 signed: false bigEndian: true 
%

category: 'accessing-bytes'
method: ZnBufferedReadStream
uint8
	^ self nextIntegerOfSize: 1 signed: false bigEndian: true
%

category: 'accessing'
method: ZnBufferedReadStream
upTo: value 
	"Read upto but not including value and return them as a collection.
	If value is not found, return the entire contents of the stream.
	This could be further optimzed."
	
	^ self collectionSpecies 
		streamContents: [ :writeStream | | element |
			[ self atEnd or: [ (element := self next) = value ] ] whileFalse: [ 
				writeStream nextPut: element ] ]
%

category: 'accessing'
method: ZnBufferedReadStream
upToAll: aCollection 
"Answer a subcollection from the current access position to the occurrence (if any, but not inclusive) of aCollection. If aCollection is not in the stream, answer the entire rest of the stream."

| startPos endMatch result x |
startPos := self position.
x := self upTo: aCollection first.
self atEnd ifTrue: [ ^ x ].
2 to: aCollection size do: [:i |
	self peek = (aCollection at: i)
		ifTrue: [ self next ]
		ifFalse: [ self position: startPos.
		^ self upToEnd ] ].
endMatch := self position.
self position: startPos.
result := self next: endMatch - startPos - aCollection size.
self position: endMatch.
^ result
%

category: 'accessing'
method: ZnBufferedReadStream
upToEnd
	"Read elements until the stream is atEnd and return them as a collection."

	^ self collectionSpecies
		streamContents: [ :out | 
			[ self atEnd ] whileFalse: [ 
				position > limit
					ifTrue: [ self nextBuffer ].	
				out next: limit - position + 1 putAll: buffer startingAt: position.
				position := limit + 1 ] ]
%

category: 'accessing'
method: ZnBufferedReadStream
wrappedStream
	^ stream
%

! Class implementation for 'ZnBufferedReadWriteStream'

!		Class methods for 'ZnBufferedReadWriteStream'

category: 'instance creation'
classmethod: ZnBufferedReadWriteStream
on: writeStream
	^ self basicNew
		on: writeStream;
		yourself
%

category: 'convenience'
classmethod: ZnBufferedReadWriteStream
on: readStream do: block
	"Execute block with as argument a ZnBufferedReadStream on readStream.
	Return the value of block."

	^ block value: (self on: readStream)
%

!		Instance methods for 'ZnBufferedReadWriteStream'

category: 'testing'
method: ZnBufferedReadWriteStream
atEnd
	
	^ self readingActionDo: [ readStream atEnd ]
%

category: 'accessing'
method: ZnBufferedReadWriteStream
close
	
	writeStream flush.
	writeStream close.
%

category: 'testing'
method: ZnBufferedReadWriteStream
closed
	^ readStream closed
%

category: 'accessing'
method: ZnBufferedReadWriteStream
flush
	
	self writingActionDo: [ writeStream flush ]
%

category: 'testing'
method: ZnBufferedReadWriteStream
isBinary
	
	^ readStream isBinary
%

category: 'testing'
method: ZnBufferedReadWriteStream
isReadOnly
	
	^ false
%

category: 'testing'
method: ZnBufferedReadWriteStream
isStream

	^ true
%

category: 'accessing'
method: ZnBufferedReadWriteStream
next
	
	^ self readingActionDo: [ 
		readStream next ]
%

category: 'accessing'
method: ZnBufferedReadWriteStream
next: anInteger 
	
	^ self readingActionDo: [ 
		readStream next: anInteger ]
%

category: 'accessing'
method: ZnBufferedReadWriteStream
next: count putAll: collection

	self writingActionDo: [ 
		writeStream next: count putAll: collection ]
%

category: 'accessing'
method: ZnBufferedReadWriteStream
next: count putAll: collection startingAt: offset
	
	self writingActionDo: [
		writeStream next: count putAll: collection startingAt: offset ]
%

category: 'accessing'
method: ZnBufferedReadWriteStream
nextPut: aCharacter 
	
	self writingActionDo: [ writeStream nextPut: aCharacter ]
%

category: 'accessing'
method: ZnBufferedReadWriteStream
nextPutAll: aString 
	
	^ self writingActionDo: [ writeStream nextPutAll: aString ]
%

category: 'instance creation'
method: ZnBufferedReadWriteStream
on: aStream

	lastRead := true.
	readStream := ZnBufferedReadStream on: aStream.
	writeStream := ZnBufferedWriteStream on: aStream.
%

category: 'accessing'
method: ZnBufferedReadWriteStream
peek
	
	^ self readingActionDo: [ 
		readStream peek ]
%

category: 'accessing'
method: ZnBufferedReadWriteStream
position
	
	^ lastRead
		ifTrue: [ readStream position ]
		ifFalse: [ writeStream position ]
%

category: 'accessing'
method: ZnBufferedReadWriteStream
position: anInteger 
	
	self writingActionDo: [ 
		writeStream position: anInteger ]
	
%

category: 'private'
method: ZnBufferedReadWriteStream
readingActionDo: aBlock

	"Reading from the read stream.
	We should 
	 - flush the write stream
	 - discard the read buffer (which may contain incorrect data).
	 - and then perform the read."
	
	lastRead ifFalse: [ 
		writeStream flush.
		readStream discardBuffer ].
	^ aBlock ensure: [ lastRead := true ]
%

category: 'accessing'
method: ZnBufferedReadWriteStream
readInto: collection startingAt: offset count: requestedCount
	
	^ self readingActionDo: [ 
		readStream readInto: collection startingAt: offset count: requestedCount ]
%

category: 'accessing'
method: ZnBufferedReadWriteStream
setToEnd
	
	^ self writingActionDo: [ 
		writeStream setToEnd ]
%

category: 'accessing'
method: ZnBufferedReadWriteStream
size
	^ readStream size
%

category: 'initialize-release'
method: ZnBufferedReadWriteStream
sizeBuffer: anInteger 
	
	readStream sizeBuffer: anInteger.
	writeStream sizeBuffer: anInteger.
%

category: 'accessing'
method: ZnBufferedReadWriteStream
skip: anInteger 
	
	self readingActionDo: [ 
		readStream skip: anInteger ]
%

category: 'accessing'
method: ZnBufferedReadWriteStream
truncate
	
	self writingActionDo: [ writeStream truncate ]
%

category: 'accessing'
method: ZnBufferedReadWriteStream
truncate: anInteger 
	
	self writingActionDo: [ writeStream truncate: anInteger ]
%

category: 'accessing'
method: ZnBufferedReadWriteStream
upTo: aCharacter 
	
	^ self readingActionDo: [ readStream upTo: aCharacter ]
%

category: 'accessing'
method: ZnBufferedReadWriteStream
upToEnd
	
	^ self readingActionDo: [ readStream upToEnd ]
%

category: 'accessing'
method: ZnBufferedReadWriteStream
wrappedStream

	^ readStream wrappedStream
%

category: 'accessing'
method: ZnBufferedReadWriteStream
writingActionDo: aBlock
	
	"Writing to the write stream.
	We should 
	 - write the write stream
	 - discard the read buffer (which may contain incorrect data)"
	lastRead ifTrue: [ 
		writeStream discardBuffer ].
	readStream discardBuffer.
	^ aBlock ensure: [ lastRead := false ]
%

! Class implementation for 'ZnBufferedWriteStream'

!		Class methods for 'ZnBufferedWriteStream'

category: 'instance creation'
classmethod: ZnBufferedWriteStream
on: writeStream
	^ self basicNew
		on: writeStream;
		yourself
%

category: 'convenience'
classmethod: ZnBufferedWriteStream
on: writeStream do: block
	"Execute block with as argument a ZnBufferedWriteStream on writeStream,
	making sure #flush is called at the end. Return the value of block."
	
	| bufferedWriteStream result |
	bufferedWriteStream := self on: writeStream.
	result := block value: bufferedWriteStream.
	bufferedWriteStream flush.
	^ result
%

!		Instance methods for 'ZnBufferedWriteStream'

category: 'private'
method: ZnBufferedWriteStream
buffer
	buffer isNil 
		ifTrue: [ self sizeBuffer: self defaultBufferSize ].
	^ buffer
%

category: 'accessing'
method: ZnBufferedWriteStream
bufferFreeSize
	^ self bufferSize - position
%

category: 'accessing'
method: ZnBufferedWriteStream
bufferSize
	^ buffer isNil 
		ifTrue: [ self defaultBufferSize ]
		ifFalse: [ buffer size ]
%

category: 'initialize-release'
method: ZnBufferedWriteStream
close
	self flush.
	stream close
%

category: 'accessing'
method: ZnBufferedWriteStream
cr
	self nextPut: Character cr
%

category: 'accessing'
method: ZnBufferedWriteStream
crlf
	self cr; lf
%

category: 'accessing'
method: ZnBufferedWriteStream
defaultBufferSize
	^ 2 raisedToInteger: 16
%

category: 'private'
method: ZnBufferedWriteStream
discardBuffer

	position := 0
%

category: 'accessing'
method: ZnBufferedWriteStream
flush
	self flushBuffer.
	stream flush
%

category: 'private'
method: ZnBufferedWriteStream
flushBuffer
	position = 0 ifTrue: [ ^ self ].
	position = self bufferSize
		ifTrue: [ 
			stream nextPutAll: buffer ]
		ifFalse: [ 
			(stream respondsTo: #next:putAll:startingAt:)  "This was added from Pharo"
				ifTrue: [ stream next: position putAll: buffer startingAt: 1 ]
				ifFalse: [ stream nextPutAll: (buffer copyFrom: 1 to: position) ] ].
	position := 0
%

category: 'private'
method: ZnBufferedWriteStream
flushBufferIfFull
	position = self bufferSize 
		ifTrue: [ self flushBuffer ]
%

category: 'accessing-bytes'
method: ZnBufferedWriteStream
int16: integer
	^ self nextIntegerOfSize: 2 signed: true bigEndian: true put: integer
%

category: 'accessing-bytes'
method: ZnBufferedWriteStream
int32: integer
	^ self nextIntegerOfSize: 4 signed: true bigEndian: true put: integer
%

category: 'accessing-bytes'
method: ZnBufferedWriteStream
int8: integer
	^ self nextIntegerOfSize: 1 signed: true bigEndian: true put: integer
%

category: 'testing'
method: ZnBufferedWriteStream
isBinary

	^ stream isBinary
%

category: 'testing'
method: ZnBufferedWriteStream
isStream

	^ true
%

category: 'accessing'
method: ZnBufferedWriteStream
lf
	self nextPut: Character lf
%

category: 'accessing'
method: ZnBufferedWriteStream
next: count putAll: collection
	"Write count elements from collection"
	
	self 
		next: count 
		putAll: collection 
		startingAt: 1
%

category: 'accessing'
method: ZnBufferedWriteStream
next: count putAll: collection startingAt: offset
	"Write count elements from collection starting at offset."
	
	self flushBufferIfFull.
	count <= self bufferFreeSize
		ifTrue: [
			self buffer replaceFrom: position + 1 to: position + count with: collection startingAt: offset.
			position := position + count ]
		ifFalse: [
			self flushBuffer.
			count > (self bufferSize / 2)
				ifTrue: [ stream next: count putAll: collection startingAt: offset ]
				ifFalse: [ self next: count putAll: collection startingAt: offset ] ]
%

category: 'accessing-bytes'
method: ZnBufferedWriteStream
nextInt32Put: integer
	^ self nextIntegerOfSize: 4 signed: true bigEndian: true put: integer
%

category: 'accessing-bytes'
method: ZnBufferedWriteStream
nextIntegerOfSize: numberOfBytes signed: signed bigEndian: bigEndian put: value
	"Assuming the receiver is a stream of bytes, write value as the next integer of size numberOfBytes.
	If bigEndian is true, use network byte order, most significant byte first, 
	else use little endian order, least significant byte first.
	If signed is true, encode as a two-complement signed value, 
	else encode as a plain unsigned value."
	
	| unsignedValue |
	unsignedValue := (signed and: [ value negative ])
		ifTrue: [ (1 << (numberOfBytes * 8)) + value ] 
		ifFalse: [ value ].
	(unsignedValue between: 0 and: (2 raisedTo: (numberOfBytes * 8)) - 1)
		ifFalse: [ self error: 'Domain Error ', unsignedValue printString, ' outside of expected range' ].
	bigEndian
		ifTrue: [ 
			numberOfBytes to: 1 by: -1 do: [ :index |
				self nextPut: (unsignedValue digitAt: index) ] ]
		ifFalse: [ 
			1 to: numberOfBytes do: [ :index |
				self nextPut: (unsignedValue digitAt: index) ] ].
	^ value
%

category: 'accessing-bytes'
method: ZnBufferedWriteStream
nextLittleEndianNumber: numberOfBytes put: integer
	^ self nextIntegerOfSize: numberOfBytes signed: false bigEndian: false put: integer
%

category: 'accessing-bytes'
method: ZnBufferedWriteStream
nextNumber: numberOfBytes put: integer
	^ self nextIntegerOfSize: numberOfBytes signed: false bigEndian: true put: integer
%

category: 'accessing'
method: ZnBufferedWriteStream
nextPut: object
	self flushBufferIfFull.
	position := position + 1.
	self buffer at: position put: object
%

category: 'accessing'
method: ZnBufferedWriteStream
nextPutAll: collection
	"Write a collection"
	
	self 
		next: collection size 
		putAll: collection 
		startingAt: 1
%

category: 'accessing-bytes'
method: ZnBufferedWriteStream
nextWordPut: integer
	^ self nextIntegerOfSize: 2 signed: false bigEndian: true put: integer
%

category: 'initialize-release'
method: ZnBufferedWriteStream
on: writeStream
	stream := writeStream.
	position := 0
%

category: 'accessing'
method: ZnBufferedWriteStream
position

	^ stream position + position
%

category: 'accessing'
method: ZnBufferedWriteStream
position: anInteger 
	self flush.
	stream position: anInteger
%

category: 'accessing'
method: ZnBufferedWriteStream
print: object
	object printOn: self
%

category: 'printing'
method: ZnBufferedWriteStream
printOn: aStream
	aStream 
		nextPutAll: 'a '; 
		nextPutAll: self class name
%

category: 'accessing'
method: ZnBufferedWriteStream
setToEnd
	
	self flush.
	stream setToEnd
%

category: 'accessing'
method: ZnBufferedWriteStream
sizeBuffer: size
	buffer := (stream isBinary ifTrue: [ ByteArray ] ifFalse: [ String ]) new: size
%

category: 'accessing'
method: ZnBufferedWriteStream
space
	self nextPut: Character space
%

category: 'accessing'
method: ZnBufferedWriteStream
tab
	self nextPut: Character tab
%

category: 'accessing'
method: ZnBufferedWriteStream
truncate
	
	stream truncate
%

category: 'accessing'
method: ZnBufferedWriteStream
truncate: anInteger 
	
	self flush.
	stream truncate: anInteger
%

category: 'accessing-bytes'
method: ZnBufferedWriteStream
uint16: integer
	^ self nextIntegerOfSize: 2 signed: false bigEndian: true put: integer
%

category: 'accessing-bytes'
method: ZnBufferedWriteStream
uint32: integer
	^ self nextIntegerOfSize: 4 signed: false bigEndian: true put: integer
%

category: 'accessing-bytes'
method: ZnBufferedWriteStream
uint8: integer
	^ self nextIntegerOfSize: 1 signed: false bigEndian: true put: integer
%

category: 'accessing'
method: ZnBufferedWriteStream
wrappedStream
	^stream
%

! Class implementation for 'ZnCharacterEncoder'

!		Class methods for 'ZnCharacterEncoder'

category: 'convenience'
classmethod: ZnCharacterEncoder
ascii
	^ self newForEncoding: 'ASCII'
%

category: 'accessing'
classmethod: ZnCharacterEncoder
canonicalEncodingIdentifier: string
	^ (string select: [ :each | each isAlphaNumeric ]) asLowercase
%

category: 'accessing'
classmethod: ZnCharacterEncoder
default
	"Return the default ZnCharacterEncoder to be used 
	when none is otherwise specified."
	
	^ ZnDefaultCharacterEncoder value
%

category: 'instance creation'
classmethod: ZnCharacterEncoder
detectEncoding: bytes
	"Return one of my instances capable of decoding bytes.
	This is done by successively trying known encodings in a specific order.
	If no one is found, signal ZnCharacterEncodingError.
	This is a heuristic and unreliable [https://en.wikipedia.org/wiki/Charset_detection]."
	
	| candidates |
	"Set up an ordered candidates list, 7-bit ascii and utf8 are reasonably reliable, iso88591 is a reasonable default"

	candidates := #(ascii utf8 iso88591).
	candidates := candidates , (ZnByteEncoder knownEncodingIdentifiers difference: candidates) asArray.
	candidates := candidates , (self knownEncodingIdentifiers difference: candidates) asArray.

	"Try each and return the first one that succeeeds."
	candidates do: [ :identifier | | encoder |
		encoder := self newForEncoding: identifier.
		[ ^ encoder decodeBytes: bytes; yourself ] on: ZnCharacterEncodingError do: [:s | s return: nil ] ]. "s return: nil <== This is needed to mimic Pharo"
	ZnCharacterEncodingError signal: 'No suitable encoder found'
%

category: 'accessing'
classmethod: ZnCharacterEncoder
handlesEncoding: string
	"Return true when my instances handle the encoding described by string"
	
	self subclassResponsibility: #handlesEncoding:
%

category: 'convenience'
classmethod: ZnCharacterEncoder
iso88591
	^ self newForEncoding: 'iso-8859-1'
%

category: 'accessing'
classmethod: ZnCharacterEncoder
knownEncodingIdentifiers
	"Return a collection of all known encoding identifiers in the system"
	
	self = ZnCharacterEncoder ifFalse: [ ^ #() ].
	^ Array streamContents: [ :all |
		self allSubclasses do: [ :subClass |
			all nextPutAll: subClass knownEncodingIdentifiers ] ]
%

category: 'convenience'
classmethod: ZnCharacterEncoder
latin1
	^ self newForEncoding: 'latin1'
%

category: 'instance creation'
classmethod: ZnCharacterEncoder
new
	^super new
		initialize;
		yourself
%

category: 'instance creation'
classmethod: ZnCharacterEncoder
newForEncoding: string
	"Return a new character encoder object for an encoding described by string.
	Search for a subclass that handles it and delegate (subclassResponsibility)."
	
	| concreteSubclass |
	concreteSubclass := self allSubclasses 
		detect: [ :each | each handlesEncoding: string ] 
		ifNone: [ ^ self default ].
	^ concreteSubclass newForEncoding: string
%

category: 'convenience'
classmethod: ZnCharacterEncoder
utf8
	^ ZnUTF8Encoder default
%

!		Instance methods for 'ZnCharacterEncoder'

category: 'comparing'
method: ZnCharacterEncoder
= anObject
	^ self class == anObject class
%

category: 'converting'
method: ZnCharacterEncoder
asZnCharacterEncoder
	^ self
%

category: 'converting'
method: ZnCharacterEncoder
backOnStream: stream
	"Move back one character on stream, assuming stream understands #back"
	
	self subclassResponsibility: #backOnStream:
%

category: 'initialize-release'
method: ZnCharacterEncoder
beLenient
	"Don't be strict, which is the default"
%

category: 'convenience'
method: ZnCharacterEncoder
decodeAsCodePoints: bytes
	"Decode bytes and return the resulting code points"
	
	| byteStream |
	byteStream := bytes readStream.
	^ Array streamContents: [ :stream |
		[ byteStream atEnd ] whileFalse: [
			stream nextPut: (self nextCodePointFromStream: byteStream) ] ]
%

category: 'convenience'
method: ZnCharacterEncoder
decodeBytes: bytes
	"Decode bytes and return the resulting string"
	
	| byteStream |
	byteStream := bytes readStream.
	^ String streamContents: [ :stream |
		[ byteStream atEnd ] whileFalse: [
			stream nextPut: (self nextFromStream: byteStream) ] ]
%

category: 'convenience'
method: ZnCharacterEncoder
encodeCodePoints: codePoints
	"Encode codePoints and return the resulting byte array"
	
	^ ByteArray streamContents: [ :stream |
		codePoints do: [ :each | 
			self nextPutCodePoint: each toStream: stream ] ]
%

category: 'converting'
method: ZnCharacterEncoder
encodedByteCountFor: character
	"Return how many bytes are needed to encode character"
	
	"We should use #codePoint but #asInteger is faster"
	
	^ self encodedByteCountForCodePoint: character asInteger
%

category: 'converting'
method: ZnCharacterEncoder
encodedByteCountForCodePoint: codePoint
	"Return how many bytes are needed to encode integer code point"
	
	self subclassResponsibility: #encodedByteCountForCodePoint:
%

category: 'convenience'
method: ZnCharacterEncoder
encodedByteCountForCodePoints: codePoints
	"Return the exact number of bytes it would take to encode codePoints as a byte array"

	^ codePoints 
		inject: 0 
		into: [ :sum :each |
			sum + (self encodedByteCountForCodePoint: each) ]
%

category: 'convenience'
method: ZnCharacterEncoder
encodedByteCountForString: string
	"Return the exact number of bytes it would take to encode string as a byte array"

	^ string 
		inject: 0 
		into: [ :sum :each |
			sum + (self encodedByteCountFor: each) ]
%

category: 'convenience'
method: ZnCharacterEncoder
encodeString: string
	"Encode string and return the resulting byte array"
	
	^ ByteArray streamContents: [ :stream |
		self next: string size putAll: string startingAt: 1 toStream: stream ]
%

category: 'error handling'
method: ZnCharacterEncoder
error: message
	^ ZnCharacterEncodingError signal: message
%

category: 'error handling'
method: ZnCharacterEncoder
errorIncomplete
	^ ZnIncomplete signal: 'Incomplete input for character decoding'
%

category: 'error handling'
method: ZnCharacterEncoder
errorOutsideRange
	^ self error: 'Character Unicode code point outside encoder range'
%

category: 'comparing'
method: ZnCharacterEncoder
hash
	^ self class hash
%

category: 'accessing'
method: ZnCharacterEncoder
identifier
	^ self subclassResponsibility
%

category: 'initialization'
method: ZnCharacterEncoder
initialize
%

category: 'convenience'
method: ZnCharacterEncoder
next: count putAll: string startingAt: offset toStream: stream
	"Write count characters from string starting at offset to stream."
	
	offset to: offset + count - 1 do: [ :index | 
		self nextPut: (string at: index) toStream: stream ]
%

category: 'converting'
method: ZnCharacterEncoder
nextCodePointFromStream: stream
	"Read and return the next integer code point from stream"
	
	self subclassResponsibility: #nextCodePointFromStream:
%

category: 'converting'
method: ZnCharacterEncoder
nextFromStream: stream
	"Read and return the next character from stream"
	
	"We should use #codePoint: but #value: is faster"
	
	^ Character withValue: (self nextCodePointFromStream: stream)
%

category: 'converting'
method: ZnCharacterEncoder
nextPut: character toStream: stream
	"Write the encoding for character to stream"
	
	"We should use #codePoint but #asInteger is faster"
	
	self nextPutCodePoint: character asInteger toStream: stream
%

category: 'converting'
method: ZnCharacterEncoder
nextPutCodePoint: codePoint toStream: stream
	"Write the encoding for Integer code point to stream"
	
	self subclassResponsibility: #nextPutCodePoint:toStream:
%

category: 'convenience'
method: ZnCharacterEncoder
readInto: string startingAt: offset count: requestedCount fromStream: stream
	"Read requestedCount characters into string starting at offset,
	returning the number read, there could be less available when stream is atEnd"

	offset to: offset + requestedCount - 1 do: [ :index |
		stream atEnd ifTrue: [ ^ index - offset ].  
		string at: index put: (self nextFromStream: stream) ].
	^ requestedCount
%

! Class implementation for 'ZnNullEncoder'

!		Class methods for 'ZnNullEncoder'

category: 'accessing'
classmethod: ZnNullEncoder
handlesEncoding: string
	"Return true when my instances handle the encoding described by string.
	Note that in principle I could handle latin1 (iso-8859-1) and ASCII, 
	although that is not completely correct. 
	To get maximum efficiency, it remains an option."
	
	^ (self canonicalEncodingIdentifier: string) = 'null'
%

category: 'accessing'
classmethod: ZnNullEncoder
knownEncodingIdentifiers
	^ #( 'null' )
%

category: 'instance creation'
classmethod: ZnNullEncoder
newForEncoding: string
	"No further parametrization needed"
	
	^ self new
%

!		Instance methods for 'ZnNullEncoder'

category: 'converting'
method: ZnNullEncoder
backOnStream: stream
	"Move back one character on stream"

	stream back
%

category: 'convenience'
method: ZnNullEncoder
decodeBytes: bytes
	"Decode bytes and return the resulting string"
	"Overwritten for performance reasons"

	^ bytes asString
%

category: 'converting'
method: ZnNullEncoder
encodedByteCountFor: character
	"Return how many bytes are needed to encode character"
	"Overwritten for performance reasons"

	^ 1
%

category: 'converting'
method: ZnNullEncoder
encodedByteCountForCodePoint: codePoint
	"Return how many bytes are needed to encode integer code point"

	^ 1
%

category: 'convenience'
method: ZnNullEncoder
encodedByteCountForCodePoints: codePoints
	"Return the exact number of bytes it would take to encode codePoints as a byte array"
	"Overwritten for performance reasons"

	^ codePoints size
%

category: 'convenience'
method: ZnNullEncoder
encodedByteCountForString: string
	"Return the exact number of bytes it would take to encode string as a byte array"
	"Overwritten for performance reasons"

	^ string size
%

category: 'convenience'
method: ZnNullEncoder
encodeString: string
	"Encode string and return the resulting byte array"
	"Overwritten for performance reasons"

	^ string asByteArray
%

category: 'accessing'
method: ZnNullEncoder
identifier
	^ #ascii
%

category: 'convenience'
method: ZnNullEncoder
next: count putAll: string startingAt: offset toStream: stream
	"Write count bytes from string starting at offset to stream."
	"Overwritten for performance reasons"

	string isByteString
		ifTrue: [ 
			offset to: offset + count - 1 do: [ :index | 
				stream nextPut: (string byteAt: index) ] ]
		ifFalse: [ 
			super next: count putAll: string startingAt: offset toStream: stream ]
%

category: 'converting'
method: ZnNullEncoder
nextCodePointFromStream: stream
	"Read and return the next integer code point from stream"

	^ stream next
%

category: 'converting'
method: ZnNullEncoder
nextPutCodePoint: codePoint toStream: stream
	"Write the encoding for Integer code point to stream"

	codePoint < 256
		ifTrue: [ stream nextPut: codePoint ]
		ifFalse: [ self errorOutsideRange ]
%

category: 'convenience'
method: ZnNullEncoder
readInto: string startingAt: offset count: requestedCount fromStream: stream
	"Read requestedCount characters into string starting at offset,
	returning the number read, there could be less available when stream is atEnd"
	"Overwritten for performance reasons"

	offset to: offset + requestedCount - 1 do: [ :index |
		stream atEnd ifTrue: [ ^ index - offset ].  
		string byteAt: index put: stream next ].
	^ requestedCount
%

! Class implementation for 'ZnSimplifiedByteEncoder'

!		Class methods for 'ZnSimplifiedByteEncoder'

category: 'mappings'
classmethod: ZnSimplifiedByteEncoder
asciiMapping
	"ASCII is only defined for the first 127 codes (7-bit)"

	^ #(
	nil nil nil nil nil nil nil nil 
	nil nil nil nil nil nil nil nil 
	nil nil nil nil nil nil nil nil 
	nil nil nil nil nil nil nil nil 
	nil nil nil nil nil nil nil nil 
	nil nil nil nil nil nil nil nil 
	nil nil nil nil nil nil nil nil 
	nil nil nil nil nil nil nil nil 
	nil nil nil nil nil nil nil nil 
	nil nil nil nil nil nil nil nil 
	nil nil nil nil nil nil nil nil 
	nil nil nil nil nil nil nil nil 
	nil nil nil nil nil nil nil nil 
	nil nil nil nil nil nil nil nil 
	nil nil nil nil nil nil nil nil 
	nil nil nil nil nil nil nil nil )
%

category: 'private'
classmethod: ZnSimplifiedByteEncoder
byteTextConverters
	^ byteTextConverters ifNil: [ self initializeByteTextConverters ]
%

category: 'accessing'
classmethod: ZnSimplifiedByteEncoder
handlesEncoding: string
	"Return true when my instances handle the encoding described by string"
	
	^ self byteTextConverters includesKey: (self canonicalEncodingIdentifier: string)
%

category: 'class initialization'
classmethod: ZnSimplifiedByteEncoder
initialize
	"Initialize and cache the converters that I know of.
	This includes all of their aliases.
	This method must be changed to make sure it runs 
	when loading in images where it is already present."
	
	self initializeByteTextConverters
%

category: 'private'
classmethod: ZnSimplifiedByteEncoder
initializeByteTextConverters
	"Initialize and cache convertors based on specifications in methods that were autogenerated."

	byteTextConverters := Dictionary new.
	self mappingToIdentifiers
		keysAndValuesDo: [ :mapping :identifiers | 
			| tables |
			tables := self tablesFromSpec: (self perform: mapping).
			identifiers do: [ :each | byteTextConverters at: each put: tables ] ].
	^ byteTextConverters
%

category: 'mappings'
classmethod: ZnSimplifiedByteEncoder
iso88591Mapping
	"Specification generated by my optional subclass, ZnByteEncoder."
	"ZnByteEncoder generateByteToUnicodeSpec: 'http://unicode.org/Public/MAPPINGS/ISO8859/8859-1.TXT'"

	^ #(
	nil nil nil nil nil nil nil nil 
	nil nil nil nil nil nil nil nil 
	nil nil nil nil nil nil nil nil 
	nil nil nil nil nil nil nil nil 
	16r00A0 16r00A1 16r00A2 16r00A3 16r00A4 16r00A5 16r00A6 16r00A7 
	16r00A8 16r00A9 16r00AA 16r00AB 16r00AC 16r00AD 16r00AE 16r00AF 
	16r00B0 16r00B1 16r00B2 16r00B3 16r00B4 16r00B5 16r00B6 16r00B7 
	16r00B8 16r00B9 16r00BA 16r00BB 16r00BC 16r00BD 16r00BE 16r00BF 
	16r00C0 16r00C1 16r00C2 16r00C3 16r00C4 16r00C5 16r00C6 16r00C7 
	16r00C8 16r00C9 16r00CA 16r00CB 16r00CC 16r00CD 16r00CE 16r00CF 
	16r00D0 16r00D1 16r00D2 16r00D3 16r00D4 16r00D5 16r00D6 16r00D7 
	16r00D8 16r00D9 16r00DA 16r00DB 16r00DC 16r00DD 16r00DE 16r00DF 
	16r00E0 16r00E1 16r00E2 16r00E3 16r00E4 16r00E5 16r00E6 16r00E7 
	16r00E8 16r00E9 16r00EA 16r00EB 16r00EC 16r00ED 16r00EE 16r00EF 
	16r00F0 16r00F1 16r00F2 16r00F3 16r00F4 16r00F5 16r00F6 16r00F7 
	16r00F8 16r00F9 16r00FA 16r00FB 16r00FC 16r00FD 16r00FE 16r00FF )
%

category: 'accessing'
classmethod: ZnSimplifiedByteEncoder
knownEncodingIdentifiers
	^ self byteTextConverters keys
%

category: 'mappings'
classmethod: ZnSimplifiedByteEncoder
mappingToIdentifiers
	"Return a dictionay mapping from encoding specifications to a list of encoding names."
	
	^ Dictionary newFromPairs: #( 
		#iso88591Mapping #('iso88591' 'latin1')
		#asciiMapping #('ascii') )
%

category: 'instance creation'
classmethod: ZnSimplifiedByteEncoder
newForEncoding: string
	"Return a new character encoder object for an encoding described by string.
	We use our precomputed ByteTextConverters tables."

	| tables canonicalName |
	canonicalName := self canonicalEncodingIdentifier: string.
	tables := self byteTextConverters at: canonicalName.
	^ self new
		identifier: canonicalName;
		byteToUnicode: tables first;
		unicodeToByte: tables second;
		yourself
%

category: 'private'
classmethod: ZnSimplifiedByteEncoder
tablesFromSpec: mapping
	"Initialize the mappings to and from Unicode based on the 128 element array mapping"

	| byteToUnicode unicodeToByte |
	byteToUnicode := Array new: 128.
	unicodeToByte := Dictionary new.	
	"Mind the offset because first 128 characters are not stored into byteToUnicodeSpec"	
	"Note that some entries are nil"
	mapping
		keysAndValuesDo: [ :index :unicode | 
			unicode ifNotNil: [ 
				byteToUnicode at: index put: (Character value: unicode).
				unicodeToByte at: unicode put: 127 + index ] ].
	^ Array with: byteToUnicode with: unicodeToByte
%

!		Instance methods for 'ZnSimplifiedByteEncoder'

category: 'comparing'
method: ZnSimplifiedByteEncoder
= anObject
	^ super = anObject and: [ self identifier = anObject identifier ]
%

category: 'converting'
method: ZnSimplifiedByteEncoder
backOnStream: stream
	stream back
%

category: 'initialize-release'
method: ZnSimplifiedByteEncoder
beLenient
	"Don't be strict, which is the default.
	This means that holes in the mapping are let to pass through."

	strict := false
%

category: 'accessing'
method: ZnSimplifiedByteEncoder
byteDomain
	"Return an array with the byte values that are in my domain, that I can decode"
	| a b index |
	a := (0 to: 127) asArray.
	index := 1.
	b := (byteToUnicode collect: [ :each | 
				each ifNotNil: [ index + 127 ]. 
				index := index + 1 ]) reject: [ :each | each isNil ].
	^  a , b
%

category: 'initialize-release'
method: ZnSimplifiedByteEncoder
byteToUnicode: map
	byteToUnicode := map
%

category: 'accessing'
method: ZnSimplifiedByteEncoder
characterDomain
	"Return a set with the characters that are in my domain, that I can encode"

	^ ((0 to: 127) asSet addAll: unicodeToByte keys; yourself) collect: [ :each | Character value: each  ]
%

category: 'converting'
method: ZnSimplifiedByteEncoder
encodedByteCountFor: character
	"Overwritten for performance reasons"
	
	^ 1
%

category: 'converting'
method: ZnSimplifiedByteEncoder
encodedByteCountForCodePoint: character
	^ 1
%

category: 'comparing'
method: ZnSimplifiedByteEncoder
hash
	^ self identifier hash
%

category: 'accessing'
method: ZnSimplifiedByteEncoder
identifier
	^ identifier
%

category: 'initialize-release'
method: ZnSimplifiedByteEncoder
identifier: object
	identifier := object
%

category: 'initialization'
method: ZnSimplifiedByteEncoder
initialize
	super initialize.
	strict := true
%

category: 'testing'
method: ZnSimplifiedByteEncoder
isLenient
	^ strict not
%

category: 'testing'
method: ZnSimplifiedByteEncoder
isStrict
	^ strict
%

category: 'converting'
method: ZnSimplifiedByteEncoder
nextCodePointFromStream: stream
	"In non-strict mode, we let byte values for holes in our mapping pass through"

	| byteValue |
	^ (byteValue := stream next) < 128
		ifTrue: [ byteValue ]
		ifFalse: [ 
			(byteToUnicode at: byteValue - 127 ifAbsent: [ nil ])
				ifNotNil: [ :unicode | unicode asInteger ]
				ifNil: [ 
					strict
						ifTrue: [ self errorOutsideRange ]
						ifFalse: [ byteValue ] ] ]
%

category: 'converting'
method: ZnSimplifiedByteEncoder
nextFromStream: stream
	"In non-strict mode, we let byte values for holes in our mapping pass through.
	Overwritten for performance reasons"

	| byteValue |
	^ (byteValue := stream next ifNil: [ ^ self errorIncomplete asCharacter ]) < 128
		ifTrue: [ Character value: byteValue ]
		ifFalse: [ 
			(byteToUnicode at: byteValue - 127 ifAbsent: [ nil ])
				ifNotNil: [ :unicode | unicode ]
				ifNil: [ 
					strict
						ifTrue: [ self errorOutsideRange ]
						ifFalse: [ Character value: byteValue ] ] ]
%

category: 'converting'
method: ZnSimplifiedByteEncoder
nextPutCodePoint: codePoint toStream: stream
	"In non-strict mode, we let code points for holes in our mapping table pass through"
	
	codePoint < 128
		ifTrue: [ stream nextPut: codePoint ]
		ifFalse: [ 
			| byte |
			byte := unicodeToByte at: codePoint ifAbsent: [ nil ].
			(byte isNil and: [ strict or: [ codePoint > 255 ] ])
				ifTrue: [ self errorOutsideRange ].
			stream nextPut: (byte ifNil: [ codePoint ]) ]
%

category: 'printing'
method: ZnSimplifiedByteEncoder
printOn: stream
	super printOn: stream.
	stream
		nextPut: $(;
		print: identifier.
	strict
		ifTrue: [ stream nextPutAll: ' strict' ].
	stream nextPut: $)
%

category: 'initialize-release'
method: ZnSimplifiedByteEncoder
unicodeToByte: map
	unicodeToByte := map
%

! Class implementation for 'ZnByteEncoder'

!		Class methods for 'ZnByteEncoder'

category: 'mappings'
classmethod: ZnByteEncoder
cp1250Mapping
	"self generateByteToUnicodeSpec: 'http://unicode.org/Public/MAPPINGS/VENDORS/MICSFT/WINDOWS/CP1250.TXT'"

	^ #(
	16r20AC nil 16r201A nil 16r201E 16r2026 16r2020 16r2021 
	nil 16r2030 16r0160 16r2039 16r015A 16r0164 16r017D 16r0179 
	nil 16r2018 16r2019 16r201C 16r201D 16r2022 16r2013 16r2014 
	nil 16r2122 16r0161 16r203A 16r015B 16r0165 16r017E 16r017A 
	16r00A0 16r02C7 16r02D8 16r0141 16r00A4 16r0104 16r00A6 16r00A7 
	16r00A8 16r00A9 16r015E 16r00AB 16r00AC 16r00AD 16r00AE 16r017B 
	16r00B0 16r00B1 16r02DB 16r0142 16r00B4 16r00B5 16r00B6 16r00B7 
	16r00B8 16r0105 16r015F 16r00BB 16r013D 16r02DD 16r013E 16r017C 
	16r0154 16r00C1 16r00C2 16r0102 16r00C4 16r0139 16r0106 16r00C7 
	16r010C 16r00C9 16r0118 16r00CB 16r011A 16r00CD 16r00CE 16r010E 
	16r0110 16r0143 16r0147 16r00D3 16r00D4 16r0150 16r00D6 16r00D7 
	16r0158 16r016E 16r00DA 16r0170 16r00DC 16r00DD 16r0162 16r00DF 
	16r0155 16r00E1 16r00E2 16r0103 16r00E4 16r013A 16r0107 16r00E7 
	16r010D 16r00E9 16r0119 16r00EB 16r011B 16r00ED 16r00EE 16r010F 
	16r0111 16r0144 16r0148 16r00F3 16r00F4 16r0151 16r00F6 16r00F7 
	16r0159 16r016F 16r00FA 16r0171 16r00FC 16r00FD 16r0163 16r02D9 )
%

category: 'mappings'
classmethod: ZnByteEncoder
cp1251Mapping
	"self generateByteToUnicodeSpec: 'http://unicode.org/Public/MAPPINGS/VENDORS/MICSFT/WINDOWS/CP1251.TXT'"

	^ #(
	16r0402 16r0403 16r201A 16r0453 16r201E 16r2026 16r2020 16r2021 
	16r20AC 16r2030 16r0409 16r2039 16r040A 16r040C 16r040B 16r040F 
	16r0452 16r2018 16r2019 16r201C 16r201D 16r2022 16r2013 16r2014 
	nil 16r2122 16r0459 16r203A 16r045A 16r045C 16r045B 16r045F 
	16r00A0 16r040E 16r045E 16r0408 16r00A4 16r0490 16r00A6 16r00A7 
	16r0401 16r00A9 16r0404 16r00AB 16r00AC 16r00AD 16r00AE 16r0407 
	16r00B0 16r00B1 16r0406 16r0456 16r0491 16r00B5 16r00B6 16r00B7 
	16r0451 16r2116 16r0454 16r00BB 16r0458 16r0405 16r0455 16r0457 
	16r0410 16r0411 16r0412 16r0413 16r0414 16r0415 16r0416 16r0417 
	16r0418 16r0419 16r041A 16r041B 16r041C 16r041D 16r041E 16r041F 
	16r0420 16r0421 16r0422 16r0423 16r0424 16r0425 16r0426 16r0427 
	16r0428 16r0429 16r042A 16r042B 16r042C 16r042D 16r042E 16r042F 
	16r0430 16r0431 16r0432 16r0433 16r0434 16r0435 16r0436 16r0437 
	16r0438 16r0439 16r043A 16r043B 16r043C 16r043D 16r043E 16r043F 
	16r0440 16r0441 16r0442 16r0443 16r0444 16r0445 16r0446 16r0447 
	16r0448 16r0449 16r044A 16r044B 16r044C 16r044D 16r044E 16r044F )
%

category: 'mappings'
classmethod: ZnByteEncoder
cp1252Mapping
	"self generateByteToUnicodeSpec: 'http://unicode.org/Public/MAPPINGS/VENDORS/MICSFT/WINDOWS/CP1252.TXT'"

	^ #(
	16r20AC nil 16r201A 16r0192 16r201E 16r2026 16r2020 16r2021 
	16r02C6 16r2030 16r0160 16r2039 16r0152 nil 16r017D nil 
	nil 16r2018 16r2019 16r201C 16r201D 16r2022 16r2013 16r2014 
	16r02DC 16r2122 16r0161 16r203A 16r0153 nil 16r017E 16r0178 
	16r00A0 16r00A1 16r00A2 16r00A3 16r00A4 16r00A5 16r00A6 16r00A7 
	16r00A8 16r00A9 16r00AA 16r00AB 16r00AC 16r00AD 16r00AE 16r00AF 
	16r00B0 16r00B1 16r00B2 16r00B3 16r00B4 16r00B5 16r00B6 16r00B7 
	16r00B8 16r00B9 16r00BA 16r00BB 16r00BC 16r00BD 16r00BE 16r00BF 
	16r00C0 16r00C1 16r00C2 16r00C3 16r00C4 16r00C5 16r00C6 16r00C7 
	16r00C8 16r00C9 16r00CA 16r00CB 16r00CC 16r00CD 16r00CE 16r00CF 
	16r00D0 16r00D1 16r00D2 16r00D3 16r00D4 16r00D5 16r00D6 16r00D7 
	16r00D8 16r00D9 16r00DA 16r00DB 16r00DC 16r00DD 16r00DE 16r00DF 
	16r00E0 16r00E1 16r00E2 16r00E3 16r00E4 16r00E5 16r00E6 16r00E7 
	16r00E8 16r00E9 16r00EA 16r00EB 16r00EC 16r00ED 16r00EE 16r00EF 
	16r00F0 16r00F1 16r00F2 16r00F3 16r00F4 16r00F5 16r00F6 16r00F7 
	16r00F8 16r00F9 16r00FA 16r00FB 16r00FC 16r00FD 16r00FE 16r00FF )
%

category: 'mappings'
classmethod: ZnByteEncoder
cp1253Mapping
	"self generateByteToUnicodeSpec: 'http://unicode.org/Public/MAPPINGS/VENDORS/MICSFT/WINDOWS/CP1253.TXT'"

	^ #(
	16r20AC nil 16r201A 16r0192 16r201E 16r2026 16r2020 16r2021 
	nil 16r2030 nil 16r2039 nil nil nil nil 
	nil 16r2018 16r2019 16r201C 16r201D 16r2022 16r2013 16r2014 
	nil 16r2122 nil 16r203A nil nil nil nil 
	16r00A0 16r0385 16r0386 16r00A3 16r00A4 16r00A5 16r00A6 16r00A7 
	16r00A8 16r00A9 nil 16r00AB 16r00AC 16r00AD 16r00AE 16r2015 
	16r00B0 16r00B1 16r00B2 16r00B3 16r0384 16r00B5 16r00B6 16r00B7 
	16r0388 16r0389 16r038A 16r00BB 16r038C 16r00BD 16r038E 16r038F 
	16r0390 16r0391 16r0392 16r0393 16r0394 16r0395 16r0396 16r0397 
	16r0398 16r0399 16r039A 16r039B 16r039C 16r039D 16r039E 16r039F 
	16r03A0 16r03A1 nil 16r03A3 16r03A4 16r03A5 16r03A6 16r03A7 
	16r03A8 16r03A9 16r03AA 16r03AB 16r03AC 16r03AD 16r03AE 16r03AF 
	16r03B0 16r03B1 16r03B2 16r03B3 16r03B4 16r03B5 16r03B6 16r03B7 
	16r03B8 16r03B9 16r03BA 16r03BB 16r03BC 16r03BD 16r03BE 16r03BF 
	16r03C0 16r03C1 16r03C2 16r03C3 16r03C4 16r03C5 16r03C6 16r03C7 
	16r03C8 16r03C9 16r03CA 16r03CB 16r03CC 16r03CD 16r03CE nil )
%

category: 'mappings'
classmethod: ZnByteEncoder
cp1254Mapping
	"self generateByteToUnicodeSpec: 'http://unicode.org/Public/MAPPINGS/VENDORS/MICSFT/WINDOWS/CP1254.TXT'"

	^ #(
	16r20AC nil 16r201A 16r0192 16r201E 16r2026 16r2020 16r2021 
	16r02C6 16r2030 16r0160 16r2039 16r0152 nil nil nil 
	nil 16r2018 16r2019 16r201C 16r201D 16r2022 16r2013 16r2014 
	16r02DC 16r2122 16r0161 16r203A 16r0153 nil nil 16r0178 
	16r00A0 16r00A1 16r00A2 16r00A3 16r00A4 16r00A5 16r00A6 16r00A7 
	16r00A8 16r00A9 16r00AA 16r00AB 16r00AC 16r00AD 16r00AE 16r00AF 
	16r00B0 16r00B1 16r00B2 16r00B3 16r00B4 16r00B5 16r00B6 16r00B7 
	16r00B8 16r00B9 16r00BA 16r00BB 16r00BC 16r00BD 16r00BE 16r00BF 
	16r00C0 16r00C1 16r00C2 16r00C3 16r00C4 16r00C5 16r00C6 16r00C7 
	16r00C8 16r00C9 16r00CA 16r00CB 16r00CC 16r00CD 16r00CE 16r00CF 
	16r011E 16r00D1 16r00D2 16r00D3 16r00D4 16r00D5 16r00D6 16r00D7 
	16r00D8 16r00D9 16r00DA 16r00DB 16r00DC 16r0130 16r015E 16r00DF 
	16r00E0 16r00E1 16r00E2 16r00E3 16r00E4 16r00E5 16r00E6 16r00E7 
	16r00E8 16r00E9 16r00EA 16r00EB 16r00EC 16r00ED 16r00EE 16r00EF 
	16r011F 16r00F1 16r00F2 16r00F3 16r00F4 16r00F5 16r00F6 16r00F7 
	16r00F8 16r00F9 16r00FA 16r00FB 16r00FC 16r0131 16r015F 16r00FF )
%

category: 'mappings'
classmethod: ZnByteEncoder
cp1255Mapping
	"self generateByteToUnicodeSpec: 'http://unicode.org/Public/MAPPINGS/VENDORS/MICSFT/WINDOWS/CP1255.TXT'"

	^ #(
	16r20AC nil 16r201A 16r0192 16r201E 16r2026 16r2020 16r2021 
	16r02C6 16r2030 nil 16r2039 nil nil nil nil 
	nil 16r2018 16r2019 16r201C 16r201D 16r2022 16r2013 16r2014 
	16r02DC 16r2122 nil 16r203A nil nil nil nil 
	16r00A0 16r00A1 16r00A2 16r00A3 16r20AA 16r00A5 16r00A6 16r00A7 
	16r00A8 16r00A9 16r00D7 16r00AB 16r00AC 16r00AD 16r00AE 16r00AF 
	16r00B0 16r00B1 16r00B2 16r00B3 16r00B4 16r00B5 16r00B6 16r00B7 
	16r00B8 16r00B9 16r00F7 16r00BB 16r00BC 16r00BD 16r00BE 16r00BF 
	16r05B0 16r05B1 16r05B2 16r05B3 16r05B4 16r05B5 16r05B6 16r05B7 
	16r05B8 16r05B9 nil 16r05BB 16r05BC 16r05BD 16r05BE 16r05BF 
	16r05C0 16r05C1 16r05C2 16r05C3 16r05F0 16r05F1 16r05F2 16r05F3 
	16r05F4 nil nil nil nil nil nil nil 
	16r05D0 16r05D1 16r05D2 16r05D3 16r05D4 16r05D5 16r05D6 16r05D7 
	16r05D8 16r05D9 16r05DA 16r05DB 16r05DC 16r05DD 16r05DE 16r05DF 
	16r05E0 16r05E1 16r05E2 16r05E3 16r05E4 16r05E5 16r05E6 16r05E7 
	16r05E8 16r05E9 16r05EA nil nil 16r200E 16r200F nil )
%

category: 'mappings'
classmethod: ZnByteEncoder
cp1256Mapping
	"self generateByteToUnicodeSpec: 'http://unicode.org/Public/MAPPINGS/VENDORS/MICSFT/WINDOWS/CP1256.TXT'"

	^ #(
	16r20AC 16r067E 16r201A 16r0192 16r201E 16r2026 16r2020 16r2021 
	16r02C6 16r2030 16r0679 16r2039 16r0152 16r0686 16r0698 16r0688 
	16r06AF 16r2018 16r2019 16r201C 16r201D 16r2022 16r2013 16r2014 
	16r06A9 16r2122 16r0691 16r203A 16r0153 16r200C 16r200D 16r06BA 
	16r00A0 16r060C 16r00A2 16r00A3 16r00A4 16r00A5 16r00A6 16r00A7 
	16r00A8 16r00A9 16r06BE 16r00AB 16r00AC 16r00AD 16r00AE 16r00AF 
	16r00B0 16r00B1 16r00B2 16r00B3 16r00B4 16r00B5 16r00B6 16r00B7 
	16r00B8 16r00B9 16r061B 16r00BB 16r00BC 16r00BD 16r00BE 16r061F 
	16r06C1 16r0621 16r0622 16r0623 16r0624 16r0625 16r0626 16r0627 
	16r0628 16r0629 16r062A 16r062B 16r062C 16r062D 16r062E 16r062F 
	16r0630 16r0631 16r0632 16r0633 16r0634 16r0635 16r0636 16r00D7 
	16r0637 16r0638 16r0639 16r063A 16r0640 16r0641 16r0642 16r0643 
	16r00E0 16r0644 16r00E2 16r0645 16r0646 16r0647 16r0648 16r00E7 
	16r00E8 16r00E9 16r00EA 16r00EB 16r0649 16r064A 16r00EE 16r00EF 
	16r064B 16r064C 16r064D 16r064E 16r00F4 16r064F 16r0650 16r00F7 
	16r0651 16r00F9 16r0652 16r00FB 16r00FC 16r200E 16r200F 16r06D2 )
%

category: 'mappings'
classmethod: ZnByteEncoder
cp1257Mapping
	"self generateByteToUnicodeSpec: 'http://unicode.org/Public/MAPPINGS/VENDORS/MICSFT/WINDOWS/CP1257.TXT'"

	^ #(
	16r20AC nil 16r201A nil 16r201E 16r2026 16r2020 16r2021 
	nil 16r2030 nil 16r2039 nil 16r00A8 16r02C7 16r00B8 
	nil 16r2018 16r2019 16r201C 16r201D 16r2022 16r2013 16r2014 
	nil 16r2122 nil 16r203A nil 16r00AF 16r02DB nil 
	16r00A0 nil 16r00A2 16r00A3 16r00A4 nil 16r00A6 16r00A7 
	16r00D8 16r00A9 16r0156 16r00AB 16r00AC 16r00AD 16r00AE 16r00C6 
	16r00B0 16r00B1 16r00B2 16r00B3 16r00B4 16r00B5 16r00B6 16r00B7 
	16r00F8 16r00B9 16r0157 16r00BB 16r00BC 16r00BD 16r00BE 16r00E6 
	16r0104 16r012E 16r0100 16r0106 16r00C4 16r00C5 16r0118 16r0112 
	16r010C 16r00C9 16r0179 16r0116 16r0122 16r0136 16r012A 16r013B 
	16r0160 16r0143 16r0145 16r00D3 16r014C 16r00D5 16r00D6 16r00D7 
	16r0172 16r0141 16r015A 16r016A 16r00DC 16r017B 16r017D 16r00DF 
	16r0105 16r012F 16r0101 16r0107 16r00E4 16r00E5 16r0119 16r0113 
	16r010D 16r00E9 16r017A 16r0117 16r0123 16r0137 16r012B 16r013C 
	16r0161 16r0144 16r0146 16r00F3 16r014D 16r00F5 16r00F6 16r00F7 
	16r0173 16r0142 16r015B 16r016B 16r00FC 16r017C 16r017E 16r02D9 )
%

category: 'mappings'
classmethod: ZnByteEncoder
cp1258Mapping
	"self generateByteToUnicodeSpec: 'http://unicode.org/Public/MAPPINGS/VENDORS/MICSFT/WINDOWS/CP1258.TXT'"

	^ #(
	16r20AC nil 16r201A 16r0192 16r201E 16r2026 16r2020 16r2021 
	16r02C6 16r2030 nil 16r2039 16r0152 nil nil nil 
	nil 16r2018 16r2019 16r201C 16r201D 16r2022 16r2013 16r2014 
	16r02DC 16r2122 nil 16r203A 16r0153 nil nil 16r0178 
	16r00A0 16r00A1 16r00A2 16r00A3 16r00A4 16r00A5 16r00A6 16r00A7 
	16r00A8 16r00A9 16r00AA 16r00AB 16r00AC 16r00AD 16r00AE 16r00AF 
	16r00B0 16r00B1 16r00B2 16r00B3 16r00B4 16r00B5 16r00B6 16r00B7 
	16r00B8 16r00B9 16r00BA 16r00BB 16r00BC 16r00BD 16r00BE 16r00BF 
	16r00C0 16r00C1 16r00C2 16r0102 16r00C4 16r00C5 16r00C6 16r00C7 
	16r00C8 16r00C9 16r00CA 16r00CB 16r0300 16r00CD 16r00CE 16r00CF 
	16r0110 16r00D1 16r0309 16r00D3 16r00D4 16r01A0 16r00D6 16r00D7 
	16r00D8 16r00D9 16r00DA 16r00DB 16r00DC 16r01AF 16r0303 16r00DF 
	16r00E0 16r00E1 16r00E2 16r0103 16r00E4 16r00E5 16r00E6 16r00E7 
	16r00E8 16r00E9 16r00EA 16r00EB 16r0301 16r00ED 16r00EE 16r00EF 
	16r0111 16r00F1 16r0323 16r00F3 16r00F4 16r01A1 16r00F6 16r00F7 
	16r00F8 16r00F9 16r00FA 16r00FB 16r00FC 16r01B0 16r20AB 16r00FF )
%

category: 'mappings'
classmethod: ZnByteEncoder
cp850Mapping
	"This is not included in the MS mappings on the Unicode site, 
	but still in modern use as the default codepage in 
	the Windows Command Shell for multilingual locales"
	"Technically, due to structure of ByteEncoder, it is incomplete, 
	as the lower range corresponds not to ASCII, but to cp437, 
	where character 0 - 31 and 127 have different meanings."
	"See also http://en.wikipedia.org/wiki/Code_page_850"
	
	^ #(
	16r00C7 16r00FC 16r00E9 16r00E2 16r00E4 16r00E0 16r00E5 16r00E7 
	16r00EA 16r00EB 16r00E8 16r00EF 16r00EE 16r00EC 16r00C4 16r00C5 
	16r00C9 16r00E6 16r00C6 16r00F4 16r00F6 16r00F2 16r00FB 16r00F9 
	16r00FF 16r00D6 16r00DC 16r00F8 16r00A3 16r00D8 16r00D7 16r0192 
	16r00E1 16r00ED 16r00F3 16r00FA 16r00F1 16r00D1 16r00AA 16r00BA 
	16r00BF 16r00AE 16r00AC 16r00BD 16r00BC 16r00A1 16r00AB 16r00BB 
	16r2591 16r2592 16r2593 16r2502 16r2524 16r00C1 16r00C2 16r00C0 
	16r00A9 16r2563 16r2551 16r2557 16r255D 16r00A2 16r00A5 16r2510 
	16r2514 16r2534 16r252C 16r251C 16r2500 16r253C 16r00E3 16r00C3 
	16r255A 16r2554 16r2569 16r2566 16r2560 16r2550 16r256C 16r00A4 
	16r00F0 16r00D0 16r00CA 16r00CB 16r00C8 16r0131 16r00CD 16r00CE 
	16r00CF 16r2518 16r250C 16r2588 16r2584 16r00A6 16r00CC 16r2580 
	16r00D3 16r00DF 16r00D4 16r00D2 16r00F5 16r00D5 16r00B5 16r00FE 
	16r00DE 16r00DA 16r00DB 16r00D9 16r00FD 16r00DD 16r00AF 16r00B4 
	16r00AD 16r00B1 16r2017 16r00BE 16r00B6 16r00A7 16r00F7 16r00B8 
	16r00B0 16r00A8 16r00B7 16r00B9 16r00B3 16r00B2 16r25A0 16r00A0 )
%

category: 'mappings'
classmethod: ZnByteEncoder
cp866Mapping
	"self generateByteToUnicodeSpec: 'http://unicode.org/Public/MAPPINGS/VENDORS/MICSFT/PC/CP866.TXT'"

	^ #(
	16r0410 16r0411 16r0412 16r0413 16r0414 16r0415 16r0416 16r0417 
	16r0418 16r0419 16r041A 16r041B 16r041C 16r041D 16r041E 16r041F 
	16r0420 16r0421 16r0422 16r0423 16r0424 16r0425 16r0426 16r0427 
	16r0428 16r0429 16r042A 16r042B 16r042C 16r042D 16r042E 16r042F 
	16r0430 16r0431 16r0432 16r0433 16r0434 16r0435 16r0436 16r0437 
	16r0438 16r0439 16r043A 16r043B 16r043C 16r043D 16r043E 16r043F 
	16r2591 16r2592 16r2593 16r2502 16r2524 16r2561 16r2562 16r2556 
	16r2555 16r2563 16r2551 16r2557 16r255D 16r255C 16r255B 16r2510 
	16r2514 16r2534 16r252C 16r251C 16r2500 16r253C 16r255E 16r255F 
	16r255A 16r2554 16r2569 16r2566 16r2560 16r2550 16r256C 16r2567 
	16r2568 16r2564 16r2565 16r2559 16r2558 16r2552 16r2553 16r256B 
	16r256A 16r2518 16r250C 16r2588 16r2584 16r258C 16r2590 16r2580 
	16r0440 16r0441 16r0442 16r0443 16r0444 16r0445 16r0446 16r0447 
	16r0448 16r0449 16r044A 16r044B 16r044C 16r044D 16r044E 16r044F 
	16r0401 16r0451 16r0404 16r0454 16r0407 16r0457 16r040E 16r045E 
	16r00B0 16r2219 16r00B7 16r221A 16r2116 16r00A4 16r25A0 16r00A0 )
%

category: 'mappings'
classmethod: ZnByteEncoder
cp874Mapping
	"self generateByteToUnicodeSpec: 'http://unicode.org/Public/MAPPINGS/VENDORS/MICSFT/WINDOWS/CP874.TXT'"

	^ #(
	16r20AC nil nil nil nil 16r2026 nil nil 
	nil nil nil nil nil nil nil nil 
	nil 16r2018 16r2019 16r201C 16r201D 16r2022 16r2013 16r2014 
	nil nil nil nil nil nil nil nil 
	16r00A0 16r0E01 16r0E02 16r0E03 16r0E04 16r0E05 16r0E06 16r0E07 
	16r0E08 16r0E09 16r0E0A 16r0E0B 16r0E0C 16r0E0D 16r0E0E 16r0E0F 
	16r0E10 16r0E11 16r0E12 16r0E13 16r0E14 16r0E15 16r0E16 16r0E17 
	16r0E18 16r0E19 16r0E1A 16r0E1B 16r0E1C 16r0E1D 16r0E1E 16r0E1F 
	16r0E20 16r0E21 16r0E22 16r0E23 16r0E24 16r0E25 16r0E26 16r0E27 
	16r0E28 16r0E29 16r0E2A 16r0E2B 16r0E2C 16r0E2D 16r0E2E 16r0E2F 
	16r0E30 16r0E31 16r0E32 16r0E33 16r0E34 16r0E35 16r0E36 16r0E37 
	16r0E38 16r0E39 16r0E3A nil nil nil nil 16r0E3F 
	16r0E40 16r0E41 16r0E42 16r0E43 16r0E44 16r0E45 16r0E46 16r0E47 
	16r0E48 16r0E49 16r0E4A 16r0E4B 16r0E4C 16r0E4D 16r0E4E 16r0E4F 
	16r0E50 16r0E51 16r0E52 16r0E53 16r0E54 16r0E55 16r0E56 16r0E57 
	16r0E58 16r0E59 16r0E5A 16r0E5B nil nil nil nil )
%

category: 'utilities'
classmethod: ZnByteEncoder
generateByteToUnicodeSpec: url
	"Return the formatted source code for an array mapping 
	the top 128 byte to unicode values from a Unicode.org url"
	"self generateByteToUnicodeSpec: 'http://unicode.org/Public/MAPPINGS/ISO8859/8859-2.TXT'."

	| mapping |
	mapping := self parseUnicodeOrgSpec: url.  
	^ String streamContents: [ :stream |
		stream tab; << '"'; << 'self generateByteToUnicodeSpec: '; print: url; << '"'; cr; cr; tab; << '^ #('.
		(self top128FromUnicodeSpec: mapping) doWithIndex: [ :each :index |
			index - 1 \\ 8 = 0 ifTrue: [ stream cr; tab ].
			each isNil
				ifTrue: [ stream print: nil; space ]  
				ifFalse: [ stream << '16r' << (each printPaddedWith: $0 to: 4 base: 16); space ] ].
		stream nextPut: $); cr ]
%

category: 'mappings'
classmethod: ZnByteEncoder
iso885910Mapping
	"self generateByteToUnicodeSpec: 'http://unicode.org/Public/MAPPINGS/ISO8859/8859-10.TXT'"

	^ #(
	nil nil nil nil nil nil nil nil 
	nil nil nil nil nil nil nil nil 
	nil nil nil nil nil nil nil nil 
	nil nil nil nil nil nil nil nil 
	16r00A0 16r0104 16r0112 16r0122 16r012A 16r0128 16r0136 16r00A7 
	16r013B 16r0110 16r0160 16r0166 16r017D 16r00AD 16r016A 16r014A 
	16r00B0 16r0105 16r0113 16r0123 16r012B 16r0129 16r0137 16r00B7 
	16r013C 16r0111 16r0161 16r0167 16r017E 16r2015 16r016B 16r014B 
	16r0100 16r00C1 16r00C2 16r00C3 16r00C4 16r00C5 16r00C6 16r012E 
	16r010C 16r00C9 16r0118 16r00CB 16r0116 16r00CD 16r00CE 16r00CF 
	16r00D0 16r0145 16r014C 16r00D3 16r00D4 16r00D5 16r00D6 16r0168 
	16r00D8 16r0172 16r00DA 16r00DB 16r00DC 16r00DD 16r00DE 16r00DF 
	16r0101 16r00E1 16r00E2 16r00E3 16r00E4 16r00E5 16r00E6 16r012F 
	16r010D 16r00E9 16r0119 16r00EB 16r0117 16r00ED 16r00EE 16r00EF 
	16r00F0 16r0146 16r014D 16r00F3 16r00F4 16r00F5 16r00F6 16r0169 
	16r00F8 16r0173 16r00FA 16r00FB 16r00FC 16r00FD 16r00FE 16r0138 )
%

category: 'mappings'
classmethod: ZnByteEncoder
iso885913Mapping
	"self generateByteToUnicodeSpec: 'http://unicode.org/Public/MAPPINGS/ISO8859/8859-13.TXT'"

	^ #(
	nil nil nil nil nil nil nil nil 
	nil nil nil nil nil nil nil nil 
	nil nil nil nil nil nil nil nil 
	nil nil nil nil nil nil nil nil 
	16r00A0 16r201D 16r00A2 16r00A3 16r00A4 16r201E 16r00A6 16r00A7 
	16r00D8 16r00A9 16r0156 16r00AB 16r00AC 16r00AD 16r00AE 16r00C6 
	16r00B0 16r00B1 16r00B2 16r00B3 16r201C 16r00B5 16r00B6 16r00B7 
	16r00F8 16r00B9 16r0157 16r00BB 16r00BC 16r00BD 16r00BE 16r00E6 
	16r0104 16r012E 16r0100 16r0106 16r00C4 16r00C5 16r0118 16r0112 
	16r010C 16r00C9 16r0179 16r0116 16r0122 16r0136 16r012A 16r013B 
	16r0160 16r0143 16r0145 16r00D3 16r014C 16r00D5 16r00D6 16r00D7 
	16r0172 16r0141 16r015A 16r016A 16r00DC 16r017B 16r017D 16r00DF 
	16r0105 16r012F 16r0101 16r0107 16r00E4 16r00E5 16r0119 16r0113 
	16r010D 16r00E9 16r017A 16r0117 16r0123 16r0137 16r012B 16r013C 
	16r0161 16r0144 16r0146 16r00F3 16r014D 16r00F5 16r00F6 16r00F7 
	16r0173 16r0142 16r015B 16r016B 16r00FC 16r017C 16r017E 16r2019 )
%

category: 'mappings'
classmethod: ZnByteEncoder
iso885914Mapping
	"self generateByteToUnicodeSpec: 'http://unicode.org/Public/MAPPINGS/ISO8859/8859-14.TXT'"

	^ #(
	nil nil nil nil nil nil nil nil 
	nil nil nil nil nil nil nil nil 
	nil nil nil nil nil nil nil nil 
	nil nil nil nil nil nil nil nil 
	16r00A0 16r1E02 16r1E03 16r00A3 16r010A 16r010B 16r1E0A 16r00A7 
	16r1E80 16r00A9 16r1E82 16r1E0B 16r1EF2 16r00AD 16r00AE 16r0178 
	16r1E1E 16r1E1F 16r0120 16r0121 16r1E40 16r1E41 16r00B6 16r1E56 
	16r1E81 16r1E57 16r1E83 16r1E60 16r1EF3 16r1E84 16r1E85 16r1E61 
	16r00C0 16r00C1 16r00C2 16r00C3 16r00C4 16r00C5 16r00C6 16r00C7 
	16r00C8 16r00C9 16r00CA 16r00CB 16r00CC 16r00CD 16r00CE 16r00CF 
	16r0174 16r00D1 16r00D2 16r00D3 16r00D4 16r00D5 16r00D6 16r1E6A 
	16r00D8 16r00D9 16r00DA 16r00DB 16r00DC 16r00DD 16r0176 16r00DF 
	16r00E0 16r00E1 16r00E2 16r00E3 16r00E4 16r00E5 16r00E6 16r00E7 
	16r00E8 16r00E9 16r00EA 16r00EB 16r00EC 16r00ED 16r00EE 16r00EF 
	16r0175 16r00F1 16r00F2 16r00F3 16r00F4 16r00F5 16r00F6 16r1E6B 
	16r00F8 16r00F9 16r00FA 16r00FB 16r00FC 16r00FD 16r0177 16r00FF )
%

category: 'mappings'
classmethod: ZnByteEncoder
iso885915Mapping
	"self generateByteToUnicodeSpec: 'http://unicode.org/Public/MAPPINGS/ISO8859/8859-15.TXT'"

	^ #(
	nil nil nil nil nil nil nil nil 
	nil nil nil nil nil nil nil nil 
	nil nil nil nil nil nil nil nil 
	nil nil nil nil nil nil nil nil 
	16r00A0 16r00A1 16r00A2 16r00A3 16r20AC 16r00A5 16r0160 16r00A7 
	16r0161 16r00A9 16r00AA 16r00AB 16r00AC 16r00AD 16r00AE 16r00AF 
	16r00B0 16r00B1 16r00B2 16r00B3 16r017D 16r00B5 16r00B6 16r00B7 
	16r017E 16r00B9 16r00BA 16r00BB 16r0152 16r0153 16r0178 16r00BF 
	16r00C0 16r00C1 16r00C2 16r00C3 16r00C4 16r00C5 16r00C6 16r00C7 
	16r00C8 16r00C9 16r00CA 16r00CB 16r00CC 16r00CD 16r00CE 16r00CF 
	16r00D0 16r00D1 16r00D2 16r00D3 16r00D4 16r00D5 16r00D6 16r00D7 
	16r00D8 16r00D9 16r00DA 16r00DB 16r00DC 16r00DD 16r00DE 16r00DF 
	16r00E0 16r00E1 16r00E2 16r00E3 16r00E4 16r00E5 16r00E6 16r00E7 
	16r00E8 16r00E9 16r00EA 16r00EB 16r00EC 16r00ED 16r00EE 16r00EF 
	16r00F0 16r00F1 16r00F2 16r00F3 16r00F4 16r00F5 16r00F6 16r00F7 
	16r00F8 16r00F9 16r00FA 16r00FB 16r00FC 16r00FD 16r00FE 16r00FF )
%

category: 'mappings'
classmethod: ZnByteEncoder
iso885916Mapping
	"self generateByteToUnicodeSpec: 'http://unicode.org/Public/MAPPINGS/ISO8859/8859-16.TXT'"

	^ #(
	nil nil nil nil nil nil nil nil 
	nil nil nil nil nil nil nil nil 
	nil nil nil nil nil nil nil nil 
	nil nil nil nil nil nil nil nil 
	16r00A0 16r0104 16r0105 16r0141 16r20AC 16r201E 16r0160 16r00A7 
	16r0161 16r00A9 16r0218 16r00AB 16r0179 16r00AD 16r017A 16r017B 
	16r00B0 16r00B1 16r010C 16r0142 16r017D 16r201D 16r00B6 16r00B7 
	16r017E 16r010D 16r0219 16r00BB 16r0152 16r0153 16r0178 16r017C 
	16r00C0 16r00C1 16r00C2 16r0102 16r00C4 16r0106 16r00C6 16r00C7 
	16r00C8 16r00C9 16r00CA 16r00CB 16r00CC 16r00CD 16r00CE 16r00CF 
	16r0110 16r0143 16r00D2 16r00D3 16r00D4 16r0150 16r00D6 16r015A 
	16r0170 16r00D9 16r00DA 16r00DB 16r00DC 16r0118 16r021A 16r00DF 
	16r00E0 16r00E1 16r00E2 16r0103 16r00E4 16r0107 16r00E6 16r00E7 
	16r00E8 16r00E9 16r00EA 16r00EB 16r00EC 16r00ED 16r00EE 16r00EF 
	16r0111 16r0144 16r00F2 16r00F3 16r00F4 16r0151 16r00F6 16r015B 
	16r0171 16r00F9 16r00FA 16r00FB 16r00FC 16r0119 16r021B 16r00FF )
%

category: 'mappings'
classmethod: ZnByteEncoder
iso88592Mapping
	"self generateByteToUnicodeSpec: 'http://unicode.org/Public/MAPPINGS/ISO8859/8859-2.TXT'"

	^ #(
	nil nil nil nil nil nil nil nil 
	nil nil nil nil nil nil nil nil 
	nil nil nil nil nil nil nil nil 
	nil nil nil nil nil nil nil nil 
	16r00A0 16r0104 16r02D8 16r0141 16r00A4 16r013D 16r015A 16r00A7 
	16r00A8 16r0160 16r015E 16r0164 16r0179 16r00AD 16r017D 16r017B 
	16r00B0 16r0105 16r02DB 16r0142 16r00B4 16r013E 16r015B 16r02C7 
	16r00B8 16r0161 16r015F 16r0165 16r017A 16r02DD 16r017E 16r017C 
	16r0154 16r00C1 16r00C2 16r0102 16r00C4 16r0139 16r0106 16r00C7 
	16r010C 16r00C9 16r0118 16r00CB 16r011A 16r00CD 16r00CE 16r010E 
	16r0110 16r0143 16r0147 16r00D3 16r00D4 16r0150 16r00D6 16r00D7 
	16r0158 16r016E 16r00DA 16r0170 16r00DC 16r00DD 16r0162 16r00DF 
	16r0155 16r00E1 16r00E2 16r0103 16r00E4 16r013A 16r0107 16r00E7 
	16r010D 16r00E9 16r0119 16r00EB 16r011B 16r00ED 16r00EE 16r010F 
	16r0111 16r0144 16r0148 16r00F3 16r00F4 16r0151 16r00F6 16r00F7 
	16r0159 16r016F 16r00FA 16r0171 16r00FC 16r00FD 16r0163 16r02D9 )
%

category: 'mappings'
classmethod: ZnByteEncoder
iso88593Mapping
	"self generateByteToUnicodeSpec: 'http://unicode.org/Public/MAPPINGS/ISO8859/8859-3.TXT'"

	^ #(
	nil nil nil nil nil nil nil nil 
	nil nil nil nil nil nil nil nil 
	nil nil nil nil nil nil nil nil 
	nil nil nil nil nil nil nil nil 
	16r00A0 16r0126 16r02D8 16r00A3 16r00A4 nil 16r0124 16r00A7 
	16r00A8 16r0130 16r015E 16r011E 16r0134 16r00AD nil 16r017B 
	16r00B0 16r0127 16r00B2 16r00B3 16r00B4 16r00B5 16r0125 16r00B7 
	16r00B8 16r0131 16r015F 16r011F 16r0135 16r00BD nil 16r017C 
	16r00C0 16r00C1 16r00C2 nil 16r00C4 16r010A 16r0108 16r00C7 
	16r00C8 16r00C9 16r00CA 16r00CB 16r00CC 16r00CD 16r00CE 16r00CF 
	nil 16r00D1 16r00D2 16r00D3 16r00D4 16r0120 16r00D6 16r00D7 
	16r011C 16r00D9 16r00DA 16r00DB 16r00DC 16r016C 16r015C 16r00DF 
	16r00E0 16r00E1 16r00E2 nil 16r00E4 16r010B 16r0109 16r00E7 
	16r00E8 16r00E9 16r00EA 16r00EB 16r00EC 16r00ED 16r00EE 16r00EF 
	nil 16r00F1 16r00F2 16r00F3 16r00F4 16r0121 16r00F6 16r00F7 
	16r011D 16r00F9 16r00FA 16r00FB 16r00FC 16r016D 16r015D 16r02D9 )
%

category: 'mappings'
classmethod: ZnByteEncoder
iso88594Mapping
	"self generateByteToUnicodeSpec: 'http://unicode.org/Public/MAPPINGS/ISO8859/8859-4.TXT'"

	^ #(
	nil nil nil nil nil nil nil nil 
	nil nil nil nil nil nil nil nil 
	nil nil nil nil nil nil nil nil 
	nil nil nil nil nil nil nil nil 
	16r00A0 16r0104 16r0138 16r0156 16r00A4 16r0128 16r013B 16r00A7 
	16r00A8 16r0160 16r0112 16r0122 16r0166 16r00AD 16r017D 16r00AF 
	16r00B0 16r0105 16r02DB 16r0157 16r00B4 16r0129 16r013C 16r02C7 
	16r00B8 16r0161 16r0113 16r0123 16r0167 16r014A 16r017E 16r014B 
	16r0100 16r00C1 16r00C2 16r00C3 16r00C4 16r00C5 16r00C6 16r012E 
	16r010C 16r00C9 16r0118 16r00CB 16r0116 16r00CD 16r00CE 16r012A 
	16r0110 16r0145 16r014C 16r0136 16r00D4 16r00D5 16r00D6 16r00D7 
	16r00D8 16r0172 16r00DA 16r00DB 16r00DC 16r0168 16r016A 16r00DF 
	16r0101 16r00E1 16r00E2 16r00E3 16r00E4 16r00E5 16r00E6 16r012F 
	16r010D 16r00E9 16r0119 16r00EB 16r0117 16r00ED 16r00EE 16r012B 
	16r0111 16r0146 16r014D 16r0137 16r00F4 16r00F5 16r00F6 16r00F7 
	16r00F8 16r0173 16r00FA 16r00FB 16r00FC 16r0169 16r016B 16r02D9 )
%

category: 'mappings'
classmethod: ZnByteEncoder
iso88595Mapping
	"self generateByteToUnicodeSpec: 'http://unicode.org/Public/MAPPINGS/ISO8859/8859-5.TXT'"

	^ #(
	nil nil nil nil nil nil nil nil 
	nil nil nil nil nil nil nil nil 
	nil nil nil nil nil nil nil nil 
	nil nil nil nil nil nil nil nil 
	16r00A0 16r0401 16r0402 16r0403 16r0404 16r0405 16r0406 16r0407 
	16r0408 16r0409 16r040A 16r040B 16r040C 16r00AD 16r040E 16r040F 
	16r0410 16r0411 16r0412 16r0413 16r0414 16r0415 16r0416 16r0417 
	16r0418 16r0419 16r041A 16r041B 16r041C 16r041D 16r041E 16r041F 
	16r0420 16r0421 16r0422 16r0423 16r0424 16r0425 16r0426 16r0427 
	16r0428 16r0429 16r042A 16r042B 16r042C 16r042D 16r042E 16r042F 
	16r0430 16r0431 16r0432 16r0433 16r0434 16r0435 16r0436 16r0437 
	16r0438 16r0439 16r043A 16r043B 16r043C 16r043D 16r043E 16r043F 
	16r0440 16r0441 16r0442 16r0443 16r0444 16r0445 16r0446 16r0447 
	16r0448 16r0449 16r044A 16r044B 16r044C 16r044D 16r044E 16r044F 
	16r2116 16r0451 16r0452 16r0453 16r0454 16r0455 16r0456 16r0457 
	16r0458 16r0459 16r045A 16r045B 16r045C 16r00A7 16r045E 16r045F )
%

category: 'mappings'
classmethod: ZnByteEncoder
iso88596Mapping
	"self generateByteToUnicodeSpec: 'http://unicode.org/Public/MAPPINGS/ISO8859/8859-6.TXT'"

	^ #(
	nil nil nil nil nil nil nil nil 
	nil nil nil nil nil nil nil nil 
	nil nil nil nil nil nil nil nil 
	nil nil nil nil nil nil nil nil 
	16r00A0 nil nil nil 16r00A4 nil nil nil 
	nil nil nil nil 16r060C 16r00AD nil nil 
	nil nil nil nil nil nil nil nil 
	nil nil nil 16r061B nil nil nil 16r061F 
	nil 16r0621 16r0622 16r0623 16r0624 16r0625 16r0626 16r0627 
	16r0628 16r0629 16r062A 16r062B 16r062C 16r062D 16r062E 16r062F 
	16r0630 16r0631 16r0632 16r0633 16r0634 16r0635 16r0636 16r0637 
	16r0638 16r0639 16r063A nil nil nil nil nil 
	16r0640 16r0641 16r0642 16r0643 16r0644 16r0645 16r0646 16r0647 
	16r0648 16r0649 16r064A 16r064B 16r064C 16r064D 16r064E 16r064F 
	16r0650 16r0651 16r0652 nil nil nil nil nil 
	nil nil nil nil nil nil nil nil )
%

category: 'mappings'
classmethod: ZnByteEncoder
iso88597Mapping
	"self generateByteToUnicodeSpec: 'http://unicode.org/Public/MAPPINGS/ISO8859/8859-7.TXT'"

	^ #(
	nil nil nil nil nil nil nil nil 
	nil nil nil nil nil nil nil nil 
	nil nil nil nil nil nil nil nil 
	nil nil nil nil nil nil nil nil 
	16r00A0 16r2018 16r2019 16r00A3 16r20AC 16r20AF 16r00A6 16r00A7 
	16r00A8 16r00A9 16r037A 16r00AB 16r00AC 16r00AD nil 16r2015 
	16r00B0 16r00B1 16r00B2 16r00B3 16r0384 16r0385 16r0386 16r00B7 
	16r0388 16r0389 16r038A 16r00BB 16r038C 16r00BD 16r038E 16r038F 
	16r0390 16r0391 16r0392 16r0393 16r0394 16r0395 16r0396 16r0397 
	16r0398 16r0399 16r039A 16r039B 16r039C 16r039D 16r039E 16r039F 
	16r03A0 16r03A1 nil 16r03A3 16r03A4 16r03A5 16r03A6 16r03A7 
	16r03A8 16r03A9 16r03AA 16r03AB 16r03AC 16r03AD 16r03AE 16r03AF 
	16r03B0 16r03B1 16r03B2 16r03B3 16r03B4 16r03B5 16r03B6 16r03B7 
	16r03B8 16r03B9 16r03BA 16r03BB 16r03BC 16r03BD 16r03BE 16r03BF 
	16r03C0 16r03C1 16r03C2 16r03C3 16r03C4 16r03C5 16r03C6 16r03C7 
	16r03C8 16r03C9 16r03CA 16r03CB 16r03CC 16r03CD 16r03CE nil )
%

category: 'mappings'
classmethod: ZnByteEncoder
iso88598Mapping
	"self generateByteToUnicodeSpec: 'http://unicode.org/Public/MAPPINGS/ISO8859/8859-8.TXT'"

	^ #(
	nil nil nil nil nil nil nil nil 
	nil nil nil nil nil nil nil nil 
	nil nil nil nil nil nil nil nil 
	nil nil nil nil nil nil nil nil 
	16r00A0 nil 16r00A2 16r00A3 16r00A4 16r00A5 16r00A6 16r00A7 
	16r00A8 16r00A9 16r00D7 16r00AB 16r00AC 16r00AD 16r00AE 16r00AF 
	16r00B0 16r00B1 16r00B2 16r00B3 16r00B4 16r00B5 16r00B6 16r00B7 
	16r00B8 16r00B9 16r00F7 16r00BB 16r00BC 16r00BD 16r00BE nil 
	nil nil nil nil nil nil nil nil 
	nil nil nil nil nil nil nil nil 
	nil nil nil nil nil nil nil nil 
	nil nil nil nil nil nil nil 16r2017 
	16r05D0 16r05D1 16r05D2 16r05D3 16r05D4 16r05D5 16r05D6 16r05D7 
	16r05D8 16r05D9 16r05DA 16r05DB 16r05DC 16r05DD 16r05DE 16r05DF 
	16r05E0 16r05E1 16r05E2 16r05E3 16r05E4 16r05E5 16r05E6 16r05E7 
	16r05E8 16r05E9 16r05EA nil nil 16r200E 16r200F nil )
%

category: 'mappings'
classmethod: ZnByteEncoder
koi8rMapping
	"self generateByteToUnicodeSpec: 'http://unicode.org/Public/MAPPINGS/VENDORS/MISC/KOI8-R.TXT'"

	^ #(
	16r2500 16r2502 16r250C 16r2510 16r2514 16r2518 16r251C 16r2524 
	16r252C 16r2534 16r253C 16r2580 16r2584 16r2588 16r258C 16r2590 
	16r2591 16r2592 16r2593 16r2320 16r25A0 16r2219 16r221A 16r2248 
	16r2264 16r2265 16r00A0 16r2321 16r00B0 16r00B2 16r00B7 16r00F7 
	16r2550 16r2551 16r2552 16r0451 16r2553 16r2554 16r2555 16r2556 
	16r2557 16r2558 16r2559 16r255A 16r255B 16r255C 16r255D 16r255E 
	16r255F 16r2560 16r2561 16r0401 16r2562 16r2563 16r2564 16r2565 
	16r2566 16r2567 16r2568 16r2569 16r256A 16r256B 16r256C 16r00A9 
	16r044E 16r0430 16r0431 16r0446 16r0434 16r0435 16r0444 16r0433 
	16r0445 16r0438 16r0439 16r043A 16r043B 16r043C 16r043D 16r043E 
	16r043F 16r044F 16r0440 16r0441 16r0442 16r0443 16r0436 16r0432 
	16r044C 16r044B 16r0437 16r0448 16r044D 16r0449 16r0447 16r044A 
	16r042E 16r0410 16r0411 16r0426 16r0414 16r0415 16r0424 16r0413 
	16r0425 16r0418 16r0419 16r041A 16r041B 16r041C 16r041D 16r041E 
	16r041F 16r042F 16r0420 16r0421 16r0422 16r0423 16r0416 16r0412 
	16r042C 16r042B 16r0417 16r0428 16r042D 16r0429 16r0427 16r042A )
%

category: 'mappings'
classmethod: ZnByteEncoder
koi8uMapping
	"self generateByteToUnicodeSpec: 'http://unicode.org/Public/MAPPINGS/VENDORS/MISC/KOI8-U.TXT'"

	^ #(
	16r2500 16r2502 16r250C 16r2510 16r2514 16r2518 16r251C 16r2524 
	16r252C 16r2534 16r253C 16r2580 16r2584 16r2588 16r258C 16r2590 
	16r2591 16r2592 16r2593 16r2320 16r25A0 16r2219 16r221A 16r2248 
	16r2264 16r2265 16r00A0 16r2321 16r00B0 16r00B2 16r00B7 16r00F7 
	16r2550 16r2551 16r2552 16r0451 16r0454 16r2554 16r0456 16r0457 
	16r2557 16r2558 16r2559 16r255A 16r255B 16r0491 16r255D 16r255E 
	16r255F 16r2560 16r2561 16r0401 16r0404 16r2563 16r0406 16r0407 
	16r2566 16r2567 16r2568 16r2569 16r256A 16r0490 16r256C 16r00A9 
	16r044E 16r0430 16r0431 16r0446 16r0434 16r0435 16r0444 16r0433 
	16r0445 16r0438 16r0439 16r043A 16r043B 16r043C 16r043D 16r043E 
	16r043F 16r044F 16r0440 16r0441 16r0442 16r0443 16r0436 16r0432 
	16r044C 16r044B 16r0437 16r0448 16r044D 16r0449 16r0447 16r044A 
	16r042E 16r0410 16r0411 16r0426 16r0414 16r0415 16r0424 16r0413 
	16r0425 16r0418 16r0419 16r041A 16r041B 16r041C 16r041D 16r041E 
	16r041F 16r042F 16r0420 16r0421 16r0422 16r0423 16r0416 16r0412 
	16r042C 16r042B 16r0417 16r0428 16r042D 16r0429 16r0427 16r042A )
%

category: 'mappings'
classmethod: ZnByteEncoder
macCyrillicMapping
	"self generateByteToUnicodeSpec: 'http://unicode.org/Public/MAPPINGS/VENDORS/APPLE/CYRILLIC.TXT'"

	^ #(
	16r0410 16r0411 16r0412 16r0413 16r0414 16r0415 16r0416 16r0417 
	16r0418 16r0419 16r041A 16r041B 16r041C 16r041D 16r041E 16r041F 
	16r0420 16r0421 16r0422 16r0423 16r0424 16r0425 16r0426 16r0427 
	16r0428 16r0429 16r042A 16r042B 16r042C 16r042D 16r042E 16r042F 
	16r2020 16r00B0 16r0490 16r00A3 16r00A7 16r2022 16r00B6 16r0406 
	16r00AE 16r00A9 16r2122 16r0402 16r0452 16r2260 16r0403 16r0453 
	16r221E 16r00B1 16r2264 16r2265 16r0456 16r00B5 16r0491 16r0408 
	16r0404 16r0454 16r0407 16r0457 16r0409 16r0459 16r040A 16r045A 
	16r0458 16r0405 16r00AC 16r221A 16r0192 16r2248 16r2206 16r00AB 
	16r00BB 16r2026 16r00A0 16r040B 16r045B 16r040C 16r045C 16r0455 
	16r2013 16r2014 16r201C 16r201D 16r2018 16r2019 16r00F7 16r201E 
	16r040E 16r045E 16r040F 16r045F 16r2116 16r0401 16r0451 16r044F 
	16r0430 16r0431 16r0432 16r0433 16r0434 16r0435 16r0436 16r0437 
	16r0438 16r0439 16r043A 16r043B 16r043C 16r043D 16r043E 16r043F 
	16r0440 16r0441 16r0442 16r0443 16r0444 16r0445 16r0446 16r0447 
	16r0448 16r0449 16r044A 16r044B 16r044C 16r044D 16r044E 16r20AC )
%

category: 'mappings'
classmethod: ZnByteEncoder
macRomanMapping
	"self generateByteToUnicodeSpec: 'http://unicode.org/Public/MAPPINGS/VENDORS/APPLE/ROMAN.TXT'"

	^ #(
	16r00C4 16r00C5 16r00C7 16r00C9 16r00D1 16r00D6 16r00DC 16r00E1 
	16r00E0 16r00E2 16r00E4 16r00E3 16r00E5 16r00E7 16r00E9 16r00E8 
	16r00EA 16r00EB 16r00ED 16r00EC 16r00EE 16r00EF 16r00F1 16r00F3 
	16r00F2 16r00F4 16r00F6 16r00F5 16r00FA 16r00F9 16r00FB 16r00FC 
	16r2020 16r00B0 16r00A2 16r00A3 16r00A7 16r2022 16r00B6 16r00DF 
	16r00AE 16r00A9 16r2122 16r00B4 16r00A8 16r2260 16r00C6 16r00D8 
	16r221E 16r00B1 16r2264 16r2265 16r00A5 16r00B5 16r2202 16r2211 
	16r220F 16r03C0 16r222B 16r00AA 16r00BA 16r03A9 16r00E6 16r00F8 
	16r00BF 16r00A1 16r00AC 16r221A 16r0192 16r2248 16r2206 16r00AB 
	16r00BB 16r2026 16r00A0 16r00C0 16r00C3 16r00D5 16r0152 16r0153 
	16r2013 16r2014 16r201C 16r201D 16r2018 16r2019 16r00F7 16r25CA 
	16r00FF 16r0178 16r2044 16r20AC 16r2039 16r203A 16rFB01 16rFB02 
	16r2021 16r00B7 16r201A 16r201E 16r2030 16r00C2 16r00CA 16r00C1 
	16r00CB 16r00C8 16r00CD 16r00CE 16r00CF 16r00CC 16r00D3 16r00D4 
	16rF8FF 16r00D2 16r00DA 16r00DB 16r00D9 16r0131 16r02C6 16r02DC 
	16r00AF 16r02D8 16r02D9 16r02DA 16r00B8 16r02DD 16r02DB 16r02C7 )
%

category: 'mappings'
classmethod: ZnByteEncoder
mappingToIdentifiers
	"Return a dictionay mapping from encoding specifications to a list of encoding names."
	
	^ Dictionary newFromPairs: #(
		"#asciiMapping #('ascii')"
		"#iso88591Mapping #('iso88591' 'latin1')"
		#iso88592Mapping #('iso88592' 'latin2') 
		#iso88593Mapping #('iso88593' 'latin3') 
		#iso88594Mapping #('iso88594' 'latin4') 
		#iso88595Mapping #('iso88595' 'cyrillic') 
		#iso88596Mapping #('iso88596' 'arabic') 
		#iso88597Mapping #('iso88597' 'greek') 
		#iso88598Mapping #('iso88598' 'hebrew') 
		#iso885910Mapping #('iso885910' 'latin6') 
		#iso885913Mapping #('iso885913') 
		#iso885914Mapping #('iso885914') 
		#iso885915Mapping #('iso885915') 
		#iso885916Mapping #('iso885916') 
		#cp1250Mapping #('cp1250' 'windows1250' 'xcp1250') 
		#cp1251Mapping #('cp1251' 'windows1251' 'xcp1251') 
		#cp1252Mapping #('cp1252' 'windows1252' 'xcp1252' 'ibm819')
		#cp1253Mapping #('cp1253' 'windows1253' 'xcp1253')
		#cp1254Mapping #('cp1254' 'windows1254' 'xcp1254' 'iso88599' 'latin5')
		#cp1255Mapping #('cp1255' 'windows1255' 'xcp1255')
		#cp1256Mapping #('cp1256' 'windows1256' 'xcp1256')
		#cp1257Mapping #('cp1257' 'windows1257' 'xcp1257')
		#cp1258Mapping #('cp1258' 'windows1258' 'xcp1258')
		#cp850Mapping #('cp850' 'ibm850' 'oem850' 'doslatin1')
		#cp866Mapping #('cp866' 'ibm866')
		#cp874Mapping #('cp874' 'iso885911' 'windows874' 'dos874')
		#koi8rMapping #('koi8r' 'koi8')
		#koi8uMapping #('koi8u')
		#macRomanMapping #('macroman' 'xmacroman' 'mac' 'macintosh')
		#macCyrillicMapping #('maccyrillic' 'xmaccyrillic') )
%

category: 'instance creation'
classmethod: ZnByteEncoder
newFromUrl: url
	"Instanciate a new encoder directly from a Unicode.org url"
	"self newFromUrl: 'http://unicode.org/Public/MAPPINGS/ISO8859/8859-2.TXT'"
	"self newFromUrl: 'http://www.unicode.org/Public/MAPPINGS/VENDORS/MICSFT/PC/CP437.TXT'"

	| mapping spec tables |
	mapping := self parseUnicodeOrgSpec: url.
	spec := self top128FromUnicodeSpec: mapping.
	tables := self tablesFromSpec: spec.
	^ self new
		identifier: url;
		byteToUnicode: tables first;
		unicodeToByte: tables second;
		yourself
%

category: 'utilities'
classmethod: ZnByteEncoder
parseUnicodeOrgSpec: url
	"Parse and return a mapping from byte to unicode values from url."
	"Basic syntax: lines starting with # are comments, else first two fields are read as 0x hex values"
	"self parseUnicodeOrgSpec: 'http://unicode.org/Public/MAPPINGS/ISO8859/8859-2.TXT'."
	
	| mapping |
	mapping := Dictionary new: 256.
	url asZnUrl retrieveContents linesDo: [ :each |
		(each isEmpty or: [ each beginsWith: '#' ])
			ifFalse: [ | tokens hexReader |
				hexReader := [ :string | Integer readFrom: (string readStream skip: 2; yourself) base: 16 ].
				tokens := each findTokens: String tab. 
				(tokens size < 3 or: [ tokens last = '<control>' or: [ tokens last = '#UNDEFINED' ] ]) ifFalse: [
					mapping
						at: (hexReader value: tokens first)
						put: (hexReader value: tokens second) ] ] ].
	^ mapping
%

category: 'utilities'
classmethod: ZnByteEncoder
top128FromUnicodeSpec: mapping
	"Return an array mapping the top 128 byte to unicode values from a Unicode.org specification map"

	^ Array new: 128 streamContents: [ :stream |
		128 to: 255 do: [ :each |
			stream nextPut: (mapping at: each ifAbsent: [ nil ]) ] ]
%

!		Instance methods for 'ZnByteEncoder'

category: 'convenience'
method: ZnByteEncoder
decodeBytesIntoWideString: bytes
	"Variant of #decodeBytes: that is faster when you know upfront 
	that a WideString is probably needed"

	| byteStream |
self flag: 'TODO: WideString does not exist in Gemstone. Using String which can grow to a QuadByteString'.
	byteStream := bytes readStream.
	^ String streamContents: [ :stream |
		[ byteStream atEnd ] whileFalse: [
			stream nextPut: (self nextFromStream: byteStream) ] ]
%

! Class implementation for 'ZnUTFEncoder'

!		Class methods for 'ZnUTFEncoder'

category: 'accessing'
classmethod: ZnUTFEncoder
handlesEncoding: string
	"Return true when my instances handle the encoding described by string"
	
	^ false
%

!		Instance methods for 'ZnUTFEncoder'

category: 'accessing'
method: ZnUTFEncoder
byteOrderMark
	"The code point of the Unicode Byte-Order-Mark or BOM character.
	See https://en.wikipedia.org/wiki/Byte_order_mark"
	
	^ 16rFEFF
%

category: 'convenience'
method: ZnUTFEncoder
decodeBytes: bytes
	"Overridden to prevent the automagic switch from ByteString to WideString 
	and the slow #becomeForward: in there. See also #decodeBytesIntoWideString:"
	
	| byteStream |

self flag: 'TODO: WideString does not exist in Gemstone. Using String which can grow to a QuadByteString'.
	byteStream := bytes readStream.
	^ String streamContents: [ :stream |
		[ byteStream atEnd ] whileFalse: [ | codePoint |
			codePoint := self nextCodePointFromStream: byteStream.
			(codePoint > 255 and: [ stream originalContents isWideString not ])
				ifTrue: [ | wideString position |
					position := stream position.
					wideString := String from: stream originalContents.
					stream on: wideString; setFrom: position + 1 to: position ].
			stream nextPut: (Character value: codePoint) ] ]
%

category: 'convenience'
method: ZnUTFEncoder
decodeBytesIntoWideString: bytes
	"Variant of #decodeBytes: that is faster when you know upfront that a WideString is probably needed"

	| byteStream |
	byteStream := bytes readStream.
self flag: 'TODO: WideString does not exist in Gemstone. Using String which can grow to a QuadByteString'.

	^ String streamContents: [ :stream |
		[ byteStream atEnd ] whileFalse: [
			stream nextPut: (self nextFromStream: byteStream) ] ]
%

category: 'convenience'
method: ZnUTFEncoder
encodeStringWithByteOrderMark: string
	"Encode string and return the resulting byte array.
	Always add a Unicode byte order mark (BOM) in front."
	
	^ ByteArray streamContents: [ :stream |
		self nextPutByteOrderMarkToStream: stream.
		self next: string size putAll: string startingAt: 1 toStream: stream ]
%

category: 'testing'
method: ZnUTFEncoder
isSurrogateCodePoint: codePoint
	"Surrogate Code Points should not be encoded or decoded because they are not Unicode scalar values"
	
	^ codePoint between: 16rD800 and: 16rDFFF
%

category: 'accessing'
method: ZnUTFEncoder
maximumUTFCode
	^ 16r10FFFF
%

category: 'convenience'
method: ZnUTFEncoder
nextPutByteOrderMarkToStream: stream
	"Write the encoded byte-order-mark (BOM) to stream"
	
	self nextPutCodePoint: self byteOrderMark toStream: stream
%

! Class implementation for 'ZnEndianSensitiveUTFEncoder'

!		Class methods for 'ZnEndianSensitiveUTFEncoder'

category: 'accessing'
classmethod: ZnEndianSensitiveUTFEncoder
handlesEncoding: string
	"Return true when my instances handle the encoding described by string"
	
	^ false
%

category: 'instance creation'
classmethod: ZnEndianSensitiveUTFEncoder
newForEncoding: string
	"Return a new character encoder object for an encoding described by string.
	Try to infer endianness from string, defaulting to big endian."
	
	| encoder |
	encoder := self new.
	(string asLowercase endsWith: 'be') ifTrue: [ encoder beBigEndian ].
	(string asLowercase endsWith: 'le') ifTrue: [ encoder beLittleEndian ].
	^ encoder
%

!		Instance methods for 'ZnEndianSensitiveUTFEncoder'

category: 'comparing'
method: ZnEndianSensitiveUTFEncoder
= anObject
	^ super = anObject and: [ self endianness == anObject endianness ]
%

category: 'initialize'
method: ZnEndianSensitiveUTFEncoder
beBigEndian
	endianness := #big
%

category: 'initialize'
method: ZnEndianSensitiveUTFEncoder
beLittleEndian
	endianness := #little
%

category: 'accessing'
method: ZnEndianSensitiveUTFEncoder
endianness
	^ endianness
%

category: 'comparing'
method: ZnEndianSensitiveUTFEncoder
hash
	^ super hash bitXor: self endianness hash
%

category: 'initialize'
method: ZnEndianSensitiveUTFEncoder
initialize
	endianness := #big
%

category: 'testing'
method: ZnEndianSensitiveUTFEncoder
isBigEndian
	^ endianness = #big
%

category: 'testing'
method: ZnEndianSensitiveUTFEncoder
isLittleEndian
	^ endianness = #little
%

category: 'printing'
method: ZnEndianSensitiveUTFEncoder
printOn: stream
	super printOn: stream.
	stream nextPut: $(.
	stream nextPutAll: endianness; nextPutAll: ' endian'.
	stream nextPut: $)
%

category: 'private'
method: ZnEndianSensitiveUTFEncoder
swapEndianness
	self isLittleEndian
		ifTrue: [ self beBigEndian ]
		ifFalse: [ self beLittleEndian ]
%

! Class implementation for 'ZnUTF16Encoder'

!		Class methods for 'ZnUTF16Encoder'

category: 'accessing'
classmethod: ZnUTF16Encoder
handlesEncoding: string
	"Return true when my instances handle the encoding described by string"

	^ self knownEncodingIdentifiers includes: (self canonicalEncodingIdentifier: string)
%

category: 'accessing'
classmethod: ZnUTF16Encoder
knownEncodingIdentifiers
	^ #( 'utf16' 'utf16be' 'utf16le' )
%

!		Instance methods for 'ZnUTF16Encoder'

category: 'private'
method: ZnUTF16Encoder
back16BitWordOnStream: stream
	| firstByte secondByte |
	firstByte := stream back.
	secondByte := stream back.
	^ self isBigEndian 
		ifTrue: [ secondByte + (firstByte << 8) ]
		ifFalse: [ firstByte + (secondByte << 8) ]
%

category: 'converting'
method: ZnUTF16Encoder
backOnStream: stream
	"Move back one character on stream"

	| word |
	word := self back16BitWordOnStream: stream.
	(word < 16rD800 or: [ word > 16rDBFF ])
		ifFalse: [ 
			self back16BitWordOnStream: stream ]
%

category: 'converting'
method: ZnUTF16Encoder
encodedByteCountForCodePoint: codePoint
	"Return how many bytes are needed to encode integer code point"

	codePoint <= 65535 ifTrue: [ ^ 2 ].
	codePoint <= self maximumUTFCode ifTrue: [ ^ 4 ].
	self errorOutsideRange
%

category: 'error handling'
method: ZnUTF16Encoder
errorIncomplete
	self error: 'Incomplete utf-16 encoding'
%

category: 'accessing'
method: ZnUTF16Encoder
identifier
	^ #utf16
%

category: 'converting'
method: ZnUTF16Encoder
nextCodePointFromStream: stream
	"Read and return the next integer code point from stream"

	| word leadSurrogate trailSurrogate code |
	word := self read16BitWordFromStream: stream.
	(self processByteOrderMark: word)
		ifTrue: [ word := self read16BitWordFromStream: stream ].
	^ (word < 16rD800 or: [ word > 16rDBFF ])
		ifTrue: [
			word ]
		ifFalse: [ 
			leadSurrogate := word.
			trailSurrogate := self read16BitWordFromStream: stream.
			code := (leadSurrogate - 16rD800) * 16r400 + (trailSurrogate - 16rDC00).
			16r10000 + code ]
%

category: 'converting'
method: ZnUTF16Encoder
nextPutCodePoint: codePoint toStream: stream
	"Write the encoding for integer code point to stream"

	| leadSurrogate trailSurrogate shifted |
	(self isSurrogateCodePoint: codePoint) ifTrue: [ self errorOutsideRange ].
	codePoint <= 65535
		ifTrue: [ 
			^ self write16BitWord: codePoint toStream: stream ].
	codePoint <= self maximumUTFCode
		ifTrue: [
			shifted := codePoint - 16r10000.
			leadSurrogate := 16rD800 + (shifted // 16r400).
			trailSurrogate := 16rDC00 + (shifted \\ 16r400).
			self write16BitWord: leadSurrogate toStream: stream.
			self write16BitWord: trailSurrogate toStream: stream ]
		ifFalse: [
			self errorOutsideRange ]
%

category: 'private'
method: ZnUTF16Encoder
processByteOrderMark: word
	^ (word = 16rFEFF or: [ word = 16rFFFE ])
		ifTrue: [ 
			word = 16rFFFE
				ifTrue: [ self swapEndianness ].
			true ]
		ifFalse: [ false ]
%

category: 'private'
method: ZnUTF16Encoder
read16BitWordFromStream: stream
	| firstByte secondByte |
	firstByte := stream next.
	secondByte := stream next.
	(firstByte isNil or: [ secondByte isNil ])
		ifTrue: [ ^ self errorIncomplete ].
	^ self isBigEndian 
		ifTrue: [ secondByte + (firstByte << 8) ]
		ifFalse: [ firstByte + (secondByte << 8) ]
%

category: 'private'
method: ZnUTF16Encoder
write16BitWord: word toStream: stream
	self isBigEndian
		ifTrue: [ 
			stream
				nextPut: (word digitAt: 2);
				nextPut: (word digitAt: 1) ]
		ifFalse: [ 
			stream
				nextPut: (word digitAt: 1);
				nextPut: (word digitAt: 2) ]
%

! Class implementation for 'ZnUTF32Encoder'

!		Class methods for 'ZnUTF32Encoder'

category: 'accessing'
classmethod: ZnUTF32Encoder
handlesEncoding: string
	"Return true when my instances handle the encoding described by string"

	^ self knownEncodingIdentifiers includes: (self canonicalEncodingIdentifier: string)
%

category: 'accessing'
classmethod: ZnUTF32Encoder
knownEncodingIdentifiers
	^ #( 'utf32' 'utf32be' 'utf32le' 'ucs4' 'ucs4be' 'ucs4le')
%

!		Instance methods for 'ZnUTF32Encoder'

category: 'converting'
method: ZnUTF32Encoder
backOnStream: stream
	"Move back one character on stream"

	4 timesRepeat: [ stream back ]
%

category: 'converting'
method: ZnUTF32Encoder
encodedByteCountForCodePoint: codePoint
	"Return how many bytes are needed to encode integer codePoint"

	codePoint > self maximumUTFCode 
		ifTrue: [ ^ self errorOutsideRange ].
	^ 4
%

category: 'error handling'
method: ZnUTF32Encoder
errorIncomplete
	self error: 'Incomplete utf-32 encoding'
%

category: 'accessing'
method: ZnUTF32Encoder
identifier
	^ #utf32
%

category: 'converting'
method: ZnUTF32Encoder
nextCodePointFromStream: stream
	"Read and return the next integer code point from stream"

	| codePoint |
	codePoint := self readCodePointFrom: stream.
	(self processByteOrderMark: codePoint)
		ifTrue: [ codePoint := self readCodePointFrom: stream ].
	((self isSurrogateCodePoint: codePoint) or: [ codePoint > self maximumUTFCode ]) 
		ifTrue: [ ^ self errorOutsideRange ].
	^ codePoint
%

category: 'converting'
method: ZnUTF32Encoder
nextPutCodePoint: codePoint toStream: stream
	"Write the encoding for integer code point to stream"

	(self isSurrogateCodePoint: codePoint) 
		ifTrue: [ self errorOutsideRange ].
	codePoint <= self maximumUTFCode
		ifTrue: [
			self writeCodePoint: codePoint to: stream ]
		ifFalse: [
			self errorOutsideRange ]
%

category: 'private'
method: ZnUTF32Encoder
processByteOrderMark: codePoint
	^ (codePoint = 16rFEFF or: [ codePoint = 16rFFFE0000 ])
		ifTrue: [ 
			codePoint = 16rFFFE0000
				ifTrue: [ self swapEndianness ].
			true ]
		ifFalse: [ false ]
%

category: 'private'
method: ZnUTF32Encoder
readCodePointFrom: stream
	| byte1 byte2 byte3 byte4 |
	byte1 := stream next.
	byte2 := stream next.
	byte3 := stream next.
	byte4 := stream next.
	(byte1 isNil or: [ byte2 isNil or: [ byte3 isNil or: [ byte4 isNil ] ] ])
		ifTrue: [ ^ self errorIncomplete ].
	^ self isBigEndian
		ifTrue: [ 
			(byte1 bitShift: 24) + (byte2 bitShift: 16) + (byte3 bitShift: 8) + byte4 ] 
		ifFalse: [ 
			(byte4 bitShift: 24) + (byte3 bitShift: 16) + (byte2 bitShift: 8) + byte1 ]
%

category: 'private'
method: ZnUTF32Encoder
writeCodePoint: codePoint to: stream
	self isBigEndian 
		ifTrue: [ 
			stream 
				nextPut: (codePoint digitAt: 4); 
				nextPut: (codePoint digitAt: 3); 
				nextPut: (codePoint digitAt: 2); 
				nextPut: (codePoint digitAt: 1) ]
		ifFalse: [ 
			stream 
				nextPut: (codePoint digitAt: 1); 
				nextPut: (codePoint digitAt: 2); 
				nextPut: (codePoint digitAt: 3); 
				nextPut: (codePoint digitAt: 4) ]
%

! Class implementation for 'ZnUTF8Encoder'

!		Class methods for 'ZnUTF8Encoder'

category: 'accessing'
classmethod: ZnUTF8Encoder
byteASCIISet
	
	^ ByteASCIISet
%

category: 'accessing'
classmethod: ZnUTF8Encoder
byteUTF8Encoding
	
	^ ByteUTF8Encoding
%

category: 'accessing'
classmethod: ZnUTF8Encoder
default
	"Return a cached instance of the most commonly used encoder,
	which is faster than going via #newForEncoding: that does a subclass search"
	
	^ Default ifNil: [ Default := self new ]
%

category: 'accessing'
classmethod: ZnUTF8Encoder
handlesEncoding: string
	"Return true when my instances handle the encoding described by string"
	
	^ (self canonicalEncodingIdentifier: string) = 'utf8'
%

category: 'class initialization'
classmethod: ZnUTF8Encoder
initialize
	| encoder stream |
	ByteASCIISet := ByteArray new: 256.
	ByteUTF8Encoding := Array new: 256.
	encoder := self new.
	stream := ByteArray new writeStream.
	0 to: 255 do: [ :each |
		| bytes |
		stream reset.
		encoder nextPut: (Character value: each) toStream: stream.
		bytes := stream contents.
		(bytes size = 1 and: [ bytes first = each ])
			ifTrue: [
				ByteASCIISet at: each + 1 put: 0 ]
			ifFalse: [  
				ByteASCIISet at: each + 1 put: 1.
				ByteUTF8Encoding at: each + 1 put: bytes ] ]
%

category: 'accessing'
classmethod: ZnUTF8Encoder
knownEncodingIdentifiers
	^ #( 'utf8' )
%

category: 'instance creation'
classmethod: ZnUTF8Encoder
newForEncoding: string
	"No further parametrization needed"
	
	^ self new
%

category: 'accessing'
classmethod: ZnUTF8Encoder
reset
	"ZnUTF8Encoder reset"
	
	^ Default := self new
%

!		Instance methods for 'ZnUTF8Encoder'

category: 'converting'
method: ZnUTF8Encoder
backOnStream: stream
	"Move back one character on stream"

	[ (stream back bitAnd: 2r11000000) == 2r10000000 ] whileTrue
%

category: 'convenience'
method: ZnUTF8Encoder
decodeAsCodePoints: bytes
	"Decode bytes and return the resulting code points - This is a Gemstone implementation"

	self flag: 'GemStone/Pharo code switched for research'.

	self isGemStoneUtf8Encoding
		ifTrue: [
			| ar |
			ar := {}.
			bytes decodeFromUTF8 do: [:char | ar add: char codePoint ].
			^ ar]
		ifFalse: [
			^super decodeAsCodePoints: bytes]
%

category: 'convenience'
method: ZnUTF8Encoder
decodeBytes: bytes
	"Decode bytes and return the resulting string"

	self flag: 'GemStone/Pharo code switched for research'.

	self isGemStoneUtf8Encoding
		ifTrue: [
			^ [bytes decodeFromUTF8] on: (ExceptionSet with: ArgumentError  with: MessageNotUnderstood) do: [:s | ZnCharacterEncodingError signal]] 
		ifFalse: [
			^super decodeBytes: bytes]
%

category: 'convenience'
method: ZnUTF8Encoder
encodeCodePoints: codePoints
	"Encode codePoints and return the resulting byte array"

	self flag: 'GemStone/Pharo code switched for research'.

	self isGemStoneUtf8Encoding
		ifTrue: [
			^ codePoints asByteArray asUnicodeString encodeAsUTF8]
		ifFalse: [
			^super encodeCodePoints: codePoints]
%

category: 'converting'
method: ZnUTF8Encoder
encodedByteCountFor: character
	"Return how many bytes are needed to encode character"

	self flag: 'GemStone/Pharo code switched for research'.

	self isGemStoneUtf8Encoding
		ifTrue: [
			^ character asString encodeAsUTF8 size]
		ifFalse: [
			^super encodedByteCountFor: character]
%

category: 'converting'
method: ZnUTF8Encoder
encodedByteCountForCodePoint: codePoint
	"Return how many bytes are needed to encode integer code point"

	codePoint < 128 ifTrue: [ ^ 1 ].
	codePoint < 2048 ifTrue: [ ^ 2 ].
	codePoint < 65535 ifTrue: [ ^ 3 ].
	codePoint <= self maximumUTFCode ifTrue: [ ^ 4 ].
	self errorOutsideRange.
	^ 0
%

category: 'convenience'
method: ZnUTF8Encoder
encodedByteCountForCodePoints: codePoints
	"Return the exact number of bytes it would take to encode codePoints as a byte array"

	self flag: 'GemStone/Pharo code switched for research'.
	
	self isGemStoneUtf8Encoding
		ifTrue: [
			^ (self encodeCodePoints: codePoints) size]
		ifFalse: [
			^ super encodedByteCountForCodePoints: codePoints]
%

category: 'convenience'
method: ZnUTF8Encoder
encodedByteCountForString: string
	"Return the exact number of bytes it would take to encode string as a byte array"

	self flag: 'GemStone/Pharo code switched for research'.
	
	self isGemStoneUtf8Encoding
		ifTrue: [
			^ (self encodeString: string) size]
		ifFalse: [
			^super encodedByteCountForString: string]
%

category: 'convenience'
method: ZnUTF8Encoder
encodeString: string
	"Encode string and return the resulting Utf8 instance"

	self flag: 'GemStone/Pharo code switched for research'.

	self isGemStoneUtf8Encoding
		ifTrue: [
			^ [string encodeAsUTF8 asByteArray] on: (ExceptionSet with: ArgumentError  with: MessageNotUnderstood) do: [:s | ZnInvalidUTF8 signal]  ]
		ifFalse: [
			^super encodeString: string]
%

category: 'error handling'
method: ZnUTF8Encoder
error: message
	^ZnInvalidUTF8 signal: message
%

category: 'error handling'
method: ZnUTF8Encoder
errorIllegalContinuationByte
	^ self error: 'Illegal continuation byte for utf-8 encoding'
%

category: 'error handling'
method: ZnUTF8Encoder
errorIllegalLeadingByte
	^ self error: 'Illegal leading byte for utf-8 encoding'
%

category: 'error handling'
method: ZnUTF8Encoder
errorOverlong
	^ self error: 'Overlong utf-8 encoding (non-shortest form)'
%

category: 'private'
method: ZnUTF8Encoder
findFirstNonASCIIIn: string startingAt: offset
	"This calls a fast primitive. Note that string can be a ByteString or ByteArray"
	
self error: 'The classes ByteString and ByteASCIISet are not implemented in Gemstone'.
	offset > string size ifTrue: [ ^ 0 ].
	^ #ByteString 
		findFirstInString: string 
		inSet: ByteASCIISet 
		startingAt: offset
%

category: 'accessing'
method: ZnUTF8Encoder
identifier
	^ #utf8
%

category: 'convenience'
method: ZnUTF8Encoder
next: count putAll: string startingAt: offset toStream: stream
	"Write count characters from string starting at offset to stream."
	"Overwritten for performance reasons - create a fast path for byte strings"
	
	self flag: 'GemStone/Pharo code switched for research'.

	self isGemStoneUtf8Encoding
		ifTrue: [
			stream nextPutAll: (string copyFrom: offset to: offset + count - 1) encodeAsUTF8 asByteArray]
		ifFalse: [
			string isByteString
				ifTrue: [ self next: count putAllByteString: string startingAt: offset toStream: stream ]
				ifFalse: [ super next: count putAll: string startingAt: offset toStream: stream ] ]
%

category: 'private'
method: ZnUTF8Encoder
next: count putAllASCII: string startingAt: offset toStream: stream
	"Write count bytes from string starting at offset to stream,
	assuming all characters are in the ASCII set and need no translation"
	
	offset to: offset + count - 1 do: [ :index | 
		stream nextPut: (string byteAt: index) ]
%

category: 'private'
method: ZnUTF8Encoder
next: count putAllByteString: string startingAt: offset toStream: stream
	"A faster version when string is a ByteString"
	
	| lastIndex nextIndex |

	lastIndex := offset.
	nextIndex := self findFirstNonASCIIIn: string startingAt: lastIndex.
	(nextIndex = 0 or: [ offset + count <= nextIndex ]) 
		ifTrue: [  
			^ self next: count putAllASCII: string startingAt: offset toStream: stream ].
	[	nextIndex > lastIndex 
			ifTrue: [ 
				self next: nextIndex - lastIndex putAllASCII: string startingAt: lastIndex toStream: stream ].
		stream nextPutAll: (ByteUTF8Encoding at: (string byteAt: nextIndex) + 1).
		lastIndex := nextIndex + 1.
		nextIndex := self findFirstNonASCIIIn: string startingAt: lastIndex.
		nextIndex = 0 or: [ offset + count <= nextIndex ] ] whileFalse.
	offset + count <= lastIndex 
		ifFalse: [ 
			self next: offset + count - lastIndex putAllASCII: string startingAt: lastIndex toStream: stream ]
%

category: 'converting'
method: ZnUTF8Encoder
nextCodePointFromStream: stream
	"Read and return the next integer code point from stream"

	| code byte next |
	(byte := stream next ifNil: [ ^ self errorIncomplete ]) < 128
		ifTrue: [ ^ byte ].
	(byte bitAnd: 2r11100000) == 2r11000000
		ifTrue: [ 
			code := byte bitAnd: 2r00011111.
			((next := stream next ifNil: [ ^ self errorIncomplete ]) bitAnd: 2r11000000) == 2r10000000
				ifTrue: [ code := (code bitShift: 6) + (next bitAnd: 2r00111111) ]
				ifFalse: [ ^ self errorIllegalContinuationByte ].
			code < 128 ifTrue: [ ^ self errorOverlong ].
			^ code ].
	(byte bitAnd: 2r11110000) == 2r11100000
		ifTrue: [ 
			code := byte bitAnd: 2r00001111.
			2 timesRepeat: [ 
				((next := stream next ifNil: [ ^ self errorIncomplete ]) bitAnd: 2r11000000) == 2r10000000
					ifTrue: [ code := (code bitShift: 6) + (next bitAnd: 2r00111111) ]
					ifFalse: [ ^ self errorIllegalContinuationByte ] ].
			code < 2048 ifTrue: [ ^ self errorOverlong ].
			(self isSurrogateCodePoint: code) ifTrue: [ ^ self errorOutsideRange ].
			code = 65279 "Unicode Byte Order Mark" ifTrue: [ 
				stream atEnd ifTrue: [ ^ self errorIncomplete ].
				^ self nextCodePointFromStream: stream ].
			^ code ].
	(byte bitAnd: 2r11111000) == 2r11110000
		ifTrue: [ 
			code := byte bitAnd: 2r00000111.
			3 timesRepeat: [ 
				((next := stream next ifNil: [ ^ self errorIncomplete ]) bitAnd: 2r11000000) == 2r10000000
					ifTrue: [ code := (code bitShift: 6) + (next bitAnd: 2r00111111) ]
					ifFalse: [ ^ self errorIllegalContinuationByte ] ].
			code < 65535 ifTrue: [ ^ self errorOverlong ].
			^ code ].
	^ self errorIllegalLeadingByte
%

category: 'converting'
method: ZnUTF8Encoder
nextPutCodePoint: codePoint toStream: stream
	"Write the encoding for Integer code point to stream"

	codePoint < 128 ifTrue: [ 
		^ stream nextPut: codePoint ].
	codePoint < 2048 ifTrue: [ 
		^ stream 
			nextPut: (2r11000000 + (codePoint bitShift: -6)); 
			nextPut: (2r10000000 + (codePoint bitAnd: 2r111111)) ].
	(self isSurrogateCodePoint: codePoint) ifTrue: [ ^ self errorOutsideRange ].
	codePoint < 65536 ifTrue: [ 
		^ stream 
			nextPut: (2r11100000 + (codePoint bitShift: -12));
			nextPut: (2r10000000 + ((codePoint bitShift: -6) bitAnd: 2r111111)); 
			nextPut: (2r10000000 + (codePoint bitAnd: 2r111111)) ].
	codePoint <= self maximumUTFCode ifTrue: [ 
		^ stream 
			nextPut: (2r11110000 + (codePoint bitShift: -18));
			nextPut: (2r10000000 + ((codePoint bitShift: -12) bitAnd: 2r111111));
			nextPut: (2r10000000 + ((codePoint bitShift: -6) bitAnd: 2r111111)); 
			nextPut: (2r10000000 + (codePoint bitAnd: 2r111111)) ].
	^ self errorOutsideRange
%

category: 'convenience'
method: ZnUTF8Encoder
readInto: string startingAt: offset count: requestedCount fromStream: stream
	"Read requestedCount characters into string starting at offset,
	returning the number read, there could be less available when stream is atEnd"

	self flag: 'GemStone/Pharo code switched for research'.

	self isGemStoneUtf8Encoding
		ifTrue: [
			| decodedCollection pos |

			pos := stream position.
			decodedCollection := self decodeBytes: stream contents.
			string 
				replaceFrom: offset 
				to: offset + (requestedCount min: decodedCollection size) - 1
				with: decodedCollection
				startingAt: pos + 1.
			stream setToEnd.
			^ 	requestedCount min: decodedCollection size]
		ifFalse: [
			| stringBuffer |

			stringBuffer := string.
			offset to: offset + requestedCount - 1 do: [ :index | | codePoint |
			stream atEnd ifTrue: [ ^ index - offset ].  
			codePoint := self nextCodePointFromStream: stream.
			(codePoint > 255 and: [ stringBuffer isWideString not ])
				ifTrue: [ stringBuffer := ZnByteStringBecameWideString convert: stringBuffer ].
			stringBuffer at: index put: (Character value: codePoint) ].
			^ requestedCount]
%

! Class implementation for 'ZnCharacterReadWriteStream'

!		Class methods for 'ZnCharacterReadWriteStream'

category: 'instance creation'
classmethod: ZnCharacterReadWriteStream
on: wrappedStream encoding: encoding

	^ self new
		on: wrappedStream encoding: encoding;
		yourself
%

!		Instance methods for 'ZnCharacterReadWriteStream'

category: 'accessing'
method: ZnCharacterReadWriteStream
atEnd
	
	^ readStream atEnd
%

category: 'accessing'
method: ZnCharacterReadWriteStream
close
	
	writeStream close
%

category: 'testing'
method: ZnCharacterReadWriteStream
closed
	^ writeStream closed
%

category: 'accessing'
method: ZnCharacterReadWriteStream
collectionSpecies
	^ String
%

category: 'accessing'
method: ZnCharacterReadWriteStream
cr
	
	writeStream cr
%

category: 'accessing'
method: ZnCharacterReadWriteStream
flush

	writeStream flush
%

category: 'testing'
method: ZnCharacterReadWriteStream
isReadOnly
	
	^ false
%

category: 'accessing'
method: ZnCharacterReadWriteStream
next
	
	^ readStream next
%

category: 'accessing'
method: ZnCharacterReadWriteStream
next: anInteger 
	
	^ readStream next: anInteger
%

category: 'accessing'
method: ZnCharacterReadWriteStream
nextPut: aCharacter

	^ writeStream nextPut: aCharacter
%

category: 'accessing'
method: ZnCharacterReadWriteStream
nextPutAll: aString

	^ writeStream nextPutAll: aString
%

category: 'instance creation'
method: ZnCharacterReadWriteStream
on: aStream encoding: encoding
	| encoder |
	encoder := encoding asZnCharacterEncoder.
	readStream := ZnCharacterReadStream on: aStream encoding: encoder.
	writeStream := ZnCharacterWriteStream on: aStream encoding: encoder
%

category: 'accessing'
method: ZnCharacterReadWriteStream
peek
	
	^ readStream peek
%

category: 'accessing'
method: ZnCharacterReadWriteStream
position

	^ readStream position
%

category: 'accessing'
method: ZnCharacterReadWriteStream
position: anInteger 
	
	readStream position: anInteger
%

category: 'accessing'
method: ZnCharacterReadWriteStream
readOnlyCopy

	^ readStream
%

category: 'accessing'
method: ZnCharacterReadWriteStream
setToEnd

	writeStream setToEnd
%

category: 'accessing'
method: ZnCharacterReadWriteStream
size

	^ readStream size
%

category: 'accessing'
method: ZnCharacterReadWriteStream
skip: anInteger 
	
	readStream skip: anInteger
%

category: 'accessing'
method: ZnCharacterReadWriteStream
space
	
	writeStream space
%

category: 'accessing'
method: ZnCharacterReadWriteStream
upToAll: aCollection
	"Answer a subcollection from the current access position to the occurrence (if any, but not inclusive) of aCollection. If aCollection is not in the stream, answer the entire rest of the stream."
	
	^ self collectionSpecies streamContents: [ :out |
		| partialMatch pattern matched |
		partialMatch := (self collectionSpecies new: aCollection size) writeStream.
		pattern := aCollection readStream.
		matched := false.
		([ matched or: [ self atEnd or: [ pattern atEnd ] ] ]) whileFalse: [
			| ch |
			(ch := self next) = pattern next
				ifTrue: [
					pattern atEnd
						ifTrue: [ matched := true ]
						ifFalse: [ partialMatch nextPut: ch ] ]
				ifFalse: [
					pattern reset.
					out nextPutAll: partialMatch contents.
					partialMatch reset.
					out nextPut: ch ] ].
		matched ifFalse: [ out nextPutAll: partialMatch contents ] ]
%

category: 'accessing'
method: ZnCharacterReadWriteStream
upToEnd
	
	^ readStream upToEnd
%

! Class implementation for 'ZnEncodedStream'

!		Class methods for 'ZnEncodedStream'

category: 'accessing'
classmethod: ZnEncodedStream
defaultEncoder
	^ ZnUTF8Encoder default
%

category: 'instance creation'
classmethod: ZnEncodedStream
on: wrappedStream
	^ self new
		on: wrappedStream;
		yourself
%

category: 'instance creation'
classmethod: ZnEncodedStream
on: wrappedStream encoding: encoding
	^ self new
		on: wrappedStream;
		encoding: encoding;
		yourself
%

!		Instance methods for 'ZnEncodedStream'

category: 'initialize-release'
method: ZnEncodedStream
close
	stream close
%

category: 'testing'
method: ZnEncodedStream
closed
	^ stream closed
%

category: 'accessing'
method: ZnEncodedStream
encoder
	^ encoder ifNil: [ encoder := self class defaultEncoder ]
%

category: 'initialize-release'
method: ZnEncodedStream
encoder: characterEncoder
	encoder := characterEncoder
%

category: 'initialize-release'
method: ZnEncodedStream
encoding: encoding
	encoder := encoding asZnCharacterEncoder
%

category: 'testing'
method: ZnEncodedStream
isStream
  ^ true
%

category: 'initialize-release'
method: ZnEncodedStream
on: wrappedStream
	stream := wrappedStream
%

category: 'accessing'
method: ZnEncodedStream
position
	^ stream position
%

category: 'accessing'
method: ZnEncodedStream
position: aPosition
	stream position: aPosition
%

category: 'accessing'
method: ZnEncodedStream
setToEnd
	stream setToEnd
%

category: 'accessing'
method: ZnEncodedStream
size
	^ stream size
%

category: 'accessing'
method: ZnEncodedStream
truncate
	stream truncate
%

category: 'accessing'
method: ZnEncodedStream
truncate: anInteger 
	stream truncate: anInteger
%

category: 'accessing'
method: ZnEncodedStream
wrappedStream
	^ stream
%

! Class implementation for 'ZnEncodedReadStream'

!		Instance methods for 'ZnEncodedReadStream'

category: 'testing'
method: ZnEncodedReadStream
atEnd
	^ peeked isNil and: [ stream atEnd ]
%

category: 'accessing'
method: ZnEncodedReadStream
contents

	self flag: 'GemStone/Pharo code switched for research'.

	self isGemStoneUtf8Encoding
		ifTrue: [
			^ self collectionSpecies
				streamContents: [ :collectionStream | 
					collectionStream nextPutAll: (self encoder decodeBytes: stream  contents) ] ]
		ifFalse: [
			^self upToEnd]
%

category: 'testing'
method: ZnEncodedReadStream
isBinary
	^ false
%

category: 'testing'
method: ZnEncodedReadStream
isReadOnly

	^ true
%

category: 'accessing'
method: ZnEncodedReadStream
next
	^ peeked
		ifNil: [ 
			stream atEnd ifFalse: [ self nextElement ] ]
		ifNotNil: [ | character |
			character := peeked.
			peeked := nil.
			character ]
%

category: 'accessing'
method: ZnEncodedReadStream
next: requestedCount 
	"Read requestedCount elements into new collection and return it,
	 it could be that less elements were available"

	^ self 
		next: requestedCount 
		into: (self collectionSpecies new: requestedCount)
%

category: 'accessing'
method: ZnEncodedReadStream
next: requestedCount into: collection
	"Read requestedCount elements into collection,
	returning a copy if less elements are available"
	
	^ self
		next: requestedCount
		into: collection
		startingAt: 1
%

category: 'accessing'
method: ZnEncodedReadStream
next: requestedCount into: collection startingAt: offset
	"Read requestedCount elements into collection starting at offset,
	returning a copy if less elements are available"
	
	| readCount |
	readCount := self 
		readInto: collection 
		startingAt: offset 
		count: requestedCount.
	^ requestedCount = readCount
		ifTrue: [ collection ]
		ifFalse: [ collection copyFrom: 1 to: offset + readCount - 1 ]
%

category: 'private'
method: ZnEncodedReadStream
nextElement
	self subclassResponsibility 
%

category: 'accessing'
method: ZnEncodedReadStream
nextInto: collection
	"Read the next elements of the receiver into collection,
	returning a copy if less elements are available"
	
	^ self
		next: collection size
		into: collection
%

category: 'accessing'
method: ZnEncodedReadStream
peek
	^ peeked
		ifNil: [
			stream atEnd ifFalse: [ peeked := self nextElement ] ]
%

category: 'accessing'
method: ZnEncodedReadStream
peekFor: object
	^ self peek = object
		ifTrue: [ 
			self next.
			true ]
		ifFalse: [ false ]
%

category: 'accessing'
method: ZnEncodedReadStream
position

	^ super position - (peeked ifNil: [ 0 ] ifNotNil: [ 1 ])
%

category: 'accessing'
method: ZnEncodedReadStream
position: anInteger
	super position: anInteger.
	peeked := nil
%

category: 'accessing'
method: ZnEncodedReadStream
readInto: collection startingAt: offset count: requestedCount
	"Read requestedCount elements into collection starting at offset,
	returning the number of elements read, there could be less elements available.
	This is an inefficient abstract implementation, reading one by one."
	
	0 to: requestedCount - 1 do: [ :count | | object |
		(object := self next) ifNil: [ ^ count ].  
		collection at: offset + count put: object ].
	^ requestedCount
%

category: 'accessing'
method: ZnEncodedReadStream
skip: count
	count timesRepeat: [ self next ]
%

category: 'accessing'
method: ZnEncodedReadStream
upTo: anObject 	
	^ self collectionSpecies 
		streamContents: [ :out | | element |
			[ self atEnd or: [ (element := self next) = anObject ] ] whileFalse: [ 
				out nextPut: element ] ]
%

category: 'accessing'
method: ZnEncodedReadStream
upToEnd
	^ self collectionSpecies
		streamContents: [ :collectionStream | 
			[ self atEnd ] whileFalse: [ collectionStream nextPut: self next ] ]
%

! Class implementation for 'ZnCharacterReadStream'

!		Instance methods for 'ZnCharacterReadStream'

category: 'accessing'
method: ZnCharacterReadStream
collectionSpecies
	^ String
%

category: 'accessing'
method: ZnCharacterReadStream
match: subCollection 
	"Set the access position of the receiver to be past the next occurrence of the subCollection. Answer whether subCollection is found.  No wildcards, and case does matter."
	| pattern startMatch |
	pattern := subCollection readStream.
	startMatch := nil.
	[ pattern atEnd ] whileFalse: 
		[ self atEnd ifTrue: [ ^ false ].
		self next = pattern next 
			ifTrue: [ pattern position = 1 ifTrue: [ startMatch := self position ] ]
			ifFalse: 
				[ pattern position: 0.
				startMatch ifNotNil: 
					[ self position: startMatch.
					startMatch := nil ] ] ].
	^ true
%

category: 'private'
method: ZnCharacterReadStream
nextElement
	^ self encoder nextFromStream: stream
%

category: 'accessing'
method: ZnCharacterReadStream
nextLine
	"Read a CR, LF or CRLF terminated line, returning the contents of the line without the EOL. Return nil when the receiver is #atEnd."
	
"	self atEnd ifTrue: [ ^ nil ].
	^ self collectionSpecies streamContents: [ :out |
		out nextPutAll: (self encoder decodeBytes: stream nextLine) ]"

	self atEnd ifTrue: [ ^ nil ].
	^ self collectionSpecies streamContents: [ :out | | eol char |
		eol := false.
		[ eol ] whileFalse: [ 
			char := self next.
			(char isNil or: [ char = Character lf ])
				ifTrue: [ eol := true ]
				ifFalse: [ 
					char = Character cr 
						ifTrue: [ eol := true. self peekFor: Character lf ]
						ifFalse: [ out nextPut: char ] ] ] ]
%

category: 'accessing'
method: ZnCharacterReadStream
readInto: collection startingAt: offset count: requestedCount 
	"Read count elements and place them in collection starting at offset.
	Return the number of elements actually read."
	
	^ peeked 
		ifNil: [ | readCount |
			[ readCount := self encoder 
					readInto: collection 
					startingAt: offset 
					count: requestedCount 
					fromStream: stream ]
				on: ZnByteStringBecameWideString 			"RK - Added on:do: to make consistent with Pharo"
				do: [ :byteStringBecameWideString | 
					byteStringBecameWideString becomeForward; resume ].
			readCount ]
		ifNotNil: [ 
			collection at: offset put: peeked.
			peeked := nil.
			(self 
				readInto: collection 
				startingAt: offset + 1
				count: requestedCount - 1) + 1 ]
%

category: 'accessing'
method: ZnCharacterReadStream
upToAll: aCollection
	"Answer a subcollection from the current access position to the occurrence (if any, but not inclusive) of aCollection. If aCollection is not in the stream, answer the entire rest of the stream."
	
	^ self collectionSpecies streamContents: [ :out |
		| partialMatch pattern matched |
		partialMatch := (self collectionSpecies new: aCollection size) writeStreamPortable.
		pattern := aCollection readStreamPortable.
		matched := false.
		([ matched or: [ self atEnd or: [ pattern atEnd ] ] ]) whileFalse: [
			| ch |
			(ch := self next) = pattern next
				ifTrue: [
					pattern atEnd
						ifTrue: [ matched := true ]
						ifFalse: [ partialMatch nextPut: ch ] ]
				ifFalse: [
					pattern reset.
					out nextPutAll: partialMatch contents.
					partialMatch reset.
					out nextPut: ch ] ].
		matched ifFalse: [ out nextPutAll: partialMatch contents ] ]
%

! Class implementation for 'ZnCodePointReadStream'

!		Instance methods for 'ZnCodePointReadStream'

category: 'accessing'
method: ZnCodePointReadStream
collectionSpecies
	^ Array
%

category: 'private'
method: ZnCodePointReadStream
nextElement
	^ self encoder nextCodePointFromStream: stream
%

! Class implementation for 'ZnEncodedWriteStream'

!		Instance methods for 'ZnEncodedWriteStream'

category: 'accessing'
method: ZnEncodedWriteStream
<< collection
	^ self nextPutAll: collection
%

category: 'accessing'
method: ZnEncodedWriteStream
flush
	stream flush
%

category: 'testing'
method: ZnEncodedWriteStream
isBinary
	^ false
%

category: 'accessing'
method: ZnEncodedWriteStream
next: count putAll: collection	
	self 
		next: count 
		putAll: collection 
		startingAt: 1
%

category: 'accessing'
method: ZnEncodedWriteStream
next: count putAll: collection startingAt: offset
	"Write count items from collection starting at offset.
	This is an inefficient abstract implementation writing one by one."
	
	0 to: count - 1 do: [ :each | 
		self nextPut: (collection at: offset + each) ]
%

category: 'accessing'
method: ZnEncodedWriteStream
nextPut: anObject
	self subclassResponsibility
%

category: 'accessing'
method: ZnEncodedWriteStream
nextPutAll: collection

	self 
		next: collection size 
		putAll: collection 
		startingAt: 1
%

! Class implementation for 'ZnCharacterWriteStream'

!		Instance methods for 'ZnCharacterWriteStream'

category: 'accessing'
method: ZnCharacterWriteStream
cr
	self nextPut: Character cr
%

category: 'accessing'
method: ZnCharacterWriteStream
crlf
	self cr; lf
%

category: 'accessing'
method: ZnCharacterWriteStream
lf
	self nextPut: Character lf
%

category: 'accessing'
method: ZnCharacterWriteStream
next: count putAll: collection startingAt: offset
	"Write count characters from collection starting at offset."
	
	self encoder 
		next: count 
		putAll: collection 
		startingAt: offset 
		toStream: stream 
%

category: 'accessing'
method: ZnCharacterWriteStream
nextPut: object
	self encoder 
		nextPut: object 
		toStream: stream.
	^ object
%

category: 'accessing'
method: ZnCharacterWriteStream
print: object
	object printOn: self
%

category: 'accessing'
method: ZnCharacterWriteStream
space
	self nextPut: Character space
%

category: 'accessing'
method: ZnCharacterWriteStream
tab
	self nextPut: Character tab
%

! Class implementation for 'ZnCodePointWriteStream'

!		Instance methods for 'ZnCodePointWriteStream'

category: 'accessing'
method: ZnCodePointWriteStream
nextPut: object
	self encoder 
		nextPutCodePoint: object 
		toStream: stream.
	^ object
%

! Class implementation for 'ZnEndianessReadWriteStream'

!		Class methods for 'ZnEndianessReadWriteStream'

category: 'instance creation'
classmethod: ZnEndianessReadWriteStream
on: writeStream
	^ self basicNew
		on: writeStream;
		yourself
%

category: 'instance creation'
classmethod: ZnEndianessReadWriteStream
on: writeStream do: block
	"Execute block with as argument a ZnBufferedWriteStream on writeStream,
	making sure #flush is called at the end. Return the value of block."
	
	| bufferedWriteStream result |
	bufferedWriteStream := self on: writeStream.
	result := block value: bufferedWriteStream.
	bufferedWriteStream flush.
	^ result
%

!		Instance methods for 'ZnEndianessReadWriteStream'

category: 'endianess'
method: ZnEndianessReadWriteStream
nextLittleEndianNumber: n 
	"Answer the next n bytes as a positive Integer or LargePositiveInteger, where the bytes are ordered from least significant to most significant."

	| bytes s |
	bytes := stream next: n.
	s := 0.
	n to: 1 by: -1 do: [:i | s := (s bitShift: 8) bitOr: (bytes at: i)].
	^ s
%

category: 'endianess'
method: ZnEndianessReadWriteStream
nextLittleEndianNumber: n put: value
	"Answer the next n bytes as a positive Integer or LargePositiveInteger, where the bytes are ordered from least significant to most significant."
	| bytes |
	bytes := ByteArray new: n.
	1 to: n do: [:i | bytes at: i put: (value digitAt: i)].
	stream nextPutAll: bytes
%

category: 'initialize-release'
method: ZnEndianessReadWriteStream
on: aStream

	stream := aStream
%

! Class implementation for 'ZnFastLineReader'

!		Class methods for 'ZnFastLineReader'

category: 'instance creation'
classmethod: ZnFastLineReader
new
	^self basicNew
		initialize;
		yourself
%

category: 'instance creation'
classmethod: ZnFastLineReader
on: characterReadStream
	^ self new 
		on: characterReadStream;
		yourself
%

!		Instance methods for 'ZnFastLineReader'

category: 'testing'
method: ZnFastLineReader
atEnd
	^ readStream atEnd
%

category: 'initialize'
method: ZnFastLineReader
beWide
self error: 'The WideString is not implemented in Gemstone'.
	self bufferStream: (#WideString new: 32) writeStream
%

category: 'initialize'
method: ZnFastLineReader
bufferStream: characterWriteStream
	^ bufferStream := characterWriteStream
%

category: 'initialize'
method: ZnFastLineReader
close
	readStream close
%

category: 'initialize'
method: ZnFastLineReader
initialize
	"super initialize."
	cr := Character cr.
	lf := Character lf
%

category: 'accessing'
method: ZnFastLineReader
linesDo: block
	[ self atEnd ]
		whileFalse: [ 
			| line |
			line := self nextLine.
			line isNil
				ifFalse: [ block value: line ] ]
%

category: 'accessing'
method: ZnFastLineReader
nextLine
	"Read a CR, LF or CRLF terminated line, returning the contents of the line without the EOL. Return nil when the receiver is #atEnd."
	
	self atEnd ifTrue: [ ^ nil ].
	^ self streamContents: [ :out | | eol char |
		eol := false.
		[ eol ] whileFalse: [ 
			char := readStream next.
			(char isNil or: [ char == lf ])
				ifTrue: [ eol := true ]
				ifFalse: [ 
					char == cr 
						ifTrue: [ eol := true. readStream peekFor: lf ]
						ifFalse: [  out nextPut: char ] ] ] ]
%

category: 'initialize'
method: ZnFastLineReader
on: characterReadStream
	readStream := characterReadStream
%

category: 'private'
method: ZnFastLineReader
streamContents: block
	"Like readStream collectionSpecies streamContents: block
	but reusing the underlying buffer for improved efficiency"
	
	bufferStream 
		ifNil: [ 
			bufferStream := (readStream collectionSpecies new: 32) writeStream ].
	bufferStream reset.
	block value: bufferStream.
	^ bufferStream contents
%

! Class implementation for 'ZnPercentEncoder'

!		Class methods for 'ZnPercentEncoder'

category: 'accessing'
classmethod: ZnPercentEncoder
rfc3986UnreservedCharacters
	"Return the unreserved characters according to RFC 3986 section 2.3.
	This is the most narrow safe set to be used in a better safe than sorry approach."

	^ 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_.~'
%

!		Instance methods for 'ZnPercentEncoder'

category: 'accessing'
method: ZnPercentEncoder
characterEncoder
	"Return the character encoder that I currently use.
	If not set, I will default to using UTF-8."

	^ characterEncoder ifNil: [ characterEncoder := ZnDefaultCharacterEncoder value].

"	self flag: 'GemStone/Pharo code switched for research'.

	self isGemStoneUtf8Encoding
		ifTrue: [
			self flag: 'TODO: In Pharo, ZnDefaultCharacterEncoder is a DynamicVariable. This must be handled differently in Gemstone'.
			^ characterEncoder ifNil: [ characterEncoder := ZnCharacterEncoder utf8 ] ]
		ifFalse: [
			^ characterEncoder ifNil: [ characterEncoder := ZnDefaultCharacterEncoder value ] ]"
%

category: 'initialize-release'
method: ZnPercentEncoder
characterEncoder: object
	"Set the character encoding to use to object."
	
	characterEncoder := object
%

category: 'converting'
method: ZnPercentEncoder
decode: string
	"Decode a percent encoded string and return the resulting byte array.
	After percent decoding I will use my character encoder to convert byte values to characters."

	| bytes stringStream |
	stringStream := string readStream.
	bytes := ByteArray streamContents: [ :byteStream | 
		self decode: stringStream to: byteStream ].
	^ self characterEncoder decodeBytes: bytes
%

category: 'converting'
method: ZnPercentEncoder
decode: stringStream to: byteStream
	| char |
	self decodePlusAsSpace.
	[ stringStream atEnd ]
		whileFalse: [ 
			((char := stringStream next) == $+ and: [ decodePlusAsSpace ])
				ifTrue: [ byteStream nextPut: 32 ]
				ifFalse: [ 
					char == $%
						ifTrue: [ byteStream nextPut: (self readHexFrom: stringStream) ]
						ifFalse: [ 
							char charCode < 128
								ifTrue: [ byteStream nextPut: char charCode ]
								ifFalse: [ self errorAsciiCharacterExpected ] ] ] ]
%

category: 'accessing'
method: ZnPercentEncoder
decodePlusAsSpace
	"Return if $+ on input should be decoded as Character space.
	This is normally only done application/x-www-form-urlencoded data,
	but is is on by default anyway."
	
	^ decodePlusAsSpace ifNil: [ decodePlusAsSpace := true ]
%

category: 'initialize-release'
method: ZnPercentEncoder
decodePlusAsSpace: boolean
	"When boolean is true, $+ on input will be decoded as Character space.
	Else $+ is treated as a normal character, filtered by the safe set.
	This is normally only done application/x-www-form-urlencoded data,
	but is is on by default anyway."
	
	decodePlusAsSpace := boolean
%

category: 'converting'
method: ZnPercentEncoder
encode: string
	"Encode string using percent encoding and return the resulting string.
	I will use my character encoder to convert string to bytes and then
	percent encode all byte values that are not in my safe set."

	^ String streamContents: [ :stream | 
		self encode: string readStream to: stream ]
%

category: 'converting'
method: ZnPercentEncoder
encode: readStream to: writeStream
	| bytes buffer byte |
	buffer := (bytes := ByteArray new: 4) writeStream.
	self safeSet; characterEncoder.
	[ readStream atEnd ]
		whileFalse: [ 
			buffer reset.
			characterEncoder nextPut: readStream next toStream: buffer.
			1 to: buffer position do: [ :index | 
				(safeSet includes: (byte := bytes at: index))
					ifTrue: [ writeStream nextPut: byte asCharacter ]
					ifFalse: [ 
						writeStream nextPut: $%.
						self writeHex: byte to: writeStream ] ] ]
%

category: 'converting'
method: ZnPercentEncoder
errorAsciiCharacterExpected
	ZnCharacterEncodingError signal: 'ASCII character expected'
%

category: 'error handling'
method: ZnPercentEncoder
errorHexDigitExpected
	ZnCharacterEncodingError signal: 'hex digit expected'
%

category: 'private'
method: ZnPercentEncoder
readHexFrom: stream
	| first second |
	(stream atEnd not and: [ (first := stream next numberParserDigitalValue) between: 0 and: 15 ])
		ifFalse: [ self errorHexDigitExpected ].
	(stream atEnd not and: [ (second := stream next numberParserDigitalValue) between: 0 and: 15 ])
		ifFalse: [ self errorHexDigitExpected ].
	^ (first << 4) + second
%

category: 'accessing'
method: ZnPercentEncoder
safeSet
	"Return the safe set of characters that I will not encode, as a byte array.
	If not set, I will default to the most commonly used safe set"
	
	^ safeSet ifNil: [ safeSet := self class rfc3986UnreservedCharacters asByteArray ]
%

category: 'initialize-release'
method: ZnPercentEncoder
safeSet: string
	"Set my safe set to be the characters in string, which I will convert to bytes"
	
	safeSet := string asByteArray
%

category: 'private'
method: ZnPercentEncoder
writeHex: integer to: stream
	integer printOn: stream base: 16 length: 2 padded: true
%

! Class implementation for 'ZnPositionableReadStream'

!		Class methods for 'ZnPositionableReadStream'

category: 'instance creation'
classmethod: ZnPositionableReadStream
new
	^self basicNew
		initialize;
		yourself
%

category: 'instance creation'
classmethod: ZnPositionableReadStream
on: readStream
	"Create an instance of ZnPositionableReadStream that wraps readStream to add limited positioning capabilities to it."
	
	^ self new
		on: readStream;
		yourself
%

category: 'convenience'
classmethod: ZnPositionableReadStream
on: readStream do: block
	"Execute block with as argument a ZnPositionableReadStream on readStream.
	Return the value of block."

	^ block value: (self on: readStream)
%

!		Instance methods for 'ZnPositionableReadStream'

category: 'testing'
method: ZnPositionableReadStream
atEnd
	"Answer whether I can access any more objects."
	
	^ delta = 0 and: [ stream atEnd ]
%

category: 'positioning'
method: ZnPositionableReadStream
back
	"Go back one element and return it."

	self skip: -1.
	^ self peek
%

category: 'accessing'
method: ZnPositionableReadStream
bufferSize
	"Return the size of my buffer, which limits how far I can be positioned backwards. See #sizeBuffer: to set another buffer size."
	
	^ buffer size
%

category: 'initialize-release'
method: ZnPositionableReadStream
close
	"Close me after which I can no longer be accessed. I delegate this to the stream that I wrap."
	
	stream close
%

category: 'accessing'
method: ZnPositionableReadStream
collectionSpecies
	"Return the collection class able to hold my elements"

	^ (stream respondsTo: #collectionSpecies)
		ifTrue: [ stream collectionSpecies ]
		ifFalse: [ stream isBinary
				ifTrue: [ ByteArray ]
				ifFalse: [ String ] ]
%

category: 'accessing'
method: ZnPositionableReadStream
defaultBufferSize
	"Return the default size of my buffer, which limits how far I can be positioned backwards. See #sizeBuffer: to set another buffer size."
	
	^ 2 raisedToInteger: 8
%

category: 'initialize-release'
method: ZnPositionableReadStream
initialize
	"super initialize."
	count := index := delta := 0
%

category: 'testing'
method: ZnPositionableReadStream
isBinary
	"Return whether I am binary, whether my elements are byte values (8 bit integers between 0 and 255)"
	
	^ stream isBinary
%

category: 'accessing'
method: ZnPositionableReadStream
next
	"Return the next element and move over it"
	
	| next |
	delta = 0
		ifTrue: [ 
			(next := stream next) ifNotNil: [ 
				count := count + 1.
				buffer at: index + 1 put: next.
				index := (index + 1) \\ buffer size ] ]
		ifFalse: [ 
			next := buffer at: ((index - delta) \\ buffer size) + 1.
			delta := delta - 1 ].
	^ next
%

category: 'accessing'
method: ZnPositionableReadStream
next: requestedCount 
	"Read requestedCount elements and return them as a collection.
	If less are available, a smaller collection will be returned."

	^ self 
		next: requestedCount 
		into: (self collectionSpecies new: requestedCount)
%

category: 'accessing'
method: ZnPositionableReadStream
next: requestedCount into: collection
	"Read requestedCount elements into collection,
	returning a copy if less elements are available"
	
	^ self 
		next: requestedCount 
		into: collection 
		startingAt: 1
%

category: 'accessing'
method: ZnPositionableReadStream
next: requestedCount into: collection startingAt: offset
	"Read requestedCount elements into collection starting at offset,
	returning a copy if less elements are available"
	
	| read |
	read := self 
		readInto: collection 
		startingAt: offset 
		count: requestedCount.
	^ read = requestedCount 
		ifTrue: [ collection ]
		ifFalse: [ collection copyFrom: 1 to: offset + read - 1 ]
%

category: 'accessing'
method: ZnPositionableReadStream
nextInto: collection
	"Read the next elements of the receiver into collection,
	returning a copy if less elements are available"
	
	^ self
		next: collection size
		into: collection
%

category: 'instance creation'
method: ZnPositionableReadStream
on: readStream
	"Initialize me on readStream"
	
	stream := readStream.
	self sizeBuffer: self defaultBufferSize
%

category: 'accessing'
method: ZnPositionableReadStream
peek
	"Return the next element but do not move over it"
	
	^ self savingPositionDo: [ self next ]
%

category: 'accessing'
method: ZnPositionableReadStream
peekFor: object
	"Answer false and do not move over the next element if it is not equal to object, or if the receiver is at the end. 
	Answer true and move over the next element when it is equal to object."

	^ self peek = object
		ifTrue: [ 
			self next.
			true ]
		ifFalse: [ false ]
%

category: 'positioning'
method: ZnPositionableReadStream
position
	"Return my current position. This is an object that can be used as argument to #position: to move back to that position. Although opaque, it is currently an integer count of the number of #next operations done on the stream that I wrap."
	
	^ count - delta
%

category: 'positioning'
method: ZnPositionableReadStream
position: newPosition
	"Move my current position to newPosition, an object obtained by previously calling #position. My buffer size limits how far I can be positioned backwards. A SubscriptOutOfBounds exception will be signalled in case this operation cannot be completed. It is also no possible to go backwards unless data has been read previously."
	
	| newDelta |
	newDelta := count - newPosition.
	(newDelta between: 0 and: (buffer size min: count))
		ifFalse: [ 
			^ SubscriptOutOfBounds 
					signalFor: self 
					lowerBound: self position 
					upperBound: self position - (buffer size min: count) ].
	^ delta := newDelta
%

category: 'accessing'
method: ZnPositionableReadStream
readInto: collection startingAt: offset count: requestedCount
	"Read requestedCount elements into collection starting at offset,
	returning the number of elements read, there could be less elements available."

	0 to: requestedCount - 1 do: [ :count | | object |
		(object := self next) ifNil: [ ^ count ].  
		collection at: offset + count put: object ].
	^ requestedCount
%

category: 'positioning'
method: ZnPositionableReadStream
savingPositionDo: block
	"Execute block so that any reading from me in it has no effect afterwards. I remember the current #position and move back to it using #position: after evaluating block. My buffer size limits how long the excursion can be. A SubscriptOutOfBounds exception will be signalled in case this operation cannot be completed."
	
	| savedPosition |
	savedPosition := self position.
	^ block ensure: [ self position: savedPosition ]
%

category: 'initialize-release'
method: ZnPositionableReadStream
sizeBuffer: size
	"Change the buffer size. This should be done when I am still in my initial state."
	
	buffer := self collectionSpecies new: size
%

category: 'accessing'
method: ZnPositionableReadStream
skip: integer
	"Skip over integer count elements."
	
	integer > 0
		ifTrue: [ integer timesRepeat: [ self next ] ]
		ifFalse: [ self position: (self position + integer) ]
%

category: 'accessing'
method: ZnPositionableReadStream
upTo: value 
	"Read upto but not including value and return them as a collection.
	If value is not found, return the entire contents of the stream."
	
	^ self collectionSpecies 
		streamContents: [ :writeStream | | element |
			[ self atEnd or: [ (element := self next) = value ] ] whileFalse: [ 
				writeStream nextPut: element ] ]
%

category: 'accessing'
method: ZnPositionableReadStream
upToEnd
	"Read elements until the stream is atEnd and return them as a collection."

	^ self collectionSpecies
		streamContents: [ :collectionStream | 
			[ self atEnd ] whileFalse: [ collectionStream nextPut: self next ] ]
%

category: 'accessing'
method: ZnPositionableReadStream
wrappedStream
	"Return the read stream that I wrap."
	
	^ stream
%

! Class implementation for 'AbstractBinaryFileStream'

!		Class methods for 'AbstractBinaryFileStream'

category: 'instance creation'
classmethod: AbstractBinaryFileStream
on: aFileAdapter
	
	^ self new
		terminal: aFileAdapter;
		yourself
%

!		Instance methods for 'AbstractBinaryFileStream'

category: 'testing'
method: AbstractBinaryFileStream
atEnd

	^ self terminal atEnd
%

category: 'reading'
method: AbstractBinaryFileStream
contents
	"Answer the contents of the receiver while leaving the position unchanged.
	Fail if the receiver doesn't support positioning.
	#upToEnd provides an alternative that doesn't rely on stream positioning."

	| savedPosition contents |

	savedPosition := self position.
	self position: 0.
	contents := self upToEnd.
	self position: savedPosition.
	^contents
%

category: 'character writing'
method: AbstractBinaryFileStream
cr
	
	self nextPut: Character cr asInteger
%

category: 'character writing'
method: AbstractBinaryFileStream
crlf
	
	self nextPutAll: String crlf
%

category: 'accessing'
method: AbstractBinaryFileStream
fileName
	^self terminal fullName
%

category: 'flushing'
method: AbstractBinaryFileStream
flush
	self terminal flush
%

category: 'testing'
method: AbstractBinaryFileStream
isBinary
	^ true
%

category: 'testing'
method: AbstractBinaryFileStream
isReadStream
	^self isWritable not
%

category: 'testing'
method: AbstractBinaryFileStream
isWritable
	^self terminal  isNil ifTrue: [false] ifFalse: [self terminal isWritable]
%

category: 'testing'
method: AbstractBinaryFileStream
isWriteStream
	^self isWritable
%

category: 'character writing'
method: AbstractBinaryFileStream
lf

	self nextPut: Character lf asInteger
%

category: 'reading'
method: AbstractBinaryFileStream
next
	"Answer the next byte from this file, or nil if at the end of the file."

	^ (self next: 1) ifEmpty: [ nil ] ifNotEmpty: [ :col | col first ]
%

category: 'reading'
method: AbstractBinaryFileStream
next: n
	"Return a string with the next n characters of the filestream in it."

	^ self next: n into: (ByteArray new: n)
%

category: 'reading'
method: AbstractBinaryFileStream
next: n into: aBuffer
	"Return a string with the next n characters of the filestream in it."
	| readBuffer read |
	readBuffer := aBuffer.
	read := self terminal readInto: readBuffer startingAt: 1 count: n.
	^read = n 
		ifTrue: [ readBuffer ]
		ifFalse: [ readBuffer copyFrom: 1 to: read ]
%

category: 'reading'
method: AbstractBinaryFileStream
next: n into: aString startingAt: startIndex
	"Read n bytes into the given string.
	Return aString or a partial copy if less than
	n elements have been read."
	|read|
	read := (self readInto: aString startingAt: startIndex count: n).
	^read = n 
		ifTrue: [ aString ]
		ifFalse: [ aString copyFrom: 1 to: startIndex + read - 1 ]	
%

category: 'writing'
method: AbstractBinaryFileStream
next: amount putAll: aByteArray

	self isWritable
		ifFalse: [ ^ FileWriteError signalWith: 'Cannot write a read-only file - ' , self terminal fullName ].
	self terminal
		writeFrom: aByteArray
		startingAt: 1
		count: amount.
	^ aByteArray
%

category: 'reading'
method: AbstractBinaryFileStream
nextInto: aBuffer
	"Return a string with the next n characters of the filestream in it."

	^ self next: aBuffer size into: aBuffer
%

category: 'endianess'
method: AbstractBinaryFileStream
nextLittleEndianNumber: n 
	"Answer the next n bytes as a positive Integer or LargePositiveInteger, where the bytes are ordered from least significant to most significant."

	| bytes s |
	bytes := self next: n.
	s := 0.
	n to: 1 by: -1 do: [:i | s := (s bitShift: 8) bitOr: (bytes at: i)].
	^ s
%

category: 'endianess'
method: AbstractBinaryFileStream
nextLittleEndianNumber: n put: value
	"Answer the next n bytes as a positive Integer or LargePositiveInteger, where the bytes are ordered from least significant to most significant."
	| bytes |
	bytes := ByteArray new: n.
	1 to: n do: [:i | bytes at: i put: (value digitAt: i)].
	self nextPutAll: bytes
%

category: 'writing'
method: AbstractBinaryFileStream
nextPut: anInteger

	^ self nextPutAll: (ByteArray with: anInteger asInteger)
%

category: 'writing'
method: AbstractBinaryFileStream
nextPutAll: aByteArray
	self next: aByteArray basicSize putAll: aByteArray
%

category: 'accessing'
method: AbstractBinaryFileStream
pathString

	^self terminal pathString
%

category: 'reading'
method: AbstractBinaryFileStream
peek
	"Answer what would be returned if the message next were sent to the receiver. If the receiver is at the end, answer nil.  "
	self subclassResponsibility
%

category: 'positioning'
method: AbstractBinaryFileStream
position

	^ self terminal position
%

category: 'positioning'
method: AbstractBinaryFileStream
position: aPosition
	
	self terminal position: aPosition
%

category: 'printing'
method: AbstractBinaryFileStream
printOn: aStream
	"Put a printed version of the receiver onto aStream."

	aStream
		nextPutAll: self class name;
		nextPutAll: ': ';
		print: self fileName
%

category: 'reading'
method: AbstractBinaryFileStream
readInto: readBuffer startingAt: startIndex count: count

	^self terminal readInto: readBuffer startingAt: startIndex count: count
%

category: 'positioning'
method: AbstractBinaryFileStream
reset
	self position: 0
%

category: 'positioning'
method: AbstractBinaryFileStream
setToEnd
	
	self position: self size
%

category: 'accessing'
method: AbstractBinaryFileStream
size

	^ self terminal fileSize
%

category: 'positioning'
method: AbstractBinaryFileStream
skip: n
	"Set the character position to n characters from the current position.
	Error if not enough characters left in the file.
	By default we read n characters and we avoid reading the output"
	self next: n
%

category: 'flushing'
method: AbstractBinaryFileStream
sync	

	self terminal sync
%

category: 'accessing'
method: AbstractBinaryFileStream
terminal
	^ terminal
%

category: 'accessing'
method: AbstractBinaryFileStream
terminal: aFileAdaptor
	terminal := aFileAdaptor
%

category: 'reading'
method: AbstractBinaryFileStream
upTo: delim

	^ self upToAnyOf: (ByteArray with: delim)
%

category: 'reading'
method: AbstractBinaryFileStream
upToAnyOf: delimiters

	^ ByteArray new: 1000 streamContents: [ :stream | | ch |
		[ (ch := self next) isNil or: [ delimiters includes: ch] ] 
			whileFalse: [ stream nextPut: ch ] ]
%

category: 'reading'
method: AbstractBinaryFileStream
upToEnd
	"Answer a subcollection from the current access position through the last element of the receiver."

	^ByteArray streamContents: [ :newStream |
		| next |
		[ (next := self next) isNil ] whileFalse: [
			newStream nextPut: next ] ]
%

! Class implementation for 'BinaryFileStream'

!		Instance methods for 'BinaryFileStream'

category: 'open/close'
method: BinaryFileStream
close
	self terminal notNil ifTrue: [
		self terminal close.
		self terminal: nil]
%

category: 'testing'
method: BinaryFileStream
closed
	^ self terminal isNil or: [self isClosed]
%

category: 'finalization'
method: BinaryFileStream
finalize

	^ self close
%

category: 'reading'
method: BinaryFileStream
peek
	"Answer what would be returned if the message next were sent to the receiver. If the receiver is at the end, answer nil.  "
	| next |
	self atEnd ifTrue: [^ nil].
	next := self next.
	self position: self position - 1.
	^ next
%

category: 'positioning'
method: BinaryFileStream
skip: n
	"Set the character position to n characters from the current position."
	self position: self position + n
%

category: 'positioning'
method: BinaryFileStream
truncate

	self truncate: 0
%

category: 'positioning'
method: BinaryFileStream
truncate: pos
	"Truncate to this position"

	^ self terminal truncate: pos
%

! Class implementation for 'StdioStream'

!		Instance methods for 'StdioStream'

category: 'accessing'
method: StdioStream
next: n
	"Return a string with the next n characters of the filestream in it."
	| readBuffer read startingAt |
	readBuffer := ByteArray new: n.
	startingAt := 1.
	peekBuffer ifNotNil: [ 
		readBuffer at: 1 put: peekBuffer.
		startingAt := startingAt + 1.
		peekBuffer := nil ].
	read := self terminal readInto: readBuffer startingAt: startingAt count: n - startingAt + 1.
	^read = (n - startingAt + 1)
		ifTrue: [ readBuffer ]
		ifFalse: [ readBuffer copyFrom: 1 to: read ]
%

category: 'accessing'
method: StdioStream
peek
	"Answer the next element of the stream, but do not advance the stream pointer. 
	If the receiver is at the end, answer nil."

	self atEnd ifTrue: [ ^ nil ].
	peekBuffer ifNotNil: [ ^ peekBuffer ].
	^ peekBuffer := self next
%

! Class implementation for 'AbstractUUIDTest'

!		Class methods for 'AbstractUUIDTest'

category: 'Testing'
classmethod: AbstractUUIDTest
isAbstract
  "Override to true if a TestCase subclass is Abstract and should not have
	TestCase instances built from it"

  ^ self sunitName = #'AbstractUUIDTest'
%

!		Instance methods for 'AbstractUUIDTest'

category: 'running'
method: AbstractUUIDTest
generatorClass
  self subclassResponsibility
%

category: 'running'
method: AbstractUUIDTest
setUp
  super setUp.
  currentUuidGeneratorClass := UUID _generatorClass.
  UUID generatorClass: self generatorClass
%

category: 'running'
method: AbstractUUIDTest
tearDown
  super tearDown.
  UUID generatorClass: currentUuidGeneratorClass
%

category: 'tests'
method: AbstractUUIDTest
testComparison
  "self debug: #testComparison"

  | a b |
  a := UUID fromString: '0608b9dc-02e4-4dd0-9f8a-ea45160df641'.
  b := UUID fromString: 'e85ae7ba-3ca3-4bae-9f62-cc2ce51c525e'.
  self assert: a < b.
  self deny: a > b.
  a := UUID fromString: '0608b9dc-02e4-4dd0-9f8a-ea45160df641'.
  b := UUID fromString: '0608b9dc-02e4-4dd0-9f8a-ea45160df642'.
  self assert: a < b.
  a := UUID fromString: '0608b9dc-02e4-4dd0-9f8a-ea45160df641'.
  b := UUID fromString: '0608b9dc-02e4-4dd0-9f8a-ea45160df640'.
  self assert: a > b
%

category: 'tests'
method: AbstractUUIDTest
testComparisonA
  | a b |
  a := UUID fromString: '0608b9dc-02e4-4dd0-9f8a-ea45160df641'.
  b := UUID fromString: 'e85ae7ba-3ca3-4bae-9f62-cc2ce51c525e'.
  self assert: a = a copy.
  self assert: b = b copy.
  self assert: a < b.
  self assert: b > a.
  self deny: a > b = (b > a)
%

category: 'tests'
method: AbstractUUIDTest
testCreation
  | uuid |
  uuid := UUID new.
  self should: [ uuid size = 16 ].
  self shouldnt: [ uuid isNilUUID ].
  self should: [ uuid asString size = 36 ]
%

category: 'tests'
method: AbstractUUIDTest
testCreationEquality
  | uuid1 uuid2 |
  uuid1 := UUID new.
  uuid2 := UUID new.
  self should: [ uuid1 = uuid1 ].
  self should: [ uuid2 = uuid2 ].
  self shouldnt: [ uuid1 = uuid2 ].
  self shouldnt: [ uuid1 hash = uuid2 hash ]
%

category: 'tests'
method: AbstractUUIDTest
testCreationFromString
  | uuid string |
  string := UUID nilUUID asString.
  uuid := UUID fromString: string.
  self should: [ uuid size = 16 ].
  self should: [ uuid = UUID nilUUID ].
  self should: [ uuid isNilUUID ].
  self should: [ uuid asString size = 36 ].
  self should: [ uuid asArray asSet size = 1 ].
  self should: [ (uuid asArray asSet asArray at: 1) = 0 ]
%

category: 'tests'
method: AbstractUUIDTest
testCreationFromStringNotNil
  | uuid string |
  string := UUID new asString.
  uuid := UUID fromString: string.
  self should: [ uuid size = 16 ].
  self should: [ uuid asString size = 36 ]
%

category: 'tests'
method: AbstractUUIDTest
testCreationNil
  | uuid |
  uuid := UUID nilUUID.
  self should: [ uuid size = 16 ].
  self should: [ uuid isNilUUID ].
  self should: [ uuid asString size = 36 ].
  self should: [ uuid asArray asSet size = 1 ].
  self should: [ (uuid asArray asSet asArray at: 1) = 0 ]
%

category: 'tests'
method: AbstractUUIDTest
testCreationNodeBased
  (UUID new asString copyLast: 12) = (UUID new asString copyLast: 12)
    ifFalse: [ ^ self ].
  1000
    timesRepeat: [ 
      | uuid |
      uuid := UUID new.
      self should: [ ((uuid at: 7) bitAnd: 16rF0) = 16r10 ].
      self should: [ ((uuid at: 9) bitAnd: 16rC0) = 16r80 ] ]
%

category: 'tests'
method: AbstractUUIDTest
testDuplicationsKinda
  | check size |
  size := 5000.
  check := Set new: size.
  size
    timesRepeat: [ 
      | uuid |
      uuid := UUID new.
      self shouldnt: [ check includes: uuid ].
      check add: uuid ]
%

category: 'tests'
method: AbstractUUIDTest
testOrder
  100
    timesRepeat: [ 
      | uuid1 uuid2 |
      uuid1 := UUID new.
      uuid2 := UUID new.
      (uuid1 asString copyLast: 12) = (uuid2 asString copyLast: 12)
        ifTrue: [ 
          self should: [ uuid1 < uuid2 ].
          self should: [ uuid2 > uuid1 ].
          self shouldnt: [ uuid1 = uuid2 ] ] ]
%

category: 'tests'
method: AbstractUUIDTest
testUniqueness
  "Test uniqueness for a given number of generated values"

  | maxTestValue |
  maxTestValue := 1000.
  self
    assert:
      ((1 to: maxTestValue) collect: [ :i | UUID new asString ]) asSet size
        = maxTestValue
%

! Class implementation for 'FastUUIDTest'

!		Instance methods for 'FastUUIDTest'

category: 'running'
method: FastUUIDTest
generatorClass
  ^ FastUUIDGenerator
%

! Class implementation for 'BinaryFileStreamTest'

!		Instance methods for 'BinaryFileStreamTest'

category: 'running'
method: BinaryFileStreamTest
fileStreamForFileNamed: fileName
	^ (FileSystem * 'testFile') writeRawStream
%

category: 'running'
method: BinaryFileStreamTest
killTestFile

	(FileSystem * 'testFile') exists
		ifTrue: [ (FileSystem * 'testFile') delete ].
%

category: 'running'
method: BinaryFileStreamTest
readStreamOnTestFile
	^ (FileSystem * 'testFile') readRawStream
%

category: 'running'
method: BinaryFileStreamTest
setUp

	super setUp.
	self killTestFile.
%

category: 'running'
method: BinaryFileStreamTest
tearDown

	self killTestFile.
	"We must ensure that files are collected before running other tests.
	In windows, we cannot open the same file twice."

	"3 timesRepeat: [ Smalltalk garbageCollect ]."

	super tearDown.
%

category: 'tests'
method: BinaryFileStreamTest
testEmptyFileIsAtEnd
	
	| aBinaryFileStream |
	aBinaryFileStream := self writeStreamOnTestFile.
	self assert: aBinaryFileStream atEnd
%

category: 'tests'
method: BinaryFileStreamTest
testFileWithSomeBytesSizeIsNotZero
	
	| aBinaryFileStream |
	aBinaryFileStream := self writeStreamOnTestFile.
	aBinaryFileStream nextPut: 1.
	self assert: aBinaryFileStream position equals: 1.
	aBinaryFileStream nextPutAll: #[1 2 3].
	self assert: aBinaryFileStream position equals: 4.
	aBinaryFileStream nextPut: 1.
	self assert: aBinaryFileStream position equals: 5.
%

category: 'tests'
method: BinaryFileStreamTest
testFullFileIsAtEnd
	
	| aBinaryFileStream |
	aBinaryFileStream := self writeStreamOnTestFile.
	aBinaryFileStream nextPut: 1.
	self assert: aBinaryFileStream atEnd.
%

category: 'tests'
method: BinaryFileStreamTest
testOpenFile
	
	| aBinaryFileStream |
	aBinaryFileStream := self writeStreamOnTestFile.
	
	self assert: aBinaryFileStream terminal notNil
%

category: 'tests'
method: BinaryFileStreamTest
testPeekDoesNotAdvanceTheStream
	
	| aBinaryFileStream |
	aBinaryFileStream := self writeStreamOnTestFile.
	aBinaryFileStream nextPut: 1.
	aBinaryFileStream nextPut: 2.
	aBinaryFileStream nextPut: 3.
	aBinaryFileStream close.
	
	aBinaryFileStream := self writeStreamOnTestFile.
	self assert: aBinaryFileStream position equals: 0.
	self assert: aBinaryFileStream peek equals: aBinaryFileStream peek.
	self assert: aBinaryFileStream position equals: 0.
%

category: 'tests'
method: BinaryFileStreamTest
testReadFullFileIsAtEnd
	
	| aBinaryFileStream |
	aBinaryFileStream := self writeStreamOnTestFile.
	aBinaryFileStream nextPut: 1.
	aBinaryFileStream close.
	
	aBinaryFileStream := self readStreamOnTestFile.
	aBinaryFileStream next.
	self assert: aBinaryFileStream atEnd.
%

category: 'tests'
method: BinaryFileStreamTest
testReadLessThanAvailableYieldsJustRead
	
	| aBinaryFileStream |
	aBinaryFileStream := self fileStreamForFileNamed: 'testFile'.
	aBinaryFileStream nextPut: 1.
	aBinaryFileStream nextPut: 2.
	aBinaryFileStream nextPut: 3.
	aBinaryFileStream close.
	
	aBinaryFileStream := self readStreamOnTestFile.
	self assert: (aBinaryFileStream next:2) equals: #[1 2].
%

category: 'tests'
method: BinaryFileStreamTest
testReadMoreThanAvailableYieldsOnlyAvailable
	
	| aBinaryFileStream |
	aBinaryFileStream := self writeStreamOnTestFile.
	self assert: (aBinaryFileStream next:2) equals: #[].
	
	"then we put one element and we close it"
	aBinaryFileStream nextPut: 1.
	aBinaryFileStream close.
	
	aBinaryFileStream := self readStreamOnTestFile.
	self assert: (aBinaryFileStream next:2) equals: #[1].
%

category: 'tests'
method: BinaryFileStreamTest
testReadMultipleBytes
	
	| aBinaryFileStream |
	aBinaryFileStream := self writeStreamOnTestFile.
	aBinaryFileStream nextPut: 1.
	aBinaryFileStream nextPut: 2.
	aBinaryFileStream close.

	aBinaryFileStream := self readStreamOnTestFile.
	self assert: (aBinaryFileStream next: 2) equals: #[1 2].
%

category: 'tests'
method: BinaryFileStreamTest
testReadWhenNothingAvailableYieldsNil
	
	| aBinaryFileStream |
	aBinaryFileStream := self writeStreamOnTestFile.
	self assert: aBinaryFileStream next equals: nil.
%

category: 'tests'
method: BinaryFileStreamTest
testSkipLecture
	
	| aBinaryFileStream |
	aBinaryFileStream := self writeStreamOnTestFile.
	aBinaryFileStream nextPut: 1.
	aBinaryFileStream nextPut: 2.
	aBinaryFileStream nextPut: 3.
	aBinaryFileStream close.
	
	aBinaryFileStream := self readStreamOnTestFile.
	aBinaryFileStream skip: 2.
	self assert: aBinaryFileStream next equals: 3.
%

category: 'tests'
method: BinaryFileStreamTest
testWriteMultipleBytes
	
	| aBinaryFileStream |
	aBinaryFileStream := self writeStreamOnTestFile.
	aBinaryFileStream nextPutAll: #[1 2].
	aBinaryFileStream close.

	aBinaryFileStream := self readStreamOnTestFile.
	self assert: aBinaryFileStream next equals: 1.
	self assert: aBinaryFileStream next equals: 2.
%

category: 'tests'
method: BinaryFileStreamTest
testWriteReadInt
	
	| aBinaryFileStream |
	aBinaryFileStream := self writeStreamOnTestFile.
	aBinaryFileStream nextPut: 1.
	aBinaryFileStream close.

	aBinaryFileStream := self readStreamOnTestFile.
	self assert: aBinaryFileStream next equals: 1.
%

category: 'tests'
method: BinaryFileStreamTest
testWriteToClosedFileFails
	
	| aBinaryFileStream |
	aBinaryFileStream := self writeStreamOnTestFile.
	aBinaryFileStream close.
	self should: [ aBinaryFileStream wrappedStream nextPut: 1 ] raise: Error.
%

category: 'running'
method: BinaryFileStreamTest
writeStreamOnTestFile
	^ (FileSystem * 'testFile') writeRawStream
%

! Class implementation for 'BinaryStreamSpecTest'

!		Instance methods for 'BinaryStreamSpecTest'

category: 'running'
method: BinaryStreamSpecTest
setUp
	super setUp.
	reference := (FileLocator temp  / 'plonk') resolve.
%

category: 'running'
method: BinaryStreamSpecTest
tearDown
	super tearDown.
	reference ensureDelete
%

category: 'tests'
method: BinaryStreamSpecTest
testReadCreation
	| aSpec |
	reference createFile.
	aSpec := BinaryStreamSpec newRead on: reference.
	self assert: aSpec terminal class name = FileSystem fileClass name.
	self assert: aSpec terminal options mode equals: 'r'
%

! Class implementation for 'BufferedStreamSpecTest'

!		Instance methods for 'BufferedStreamSpecTest'

category: 'running'
method: BufferedStreamSpecTest
setUp
	super setUp.
	reference := (FileLocator temp  / 'plonk') resolve.
%

category: 'running'
method: BufferedStreamSpecTest
tearDown
	super tearDown.
	reference ensureDelete
%

category: 'tests'
method: BufferedStreamSpecTest
testReadCreation
	| aSpec |
	reference createFile.
	aSpec := BufferedStreamSpec newRead on: reference.
	self assert: aSpec class name = #ZnBufferedReadStream.
	self assert: aSpec wrappedStream class name = #BinaryFileStream.
	self assert: aSpec wrappedStream terminal options mode equals: 'r'
%

category: 'tests'
method: BufferedStreamSpecTest
testReadWrite
	| aSpec |
	reference createFile.
	aSpec := BufferedStreamSpec newReadWrite on: reference.
	self assert: aSpec class name = #ZnBufferedReadWriteStream.
	self assert: aSpec wrappedStream class name = #BinaryFileStream.
	self assert: aSpec wrappedStream terminal options mode equals: 'r+'
%

category: 'tests'
method: BufferedStreamSpecTest
testWriteCreation
	| aSpec |
	aSpec := BufferedStreamSpec newWrite on: reference.
	self assert: aSpec class name = #ZnBufferedWriteStream.
	self assert: aSpec wrappedStream class name = #BinaryFileStream.
	self assert: aSpec wrappedStream terminal options mode equals: 'w'
%

! Class implementation for 'DirectoryEntryTest'

!		Instance methods for 'DirectoryEntryTest'

category: 'accessing'
method: DirectoryEntryTest
entry
	System sessionIsOnStoneHost ifFalse: [ self error: 'DirectoryEntryTest needs to be run on the stone''s host' ].
	^ FileLocator extent1 resolve entry
%

category: 'tests'
method: DirectoryEntryTest
testCreationTimeIsADateAndTimeInstance
	"While creation is the message sent to a directory entry, creation returns a DateAndTime object"

	| creation |
	creation := self entry creation.
	self assert: creation class equals: DateAndTime
	
%

category: 'tests'
method: DirectoryEntryTest
testIsDirectory
	| ref entry |
	ref := FileLocator imageDirectory resolve.
	entry := ref entry.
	self assert: entry isDirectory.
	self assert: FileLocator imageDirectory resolve equals: FileLocator dbfScratchDir resolve
%

category: 'tests'
method: DirectoryEntryTest
testIsFile
	self assert: self entry isFile.
	self deny: self entry isDirectory
%

category: 'tests'
method: DirectoryEntryTest
testIsNotDirectory
	self deny: self entry isDirectory
%

category: 'tests'
method: DirectoryEntryTest
testIsNotFile
	| ref |
	ref := FileLocator imageDirectory resolve.
	self deny: ref entry isFile
%

category: 'tests'
method: DirectoryEntryTest
testModificationTimeIsADateAndTimeInstance
	"While modification is the message sent to a directory entry, modification returns a DateAndTime object"

	| modification |
	modification := self entry modification.
	self assert: modification class equals: DateAndTime
	
%

category: 'tests'
method: DirectoryEntryTest
testReference
	| ref entry |
	ref := FileLocator image resolve.
	entry := ref entry.
	self assert: entry reference equals: ref
%

category: 'tests'
method: DirectoryEntryTest
testSize
	self assert: self entry size _isInteger
%

! Class implementation for 'EncodedBufferedStreamSpecTest'

!		Instance methods for 'EncodedBufferedStreamSpecTest'

category: 'running'
method: EncodedBufferedStreamSpecTest
setUp
	super setUp.
	reference := (FileLocator temp  / 'plonk') resolve.
%

category: 'running'
method: EncodedBufferedStreamSpecTest
tearDown
	super tearDown.
	reference ensureDelete
%

category: 'tests'
method: EncodedBufferedStreamSpecTest
testAppend

	| aStreamSpec |
	aStreamSpec := EncodedBufferedStreamSpec newAppend textFileType.
	self assert: aStreamSpec mode = 'a'
%

category: 'tests'
method: EncodedBufferedStreamSpecTest
testAppendAndRead

	| aStreamSpec |
	aStreamSpec := EncodedBufferedStreamSpec newAppend read.
	self assert: aStreamSpec mode = 'a+'
%

category: 'tests'
method: EncodedBufferedStreamSpecTest
testAppendAndReadBinary

	| aStreamSpec |
	aStreamSpec := EncodedBufferedStreamSpec newAppend read binaryFileType.
	self assert: (aStreamSpec fileType isKindOf: FileBinaryTypeFlag).
	self assert: aStreamSpec mode = 'a+b'
%

category: 'tests'
method: EncodedBufferedStreamSpecTest
testAppendBinary

	| aStreamSpec |
	aStreamSpec := EncodedBufferedStreamSpec newAppend binaryFileType.
	self assert: aStreamSpec mode = 'ab'
%

category: 'tests'
method: EncodedBufferedStreamSpecTest
testCreation

	| aStreamSpec |
	aStreamSpec := EncodedBufferedStreamSpec newAppend.
	self assert: (aStreamSpec fileOptions isKindOf: FileOptions).
	self assert: aStreamSpec fileOptions parent == aStreamSpec
%

category: 'tests'
method: EncodedBufferedStreamSpecTest
testOnDo
	| testString fileRef |
	testString := 'ße'.
	fileRef := FileReference newTempFilePrefix: 'FileReference' suffix: 'Test'.

	[  
		EncodedBufferedStreamSpec newWrite utf8 on: fileRef do: [ :stream | stream nextPutAll: testString ].
		EncodedBufferedStreamSpec newRead utf8 on: fileRef do: [: stream |
			self assert: (stream upToAll: 'e') equals: 'ß'] 
			] ensure: [ fileRef ensureDelete ].
%

category: 'tests'
method: EncodedBufferedStreamSpecTest
testOnDoIfAbsent
	| fileRef  result |

	fileRef := FileReference newTempFilePrefix: 'FileReference' suffix: 'Test'.
	fileRef createFile.

	[ 
		result := EncodedBufferedStreamSpec newRead 
						on: fileRef 
						do: [:stream | stream upToEnd ]
						ifAbsent: [ self signalFailure: 'Should not reach here.' ].
		self assert: result isEmpty]
		ensure: [ fileRef ensureDelete ].

	[ 
		result := EncodedBufferedStreamSpec newRead 
						on: fileRef 
						do: [:stream | stream upToEnd ]
						ifAbsent: ['Should reach here.' ].
		self assert: result equals: 'Should reach here.']
			ensure: [ fileRef ensureDelete ]
%

category: 'tests'
method: EncodedBufferedStreamSpecTest
testOnDoIfPresent
	| fileRef  result |

	fileRef := FileReference newTempFilePrefix: 'FileReference' suffix: 'Test'.
	fileRef createFile.

	[ 
		result := EncodedBufferedStreamSpec newWrite 
						on: fileRef 
						do: [:stream | 'Should not reach here' ]
						ifPresent: [ 'Should reach here' ].
		self assert: result equals: 'Should reach here' ]
		ensure: [ fileRef ensureDelete ].

	[ 
		result := EncodedBufferedStreamSpec newWrite 
						on: fileRef 
						do: [:stream | 'Should reach here' ]
						ifPresent: ['Should not reach here' ].
		self assert: result equals: 'Should reach here']
			ensure: [ fileRef ensureDelete ]
%

category: 'tests'
method: EncodedBufferedStreamSpecTest
testOnOptions
	"This tests two ways to create a stream from the spec where the FileOptions is a parameter"
	| fileRef  stream |

	fileRef := FileReference newTempFilePrefix: 'FileReference' suffix: 'Test'.

	stream := (EncodedStreamSpec onOptions: FileOptions newWrite) on: fileRef.
	stream nextPutAll: 'test data'.
	stream close.
	stream := (EncodedStreamSpec onOptions: FileOptions newRead) on: fileRef.
	self assert: stream upToEnd equals: 'test data'.

	stream := (EncodedStreamSpec on: fileRef options: FileOptions newWrite).
	stream nextPutAll: 'test data'.
	stream close.
	stream := (EncodedStreamSpec on: fileRef options: FileOptions newRead).
	self assert: stream upToEnd equals: 'test data'
%

category: 'tests'
method: EncodedBufferedStreamSpecTest
testRead

	| aStreamSpec |
	aStreamSpec := EncodedBufferedStreamSpec newRead.
	self assert: aStreamSpec mode = 'r'
%

category: 'tests'
method: EncodedBufferedStreamSpecTest
testReadCreation
	| aSpec |
	reference createFile.
	aSpec := EncodedBufferedStreamSpec newRead on: reference.
	self assert: aSpec encoder class name = #ZnUTF8Encoder.
	self assert: aSpec wrappedStream class name = #ZnBufferedReadStream.
	self assert: aSpec wrappedStream wrappedStream class name = #BinaryFileStream.
	self assert: aSpec wrappedStream wrappedStream terminal options mode equals: 'r'
%

category: 'tests'
method: EncodedBufferedStreamSpecTest
testReadWrite
	| aSpec |
	reference createFile.
	aSpec := EncodedBufferedStreamSpec newReadWrite on: reference.

	self assert: aSpec class name = #ZnCharacterReadWriteStream.
	"self assert: aSpec wrappedStream class name = #BinaryFileStream.
	self assert: aSpec wrappedStream file options mode equals: 'r+'."
%

category: 'tests'
method: EncodedBufferedStreamSpecTest
testReadWriteBinary

	| aStreamSpec |
	aStreamSpec := EncodedBufferedStreamSpec newReadWrite binaryFileType.
	self assert: (aStreamSpec fileType isKindOf: FileBinaryTypeFlag).
	self assert: aStreamSpec mode = 'r+b'
%

category: 'tests'
method: EncodedBufferedStreamSpecTest
testReadWriteTruncated

	| aStreamSpec |
	aStreamSpec := EncodedBufferedStreamSpec newReadWrite truncate.
	self assert: aStreamSpec mode = 'w+'
%

category: 'tests'
method: EncodedBufferedStreamSpecTest
testReadWriteTruncatedBinary

	| aStreamSpec |
	aStreamSpec := EncodedBufferedStreamSpec newReadWrite truncate binaryFileType.
	self assert: aStreamSpec mode = 'w+b'
%

category: 'tests'
method: EncodedBufferedStreamSpecTest
testWrite

	| aStreamSpec |
	aStreamSpec := EncodedBufferedStreamSpec newWrite.
	self assert: aStreamSpec mode = 'w'
%

category: 'tests'
method: EncodedBufferedStreamSpecTest
testWriteBinary

	| aStreamSpec |
	aStreamSpec := EncodedBufferedStreamSpec newWrite binaryFileType.
	self assert: (aStreamSpec fileType isKindOf: FileBinaryTypeFlag).
	self assert: aStreamSpec mode = 'wb'
%

category: 'tests'
method: EncodedBufferedStreamSpecTest
testWriteCreation
	| aSpec |
	aSpec := EncodedBufferedStreamSpec newWrite on: reference.
	self assert: aSpec encoder class name = #ZnUTF8Encoder.
	self assert: aSpec wrappedStream class name = #ZnBufferedWriteStream.
	self assert: aSpec wrappedStream wrappedStream class name = #BinaryFileStream.
	self assert: aSpec wrappedStream wrappedStream terminal options mode equals: 'w'
%

category: 'tests'
method: EncodedBufferedStreamSpecTest
testWriteHighZipped

	| aStreamSpec |
	aStreamSpec := EncodedBufferedStreamSpec newWrite gzipHighCompression.
	self assert: aStreamSpec isGzipped.
	self assert: aStreamSpec mode = 'w9'
%

! Class implementation for 'FileLocatorTest'

!		Instance methods for 'FileLocatorTest'

category: 'compatibility tests'
method: FileLocatorTest
testAsAbsolute
	locator := FileLocator image.
	self assert: locator asAbsolute = locator
%

category: 'compatibility tests'
method: FileLocatorTest
testBasename
	locator := FileLocator image / 'griffle'.
	self assert: locator basename = 'griffle'
%

category: 'compatibility tests'
method: FileLocatorTest
testCommaAddsExtension
	locator := FileLocator image / 'griffle'.
	self assert: (locator , 'plonk') basename = 'griffle.plonk'
%

category: 'compatibility tests'
method: FileLocatorTest
testCommaAddsExtensionAgain
	locator := FileLocator image / 'griffle.plonk'.
	self assert: (locator , 'nurp') basename = 'griffle.plonk.nurp'
%

category: 'compatibility tests'
method: FileLocatorTest
testContainsLocator
	locator := FileLocator image.
	self assert: (locator contains: locator / 'griffle').
%

category: 'compatibility tests'
method: FileLocatorTest
testContainsPath
	
	locator := FileLocator image.
	self assert: (locator contains: (locator resolve / 'griffle') path).
%

category: 'compatibility tests'
method: FileLocatorTest
testContainsReference
	locator := FileLocator image.
	self assert: (locator contains: (locator resolve / 'griffle')).
%

category: 'tests'
method: FileLocatorTest
testCPath
	| ref |
	ref := FileLocator C / 'WINDOWS'.
	self assert: (ref fileSystem isKindOf: FileSystem).
	self assert: ref path = (Path / 'C:' / 'WINDOWS')
%

category: 'compatibility tests'
method: FileLocatorTest
testEqual
	| a b |
	a := FileLocator image.
	b := FileLocator image.
	self deny: a == b.
	self assert: a = b.
%

category: 'compatibility tests'
method: FileLocatorTest
testExtension
	locator := FileLocator image, 'bak'.
	self assert: (locator basename endsWith: '.bak')	
%

category: 'compatibility tests'
method: FileLocatorTest
testFileSystem
	locator := FileLocator image.
	self assert: (locator fileSystem isKindOf: FileSystem)
%

category: 'resolution tests'
method: FileLocatorTest
testImageDirectory
	locator := FileLocator image.
	self assert: locator resolve equals: FileLocator image resolve
%

category: 'compatibility tests'
method: FileLocatorTest
testIsAbsolute
	locator := FileLocator image.
	self assert: locator isAbsolute
%

category: 'compatibility tests'
method: FileLocatorTest
testIsNotRoot
	locator := FileLocator image.
	self deny: locator isRoot
%

category: 'compatibility tests'
method: FileLocatorTest
testIsRelative
	locator := FileLocator image.
	self deny: locator isRelative
%

category: 'compatibility tests'
method: FileLocatorTest
testIsRoot
	locator := FileLocator image.
	(locator resolve path size) timesRepeat: [locator := locator / '..'].
	self assert: locator canonicalize isRoot
%

category: 'compatibility tests'
method: FileLocatorTest
testMoveTo
	| old new |
	[
		old := FileLocator imageDirectory / 'testMoveTo_old'.
		old ensureCreateFile.
		
		new := FileLocator home / 'testMoveTo_new'.
		old moveTo: new.
		
		self deny: old exists.
		self assert: new exists.
	] ensure: [ 
		old ensureDelete.
		new ensureDelete.
	]
%

category: 'compatibility tests'
method: FileLocatorTest
testOriginBasename
	locator := FileLocator image.
	self assert: locator basename = FileLocator image resolve basename
%

category: 'compatibility tests'
method: FileLocatorTest
testParent
	locator := FileLocator extent1.
	self assert: locator parent resolve = FileLocator extent1Directory resolve
%

category: 'resolution tests'
method: FileLocatorTest
testResolveAbsoluteReference
	| result reference |
	locator := FileLocator image / 'plonk'.
	reference := FileSystem memory / 'griffle'.
	result := locator resolve: reference.
	self assert: result == reference
%

category: 'resolution tests'
method: FileLocatorTest
testResolveCompoundString
	| result compound |
	locator := FileLocator image / 'plonk'.
	compound := 'griffle', locator fileSystem delimiter asString, 'nurp'.
	result := locator resolve: compound.
	self assert: result class = locator class.
	self assert: result origin = locator origin.
	self assert: result path = ((Path * 'plonk') / 'griffle' / 'nurp')
%

category: 'resolution tests'
method: FileLocatorTest
testResolvePath
	| result path |
	locator := FileLocator image / 'plonk'.
	result := locator resolve: (Path * 'griffle').
	path := (Path * 'plonk') / 'griffle'.
	self assert: result class = locator class.
	self assert: result origin = locator origin.
	self assert: result path = path.
%

category: 'resolution tests'
method: FileLocatorTest
testResolveRelativeReference
	| result reference |
	locator := FileLocator image / 'plonk'.
	self flag: 'this is a bit weird...'.
	reference := FileSystem memory * 'griffle'.
	result := locator resolve: reference.
	self assert: result class equals: locator class.
	self assert: result origin equals: locator origin.
	self assert: result path equals: reference path
%

category: 'resolution tests'
method: FileLocatorTest
testResolveString
	| result path |
	locator := FileLocator image / 'plonk'.
	result := locator resolve: 'griffle'.
	path := (Path * 'plonk') / 'griffle'.
	self assert: result class = locator class.
	self assert: result origin = locator origin.
	self assert: result path = path.
%

category: 'compatibility tests'
method: FileLocatorTest
testSlash
	locator := FileLocator image / 'griffle'.
	self assert: locator = (FileLocator image / 'griffle')
%

category: 'compatibility tests'
method: FileLocatorTest
testWithExtensionAddsExtension
	locator := FileLocator image / 'griffle'.
	self assert: (locator withExtension: 'plonk') basename = 'griffle.plonk'
%

category: 'compatibility tests'
method: FileLocatorTest
testWithExtensionReplacesExtension
	locator := FileLocator image / 'griffle.nurp'.
	self assert: (locator withExtension: 'plonk') basename = 'griffle.plonk'
%

! Class implementation for 'FileOptionsTest'

!		Class methods for 'FileOptionsTest'

category: 'Testing'
classmethod: FileOptionsTest
isAbstract
	"Override to true if a TestCase subclass is Abstract and should not have
	TestCase instances built from it"

	^self sunitName = #FileOptionsTest
%

!		Instance methods for 'FileOptionsTest'

category: 'private'
method: FileOptionsTest
fileOptions
	| aFileOptions |
	aFileOptions := EncodedBufferedStreamSpec newRead fileOptions.
	aFileOptions parent store: self store.
	^aFileOptions
%

category: 'private'
method: FileOptionsTest
groupAllOctalValue
	^'70'
%

category: 'private'
method: FileOptionsTest
otherAllOctalValue
	^'7'
%

category: 'private'
method: FileOptionsTest
ownerAllOctalValue
	^'700'
%

category: 'private'
method: FileOptionsTest
store
	^self storeClass createDefault
%

category: 'private'
method: FileOptionsTest
storeClass
	self subclassResponsibity
%

category: 'tests'
method: FileOptionsTest
testAppendFlag

	| aFileOptions |
	aFileOptions := EncodedBufferedStreamSpec newAppend fileOptions.
	aFileOptions parent store: self store.
	self assert: aFileOptions appendFlag posixFlagValue equals: (FileAppendFlag perform: self valueAccessor)
%

category: 'tests'
method: FileOptionsTest
testGroupAccessPermissionFlags
	self _testPermissionFlag: FileGroupAllPermissionFlag in: self fileOptions groupAll.
	self _testPermissionFlag: FileGroupAllPermissionFlag in: self fileOptions groupAll groupRead.
	self _testPermissionFlag: FileGroupAllPermissionFlag in: self fileOptions groupRead groupAll.
	self _testPermissionFlag: FileGroupAllPermissionFlag in: self fileOptions groupRead groupWrite groupExecute.
	self _testPermissionFlag: FileGroupAllPermissionFlag in: self fileOptions groupRead groupWrite groupExecute groupAll.
	self assert: (self fileOptions groupRead groupWrite groupExecute accessPermissions printStringRadix: 8) equals: self groupAllOctalValue.
%

category: 'tests'
method: FileOptionsTest
testIndividualAccessPermissionFlags

	self assert: (self fileOptions groupAll accessPermissions printStringRadix: 8) equals: self groupAllOctalValue.
	self _testPermissionFlag: FileGroupAllPermissionFlag in: self fileOptions groupAll.
	self _testPermissionFlag: FileGroupExecutePermissionFlag in: self fileOptions groupExecute.
	self _testPermissionFlag: FileGroupReadPermissionFlag in: self fileOptions groupRead.
	self _testPermissionFlag: FileGroupWritePermissionFlag in: self fileOptions groupWrite.

	self assert: (self fileOptions otherAll accessPermissions printStringRadix: 8) equals: self otherAllOctalValue.
	self _testPermissionFlag: FileOtherAllPermissionFlag in: self fileOptions otherAll.
	self _testPermissionFlag: FileOtherExecutePermissionFlag in: self fileOptions otherExecute.
	self _testPermissionFlag: FileOtherReadPermissionFlag in: self fileOptions otherRead.
	self _testPermissionFlag: FileOtherWritePermissionFlag in: self fileOptions otherWrite.

	self assert: (self fileOptions ownerAll accessPermissions printStringRadix: 8) equals: self ownerAllOctalValue.
	self _testPermissionFlag: FileOwnerAllPermissionFlag in: self fileOptions ownerAll.
	self _testPermissionFlag: FileOwnerExecutePermissionFlag in: self fileOptions ownerExecute.
	self _testPermissionFlag: FileOwnerReadPermissionFlag in: self fileOptions ownerRead.
	self _testPermissionFlag: FileOwnerWritePermissionFlag in: self fileOptions ownerWrite.
%

category: 'tests'
method: FileOptionsTest
testOpenAndAppendAndReadMode

	| aFileOptions |
	aFileOptions := EncodedBufferedStreamSpec newAppend read fileOptions.
	aFileOptions parent store: self store.
	self assert: aFileOptions isWritable.
	self assert: aFileOptions isReadable.
	self assert: aFileOptions posixFlags size equals: 2.
	self assert: aFileOptions hasFileCreateFlag.
	self assert: aFileOptions hasFileAppendFlag.
	self deny: aFileOptions hasFileTruncateFlag.
	self assert: aFileOptions mode equals: 'a+'.
	self assert: aFileOptions fileOpenModeValue equals: 2.
	self assert: aFileOptions fileOpenFlagsTotalValue equals: self openAndAppendAndReadValue
%

category: 'tests'
method: FileOptionsTest
testOpenAppendMode

	| aFileOptions |
	aFileOptions := EncodedBufferedStreamSpec newAppend fileOptions.
	aFileOptions parent store: self store.
	self assert: aFileOptions isWritable.
	self deny: aFileOptions isReadable.
	self assert: aFileOptions posixFlags size equals: 2.
	self assert: aFileOptions hasFileCreateFlag.
	self assert: aFileOptions hasFileAppendFlag.
	self assert: aFileOptions mode equals: 'a'.
	self assert: aFileOptions fileOpenModeValue equals: 1.
	self assert: aFileOptions fileOpenFlagsTotalValue equals: self openAndAppendValue
%

category: 'tests'
method: FileOptionsTest
testOtherAccessPermissionFlags
	self _testPermissionFlag: FileOtherAllPermissionFlag in: self fileOptions otherAll.
	self _testPermissionFlag: FileOtherAllPermissionFlag in: self fileOptions otherAll otherRead.
	self _testPermissionFlag: FileOtherAllPermissionFlag in: self fileOptions otherRead otherAll.
	self _testPermissionFlag: FileOtherAllPermissionFlag in: self fileOptions otherRead otherWrite otherExecute.
	self _testPermissionFlag: FileOtherAllPermissionFlag in: self fileOptions otherRead otherWrite otherExecute otherAll.
	self assert: (self fileOptions otherRead otherWrite otherExecute accessPermissions printStringRadix: 8) equals: self otherAllOctalValue.
%

category: 'tests'
method: FileOptionsTest
testOwnerAccessPermissionFlags
	self _testPermissionFlag: FileOwnerAllPermissionFlag in: self fileOptions ownerAll.
	self _testPermissionFlag: FileOwnerAllPermissionFlag in: self fileOptions ownerAll ownerRead.
	self _testPermissionFlag: FileOwnerAllPermissionFlag in: self fileOptions ownerRead ownerAll.
	self _testPermissionFlag: FileOwnerAllPermissionFlag in: self fileOptions ownerRead ownerWrite ownerExecute.
	self _testPermissionFlag: FileOwnerAllPermissionFlag in: self fileOptions ownerRead ownerWrite ownerExecute ownerAll.
	self assert: (self fileOptions ownerRead ownerWrite ownerExecute accessPermissions printStringRadix: 8) equals: self ownerAllOctalValue
%

category: 'tests'
method: FileOptionsTest
testReadOpenMode

	| aFileOptions |
	aFileOptions := EncodedBufferedStreamSpec newRead fileOptions.
	aFileOptions parent store: self store.
	self deny: aFileOptions isWritable.
	self assert: aFileOptions isReadable.
	self assert: aFileOptions posixFlags isEmpty.
	self deny: aFileOptions hasFileCreateFlag.
	self deny: aFileOptions hasFileTruncateFlag.
	self assert: aFileOptions mode equals: 'r'.
	self assert: aFileOptions fileOpenModeValue equals: 0.
	self assert: aFileOptions fileOpenFlagsTotalValue equals: 0
%

category: 'tests'
method: FileOptionsTest
testReadWriteOpenMode

	| aFileOptions|
	aFileOptions := EncodedBufferedStreamSpec newReadWrite fileOptions.
	aFileOptions parent store: self store.
	self assert: aFileOptions isWritable.
	self assert: aFileOptions isReadable.
	self deny: aFileOptions hasFileCreateFlag.
	self deny: aFileOptions hasFileTruncateFlag.
	self assert: aFileOptions mode equals: 'r+'.
	self assert: aFileOptions fileOpenModeValue equals: 2.
	self assert: aFileOptions fileOpenFlagsTotalValue equals: self openReadWriteValue
%

category: 'tests'
method: FileOptionsTest
testReadWriteTruncateOpenMode

	| aFileOptions |
	aFileOptions := EncodedBufferedStreamSpec newReadWrite truncate create fileOptions.
	aFileOptions parent store: self store.
	self assert: aFileOptions isWritable.
	self assert: aFileOptions isReadable.
	self assert: aFileOptions posixFlags size equals: 2.
	self assert: aFileOptions hasFileCreateFlag.
	self assert: aFileOptions hasFileTruncateFlag.
	self assert: aFileOptions mode equals: 'w+'.
	self assert: aFileOptions fileOpenModeValue equals: 2.
	self assert: aFileOptions fileOpenFlagsTotalValue equals: self openReadWriteTruncateValue
%

category: 'tests'
method: FileOptionsTest
testShareFlags

	| aFileOptions |
	aFileOptions := EncodedBufferedStreamSpec newAppend fileOptions.
	aFileOptions parent store: self store.
	self assert: aFileOptions share posixFlagValue equals: (FileDenyNoneFlag perform: self valueAccessor).
	aFileOptions beReadOnlyShared.
	self assert: aFileOptions share posixFlagValue equals: (FileDenyWriteFlag perform: self valueAccessor).
	aFileOptions beWriteOnlyShared.
	self assert: aFileOptions share posixFlagValue equals: (FileDenyReadFlag perform: self valueAccessor).
	aFileOptions beNonShared.
	self assert: aFileOptions share posixFlagValue equals: (FileDenyReadWriteFlag perform: self valueAccessor).
%

category: 'tests'
method: FileOptionsTest
testWriteOpenMode

	| aFileOptions |
	aFileOptions := EncodedBufferedStreamSpec newWrite fileOptions.
	aFileOptions parent store: self store.
	self assert: aFileOptions isWritable.
	self deny: aFileOptions isReadable.
	self assert: aFileOptions posixFlags size equals: 2.
	self assert: aFileOptions hasFileCreateFlag.
	self assert: aFileOptions hasFileTruncateFlag.
	self assert: aFileOptions mode equals: 'w'.
	self assert: aFileOptions fileOpenModeValue equals: 1.
	self assert: aFileOptions fileOpenFlagsTotalValue equals: self openWriteValue
%

category: 'private'
method: FileOptionsTest
_testPermissionFlag: aPermissionFlagClass in: aFileOptions
	self assert: aFileOptions accessPermissions equals: (aPermissionFlagClass perform: self valueAccessor)
%

! Class implementation for 'FileUnixOptionsTest'

!		Instance methods for 'FileUnixOptionsTest'

category: 'private'
method: FileUnixOptionsTest
openAndAppendAndReadValue
	^1090
%

category: 'private'
method: FileUnixOptionsTest
openAndAppendValue
	^1089
%

category: 'private'
method: FileUnixOptionsTest
openReadWriteTruncateValue
	^578
%

category: 'private'
method: FileUnixOptionsTest
openReadWriteValue
	^2
%

category: 'private'
method: FileUnixOptionsTest
openWriteValue
	^577
%

category: 'private'
method: FileUnixOptionsTest
storeClass
	^UnixStore
%

category: 'tests'
method: FileUnixOptionsTest
testAccessRules

	| aFileOptions |
	aFileOptions := EncodedBufferedStreamSpec newRead fileOptions.
	aFileOptions parent store: self store.

	self should: [aFileOptions accessRule] raise: FileOptionFeatureNotSupported.

	aFileOptions openModeFlag: FileOpenWriteOnlyFlag new.
	self should: [aFileOptions accessRule] raise: FileOptionFeatureNotSupported.

	aFileOptions openModeFlag: FileOpenReadWriteFlag new.
	self should: [aFileOptions accessRule] raise: FileOptionFeatureNotSupported.
%

category: 'tests'
method: FileUnixOptionsTest
testCreateRules

	| aFileOptions |
	aFileOptions := EncodedBufferedStreamSpec newRead fileOptions.
	aFileOptions parent store: self store.

	self should: [aFileOptions createRule] raise: FileOptionFeatureNotSupported.

	aFileOptions addPosixFlag: FileCreateFlag new.
	self should: [aFileOptions createRule] raise: FileOptionFeatureNotSupported.

	aFileOptions addPosixFlag: FileExclusiveFlag new.
	self should: [aFileOptions createRule] raise: FileOptionFeatureNotSupported.
	self should: [aFileOptions truncateRule] raise: FileOptionFeatureNotSupported.

	aFileOptions addPosixFlag: FileTruncateFlag new.
	self should: [aFileOptions truncateRule] raise: FileOptionFeatureNotSupported.
%

category: 'tests'
method: FileUnixOptionsTest
testFileType

	| aFileOptions |
	aFileOptions := EncodedBufferedStreamSpec newAppend fileOptions.
	aFileOptions parent store: self store.
	self assert: aFileOptions fileType isNil
%

category: 'tests'
method: FileUnixOptionsTest
testShareRules

	| aFileOptions |
	aFileOptions := EncodedBufferedStreamSpec newRead fileOptions.
	aFileOptions parent store: self store.
	self should: [aFileOptions shareRule] raise: FileOptionFeatureNotSupported.

	aFileOptions share: FileDenyReadFlag new.
	self should: [aFileOptions shareRule] raise: FileOptionFeatureNotSupported.

	aFileOptions share: FileDenyWriteFlag new.
	self should: [aFileOptions shareRule] raise: FileOptionFeatureNotSupported.

	aFileOptions share: FileDenyReadWriteFlag new.
	self should: [aFileOptions shareRule] raise: FileOptionFeatureNotSupported.
%

category: 'private'
method: FileUnixOptionsTest
valueAccessor
	^#defaultPlatformValue
%

! Class implementation for 'FileWindowsOptionsTest'

!		Instance methods for 'FileWindowsOptionsTest'

category: 'private'
method: FileWindowsOptionsTest
groupAllOctalValue
	^'0'
%

category: 'private'
method: FileWindowsOptionsTest
openAndAppendAndReadValue
	^196610
%

category: 'private'
method: FileWindowsOptionsTest
openAndAppendValue
	^196609
%

category: 'private'
method: FileWindowsOptionsTest
openReadWriteTruncateValue
	^655362
%

category: 'private'
method: FileWindowsOptionsTest
openReadWriteValue
	^2
%

category: 'private'
method: FileWindowsOptionsTest
openWriteValue
	^655361
%

category: 'private'
method: FileWindowsOptionsTest
otherAllOctalValue
	^'0'
%

category: 'private'
method: FileWindowsOptionsTest
ownerAllOctalValue
	^'3'
%

category: 'private'
method: FileWindowsOptionsTest
storeClass
	^WindowsStore
%

category: 'tests'
method: FileWindowsOptionsTest
testAccessRules

	| aFileOptions |
	aFileOptions := EncodedBufferedStreamSpec newRead fileOptions.
	aFileOptions parent store: self store.

	self assert: aFileOptions accessRule equals: WindowsStore readOnlyAccessRule.

	aFileOptions openModeFlag: FileOpenWriteOnlyFlag new.
	self assert: aFileOptions accessRule equals: WindowsStore writeOnlyAccessRule.

	aFileOptions openModeFlag: FileOpenReadWriteFlag new.
	self assert: aFileOptions accessRule equals: WindowsStore readWriteAccessRule.
%

category: 'tests'
method: FileWindowsOptionsTest
testCreateRules

	| aFileOptions |
	aFileOptions := EncodedBufferedStreamSpec newRead fileOptions.
	aFileOptions parent store: self store.

	self assert: aFileOptions createRule equals: WindowsStore openExistingRule.

	aFileOptions addPosixFlag: FileCreateFlag new.
	self assert: aFileOptions createRule equals: WindowsStore openAlwaysRule.

	aFileOptions addPosixFlag: FileExclusiveFlag new.
	self assert: aFileOptions createRule equals: WindowsStore createNewRule.
	self assert: aFileOptions truncateRule equals: WindowsStore noTruncateRule.

	aFileOptions addPosixFlag: FileTruncateFlag new.
	self assert: aFileOptions truncateRule equals: WindowsStore truncateExistingRule.
%

category: 'tests'
method: FileWindowsOptionsTest
testDefaultReadFileRules

	| aFileOptions |
	aFileOptions := EncodedBufferedStreamSpec newRead fileOptions.
	aFileOptions parent store: self store.

	self assert: aFileOptions createRule equals: WindowsStore openExistingRule.
	self assert: aFileOptions shareRule equals: WindowsStore denyNoneShareRule.
	self assert: aFileOptions accessRule equals: WindowsStore readOnlyAccessRule
%

category: 'tests'
method: FileWindowsOptionsTest
testFileType

	| aFileOptions |
	aFileOptions := 
		EncodedBufferedStreamSpec new 
			initializeForAppend; 
			beWindowsStore; 
			fileOptions.
	aFileOptions binaryFileType.

	self assert: aFileOptions fileType posixFlagValue equals: 1.

	aFileOptions textFileType.
	self assert: aFileOptions fileType posixFlagValue equals: 0
%

category: 'tests'
method: FileWindowsOptionsTest
testShareRules

	| aFileOptions |
	aFileOptions := EncodedBufferedStreamSpec newRead fileOptions.
	aFileOptions parent store: self store.
	self assert: aFileOptions shareRule equals: WindowsStore denyNoneShareRule.

	aFileOptions share: FileDenyReadFlag new.
	self assert: aFileOptions shareRule equals: WindowsStore denyReadShareRule.

	aFileOptions share: FileDenyWriteFlag new.
	self assert: aFileOptions shareRule equals: WindowsStore denyWriteShareRule.

	aFileOptions share: FileDenyReadWriteFlag new.
	self assert: aFileOptions shareRule equals: WindowsStore denyReadWriteShareRule.
%

category: 'private'
method: FileWindowsOptionsTest
valueAccessor
	^#windowsValue
%

! Class implementation for 'FileReferenceAttributeTests'

!		Class methods for 'FileReferenceAttributeTests'

category: 'resources'
classmethod: FileReferenceAttributeTests
resources

	^Array with: DiskFileAttributesTestsResources.
%

!		Instance methods for 'FileReferenceAttributeTests'

category: 'helper methods'
method: FileReferenceAttributeTests
tempFileResource

	^DiskFileAttributesTestsResources current
%

category: 'tests'
method: FileReferenceAttributeTests
testCreationTime

	| resource creationTime x |
	resource := self tempFileResource.
	creationTime := resource file creationTime.
	self assert: (x := resource beforeCreationTime) <= creationTime.
	self assert: (x := resource afterCreationTime) >= creationTime.
%

category: 'tests'
method: FileReferenceAttributeTests
testExists

	self assert: self tempFileResource file exists
%

category: 'tests'
method: FileReferenceAttributeTests
testFileSize

	self assert: self tempFileResource file size equals: 72.
%

category: 'tests'
method: FileReferenceAttributeTests
testIsCharacter

	self deny: self tempFileResource file isCharacter
%

category: 'tests'
method: FileReferenceAttributeTests
testIsDirectory

	self deny: self tempFileResource file isDirectory.
	self assert: FileLocator temp isDirectory.
%

category: 'tests'
method: FileReferenceAttributeTests
testIsFile

	self assert: self tempFileResource file isFile.
	self deny: FileLocator temp isFile.
%

category: 'tests'
method: FileReferenceAttributeTests
testIsReadable

	self assert: self tempFileResource file isReadable.
%

category: 'tests'
method: FileReferenceAttributeTests
testIsSymlink

	self deny: self tempFileResource file resolve isSymlink.
%

category: 'tests'
method: FileReferenceAttributeTests
testIsWriteable

	self assert: self tempFileResource file isWritable.
%

category: 'tests'
method: FileReferenceAttributeTests
testModificationTime

	| resource modificationTime |
	resource := self tempFileResource.
	modificationTime := resource file modificationTime.
	self assert: resource beforeCreationTime <= modificationTime.
	self assert: resource afterCreationTime >= modificationTime.
%

! Class implementation for 'FileReferenceTest'

!		Instance methods for 'FileReferenceTest'

category: 'support'
method: FileReferenceTest
createFile: aPath
	filesystem ensureCreateDirectory: aPath parent.
	(filesystem resolve: aPath) writeStream close
%

category: 'running'
method: FileReferenceTest
setUp
	super setUp.
	filesystem := FileSystem memory
%

category: 'tests'
method: FileReferenceTest
testAllChildren
	"allChildren returns all the files and folders recursively nested in a reference"

	<publicTest>
	| ref children |
	[ filesystem createDirectory: '/alpha'.
	filesystem createDirectory: '/alpha/beta'.
	filesystem createDirectory: '/alpha/beta/delta'.
	filesystem createDirectory: '/alpha/gamma'.
	ref := filesystem / 'alpha'.
	children := ref allChildren.
	"all children returns the nodes: '/alpha', '/alpha/beta',  '/alpha/beta/delta', and '/alpha/gamma'."
	self assert: children size equals: 4.
	children
		do: [ :child | 
			self assert: child class equals: FileReference.
			self assert: (ref = child or: [ ref contains: child ]) ].
	self assert: (children collect: [ :ea | ea basename ]) equals: #('alpha' 'beta' 'gamma' 'delta') ]
		ensure: [ (filesystem / 'alpha') ensureDeleteAll ]
%

category: 'tests'
method: FileReferenceTest
testAllDirectories
	"allDirectories returns all folders recursively nested in a reference"

	<publicTest>
	| ref children |
	[ filesystem createDirectory: '/alpha'.
	filesystem createDirectory: '/alpha/beta'.
	filesystem createDirectory: '/alpha/beta/delta'.
	filesystem createDirectory: '/alpha/gamma'.
	ref := filesystem / 'alpha'.
	children := ref allDirectories.
	"all children returns the directories: '/alpha', '/alpha/beta', and '/alpha/gamma'."
	self assert: children size equals: 4.
	children
		do: [ :child | 
			self assert: child class equals: FileReference.
			self assert: (ref = child or: [ ref contains: child ]) ].
	self assert: (children collect: [ :ea | ea basename ]) equals: #('alpha' 'beta' 'gamma' 'delta') ]
		ensure: [ (filesystem / 'alpha') ensureDeleteAll ]
%

category: 'tests'
method: FileReferenceTest
testAllEntries
	| ref entries |
	[ filesystem createDirectory: '/alpha'.
	filesystem createDirectory: '/alpha/beta'.
	filesystem createDirectory: '/alpha/beta/delta'.
	filesystem createDirectory: '/alpha/gamma'.
	ref := filesystem / 'alpha'.
	entries := ref allEntries.
	self assert: entries size equals: 4.
	entries
		do: [ :entry | 
			self assert: entry class equals: FileSystemDirectoryEntry.
			self assert: (ref = entry reference or: [ ref contains: entry reference ]) ].
	self assert: (entries collect: [ :ea | ea basename ]) equals: #('alpha' 'beta' 'gamma' 'delta') ]
		ensure: [ (filesystem / 'alpha') ensureDeleteAll ]
%

category: 'tests'
method: FileReferenceTest
testAsAbsoluteConverted
	"Converts a relative reference to absolute, and asserts
	that it's absolute and still has the same path."

	| ref absolute |
	ref := filesystem * 'plonk'.
	absolute := ref asAbsolute.
	self assert: absolute isAbsolute.
	self assert: (absolute path at: 1) equals: 'plonk'
%

category: 'tests'
method: FileReferenceTest
testAsAbsoluteIdentity
	| ref |
	ref := filesystem / 'plonk'.
	self assert: ref asAbsolute == ref
%

category: 'tests'
method: FileReferenceTest
testAsReference
	| ref |
	ref := filesystem * 'plonk'.
	self assert: ref asFileReference == ref
%

category: 'tests'
method: FileReferenceTest
testBaseAndExtension
	| noExtension simpleExtension complexExtension |
	noExtension := filesystem * 'plonk'.
	self assert: noExtension extension equals: ''.

	"We create a reference to the plonk/griffle.taz in the context of filesystem"
	simpleExtension := filesystem * 'plonk' / 'griffle.taz'.
	self assert: simpleExtension base equals: 'griffle'.
	self assert: simpleExtension extension equals: 'taz'.

	"Note that the extension of a complex extension starts after the last extension delimiter"
	complexExtension := filesystem * 'plonk' / 'griffle.taz.txt'.
	self assert: complexExtension base equals: 'griffle.taz'.
	self assert: complexExtension extension equals: 'txt'
%

category: 'tests'
method: FileReferenceTest
testBasename
	| ref |
	ref := filesystem root.
	self assert: ref basename equals: '/'.
	ref := filesystem * 'plonk' / 'griffle'.
	self assert: ref basename equals: 'griffle'.
	ref := filesystem * 'plonk' / 'griffle.taz'.
	self assert: ref basename equals: 'griffle.taz'
%

category: 'tests'
method: FileReferenceTest
testBasenameWithoutExtension
	
	| ref |	
	ref := filesystem root.
	self assert: ref basename equals: '/'.
	
	ref := filesystem * 'plonk' / 'griffle'.
	self assert: ref basenameWithoutExtension equals: 'griffle'.
	self assert: (ref basenameWithoutExtension: 'griffle') equals: 'griffle'.
	self assert: (ref basenameWithoutExtension: 'taz') equals: 'griffle'.
	
	ref := filesystem * 'plonk' / 'griffle.taz'.
	self assert: ref basenameWithoutExtension equals: 'griffle'.
	self assert: (ref basenameWithoutExtension: 'taz') equals: 'griffle'.
	self assert: (ref basenameWithoutExtension: 'griffle.taz') equals: 'griffle.taz'.
	self assert: (ref basenameWithoutExtension: 'zork') equals: 'griffle.taz'.
	
	ref := filesystem * 'plonk' / 'griffle.taz.zork'.
	self assert: ref basenameWithoutExtension equals: 'griffle.taz'.
	self assert: (ref basenameWithoutExtension: 'zork') equals: 'griffle.taz'.
	self assert: (ref basenameWithoutExtension: 'taz.zork') equals: 'griffle'.
	self assert: (ref basenameWithoutExtension: 'griffle.taz.zork') equals: 'griffle.taz.zork'.
	self assert: (ref basenameWithoutExtension: 'taz') equals: 'griffle.taz.zork'.
%

category: 'tests'
method: FileReferenceTest
testCanonicalization

	| ref |

	ref := 'a/b/c' asFileReference canonicalize.
	self assert: ref path segments equals: #('a' 'b' 'c').

	ref := '/a/b/c' asFileReference canonicalize.
	self assert: ref path segments equals: #('a' 'b' 'c').

	ref := '../a/b/c' asFileReference canonicalize.
	self assert: ref path segments equals: #('..' 'a' 'b' 'c').

	ref := 'a/b/c/..' asFileReference canonicalize.
	self assert: ref path segments equals: #('a' 'b').

	ref := '/a/b/c/..' asFileReference canonicalize.
	self assert: ref path segments equals: #('a' 'b').

	ref := 'a/b/../c' asFileReference canonicalize.
	self assert: ref path segments equals: #('a' 'c').

	ref := '/a/b/../c' asFileReference canonicalize.
	self assert: ref path segments equals: #('a' 'c').
%

category: 'tests'
method: FileReferenceTest
testChildDirectories
	| childDirectories |
	[ filesystem createDirectory: '/beta'.
	filesystem createDirectory: '/gamma'.
	filesystem / 'delta' writeStreamDo: [ :stream | stream nextPutAll: '1' ].
	filesystem / 'epsilon' writeStreamDo: [ :stream | stream nextPutAll: '2' ].
	childDirectories := filesystem root directories.
	self assert: childDirectories size equals: 2.
	childDirectories
		do: [ :each | 
			self assert: each class equals: FileReference.
			self
				assert: each isDirectory
				description: 'Collection should not contain references to files.' ] ]
		ensure: [ (filesystem / 'beta') ensureDeleteAll.
			(filesystem / 'gamma') ensureDeleteAll.
			(filesystem / 'delta') ensureDelete.
			(filesystem / 'epsilon') ensureDelete ]
%

category: 'tests'
method: FileReferenceTest
testChildFiles
	| childFiles |
	[ filesystem createDirectory: '/beta'.
	filesystem createDirectory: '/gamma'.
	filesystem / 'delta' writeStreamDo: [ :stream | stream nextPutAll: '1' ].
	filesystem / 'epsilon' writeStreamDo: [ :stream | stream nextPutAll: '2' ].
	childFiles := filesystem root files.
	self assert: childFiles size equals: 2.
	childFiles
		do: [ :each | 
			self assert: each class equals: FileReference.
			self assert: each isFile description: 'Collection should not contain references to directories.' ] ]
		ensure: [ (filesystem / 'beta') ensureDeleteAll.
			(filesystem / 'gamma') ensureDeleteAll.
			(filesystem / 'delta') ensureDelete.
			(filesystem / 'epsilon') ensureDelete ]
%

category: 'tests'
method: FileReferenceTest
testChildOfPath
	| parent  child |
	parent := Path / 'griffle'.
	child := filesystem / 'griffle' / 'nurb'.
	self deny: (child isChildOf: parent).
	self deny: (parent isChildOf: child).
%

category: 'tests'
method: FileReferenceTest
testChildOfReference
	| parent  child |
	parent := filesystem / 'griffle'.
	child := filesystem / 'griffle' / 'nurb'.
	self assert: (child isChildOf: parent).
	self deny: (parent isChildOf: child).
%

category: 'tests'
method: FileReferenceTest
testChildren
	| ref children |
	[ filesystem createDirectory: '/alpha'.
	filesystem createDirectory: '/alpha/beta'.
	filesystem createDirectory: '/alpha/gamma'.
	ref := filesystem / 'alpha'.
	children := ref children.
	self assert: children size equals: 2.
	children
		do: [ :child | 
			self assert: child class equals: FileReference.
			self assert: (child isChildOf: ref).
			self assert: (#('beta' 'gamma') includes: child basename) ] ]
		ensure: [ (filesystem / 'alpha') ensureDeleteAll ]
%

category: 'tests'
method: FileReferenceTest
testCommaAddsExtension
	| ref result |
	ref := filesystem * 'plonk'.
	result := ref , 'griffle'.
	self assert: result basename equals: 'plonk.griffle'
%

category: 'tests'
method: FileReferenceTest
testCommaAddsExtensionAgain
	| ref result |
	ref := filesystem * 'plonk.griffle'.
	result := ref , 'nurp'.
	self assert: result basename equals: 'plonk.griffle.nurp'
%

category: 'tests'
method: FileReferenceTest
testContainsLocator
	| ref |
	ref := FileLocator imageDirectory resolve parent.
	self assert: (ref contains: FileLocator image)
%

category: 'tests'
method: FileReferenceTest
testContainsPath
	| ref |
	ref := filesystem  * 'griffle'.
	self assert: (ref contains: (ref / 'nurp') path)
%

category: 'tests'
method: FileReferenceTest
testContainsReference
	| ref |
	ref := filesystem * 'griffle'.
	self assert: (ref contains: ref / 'nurp')
%

category: 'tests'
method: FileReferenceTest
testContents
	| ref contents |
	contents := '12345 abcdf!'.
	ref := filesystem * 'file'.
	[ ref writeStreamDo: [ :stream | stream nextPutAll: contents ].
	self assert: ref contents asString equals: contents ]
		ensure: [ ref ensureDelete ]
%

category: 'tests'
method: FileReferenceTest
testDeleteAll
	"allChildren returns all the files and folders recursively nested in a reference"

	<publicTest>
	[ | ref |
	filesystem createDirectory: '/alpha'.
	filesystem createDirectory: '/alpha/beta'.
	filesystem createDirectory: '/alpha/beta/delta'.
	filesystem createDirectory: '/alpha/gamma'.
	ref := filesystem / 'alpha'.
	ref deleteAll.
	self deny: ref exists.
	self deny: (ref / 'beta') exists.
	self deny: (ref / 'beta' / 'delta') exists.
	self deny: (ref / 'beta' / 'gamma') exists ]
		ensure: [ (filesystem / 'alpha') ensureDeleteAll ]
%

category: 'tests'
method: FileReferenceTest
testDeleteAllChildren
	"allChildren returns all the files and folders recursively nested in a reference"

	<publicTest>
	[ | ref |
	filesystem createDirectory: '/alpha'.
	filesystem createDirectory: '/alpha/beta'.
	filesystem createDirectory: '/alpha/beta/delta'.
	filesystem createDirectory: '/alpha/gamma'.
	ref := filesystem / 'alpha'.
	ref deleteAllChildren.
	self assert: ref exists.
	self deny: (ref / 'beta') exists.
	self deny: (ref / 'beta' / 'delta') exists.
	self deny: (ref / 'beta' / 'gamma') exists ]
		ensure: [ (filesystem / 'alpha') ensureDeleteAll ]
%

category: 'tests'
method: FileReferenceTest
testDeleteIfAbsent
	| flag reference |
	flag := false.
	reference := filesystem / 'plonk'.
	reference ensureCreateFile.
	[ reference exists
		ifFalse: [ self error ].
	reference deleteIfAbsent: [ flag := true ].
	self deny: flag.
	reference exists
		ifTrue: [ self error ].
	reference deleteIfAbsent: [ flag := true ].
	self assert: flag ]
		ensure: [ reference ensureDelete ]
%

category: 'tests'
method: FileReferenceTest
testDoesContainReferenceFileSystem
	| ref other |
	ref := filesystem * 'griffle'.
	other := filesystem / 'griffle' / 'nurp'.
	self assert: (ref contains: other)
%

category: 'tests'
method: FileReferenceTest
testDoesNotContainReferenceWhenUsingDifferentInstancesOfMemoryFileSystem
	| ref other |
	ref := filesystem * 'griffle'.
	other := FileSystem memory / 'griffle' / 'nurp'.
	self deny: (ref contains: other)
%

category: 'tests'
method: FileReferenceTest
testDoesntContainLocator
	| ref |
	ref := filesystem * 'griffle'. 
	self deny: (ref contains: FileLocator image)
%

category: 'tests'
method: FileReferenceTest
testDoesntContainPath
	| ref |
	ref := filesystem * 'griffle'.
	self deny: (ref contains: (Path * 'nurp'))
%

category: 'tests'
method: FileReferenceTest
testDoesntContainReferencePath
	| ref other |
	ref := filesystem * 'griffle'.
	other := filesystem * 'nurp'.
	self deny: (ref contains: other)
%

category: 'tests'
method: FileReferenceTest
testEnsureDelete
	| reference |
	reference := filesystem / 'plonk'.	"Deletes the file if it exists"
	reference ensureCreateFile.
	[ self assert: reference exists.
	reference ensureDelete.
	self deny: reference exists	"No-op if file does not exist" ]
		ensure: [ reference ensureDelete ]
%

category: 'tests'
method: FileReferenceTest
testEnsureDeleteAll
	| reference childReference |
	reference := filesystem / 'plonk'.	"Deletes the file if it exists"
	reference ensureCreateDirectory.
	[ self assert: reference exists.
	childReference := reference / 'child'.
	childReference ensureCreateFile.
	self assert: childReference exists.
	reference ensureDeleteAll.
	self deny: childReference exists.
	self deny: reference exists ]
		ensure: [ reference ensureDeleteAll ]
%

category: 'tests'
method: FileReferenceTest
testEnsureDeleteNonEmptyDirectory
	| reference childReference |
	reference := filesystem / 'plonk'.	"Deletes the file if it exists"
	reference ensureCreateDirectory.
	[ self assert: reference exists.
	childReference := reference / 'child'.
	childReference ensureCreateFile.
	self assert: childReference exists.
	self should: [ reference ensureDelete ] raise: DirectoryIsNotEmpty ]
		ensure: [ reference ensureDeleteAll ]
%

category: 'tests'
method: FileReferenceTest
testEntries
	[ | ref entries |
	filesystem createDirectory: '/alpha'.
	filesystem createDirectory: '/alpha/beta'.
	filesystem createDirectory: '/alpha/gamma'.
	ref := filesystem / 'alpha'.
	entries := ref entries.
	self assert: entries size equals: 2.
	entries
		do: [ :entry | 
			self assert: entry class equals: FileSystemDirectoryEntry.
			self assert: (entry reference isChildOf: ref).
			self assert: (#('beta' 'gamma') includes: entry reference basename) ] ]
		ensure: [ (filesystem / 'alpha') ensureDeleteAll ]
%

category: 'tests'
method: FileReferenceTest
testEqual
	| a b |
	a := filesystem * 'plonk'.
	b := filesystem * 'plonk'.
	self deny: a == b.
	self assert: a equals: b
%

category: 'tests'
method: FileReferenceTest
testEqualityRelativeVsAbsolute

	| f1 f2 |

	f1 := FileLocator workingDirectory / 'pharo-local'.
	f2 := f1 asAbsolute.
	self assert: f1 equals: f2
%

category: 'tests'
method: FileReferenceTest
testExists
	| reference |
	reference := filesystem / 'plonk'.
	reference ensureCreateFile.
	[ self assert: reference exists.
	reference delete.
	self deny: reference exists ]
		ensure: [ reference ensureDelete ]
%

category: 'tests'
method: FileReferenceTest
testGlob
	[ | ref children |
	filesystem createDirectory: '/alpha'.
	filesystem createDirectory: '/alpha/beta'.
	filesystem createDirectory: '/alpha/gamma'.
	ref := filesystem root.
	children := ref glob: [ :node | true ].
	self assert: children size == 4.	"including root"
	children := ref glob: [ :node | node basename size > 1 ].
	self assert: children size == 3.	"without root"
	children := ref glob: [ :node | node basename = 'gamma' ].
	self assert: children size == 1.	"gamma"
	self assert: children first basename equals: 'gamma' ]
		ensure: [ (filesystem / 'alpha') ensureDeleteAll ]
%

category: 'tests'
method: FileReferenceTest
testGrandchildOfReference
	| griffle  nurb |
	griffle := filesystem / 'griffle'.
	nurb := filesystem / 'griffle' / 'plonk' / 'nurb'.
	self deny: (griffle isChildOf: nurb).
	self deny: (nurb isChildOf: griffle).
%

category: 'tests'
method: FileReferenceTest
testHasChildren
	<publicTest>
	[ | ref |
	filesystem createDirectory: '/alpha'.
	filesystem createDirectory: '/alpha/beta'.
	filesystem createDirectory: '/alpha/beta/delta'.
	filesystem createDirectory: '/alpha/gamma'.
	ref := filesystem / 'alpha'.
	self assert: ref hasChildren.
	self assert: (ref / 'beta') hasChildren.
	self deny: (ref / 'beta' / 'delta') hasChildren.
	self deny: (ref / 'beta' / 'gamma') hasChildren ]
		ensure: [ (filesystem / 'alpha') ensureDeleteAll ]
%

category: 'tests'
method: FileReferenceTest
testHasDirectories
	<publicTest>
	[ | ref |
	filesystem createDirectory: '/alpha'.
	filesystem createDirectory: '/alpha/beta'.
	(filesystem / 'alpha' / 'beta' / 'delta') ensureCreateFile.
	filesystem createDirectory: '/alpha/gamma'.
	ref := filesystem / 'alpha'.
	self assert: ref hasDirectories.
	self deny: (ref / 'beta') hasDirectories.
	self deny: (ref / 'beta' / 'gamma') hasDirectories ]
		ensure: [ (filesystem / 'alpha') ensureDeleteAll ]
%

category: 'tests'
method: FileReferenceTest
testHasFiles
	<publicTest>
	[ | ref |
	filesystem createDirectory: '/alpha'.
	filesystem createDirectory: '/alpha/beta'.
	(filesystem / 'alpha' / 'beta' / 'delta') ensureCreateFile.
	filesystem createDirectory: '/alpha/beta/eta'.
	filesystem createDirectory: '/alpha/gamma'.
	ref := filesystem / 'alpha'.
	self deny: ref hasFiles.
	self assert: (ref / 'beta') hasFiles.
	self deny: (ref / 'beta' / 'gamma') hasFiles ]
		ensure: [ (filesystem / 'alpha') ensureDeleteAll ]
%

category: 'tests'
method: FileReferenceTest
testIndicator
	| ref |
	ref := filesystem * 'plonk' / 'griffle'.
	self deny: ref exists.
	self assert: ref indicator equals: '?'.
	ref := filesystem workingDirectory / 'plonk'.
	self deny: ref exists.
	ref createDirectory.
	[ self assert: ref exists.
	self assert: ref isDirectory.
	self assert: ref indicator equals: '/' ]
		ensure: [ ref delete ].
	ref := filesystem workingDirectory / 'plonk'.
	self deny: ref exists.
	[ ref writeStreamDo: [ :stream | stream nextPutAll: 'foo' ] ifPresent: [ self fail ].
	self assert: ref exists.
	self assert: ref isFile.
	self assert: ref indicator equals: '' ]
		ensure: [ ref delete ]
%

category: 'tests'
method: FileReferenceTest
testIsAbsolute
	self assert: (filesystem / 'plonk') isAbsolute
%

category: 'tests'
method: FileReferenceTest
testIsNotAbsolute
	self deny: (filesystem * 'plonk') isAbsolute
%

category: 'tests'
method: FileReferenceTest
testIsNotRelative
	self deny: (filesystem / 'plonk') isRelative
%

category: 'tests'
method: FileReferenceTest
testIsNotRoot
	self deny: (filesystem / 'plonk') isRoot
%

category: 'tests'
method: FileReferenceTest
testIsRelative
	self assert: (filesystem * 'plonk') isRelative
%

category: 'tests'
method: FileReferenceTest
testIsRoot
	self assert: (filesystem root) isRoot
%

category: 'tests'
method: FileReferenceTest
testMakeRelative

	| parent child relative |
	parent := filesystem / 'griffle'.
	child := filesystem / 'griffle' / 'plonk' / 'nurb'.
	relative := parent makeRelative: child.
	self assert: relative equals: (Path * 'plonk' / 'nurb')
%

category: 'tests'
method: FileReferenceTest
testParent
	| ref parent |
	ref := filesystem * 'plonk' / 'griffle'.
	parent := ref parent.
	self assert: parent class = ref class.
	self assert: (parent path at: 1) = 'plonk'
%

category: 'tests'
method: FileReferenceTest
testParentResolutionWithAbsoluteReference
	| base relative absolute |
	base := filesystem / '/plonk' / 'pinto'.
	relative := filesystem / 'griffle' / 'zonk'.
	absolute := base resolve: relative.
	self assert: absolute fileSystem == relative fileSystem.
	self assert: absolute isAbsolute.
	self assert: (absolute path at: 1) equals: 'griffle'.
	self assert: (absolute path at: 2) equals: 'zonk'
%

category: 'tests'
method: FileReferenceTest
testParentResolutionWithPath
	| base relative absolute |
	base := filesystem / 'plonk' / 'pinto'.
	relative := Path parent / 'griffle' / 'zonk'.
	absolute := base resolve: relative.
	self assert: absolute isAbsolute.
	self assert: absolute path segments equals: #('plonk' 'pinto' '..' 'griffle' 'zonk').
	
%

category: 'tests'
method: FileReferenceTest
testParentResolutionWithReference
	| base relative absolute |
	base := (filesystem / 'plonk' / 'pinto').
	relative := (filesystem referenceTo: '../griffle/zonk').
	absolute := base resolve: relative.
	self assert: absolute isAbsolute.
	self assert: absolute path segments equals: #('plonk' 'pinto' '..' 'griffle' 'zonk').
	
	
%

category: 'tests'
method: FileReferenceTest
testParentResolutionWithRemoteReference
	| base relative absolute |
	base := filesystem / 'plonk' / 'pinto'.
	relative := filesystem referenceTo: '../griffle/zonk'.
	absolute := base resolve: relative.
	self assert: absolute isAbsolute.
	self assert: absolute path segments equals: #('plonk' 'pinto' '..' 'griffle' 'zonk')
%

category: 'tests'
method: FileReferenceTest
testParentUpTo
	| base a b c |
	[ 
		base := filesystem workingDirectory.
		(base / 'testParentUpTo') ensureCreateDirectory.
		a := (base / 'testParentUpTo' / 'A') ensureCreateDirectory.
		b := (base / 'testParentUpTo' / 'A' / 'B') ensureCreateDirectory.
		c := (base / 'testParentUpTo' / 'A' / 'B' / 'C') ensureCreateDirectory.
		self assert: b equals: (c parentUpTo: 'B').
		self assert: a equals: (c parentUpTo: 'A').
		self assert: (base / 'testParentUpTo') equals: (c parentUpTo: 'testParentUpTo').
		self assert: base equals: (c parentUpTo: 'notAParent') 
	] ensure: [ 
		(base / 'testParentUpTo') ensureDeleteAll 
	]
%

category: 'tests'
method: FileReferenceTest
testPathRelativeTo
	| parent childPath relative |
	parent := filesystem / 'griffle'.
	childPath := Path / 'griffle' / 'plonk' / 'nurb'.
	relative := childPath relativeTo: parent.
	self assert: relative = (Path * 'plonk' / 'nurb')
%

category: 'tests'
method: FileReferenceTest
testReadStream
	| ref stream path |
	path := Path * 'plonk'.
	filesystem store createFile: path.
	ref := filesystem referenceTo: path.
	[ [ stream := ref readStream ]
		ensure: [ stream ifNotNil: [ stream close ] ] ]
		ensure: [ ref delete ]
%

category: 'tests'
method: FileReferenceTest
testReadStreamDo
	| ref path |
	path := Path * 'plonk'.
	filesystem store createFile: path.
	ref := filesystem referenceTo: path.
	[ ref readStreamDo: [ :stream | self deny: stream isNil ] ]
		ensure: [ ref delete ]
%

category: 'tests'
method: FileReferenceTest
testReadStreamDoifAbsent
	| ref path |
	path := Path * 'plonk'.
	filesystem store createFile: path.
	ref := filesystem referenceTo: path.
	[ ref readStreamDo: [ :stream | self deny: stream isNil ] ifAbsent: [ self signalFailure: 'The file exists!' ] ]
		ensure: [ ref ensureDelete ]
%

category: 'tests'
method: FileReferenceTest
testReadStreamDoifAbsentNot
	| ref pass |
	pass := false.
	ref := filesystem * 'plonk'.
	ref 
		readStreamDo: [:stream|]
		ifAbsent: [pass := true].
	self assert: pass
%

category: 'tests stream'
method: FileReferenceTest
testReadStreamDoNotFound
	| ref |
	ref := filesystem / 'plonk'.
	self
		should: [ref readStreamDo: [:s|]]
		raise: FileDoesNotExistException
%

category: 'tests'
method: FileReferenceTest
testReadStreamIfAbsent
	| ref stream path |
	path := Path * 'plonk'.
	filesystem store createFile: path.
	ref := filesystem referenceTo: path.
	[ [ stream := ref readStreamIfAbsent: [ self signalFailure: 'Should not reach here.' ] ]
		ensure: [ stream ifNotNil: [ stream close ] ] ]
		ensure: [ ref ensureDelete ]
%

category: 'tests'
method: FileReferenceTest
testReadStreamNotFound
	| ref  |
	ref := filesystem * 'plonk'.
	self
		should: [ref readStream ]
		raise: FileDoesNotExistException
%

category: 'tests'
method: FileReferenceTest
testRelativeToPath
	| parentPath child relative |
	parentPath := Path / 'griffle'.
	child := filesystem / 'griffle' / 'plonk' / 'nurb'.
	relative := child relativeTo: parentPath.
	self assert: relative = (Path * 'plonk' / 'nurb')
%

category: 'tests'
method: FileReferenceTest
testRelativeToReference
	| parent child relative |
	parent := filesystem / 'griffle'.
	child := filesystem  / 'griffle' / 'plonk' / 'nurb'.
	relative := child relativeTo: parent.
	self assert: relative = (Path * 'plonk' / 'nurb')
%

category: 'tests'
method: FileReferenceTest
testRename
	| file tmp originalFullName |
	[ file := (FileLocator imageDirectory / 'oldName') ensureCreateFile.
	originalFullName := file fullName.
	tmp := (FileLocator imageDirectory / 'tmp') ensureCreateDirectory.
	file renameTo: 'newName'.
	self deny: originalFullName asFileReference exists.
	self assert: file basename equals: 'newName'.
	self assert: (originalFullName asFileReference parent / 'newName') exists ]
		ensure: [ file delete.
			tmp deleteAll ]
%

category: 'tests'
method: FileReferenceTest
testRenameTargetExists

	| existingFile fileToRename |
	[
		existingFile := 'existingFile' asFileReference ensureCreateFile.
		fileToRename := 'fileToRename' asFileReference ensureCreateFile.
		self should: [ fileToRename renameTo: existingFile basename ] raise: FileExists ]
		ensure: [
			existingFile delete.
			fileToRename delete ].
%

category: 'tests'
method: FileReferenceTest
testResolve
	| ref |
	ref := filesystem / 'griffle'.
	self assert: ref resolve == ref
%

category: 'tests'
method: FileReferenceTest
testRootParent
	| root |
	root := filesystem root.
	self assert: root parent == root
%

category: 'tests'
method: FileReferenceTest
testSiblingOfReference
	| griffle  nurb |
	griffle := filesystem / 'griffle'.
	nurb := filesystem / 'nurb'.
	self deny: (griffle isChildOf: nurb).
	self deny: (nurb isChildOf: griffle).
%

category: 'tests'
method: FileReferenceTest
testSimpleResolution
	| base relative absolute |
	base := filesystem / 'plonk'.
	relative := (Path * 'griffle') / 'zonk'.
	absolute := base resolve: relative.
	self assert: absolute isAbsolute.
	self assert: (absolute path at: 1) = 'plonk'.
	self assert: (absolute path at: 2) = 'griffle'.
	self assert: (absolute path at: 3) = 'zonk'.
	
	
%

category: 'tests'
method: FileReferenceTest
testSlash
	| ref result |
	ref := filesystem * 'plonk'.
	result := ref / 'griffle'.
	self assert: result class = ref class.
	self assert: result  isRelative.
	self assert: (result path at: 1) = 'plonk'.
	self assert: (result path at: 2) = 'griffle'.
%

category: 'tests'
method: FileReferenceTest
testTempFilePrefixSuffix
	| fileRef |
	fileRef := FileReference newTempFilePrefix: 'FileReference' suffix: 'Test'.
	self assert: (fileRef isKindOf: FileReference).
	self assert: fileRef exists not.
%

category: 'tests'
method: FileReferenceTest
testUnequalContent
	| a b |
	a := filesystem * 'plonk'.
	b := filesystem * 'griffle'.
	self deny: a = b.
%

category: 'tests'
method: FileReferenceTest
testUnequalSize
	| a b |
	a := filesystem * 'plonk'.
	b := filesystem / 'plonk' / 'griffle'.
	self deny: a = b.
%

category: 'tests'
method: FileReferenceTest
testUpToAll
	| testString fileRef |
	testString := 'ße'.
	fileRef := FileReference newTempFilePrefix: 'FileReference' suffix: 'Test'.
 
	[  
		fileRef writeStreamEncoded: 'utf8' do: [ :stream | stream nextPutAll: testString ].
		fileRef readStreamDo: [ :stream | self assert: (stream upToAll: 'e') equals: 'ß'] 
			] ensure: [ fileRef ensureDelete ].
%

category: 'tests'
method: FileReferenceTest
testWithExtentionAddsExtension
	| ref result |
	ref := filesystem * 'plonk'.
	result := ref withExtension: 'griffle'.
	self assert: result isRelative.
	self assert: result basename = 'plonk.griffle'
%

category: 'tests'
method: FileReferenceTest
testWithExtentionReplacesExtension
	| ref result |
	ref := filesystem * 'plonk.griffle'.
	result := ref withExtension: 'nurp'.
	self assert: result isRelative.
	self assert: result basename = 'plonk.nurp'
%

category: 'tests'
method: FileReferenceTest
testWithoutExtension
	| ref newRef |
	ref := filesystem * 'plonk' / 'griffle.txt'.
	newRef := ref withoutExtension.
	self assert: newRef parent equals: ref parent.
	self assert: newRef basename equals: 'griffle'
%

category: 'tests'
method: FileReferenceTest
testWorkingDirectoryParent
	| wd |
	wd := filesystem referenceTo: Path workingDirectory.
	self assert: wd parent path size = 1.
	self assert: (wd parent path at: 1) = '..'.
%

category: 'tests'
method: FileReferenceTest
testWriteStream
	| ref stream |
	ref := filesystem / 'plonk'.
	[ [ stream := ref writeStream ]
		ensure: [ stream ifNotNil: [ stream close ] ] ]
		ensure: [ ref ensureDelete ]
%

category: 'tests'
method: FileReferenceTest
testWriteStreamDo
	| ref s |
	ref := filesystem / 'plonk'.
	[ ref
		writeStreamDo: [ :stream | 
			s := stream.
			self deny: stream isNil ] ]
		ensure: [ ref ensureDelete ]
%

category: 'tests'
method: FileReferenceTest
testWriteStreamDoExists
	| ref path |
	path := Path * 'plonk'.
	filesystem store createFile: path.
	ref := filesystem referenceTo: path.
	[ ref writeStreamDo: [ :stream | self deny: stream isNil ] ]
		ensure: [ ref ensureDelete ]
%

category: 'tests'
method: FileReferenceTest
testWriteStreamDoifPresent
	| ref s |
	ref := filesystem / 'plonk'.
	[ ref
		writeStreamDo: [ :stream | 
			s := stream.
			self deny: stream isNil ]
		ifPresent: [ self signalFailure: 'The file does not exist!' ] ]
		ensure: [ ref ensureDelete ]
%

category: 'tests'
method: FileReferenceTest
testWriteStreamDoifPresentNot
	| ref pass path |
	pass := false.
	path := Path * 'plonk'.
	filesystem store createFile: path.
	ref := filesystem referenceTo: path.
	[ ref writeStreamDo: [ :stream |  ] ifPresent: [ pass := true ].
	self assert: pass ]
		ensure: [ ref ensureDelete ]
%

category: 'tests'
method: FileReferenceTest
testWriteStreamExists
	| ref stream path |
	path := Path * 'plonk'.
	filesystem store createFile: path.
	ref := filesystem referenceTo: path.
	[ [ stream := ref writeStream ]
		ensure: [ stream ifNotNil: [ stream close ] ] ]
		ensure: [ ref ensureDelete ]
%

category: 'tests'
method: FileReferenceTest
testWriteStreamifPresent
	| ref stream |
	ref := filesystem / 'plonk'.
	[ [ stream := ref writeStreamIfPresent: [ self signalFailure: 'Should not reach here' ] ]
		ensure: [ stream ifNotNil: [ stream close ] ] ]
		ensure: [ ref ensureDelete ]
%

category: 'tests'
method: FileReferenceTest
testWriteStreamifPresentExists
	| ref pass path |
	pass := false.
	path := Path * 'plonk'.
	filesystem store createFile: path.
	ref := filesystem referenceTo: path.
	[ ref writeStreamIfPresent: [ pass := true ].
	self assert: pass ]
		ensure: [ ref ensureDelete ]
%

! Class implementation for 'FileSystemHandleTest'

!		Class methods for 'FileSystemHandleTest'

category: 'testing'
classmethod: FileSystemHandleTest
isAbstract
	^ self name = #FileSystemHandleTest
%

category: 'testing'
classmethod: FileSystemHandleTest
shouldInheritSelectors 
	^ true
%

!		Instance methods for 'FileSystemHandleTest'

category: 'running'
method: FileSystemHandleTest
createFileSystem
	self subclassResponsibility 
%

category: 'running'
method: FileSystemHandleTest
reopenForRead
%

category: 'running'
method: FileSystemHandleTest
setUp
	super setUp.
	filesystem := self createFileSystem.
	reference := filesystem * 'plonk'.
	handle := reference openOptions: FileOptions newWrite
%

category: 'running'
method: FileSystemHandleTest
tearDown
	handle ensureClosed.
	reference ensureDelete.
	super tearDown
%

category: 'tests'
method: FileSystemHandleTest
testAt
	handle at: 1 write: (ByteArray with: 3) startingAt: 1 count: 1.
	self reopenForRead.
	self assert: (handle at: 1) = 3
%

category: 'tests'
method: FileSystemHandleTest
testAtPut
	| in |
	handle at: 1 put: 3.
	in := ByteArray new: 1.
	self reopenForRead.
	handle at: 1 read: in startingAt: 1 count: 1.
	self assert: in first = 3
%

category: 'tests'
method: FileSystemHandleTest
testAtPutBinaryAscii
	handle at: 1 put: 32.
	handle at: 1 put: Character space
%

category: 'tests'
method: FileSystemHandleTest
testAtWriteBinaryAscii
	handle
		at: 1
		write: #[32]
		startingAt: 1
		count: 1.
	handle
		at: 1
		write: (String with: Character space)
		startingAt: 1
		count: 1
%

category: 'tests'
method: FileSystemHandleTest
testClose
	handle close.
	self deny: handle isOpen
	
%

category: 'tests'
method: FileSystemHandleTest
testCreatedOpen
	
	
	self flag: 'TODO: activated once FileHandle is in use again!'.
	"self assert: handle isOpen"
%

category: 'tests'
method: FileSystemHandleTest
testEnsureClosed

	filesystem := self createFileSystem.
	reference := filesystem * 'plonk'.
	handle := reference openOptions: FileOptions newWrite.
	handle ensureClosed.
	self deny: handle isOpen.
	handle ensureClosed.
	reference ensureDelete.
	handle reference exists
		ifTrue: [ self error ].
	handle ensureClosed
%

category: 'tests'
method: FileSystemHandleTest
testIO
	| out in |
	out := #(1 2 3) asByteArray.
	in := ByteArray new: 3.
	handle at: 1 write: out startingAt: 1 count: 3.
	self reopenForRead.
	handle at: 1 read: in startingAt: 1 count: 3.
	self assert: out = in.
%

category: 'tests'
method: FileSystemHandleTest
testOpenOptions
	| aFileOptions |
	aFileOptions := FileOptions newRead.
	handle := reference openOptions: aFileOptions.
	self assert: (handle isKindOf: self targetClass).
	self assert: handle options == aFileOptions
%

category: 'tests'
method: FileSystemHandleTest
testReadBufferTooLarge
	| out in result |
	out := #(1 2 3) asByteArray.
	in := ByteArray new: 5.
	in atAllPut: 9.
	handle at: 1 write: out startingAt: 1 count: 3.
	self reopenForRead.
	result := handle at: 1 read: in startingAt: 2 count: 4.
	self assert: result = 3.
	self assert: in = #(9 1 2 3 9) asByteArray.
%

category: 'tests'
method: FileSystemHandleTest
testReadOnly
	handle close.
	handle := reference openOptions: FileOptions newRead.
	self 
		should: 
			[ handle 
				at: 1
				write: #(1 2 3 )
				startingAt: 1
				count: 3 ]
		raise: Error
%

category: 'tests'
method: FileSystemHandleTest
testReference
	self assert: handle reference = reference asAbsolute
%

category: 'tests'
method: FileSystemHandleTest
testSizeAfterGrow
	| out |
	out := #(1 2 3) asByteArray.
	handle at: 1 write: out startingAt: 1 count: 3.
	self assert: handle size = 3
%

category: 'tests'
method: FileSystemHandleTest
testSizeNoGrow
	| bytes |
	bytes := #(1 2 3 4) asByteArray.
	handle at: 1 write: bytes startingAt: 1 count: 3.
	handle at: 4 write: bytes startingAt: 4 count: 1.
	self assert: handle size = 4
%

category: 'tests'
method: FileSystemHandleTest
testTruncate
	| out |
	out := #(1 2 3 4 5) asByteArray.
	handle at: 1 write: out startingAt: 1 count: 5.
	self assert: handle size = 5.
	handle truncateTo: 3.
	"The Disk version will fail until the primitive is implemented"
	self assert: handle size = 3
%

! Class implementation for 'FileHandleTest'

!		Instance methods for 'FileHandleTest'

category: 'running'
method: FileHandleTest
createFileSystem
	"Force the filesystem to be created for each test.
	 Normally use: 'FileSystem disk' which returns the cached file system"
	^ FileSystem store: DiskStore activeClass createDefault
%

category: 'running'
method: FileHandleTest
reopenForRead
	handle close.
	handle := reference openOptions: FileOptions newRead.
	handle assureOpen
%

category: 'running'
method: FileHandleTest
targetClass
	^FileHandle
%

category: 'tests'
method: FileHandleTest
testFileWriteError
	" This test may be not needed depending on what happens with BinaryFileStream "
	| stream  |
	stream := FileSystem disk binaryWriteStreamOn: (FileSystem disk workingDirectory / 'foo.txt') path.
	stream close.
	stream := FileSystem disk binaryReadStreamOn: (FileSystem disk workingDirectory / 'foo.txt') path.
	self should: [stream  nextPutAll: 'abdef'] raise: FileWriteError
%

category: 'tests'
method: FileHandleTest
testOpenWithOptions
	| aFileOptions  result |
	aFileOptions := FileOptions newWrite.
	reference := (FileLocator temp / 'tttt') resolve.
	result := FileHandle open: reference withOptions: aFileOptions.
	self assert: (result isKindOf: FileSystem fileClass)
%

category: 'tests'
method: FileHandleTest
testTruncate
	| out |
	out := #(1 2 3 4 5) asByteArray.
	handle at: 1 write: out startingAt: 1 count: 5.
	self assert: handle size = 5.
	handle truncateTo: 3. "This actually sets the position."
	"When truncate is implemented in the file, this test will fail as the size should be 3"
	self assert: handle size = 5
%

category: 'tests'
method: FileHandleTest
testWriteStream
	| stream |
	stream := handle binaryStream.
	self assert: (stream isKindOf: BinaryFileStream).
	self assert: (stream respondsTo: #nextPut:)
%

! Class implementation for 'MemoryHandleTest'

!		Instance methods for 'MemoryHandleTest'

category: 'running'
method: MemoryHandleTest
createFileSystem
	^ FileSystem memory
%

category: 'running'
method: MemoryHandleTest
targetClass
	^MemoryHandle
%

category: 'tests'
method: MemoryHandleTest
testTruncate
	| out |
	out := #(1 2 3 4 5) asByteArray.
	handle at: 1 write: out startingAt: 1 count: 5.
	self assert: handle size = 5.
	handle truncateTo: 3.
	"The Disk version will fail until the primitive is implemented"
	self assert: handle size = 3
%

category: 'tests'
method: MemoryHandleTest
testWriteStream
	| stream |
	stream := handle binaryStream.
	self assert: (stream isKindOf: MemoryFileWriteStream).
	self assert: (stream respondsTo: #nextPut:)
%

! Class implementation for 'FileSystemResolverTest'

!		Class methods for 'FileSystemResolverTest'

category: 'testing'
classmethod: FileSystemResolverTest
isAbstract
	^ self name = #FileSystemResolverTest
%

!		Instance methods for 'FileSystemResolverTest'

category: 'asserting'
method: FileSystemResolverTest
assertOriginResolves: aSymbol
	| reference |
	reference := resolver resolve: aSymbol.
	self assert: (reference isKindOf: FileReference).
	self assert: reference exists.
	self assert: reference isAbsolute.
	^ reference
%

category: 'running'
method: FileSystemResolverTest
createResolver
	self subclassResponsibility 
%

category: 'running'
method: FileSystemResolverTest
setUp
	super setUp.
	resolver := self createResolver.
%

! Class implementation for 'ClientResolverTest'

!		Instance methods for 'ClientResolverTest'

category: 'running'
method: ClientResolverTest
createResolver
	^ ClientResolver forCurrentPlatform
%

category: 'tests'
method: ClientResolverTest
testClientHome
	| reference |
	resolver := ClientResolver forCurrentPlatform.
	reference := resolver resolve: #home.
	self assert: (reference isKindOf: FileReference).
"	self assert: reference exists."	"Currently cannot lookup directory on client"
	self assert: reference isAbsolute.
	"self assert: reference isDirectory"  "Currently cannot lookup directory on client"
%

! Class implementation for 'InteractiveResolverTest'

!		Instance methods for 'InteractiveResolverTest'

category: 'running'
method: InteractiveResolverTest
createResolver
	^ InteractiveResolver new
%

category: 'accessing'
method: InteractiveResolverTest
home
	^ FileLocator imageDirectory resolve
%

category: 'tests'
method: InteractiveResolverTest
testCached
	[ resolver resolve: #home ]
		on: ResolutionRequest
		do: [ :req | req resume: self home ].
	self assertOriginResolves: #home
%

category: 'tests'
method: InteractiveResolverTest
testNew
	[self assertOriginResolves: #home]
		on: ResolutionRequest
		do: [:req | req resume: self home].
	
%

! Class implementation for 'PlatformResolverTest'

!		Instance methods for 'PlatformResolverTest'

category: 'running'
method: PlatformResolverTest
createResolver
	^ PlatformResolver forCurrentPlatform
%

category: 'tests'
method: PlatformResolverTest
testCache
	| cache |
	cache := self assertOriginResolves: #cache
%

category: 'tests'
method: PlatformResolverTest
testHome
	| home |
	home := self assertOriginResolves: #home.
	self assert: home isDirectory
%

! Class implementation for 'SystemResolverTest'

!		Instance methods for 'SystemResolverTest'

category: 'running'
method: SystemResolverTest
createResolver
	^ SystemResolver new
%

category: 'testing'
method: SystemResolverTest
testDbfScratchDir
	self assertOriginResolves: #dbfScratchDir
%

category: 'testing'
method: SystemResolverTest
testExtent1
	self assertOriginResolves: #extent1
%

category: 'testing'
method: SystemResolverTest
testExtent1Directory
	self assertOriginResolves: #extent1Directory
%

category: 'testing'
method: SystemResolverTest
testImage
	self assertOriginResolves: #image
%

category: 'testing'
method: SystemResolverTest
testImageDirectory
	self assertOriginResolves: #imageDirectory
%

category: 'testing'
method: SystemResolverTest
testTranlog
	self assertOriginResolves: #tranlog
%

! Class implementation for 'FileSystemTest'

!		Class methods for 'FileSystemTest'

category: 'accessing'
classmethod: FileSystemTest
defaultTimeLimit
	^10 seconds
%

category: 'testing'
classmethod: FileSystemTest
isAbstract
	^ self name = #FileSystemTest
%

category: 'accessing'
classmethod: FileSystemTest
packageNamesUnderTest
	^ #('FileSystem')
%

category: 'testing'
classmethod: FileSystemTest
shouldInheritSelectors
	^ true
	
%

!		Instance methods for 'FileSystemTest'

category: 'private'
method: FileSystemTest
cleanupPaths
	toDelete
		select: [ :path | filesystem exists: path ]
		thenDo: [ :path | filesystem delete: path ]
%

category: 'initialize-release'
method: FileSystemTest
createFileSystem
	self subclassResponsibility 
%

category: 'initialize-release'
method: FileSystemTest
markForCleanup: anObject
	"[filesystem delete: anObject] on: FileDoesNotExistException do: [:s | s resume]."
	toDelete add: (filesystem resolve: anObject)
%

category: 'running'
method: FileSystemTest
setUp
	super setUp.
	filesystem := self createFileSystem.
	toDelete := OrderedCollection new.
%

category: 'private'
method: FileSystemTest
store
	^filesystem store
%

category: 'running'
method: FileSystemTest
tearDown
	self cleanupPaths.
	super tearDown
%

category: 'tests-streams-compatibility'
method: FileSystemTest
testBinaryReadStream
	| reference stream |
	self markForCleanup: (reference := filesystem workingDirectory / 'griffle').
	self should: [ reference binaryReadStream ] raise: FileDoesNotExistException.
	reference writeStreamDo: [ :ws | ws nextPutAll: 'griffle' ].
	stream := reference binaryReadStream.
	self assert: stream contents asString equals: 'griffle'.
	stream close
%

category: 'tests-streams-compatibility'
method: FileSystemTest
testBinaryReadStreamDo
	| reference |
	self markForCleanup: (reference := filesystem workingDirectory / 'griffle').
	self 
		should: [ reference binaryReadStreamDo: [ :stream | self assert: false ] ] 
		raise: FileDoesNotExistException.
	reference writeStreamDo: [ :ws | ws nextPutAll: 'griffle' ].
	self assert: (reference readStreamDo: [ :stream | stream contents asString ]) 
		= 'griffle'
%

category: 'tests-streams-compatibility'
method: FileSystemTest
testBinaryReadStreamDoIfAbsent
	| reference |
	self markForCleanup: (reference := filesystem workingDirectory / 'griffle').
	self assert: (reference 
		binaryReadStreamDo: [ :stream | false ]
		ifAbsent: [ true ]).
	reference writeStreamDo: [ :ws | ws nextPutAll: 'griffle' ].
	self assert: (reference 
		binaryReadStreamDo: [ :stream | stream contents asString = 'griffle' ]
		ifAbsent: [ false ])
%

category: 'tests-streams-compatibility'
method: FileSystemTest
testBinaryReadStreamIfAbsent
	| reference stream |
	self markForCleanup: (reference := filesystem workingDirectory / 'griffle').
	self assert: (reference binaryReadStreamIfAbsent: [ true ]).
	reference writeStreamDo: [ :ws | ws nextPutAll: 'griffle' ].
	stream := reference binaryReadStreamIfAbsent: [ false ].
	self assert: stream contents asString = 'griffle'.
	stream close
%

category: 'tests'
method: FileSystemTest
testChildrenAt
	| directory entries |
	directory := Path * 'plonk'.
	
	filesystem createDirectory: directory.
	filesystem createDirectory: directory / 'griffle'.
	filesystem createDirectory: directory / 'bint'.
	
	self markForCleanup: directory / 'griffle'.
	self markForCleanup: directory / 'bint'.
	self markForCleanup: directory.
	
	entries := filesystem childrenAt: directory.
	
	self assert: entries size = 2.
	entries do: [ :ea | 
		self assert: (ea isKindOf: Path).
		self assert: ea parent = (filesystem resolve: directory).
		self assert: (#('griffle' 'bint' ) includes: ea basename) ]
%

category: 'tests'
method: FileSystemTest
testChildrenSorting
	| directory sorted |
	
	directory := Path * 'plonk'.
	
	filesystem createDirectory: directory.
	filesystem createDirectory: directory / 'alfa'.
	filesystem createDirectory: directory / 'beta'.
	
	self markForCleanup: directory / 'alfa'.
	self markForCleanup: directory / 'beta'.
	self markForCleanup: directory.
	
	sorted := (filesystem childrenAt: directory) sort.
	self assert: sorted size = 2.
	self assert: (sorted at: 1) basename = 'alfa'.
	self assert: (sorted at: 2) basename = 'beta'.
%

category: 'tests'
method: FileSystemTest
testChildrenSortingRoot
	| directory1 directory2 |
	"self skip."
	
	directory1 := Path * 'plonk1'.
	directory2 := Path * 'plonk2'.
	
	filesystem createDirectory: directory1.
	filesystem createDirectory: directory2.
	
	self markForCleanup: directory1.
	self markForCleanup: directory2.
	
	self assert: filesystem workingDirectory children sort size equals: filesystem workingDirectory children size
%

category: 'tests'
method: FileSystemTest
testCopy
	| out in contents |
	
	self
		markForCleanup: 'gooly';
		markForCleanup: 'plonk'.
		
	out := (filesystem workingDirectory / 'gooly') writeStream.
	[ out nextPutAll: 'gooly' ] ensure: [ out close ].
	filesystem copy: 'gooly' to: 'plonk'.
	
	in := (filesystem workingDirectory / 'plonk') readStream.
	contents := [ in contents asString ] ensure: [ in close ].
	self assert: contents equals: 'gooly'
%

category: 'tests'
method: FileSystemTest
testCopyAndDelete
	"Check that FileSystem>>copyAndDelete:to: works within a filesystem.
	DiskFileSystemTest>>testMoveMemoryToDisk checks that #copyAndDelete:to: works across filesystems."
	| folder testString f1 f1s f2 |
	
	folder := filesystem workingDirectory.
	testString := 'To be copied and deleted'.

	f1 := folder / 'f1'.
	f1s := f1 writeStream.
	[ f1s nextPutAll: testString ] ensure: [ f1s close ].
	f2 := folder / 'f2'.
	
	"Cleanup after running"
	self 
		markForCleanup: f1;
		markForCleanup: f2.	
	
	filesystem copyAndDelete: f1 to: f2.
	self deny: f1 exists.
	self assert: f2 exists.
	self assert: f2 contents equals: testString.
%

category: 'tests'
method: FileSystemTest
testCopyDestExists
	| out |
	
	self 
		markForCleanup: 'gooly'; 
		markForCleanup: 'plonk'.
		
	out := (filesystem  workingDirectory / 'gooly') writeStream.
	[out nextPutAll: 'gooly'] ensure: [out close].

	(filesystem  workingDirectory / 'plonk') writeStream close.
	
	self 
		should: [filesystem copy: 'gooly' to: 'plonk']
		raise: FileExists
%

category: 'tests'
method: FileSystemTest
testCopySourceDoesntExist
	self 
		should: [filesystem copy: 'plonk' to: 'griffle']
		raise: FileDoesNotExistException
%

category: 'tests'
method: FileSystemTest
testCopyWithCorrectBasename
        | directory |
        self
                markForCleanup: 'gooly';
                markForCleanup: 'plonk'.
        directory := filesystem workingDirectory.
        (directory / 'gooly') ensureCreateFile.
        directory / 'gooly' copyTo: directory / 'plonk'.
        self assert: (directory / 'plonk') exists.
        self assert: (directory childNames includes: 'plonk')
%

category: 'tests'
method: FileSystemTest
testCreateDirectory
	| path directory |
	directory := filesystem workingDirectory.
 	self markForCleanup: directory / 'plonk' / 'griffle'.
	self markForCleanup: directory / 'plonk'.
	path := directory / 'plonk' / 'griffle'.
	(directory / 'plonk') ensureCreateDirectory.
	self shouldnt: [path createDirectory] raise:Error.
	self assert: path exists.
	(directory / 'plonk' ) deleteAll.
%

category: 'tests'
method: FileSystemTest
testCreateDirectoryExists
	| path |
	
	path := Path * 'griffle'.
	self markForCleanup: path.
	
	filesystem createDirectory: path.
	self 
		should: [ filesystem createDirectory: path]
		raise: DirectoryExists.
%

category: 'tests'
method: FileSystemTest
testCreateDirectoryNoParent
	| path |
	path := Path * 'griffle' / 'nurp'.
	self 
		should: [filesystem createDirectory: path]
		raise: DirectoryDoesNotExist.
	
	
%

category: 'tests'
method: FileSystemTest
testCreateDirectoryNotCreateParent
	| path |
	path := filesystem workingDirectory / 'plonk' / 'griffle'.
	self should:[path createDirectory] raise: DirectoryDoesNotExist.
	self assert: path exists not.
%

category: 'tests'
method: FileSystemTest
testCreateFile
	| directory path |
	directory := filesystem workingDirectory.
 	self markForCleanup: directory / 'plonk' / 'griffles'.
	self markForCleanup: directory / 'plonk'.
	path := directory / 'plonk' / 'griffles'.
	(directory / 'plonk') ensureCreateDirectory.
	self shouldnt: [ path createFile] raise:Error.
	self assert:path exists .
	(directory / 'plonk') deleteAll.
%

category: 'tests'
method: FileSystemTest
testCreateFileNotCreateParent
	| path |
	path := '/plonk/griffles' asFileReference.
	self should:[path createFile] raise: DirectoryDoesNotExist .
	self assert: path exists not.
%

category: 'tests'
method: FileSystemTest
testDefaultWorkingDirectory
	self assert: filesystem workingDirectory isRoot
%

category: 'tests'
method: FileSystemTest
testDelete
	"Unlike ensureDelete, delete raises an exception if the file does not exist."
	| reference |
	
	reference := filesystem workingDirectory / 'does-not-exist'.
	self deny: reference exists.
	
	self 
		should: [ reference delete ]
		raise: FileDoesNotExistException.
		
		
	reference := ( filesystem workingDirectory / 'file') ensureCreateFile.
	reference delete.
	
	self deny: reference exists.
%

category: 'tests'
method: FileSystemTest
testDelimiter
	self assert: filesystem delimiter isCharacter
%

category: 'tests'
method: FileSystemTest
testDirectory
	| path |
	
	path := Path * 'plonk'.
	self markForCleanup: path.
	
	filesystem createDirectory: path.
	
	self assert: (filesystem exists: path).
	self assert: (filesystem isDirectory: path).
	
	filesystem delete: path.
	self deny: (filesystem isFile: path).
	self deny: (filesystem exists: path)
%

category: 'tests'
method: FileSystemTest
testEnsureDirectory
	| path |
	
	path := Path * 'plonk'.
	self markForCleanup: path.
	
	filesystem ensureCreateDirectory: path.
	self assert: (filesystem isDirectory: path).
%

category: 'tests'
method: FileSystemTest
testEnsureDirectoryCreatesParent
	| path |
	path := Path * 'plonk' / 'griffle'.
	self markForCleanup: path.
	self markForCleanup: path parent.
	filesystem ensureCreateDirectory: path.
	self assert: (filesystem isDirectory: Path * 'plonk').
	self assert: (filesystem isDirectory: path)
%

category: 'tests'
method: FileSystemTest
testEnsureDirectoryExists
	| path |
	path := Path * 'plonk'.
	self markForCleanup: path.
	filesystem createDirectory: path.
	filesystem ensureCreateDirectory: path
%

category: 'tests'
method: FileSystemTest
testEntriesAt
	| directory entries |
	directory := Path * 'plonk'.
	
	self 
		markForCleanup: directory / 'griffle';
		markForCleanup: directory / 'bint';
		markForCleanup: directory.
	self cleanupPaths.

	filesystem createDirectory: directory.
	filesystem createDirectory: directory / 'griffle'.
	filesystem createDirectory: directory / 'bint'.
	
	entries := filesystem entriesAt: directory.
	self assert: entries size = 2.
	entries do: [ :ea | 
		self assert: (ea isKindOf: FileSystemDirectoryEntry).
		self assert: ea reference parent path = (filesystem resolve: directory).
		self assert: (#('griffle' 'bint' ) includes: ea reference basename).
		self assert: ea isDirectory ]
%

category: 'tests'
method: FileSystemTest
testEntryAt
	| path1 path2 entry1 entry2  |

	path1 := Path * 'plonk1'.
	path2 := Path * 'plonk2'.
	self markForCleanup: path1.
	self markForCleanup: path2.
	
	filesystem createDirectory: path1.
	self waitForSeconds: 4.
	filesystem createDirectory: path2.
	
	entry1 := filesystem entryAt: path1.
	entry2 := filesystem entryAt: path2.
	
	self assert: entry1 isDirectory.
	self assert: entry2 isDirectory.
	self assert: entry1 reference = (filesystem referenceTo: path1) asAbsolute.
	self assert: entry2 reference = (filesystem referenceTo: path2) asAbsolute.

	self assert: entry1 creationTime <= entry2 creationTime.
	self assert: entry1 modificationTime <= entry2 modificationTime.
%

category: 'tests'
method: FileSystemTest
testFile
	| path |
	
	path := Path * 'gooly'.
	self markForCleanup: path.
	
	(filesystem workingDirectory resolve: path) writeStream close.
	self assert: (filesystem exists: path).
	self deny: (filesystem isDirectory: path).
	self assert: (filesystem isFile: path).
	
	filesystem delete: path.
	self deny: (filesystem exists: path)
%

category: 'tests'
method: FileSystemTest
testFileNames
	| reference |
	#('test one' 'test with two' 'test-äöü' 'test.äöü') do: [ :each |
		reference := filesystem workingDirectory / each.
		self assert: reference basename = each.
		self deny: reference exists.
		reference
			writeStreamDo: [ :stream | stream nextPutAll: 'gooly' ]
			ifPresent: [ self fail ].
		[	self assert: reference exists.
			self assert: (filesystem workingDirectory children
				anySatisfy: [ :ref | ref = reference ]) ]
		ensure: [ reference delete ] ]
%

category: 'tests'
method: FileSystemTest
testMoveTo
	| base file folder |
	
	base := filesystem workingDirectory.
	
	folder := (base / 'folder') ensureCreateDirectory. 
	file := (base / 'file') ensureCreateFile.
	
	"Cleanup after running"
	self 
		markForCleanup: (base / 'folder' / 'newFile');
		markForCleanup: (base / 'folder') ;
		markForCleanup: (base / 'file').	
	
	file moveTo: (folder / 'newFile').
	self deny: (base / 'file') exists.
	self assert: (folder / 'newFile') exists.
%

category: 'tests'
method: FileSystemTest
testMoveToFailingExistingDestination
	| base file folder |
	
	base := filesystem workingDirectory.
	
	folder := (base / 'folder') ensureCreateDirectory. 
	(folder / 'newFile') ensureCreateFile.
	file := (base / 'file') ensureCreateFile.
	
	"Cleanup after running"
	self 
		markForCleanup: (base / 'folder' / 'newFile');
		markForCleanup: (base / 'folder') ;
		markForCleanup: (base / 'file').	
	
	"Destination exists already"
	self should: [ file moveTo: (folder / 'newFile') ] raise: Error.
	self assert: (base / 'file') exists.
	self assert: (folder / 'newFile') exists.
%

category: 'tests'
method: FileSystemTest
testMoveToFailingMissingDestination
	| base file |
	
	base := filesystem workingDirectory.
	
	file := (base / 'file') ensureCreateFile.
	
	"Cleanup after running"
	self 
		markForCleanup: (base / 'folder' / 'newFile');
		markForCleanup: (base / 'folder') ;
		markForCleanup: (base / 'file').	
	
	"Destination exists already"
	self deny: (base / 'folder') exists.
	self should: [ file moveTo: (base / 'folder' / 'newFile') ] raise: Error.
	self assert: (base / 'file') exists.
	self deny: (base / 'folder' / 'newFile') exists.
%

category: 'tests'
method: FileSystemTest
testMoveToFailingMissingSource
	| base folder |
	
	base := filesystem workingDirectory.
	
	folder := (base / 'folder') ensureCreateDirectory. 
	
	"Cleanup after running"
	self 
		markForCleanup: (base / 'folder' / 'newFile');
		markForCleanup: (base / 'folder').
	
	self deny: (base / 'file') exists.
	"Destination exists already"
	self should: [ (base / 'file') moveTo: (folder / 'newFile') ] raise: Error.
	self deny: (base / 'file') exists.
	self deny: (folder / 'newFile') exists.
%

category: 'tests'
method: FileSystemTest
testNonExistentEntryAt
	| path1 path2  |
	path1 := Path * 'plonk1'.
	path2 := Path * 'plonk2'.
	self markForCleanup: path1.
	filesystem createDirectory: path1.

	self shouldnt: [ filesystem entryAt: path1 ] raise: FileDoesNotExistException.
	self should: [ filesystem entryAt: path2 ] raise: FileDoesNotExistException
%

category: 'tests'
method: FileSystemTest
testNonExistentFileSize
	| base file1 file2 |
	
	base := filesystem workingDirectory.
	file1 := (base / 'file1') ensureCreateFile.
	file2 := (base / 'file2').
	self markForCleanup: base / 'file1'.
	
	self shouldnt: [ file1 size ] raise: FileDoesNotExistException.
	self should: [ file2 size ] raise: FileDoesNotExistException
%

category: 'tests-streams'
method: FileSystemTest
testReadingAfterWriting
	| reference stream |
	self markForCleanup: (reference := filesystem workingDirectory / 'griffle').
	self deny: reference exists.
	reference writeStreamDo: [ :ws | ws nextPutAll: 'griffle' ].
	stream := reference readStream.
	self assert: stream contents equals: 'griffle'.
	stream close
%

category: 'tests-streams'
method: FileSystemTest
testReadStream
	| reference stream |
	self markForCleanup: (reference := filesystem workingDirectory / 'griffle').
	self should: [ stream := reference readStream ] raise: FileDoesNotExistException.
	reference writeStreamDo: [ :ws | ws nextPutAll: 'griffle' ].
	stream := reference readStream.
	self assert: stream contents asString equals: 'griffle'.
	stream close
%

category: 'tests-streams'
method: FileSystemTest
testReadStreamDo
	| reference |
	self markForCleanup: (reference := filesystem workingDirectory / 'griffle').
	self 
		should: [ reference readStreamDo: [ :stream | self assert: false ] ] 
		raise: FileDoesNotExistException.
	reference writeStreamDo: [ :ws | ws nextPutAll: 'griffle' ].
	self assert: (reference readStreamDo: [ :stream | stream contents asString ]) 
		= 'griffle'
%

category: 'tests-streams'
method: FileSystemTest
testReadStreamDoIfAbsent
	| reference |
	self markForCleanup: (reference := filesystem workingDirectory / 'griffle').
	self assert: (reference 
		readStreamDo: [ :stream | false ]
		ifAbsent: [ true ]).
	reference writeStreamDo: [ :ws | ws nextPutAll: 'griffle' ].
	self assert: (reference 
		readStreamDo: [ :stream | stream contents asString = 'griffle' ]
		ifAbsent: [ false ])
%

category: 'tests-streams'
method: FileSystemTest
testReadStreamIfAbsent
	| reference stream |
	self markForCleanup: (reference := filesystem workingDirectory / 'griffle').
	self assert: (reference readStreamIfAbsent: [ true ]).
	reference writeStreamDo: [ :ws | ws nextPutAll: 'griffle' ].
	stream := reference readStreamIfAbsent: [ false ].
	self assert: stream contents asString = 'griffle'.
	stream close
%

category: 'tests'
method: FileSystemTest
testReferenceTo
	|path|
	"use a relative path since absolute path behavior differs on mac, linux vs win native filesystems"
	path := Path * 'a' / 'b'.
	self assert: (filesystem referenceTo: 'a/b') path = path.
%

category: 'tests'
method: FileSystemTest
testRoot
	self assert: filesystem root fileSystem = filesystem.
	self assert: filesystem root path = Path root.
	self assert: filesystem root isRoot.
%

category: 'tests'
method: FileSystemTest
testRootExists
	self assert: (filesystem exists: Path root)
%

category: 'tests'
method: FileSystemTest
testRootIsDirectory
	self assert: (filesystem isDirectory: Path root)
%

category: 'tests'
method: FileSystemTest
testRootIsNotAFile
	self deny: (filesystem isFile: Path root)
%

category: 'tests'
method: FileSystemTest
testWorking
	self assert: filesystem workingDirectory fileSystem = filesystem.
	self assert: filesystem workingDirectory path = filesystem workingDirectoryPath
%

category: 'tests-streams'
method: FileSystemTest
testWriteStream
	| reference stream |
	self markForCleanup: (reference := filesystem workingDirectory / 'griffle').
	stream := reference writeStream.
	stream nextPutAll: 'griffle'.
	stream close.
	self assert: (filesystem workingDirectory / 'griffle') isFile.
	stream := reference writeStream.
	stream close
%

category: 'tests-streams'
method: FileSystemTest
testWriteStreamDo
	| reference |
	self markForCleanup: (reference := filesystem workingDirectory / 'griffle').
	self assert: (reference writeStreamDo: [ :stream |
		stream nextPutAll: 'griffle'.
		true ]).
	self assert: (filesystem workingDirectory / 'griffle') isFile.
	self assert: (reference writeStreamDo: [ :stream | true ])
%

category: 'tests-streams'
method: FileSystemTest
testWriteStreamDoIfPresent
	| reference |
	self markForCleanup: (reference := filesystem workingDirectory / 'griffle').
	self assert: (reference 
		writeStreamDo: [ :stream |
			stream nextPutAll: 'griffle'.
			true ]
		ifPresent: [ false ]).
	self assert: (filesystem workingDirectory / 'griffle') isFile.
	self assert: (reference 
		writeStreamDo: [ :stream | true ]
		ifPresent: [ true ])
%

category: 'tests-streams'
method: FileSystemTest
testWriteStreamIfPresent
	| reference stream |
	self markForCleanup: (reference := filesystem workingDirectory / 'griffle').
	stream := reference writeStreamIfPresent: [ false ].
	stream nextPutAll: 'griffle'.
	stream close.
	self assert: (filesystem workingDirectory / 'griffle') isFile.
	self assert: (reference writeStreamIfPresent: [ true ])
%

category: 'private'
method: FileSystemTest
waitForSeconds: anIngeger
	"#creationTime seems limited to 1 second resolution"
	(Delay forSeconds: anIngeger) wait.
%

! Class implementation for 'ClientFileSystemTest'

!		Instance methods for 'ClientFileSystemTest'

category: 'tests'
method: ClientFileSystemTest
createFileSystem
	^ FileSystem store: ClientStore new
%

category: 'tests'
method: ClientFileSystemTest
testChildrenAt
	| directory entries |
	directory := Path * 'plonk'.
	
	filesystem createDirectory: directory.
	filesystem createDirectory: directory / 'griffle'.
	filesystem createDirectory: directory / 'bint'.
	
	self markForCleanup: directory / 'griffle'.
	self markForCleanup: directory / 'bint'.
	self markForCleanup: directory.
	
	self should: [filesystem childrenAt: directory] raise: Error.

"	self assert: entries size = 2.
	entries do: [ :ea | 
		self assert: (ea isKindOf: Path).
		self assert: ea parent = (filesystem resolve: directory).
		self assert: (#('griffle' 'bint' ) includes: ea basename) ]"
%

category: 'tests'
method: ClientFileSystemTest
testChildrenSorting
	| directory sorted |
	
	directory := Path * 'plonk'.
	
	filesystem createDirectory: directory.
	filesystem createDirectory: directory / 'alfa'.
	filesystem createDirectory: directory / 'beta'.
	
	self markForCleanup: directory / 'alfa'.
	self markForCleanup: directory / 'beta'.
	self markForCleanup: directory.
	
	self should: [sorted := (filesystem childrenAt: directory) sort] raise: Error.
"	self assert: sorted size = 2.
	self assert: (sorted at: 1) basename = 'alfa'.
	self assert: (sorted at: 2) basename = 'beta'."
%

category: 'tests'
method: ClientFileSystemTest
testChildrenSortingRoot
	| directory1 directory2 |
	"self skip."
	
	directory1 := Path * 'plonk1'.
	directory2 := Path * 'plonk2'.
	
	filesystem createDirectory: directory1.
	filesystem createDirectory: directory2.
	
	self markForCleanup: directory1.
	self markForCleanup: directory2.
	self should: [filesystem workingDirectory children] raise: Error
	"self assert: filesystem workingDirectory children sort size = filesystem workingDirectory children size"
%

category: 'tests'
method: ClientFileSystemTest
testCopyWithCorrectBasename
        | directory |
        self
                markForCleanup: 'gooly';
                markForCleanup: 'plonk'.
        directory := filesystem workingDirectory.
        (directory / 'gooly') ensureCreateFile.
        directory / 'gooly' copyTo: directory / 'plonk'.
        self assert: (directory / 'plonk') exists.
		"This test is not run because client does not support directory contents querries"
        "self assert: (directory childNames includes: 'plonk')"
%

category: 'tests'
method: ClientFileSystemTest
testCreateDirectory
	| path directory |
	directory := filesystem workingDirectory.
 	self markForCleanup: directory / 'plonk' / 'griffle'.
	self markForCleanup: directory / 'plonk'.
	path := directory / 'plonk' / 'griffle'.
	(directory / 'plonk') ensureCreateDirectory.
	self shouldnt: [path createDirectory] raise:Error.
	self assert: path exists.
	self should: [(directory / 'plonk' ) deleteAll] raise: Error description: 'GemStone does not support retrieval of client directory entries'
%

category: 'tests'
method: ClientFileSystemTest
testCreateFile
	| directory path |
	directory := filesystem workingDirectory.
 	self markForCleanup: directory / 'plonk' / 'griffles'.
	self markForCleanup: directory / 'plonk'.
	path := directory / 'plonk' / 'griffles'.
	(directory / 'plonk') ensureCreateDirectory.
	self shouldnt: [ path createFile] raise:Error.
	self assert:path exists .
	self should: [(directory / 'plonk' ) deleteAll] raise: Error description: 'GemStone does not support retrieval of client directory entries'
%

category: 'tests'
method: ClientFileSystemTest
testCreation

	| aClientStore |
	aClientStore := ClientStore new.
	filesystem := FileSystem store: aClientStore.
	self assert: aClientStore store class equals: ClientStore activeClass.
	self assert: aClientStore isForeignFileSystem.
	self assert: aClientStore defaultWorkingDirectory notNil.
	self assert: (aClientStore defaultWorkingDirectory isKindOf: Path).
	self assert: filesystem store defaultWorkingDirectory equals: aClientStore defaultWorkingDirectory.
%

category: 'tests'
method: ClientFileSystemTest
testDefaultWorkingDirectory
	| aFileReference |
	aFileReference := filesystem workingDirectory.

	ClientStore activeClass = WindowsStore ifTrue: [
		self assert: (aFileReference path at: 1) = 'C:'.
		self assert: (aFileReference path at: 2) = 'Users'.
		self assert: aFileReference path size = 3.
		^self].

	ClientStore activeClass = UnixStore ifTrue: [
		self assert: (aFileReference path at: 1) = 'Users'.
		^self].

	self assert: false description: 'Something is wrong here - need to fix the test'
%

category: 'tests'
method: ClientFileSystemTest
testDelimiter
	super testDelimiter.
	self assert: filesystem store delimiter = ClientStore activeClass delimiter
%

category: 'tests'
method: ClientFileSystemTest
testEntriesAt
	| directory entries |
	directory := Path * 'plonk'.
	
	self 
		markForCleanup: directory / 'griffle';
		markForCleanup: directory / 'bint';
		markForCleanup: directory.
	self cleanupPaths.

	filesystem createDirectory: directory.
	filesystem createDirectory: directory / 'griffle'.
	filesystem createDirectory: directory / 'bint'.
	
	self should: [entries := filesystem entriesAt: directory] raise: Error description: 'GemStone client does not support requests for directory entries'.
"	self assert: entries size = 2.
	entries do: [ :ea | 
		self assert: (ea isKindOf: FileSystemDirectoryEntry).
		self assert: ea reference parent path = (filesystem resolve: directory).
		self assert: (#('griffle' 'bint' ) includes: ea reference basename).
		self assert: ea isDirectory ]"
%

category: 'tests'
method: ClientFileSystemTest
testEntryAt
	| path1 path2 entry1 entry2  |

	path1 := Path * 'plonk1'.
	path2 := Path * 'plonk2'.
	self markForCleanup: path1.
	self markForCleanup: path2.
	
	filesystem createDirectory: path1.
	self waitForSeconds: 1.
	filesystem createDirectory: path2.
	
	entry1 := filesystem entryAt: path1.
	entry2 := filesystem entryAt: path2.
	
	self assert: entry1 isDirectory.
	self assert: entry2 isDirectory.
	self assert: entry1 reference = (filesystem referenceTo: path1) asAbsolute.
	self assert: entry2 reference = (filesystem referenceTo: path2) asAbsolute.

"	The following tests are removed because the creationTime is a fantasy since we cannot retrieve the client file information.
	self assert: entry1 creationTime < entry2 creationTime.
	self assert: entry1 modificationTime < entry2 modificationTime."
%

category: 'tests'
method: ClientFileSystemTest
testFileNames
	| reference |
	#('test one' 'test with two' 'test-äöü' 'test.äöü') do: [ :each |
		reference := filesystem workingDirectory / each.
		self assert: reference basename = each.
		self deny: reference exists.
		reference
			writeStreamDo: [ :stream | stream nextPutAll: 'gooly' ]
			ifPresent: [ self fail ].
		[	self assert: reference exists.
			"self assert: (filesystem workingDirectory children
				anySatisfy: [ :ref | ref = reference ]) "]
		ensure: [ reference delete ] ]
%

category: 'tests'
method: ClientFileSystemTest
testIsDiskFileSystem
	self assert: filesystem isDiskFileSystem.
%

! Class implementation for 'DiskFileSystemTest'

!		Instance methods for 'DiskFileSystemTest'

category: 'initialize-release'
method: DiskFileSystemTest
createFileSystem
	^ FileSystem store: (DiskStore activeClass createDefault)
%

category: 'tests-streams'
method: DiskFileSystemTest
testBinaryReadWriteStream
	| reference stream |

	self markForCleanup: (reference := filesystem workingDirectory / 'griffle').
	self should: [ stream := reference readRawStream ] raise: FileDoesNotExistException.

	reference writeStreamDo: [:stream | stream nextPutAll: '0123456789' ].
	stream := reference binaryReadWriteStream.
	stream sizeBuffer: 8.

	stream nextPutAll: 'ABCD'.

	self assert: stream peek asCharacter equals: $4. 
"	self assert: stream position equals: 4."
	self assert: stream upToEnd asString equals: '456789'.
"	self assert: stream position equals: 10."
	self assert: stream atEnd.
	stream close.

	self assert: reference contents asString equals: 'ABCD456789'.
%

category: 'tests'
method: DiskFileSystemTest
testDefaultWorkingDirectory
	| ref x y |
	ref := filesystem workingDirectory.
	self assert: ((x := FileSystem fileClass currentWorkingDirectoryPath) beginsWith: (y := ref pathString))
%

category: 'tests'
method: DiskFileSystemTest
testEqual
	| other |
	other := self createFileSystem.
	self assert: filesystem = other
%

category: 'tests'
method: DiskFileSystemTest
testIsDirectory
	self assert: (filesystem isDirectory: FileLocator imageDirectory resolve path)
%

category: 'tests'
method: DiskFileSystemTest
testIsDiskFileSystem
	self assert: filesystem isDiskFileSystem.
	
%

category: 'tests'
method: DiskFileSystemTest
testIsReadable
	self assert: (filesystem store isReadable: FileLocator imageDirectory resolve path)
%

category: 'tests'
method: DiskFileSystemTest
testIsWritable
	| aReference |

	self deny: (filesystem store isWritable: FileLocator imageDirectory resolve path).

	aReference := FileLocator temp / 'plonk'.
    self markForCleanup: aReference resolve path.
	self deny: aReference exists.
	aReference writeStream close. 
	self assert: (filesystem store isWritable: aReference resolve path).
%

category: 'tests'
method: DiskFileSystemTest
testMoveMemoryToDisk
	"Test moving a file from the memory filesystem to the disk filesystem.
	This ensures that the copyAndDelete:to: is called correctly."
	| memfs out testString old new |
	[
		memfs := FileSystem memory.
		old := memfs / 'testMoveMemoryToDisk_old'.
		out := old writeStream.
		testString := 'To be moved to disk'.
		[ out nextPutAll: testString ] ensure: [ out close ].
		
		new := FileLocator imageDirectory / 'testMoveMemoryToDisk_new'.
		old moveTo: new.
		
		self deny: (memfs / 'testMoveMemoryToDisk_old') exists.
		self assert: new exists.
		self assert: new contents equals: testString.
	] ensure: [ 
		old ensureDelete.
		new ensureDelete.
	]
%

category: 'tests'
method: DiskFileSystemTest
testOpenWithOptions
	| aFileOptions  result reference|
	aFileOptions := FileOptions newWrite.
	reference := (FileLocator temp / 'tttt') resolve.
	self markForCleanup: reference.
	result := reference openWithOptions: aFileOptions.
	self assert: (result isKindOf: FileSystem fileClass)
%

! Class implementation for 'MemoryFileSystemTest'

!		Instance methods for 'MemoryFileSystemTest'

category: 'initialize-release'
method: MemoryFileSystemTest
createFileSystem
	^ FileSystem memory
%

category: 'tests'
method: MemoryFileSystemTest
lastModificationTimeOf: fileReference
	"DateAndTime primUTCMicrosecondsClock is not precise across all OS so put in slight delay between calling modificationTime"

	^ [ fileReference modificationTime ]
		ensure: [ (Delay forMilliseconds: 100) wait ]
%

category: 'tests'
method: MemoryFileSystemTest
testCurrentEqual
	| other another |
	another := FileSystem currentMemoryFileSystem.
	other := FileSystem currentMemoryFileSystem.
	self assert: other equals: another
%

category: 'tests'
method: MemoryFileSystemTest
testIsMemoryFileSystem
	self assert: filesystem isMemoryFileSystem.
	
%

category: 'tests'
method: MemoryFileSystemTest
testModifiedTimeWhenFileCreated
	self assert: (filesystem / 'file.txt') ensureCreateFile modificationTime notNil
%

category: 'tests'
method: MemoryFileSystemTest
testModifiedTimeWhenFileModifiedByWriteStream
	| modifiedTime fileReference |
	fileReference := (filesystem / 'file.txt') ensureCreateFile.
	modifiedTime := self lastModificationTimeOf: fileReference.
	fileReference writeStreamDo: [ :aStream | aStream nextPutAll: 'data' ].
	self assert: modifiedTime notNil.
	self deny: modifiedTime equals: fileReference modificationTime
%

category: 'tests'
method: MemoryFileSystemTest
testModifiedTimeWhenFileModifiedWithBinaryWriteStream
	| modifiedTime fileReference data |
	fileReference := (filesystem / 'file.txt') ensureCreateFile.
	modifiedTime := self lastModificationTimeOf: fileReference.
	data := 'some data'.
	fileReference binaryWriteStreamDo: [ :aStream | aStream nextPutAll: data ].
	self assert: modifiedTime notNil.
	self deny: modifiedTime equals: fileReference modificationTime.
	self
		assert: data asByteArray
		equals: (fileReference binaryReadStreamDo: [ :aStream | aStream upToEnd ]).
	self assert: data equals: (fileReference readStreamDo: [ :aStream | aStream contents ])
%

category: 'tests'
method: MemoryFileSystemTest
testModifiedTimeWhenFileWrittenTo
	| modifiedTime fileReference |
	fileReference := (filesystem / 'file.txt') ensureCreateFile.
	modifiedTime := self lastModificationTimeOf: fileReference.
	fileReference binaryWriteStreamDo: [ :aStream | aStream nextPutAll: 'data' ].
	self assert: modifiedTime notNil.
	self deny: modifiedTime equals: fileReference modificationTime
%

category: 'tests'
method: MemoryFileSystemTest
testModifiedTimeWhenHandleTruncated

	| modifiedTime fileReference handle |

	fileReference := (filesystem / 'file.txt') ensureCreateFile.
	handle := fileReference openOptions: FileOptions newWrite.
	modifiedTime := self lastModificationTimeOf: fileReference.
	handle truncate.
	self assert: modifiedTime notNil.
	self deny: modifiedTime equals: fileReference modificationTime
%

category: 'tests'
method: MemoryFileSystemTest
testNotEqualWhenCreatingNewMemoryFileSystem
	| other |
	other := FileSystem memory.
	self deny: other equals: filesystem
%

category: 'tests'
method: MemoryFileSystemTest
testNotEqualWhenRequestingMemoryFileSystem
	| other |
	other := self createFileSystem.
	self deny: other equals: filesystem
%

! Class implementation for 'FileSystemTreeTest'

!		Class methods for 'FileSystemTreeTest'

category: 'testing'
classmethod: FileSystemTreeTest
isAbstract
	^ self name = #FileSystemTreeTest
%

!		Instance methods for 'FileSystemTreeTest'

category: 'running'
method: FileSystemTreeTest
createDirectory: aString
	self subclassResponsibility
%

category: 'running'
method: FileSystemTreeTest
createFile: aString
	self subclassResponsibility
%

category: 'running'
method: FileSystemTreeTest
setUpGreek
	self 
		createDirectory: '/alpha';
		createDirectory: '/alpha/beta';
		createFile: '/alpha/beta/gamma';
		createFile: '/alpha/beta/delta';
		createDirectory: '/alpha/epsilon';
		createFile: '/alpha/epsilon/zeta'
%

! Class implementation for 'CopyVisitorTest'

!		Instance methods for 'CopyVisitorTest'

category: 'running'
method: CopyVisitorTest
createDirectory: aString 
	source createDirectory: (source store pathFromString: aString)
%

category: 'running'
method: CopyVisitorTest
createFile: aString 
	source store createFile: (source store pathFromString: aString)
%

category: 'running'
method: CopyVisitorTest
setUp
	super setUp.
	source := FileSystem memory.
	dest := FileSystem memory
%

category: 'tests'
method: CopyVisitorTest
testAll
	self setUpGreek.
	CopyVisitor 
		copy: (source / 'alpha') 
		to: (dest / 'alpha').
	self assert: (dest isDirectory: '/alpha').
	self assert: (dest isFile: '/alpha/beta/gamma').
%

! Class implementation for 'SingleTreeTest'

!		Class methods for 'SingleTreeTest'

category: 'testing'
classmethod: SingleTreeTest
isAbstract
	^ self name = #SingleTreeTest
%

!		Instance methods for 'SingleTreeTest'

category: 'running'
method: SingleTreeTest
createDirectory: aString 
	filesystem createDirectory: (filesystem pathFromString: aString)
%

category: 'running'
method: SingleTreeTest
createFile: aString 
	filesystem store createFile: (filesystem pathFromString: aString)
%

category: 'running'
method: SingleTreeTest
setUp
	super setUp.
	filesystem := FileSystem memory
%

! Class implementation for 'AbstractEnumerationVisitorTest'

!		Class methods for 'AbstractEnumerationVisitorTest'

category: 'as yet unclassified'
classmethod: AbstractEnumerationVisitorTest
isAbstract
	^ self name = #AbstractEnumerationVisitorTest
%

!		Instance methods for 'AbstractEnumerationVisitorTest'

category: 'utilities'
method: AbstractEnumerationVisitorTest
assertEntries: references are: expected
	| strings |
	self assert: references _isArray.
	references do: [ :ea | self assert: ea class equals: FileSystemDirectoryEntry ].
	strings := references collect: [ :ea | ea reference pathString ].
	self assert: strings equals: expected
%

category: 'accessing'
method: AbstractEnumerationVisitorTest
root
	^ filesystem / 'alpha'
%

category: 'running'
method: AbstractEnumerationVisitorTest
setUp
	super setUp.
	self setUpGreek.
%

! Class implementation for 'CollectVisitorTest'

!		Instance methods for 'CollectVisitorTest'

category: 'tests'
method: CollectVisitorTest
testBreadthFirst
	| entries |
	entries := CollectVisitor breadthFirst: self root.
	self 
		assertEntries: entries
		are: #(
				'/alpha'
				'/alpha/beta'
				'/alpha/epsilon'
				'/alpha/beta/delta'
				'/alpha/beta/gamma'
				'/alpha/epsilon/zeta'
			)
%

category: 'tests'
method: CollectVisitorTest
testPostorder
	| entries |
	entries := CollectVisitor postorder: self root.
	self 
		assertEntries: entries
		are: #(
				'/alpha/beta/delta'
				'/alpha/beta/gamma'
				'/alpha/beta'
				'/alpha/epsilon/zeta'
				'/alpha/epsilon'
				'/alpha'
			)
%

category: 'tests'
method: CollectVisitorTest
testPreorder
	| entries |
	entries := CollectVisitor preorder: self root.
	self 
		assertEntries: entries
		are: #(
				'/alpha'
				'/alpha/beta'
				'/alpha/beta/delta'
				'/alpha/beta/gamma'
				'/alpha/epsilon'
				'/alpha/epsilon/zeta'
			)
%

! Class implementation for 'SelectVisitorTest'

!		Instance methods for 'SelectVisitorTest'

category: 'tests'
method: SelectVisitorTest
testBreadthFirst
	| entries |
	entries := SelectVisitor breadthFirst: self root.
	self 
		assertEntries: entries
		are: #(
				'/alpha'
				'/alpha/beta'
				'/alpha/epsilon'
				'/alpha/beta/delta'
				'/alpha/beta/gamma'
				'/alpha/epsilon/zeta'
			)
%

category: 'tests'
method: SelectVisitorTest
testBreadthFirstSelect
	| entries |
	entries := SelectVisitor breadthFirst: self root select: [ :node| node name endsWith: #a].
	self 
		assertEntries: entries
		are: #(
				'/alpha'
				'/alpha/beta'
				'/alpha/beta/delta'
				'/alpha/beta/gamma'
				'/alpha/epsilon/zeta'
			)
%

category: 'tests'
method: SelectVisitorTest
testPostorder
	| entries |
	entries := SelectVisitor postorder: self root.
	self 
		assertEntries: entries
		are: #(
				'/alpha/beta/delta'
				'/alpha/beta/gamma'
				'/alpha/beta'
				'/alpha/epsilon/zeta'
				'/alpha/epsilon'
				'/alpha'
			)
%

category: 'tests'
method: SelectVisitorTest
testPostorderSelect
	| entries |
	entries := SelectVisitor postorder: self root select: [ :node| node name endsWith: #a].
	self 
		assertEntries: entries
		are: #(
				'/alpha/beta/delta'
				'/alpha/beta/gamma'
				'/alpha/beta'
				'/alpha/epsilon/zeta'
				'/alpha'
			)
%

category: 'tests'
method: SelectVisitorTest
testPreorder
	| entries |
	entries := SelectVisitor preorder: self root.
	self 
		assertEntries: entries
		are: #(
				'/alpha'
				'/alpha/beta'
				'/alpha/beta/delta'
				'/alpha/beta/gamma'
				'/alpha/epsilon'
				'/alpha/epsilon/zeta'
			)
%

category: 'tests'
method: SelectVisitorTest
testPreorderSelect
	| entries |
	entries := SelectVisitor preorder: self root select: [ :node| node name endsWith: #a].
	self 
		assertEntries: entries
		are: #(
				'/alpha'
				'/alpha/beta'
				'/alpha/beta/delta'
				'/alpha/beta/gamma'
				'/alpha/epsilon/zeta'
			)
%

! Class implementation for 'DeleteVisitorTest'

!		Instance methods for 'DeleteVisitorTest'

category: 'tests'
method: DeleteVisitorTest
testBeta
	self setUpGreek.
	DeleteVisitor delete: (filesystem / 'alpha' / 'beta').
	self assert: (filesystem isDirectory: '/alpha').
	self assert: (filesystem isDirectory: '/alpha/epsilon').
	self deny: (filesystem exists: '/alpha/beta').
	
%

! Class implementation for 'GuideTest'

!		Class methods for 'GuideTest'

category: 'testing'
classmethod: GuideTest
isAbstract
	^ self name = #GuideTest
%

!		Instance methods for 'GuideTest'

category: 'asserting'
method: GuideTest
assertVisitedIs: anArray
	visited with: anArray do:
		[:entry :basename | 
		self assert: entry reference basename = basename]
%

category: 'running'
method: GuideTest
setUp
	super setUp.
	visited := OrderedCollection new.
	filesystem := FileSystem memory.
	self setUpGreek
%

category: 'visitor'
method: GuideTest
visitDirectory: aReference
	visited add: aReference.
%

category: 'visitor'
method: GuideTest
visitFile: aReference
	visited add: aReference.
%

! Class implementation for 'BreadthFirstGuideTest'

!		Instance methods for 'BreadthFirstGuideTest'

category: 'tests'
method: BreadthFirstGuideTest
testAll
	guide := BreadthFirstGuide for: self.
	guide show: (filesystem / 'alpha').
	self assertVisitedIs: #(
			'alpha'
			'beta'
			'epsilon'
			'delta'
			'gamma'
			'zeta'
		)
%

! Class implementation for 'PostorderGuideTest'

!		Instance methods for 'PostorderGuideTest'

category: 'tests'
method: PostorderGuideTest
testAll
	guide := PostorderGuide for: self.
	guide show: (filesystem / 'alpha').
	self assertVisitedIs: #(
			'delta'
			'gamma'
			'beta'
			'zeta'
			'epsilon'
			'alpha'
		)
%

! Class implementation for 'PreorderGuideTest'

!		Instance methods for 'PreorderGuideTest'

category: 'tests'
method: PreorderGuideTest
testAll
	guide := PreorderGuide for: self.
	guide show: (filesystem / 'alpha').
	self assertVisitedIs: #(
			'alpha'
			'beta'
			'delta'
			'gamma'
			'epsilon'
			'zeta'
		)
%

! Class implementation for 'FSGemStoneKernelTests'

!		Instance methods for 'FSGemStoneKernelTests'

category: 'private'
method: FSGemStoneKernelTests
collectionMoreThan1NoDuplicates

	^self collectionWithoutEqualElements
%

category: 'private'
method: FSGemStoneKernelTests
collectionWithoutEqualElements

	^ {1.1. 4.4. 6.5. 2.4. 3.1.}
%

category: 'private'
method: FSGemStoneKernelTests
collectionWithSameAtEndAndBegining

	^ {1.5. 5.5. 1.5 copy}.
%

category: 'private'
method: FSGemStoneKernelTests
elementNotInForIndexAccessing

	^ 9
%

category: 'private'
method: FSGemStoneKernelTests
empty

	^ #().
%

category: 'private'
method: FSGemStoneKernelTests
indexInForCollectionWithoutDuplicates
	^ 2.
%

category: 'private'
method: FSGemStoneKernelTests
indexInNonEmpty
	"Return an index between bounds of 'nonEmpty'"
	
	^ 2
%

category: 'private'
method: FSGemStoneKernelTests
moreThan3Elements

	^ #(1 2 3 4 5) copy.
%

category: 'private'
method: FSGemStoneKernelTests
nonEmpty

	^ self moreThan3Elements
%

category: 'tests'
method: FSGemStoneKernelTests
testAllButFirst

	| abf col |
	col := self moreThan3Elements.
	abf := col allButFirst.
	self deny: abf first = col first.
	self assert: abf size + 1 = col size
%

category: 'tests'
method: FSGemStoneKernelTests
testAllButFirstNElements

	| allButFirst collection |
	collection := self moreThan3Elements.
	allButFirst := collection allButFirst: 2.
	allButFirst withIndexDo: 
		 [:el :i | self assert: el equals: (collection at: i + 2) ].
	self assert: allButFirst size + 2 equals: collection size
%

category: 'tests'
method: FSGemStoneKernelTests
testCopyAfterLast

	| result index collection |
	collection := self collectionWithoutEqualElements .
	index:= self indexInForCollectionWithoutDuplicates .
	result := collection copyAfterLast: (collection  at:index ).
	
	"Verify content"
	result withIndexDo: 
		[:el :i | self assert: (collection at: (i + index )) equals: (result at: i)].

	"Verify size"
	self assert: result size equals: (collection size - index)
%

category: 'tests'
method: FSGemStoneKernelTests
testCopyUpToLast

	| result index collection |
	collection := self collectionWithoutEqualElements.
	index:= self indexInForCollectionWithoutDuplicates.
	result := collection copyUpToLast: (collection at:index).
	
	"Verify content"
	result withIndexDo: [:el :i| self assert: (collection at:i) equals: (result at: i)].
	
	"Verify size"
	self assert: result size equals: (index-1)
%

category: 'tests'
method: FSGemStoneKernelTests
testCopyWithFirst

	| index element result |
	index:= self indexInNonEmpty .
	element:= self nonEmpty at: index.
	
	result := self nonEmpty copyWithFirst: element.	
	
	self assert: result size = (self nonEmpty size + 1).
	self assert: result first = element .
	
	2 to: result size do:
	[ :i |
	self assert: (result at: i) = ( self nonEmpty at: ( i - 1 ))].
%

category: 'tests'
method: FSGemStoneKernelTests
testDifference
	"Answer the set theoretic difference of two collections."
	
	| difference |
	self assert: (self collectionWithoutEqualElements difference: self collectionWithoutEqualElements) isEmpty.
	self assert: (self empty difference: self collectionWithoutEqualElements) isEmpty.
	difference := (self collectionWithoutEqualElements difference: self empty).
	self assert: difference size = self collectionWithoutEqualElements size.
	self collectionWithoutEqualElements do: [ :each |
		self assert: (difference includes: each) ].
%

category: 'tests'
method: FSGemStoneKernelTests
testLastIndexOfIfAbsent

	| element collection |
	collection := self collectionMoreThan1NoDuplicates.
	element := collection first.
	self assert: (collection 
			lastIndexOf: element
			ifAbsent: [ 99 ]) equals: 1.
	self assert: (collection 
			lastIndexOf: self elementNotInForIndexAccessing
			ifAbsent: [ 99 ]) equals: 99
%

category: 'tests'
method: FSGemStoneKernelTests
testLastIndexOfIfAbsentDuplicate

	| collection element |
	collection := self collectionWithSameAtEndAndBegining.
	element := collection first.

	"floatCollectionWithSameAtEndAndBegining first and last elements are equals 
	'lastIndexOf: should return the position of the last occurrence :'"
	self assert: (collection 
			lastIndexOf: element
			ifAbsent: [ 55 ]) equals: collection size
%

category: 'tests'
method: FSGemStoneKernelTests
testLastIndexOfStartingAt

	| element collection |
	collection := self collectionMoreThan1NoDuplicates.
	element := collection last.
	self assert: (collection 
			lastIndexOf: element
			startingAt: collection size
			ifAbsent: [ 99 ]) equals: collection size.
	self assert: (collection 
			lastIndexOf: element
			startingAt: collection size - 1
			ifAbsent: [ 99 ]) equals: 99.
	self assert: (collection 
			lastIndexOf: self elementNotInForIndexAccessing
			startingAt: collection size
			ifAbsent: [ 99 ]) equals: 99
%

category: 'tests'
method: FSGemStoneKernelTests
testLastIndexOfStartingAtDuplicate

	| collection element |
	collection := self collectionWithSameAtEndAndBegining.
	element := collection last.

	" floatCollectionWithSameAtEndAndBegining first and last elements are equals 
	'lastIndexOf:ifAbsent:startingAt: should return the position of the last occurrence :'"
	self assert: (collection 
			lastIndexOf: element
			startingAt: collection size
			ifAbsent: [ 55 ]) equals: collection size.
	self assert: (collection 
			lastIndexOf: element
			startingAt: collection size - 1
			ifAbsent: [ 55 ]) equals: 1
%

! Class implementation for 'NumberParserTest'

!		Instance methods for 'NumberParserTest'

category: 'utilities'
method: NumberParserTest
areLowercaseDigitsAllowed
	"Answer true if lowercase letter are allowed digits."
	
	^(NumberParser parse: '16re' onError: [-1]) = 16rE
%

category: 'tests - Integer'
method: NumberParserTest
testcheckForCoverage	
	"Tests for old semantics of Number>>readFrom:"
	
	self should: [(NumberParser parse: '.') = 0 ] raise: Error.
	self should: [(NumberParser parse: '.1') asNumber ] raise: Error.
	self assert: (NumberParser parse: '0.0') asNumber equals: 0.
	self assert: (NumberParser parse: '0.1') asNumber equals: 0.1.
	self assert: (NumberParser parse: '1.1') asNumber equals: 1.1.
	self assert: (NumberParser parse: '-1') asNumber equals: -1.
%

category: 'tests - fail'
method: NumberParserTest
testFail
	"Verify that the value of a failblock is returned."
	self assert: (NumberParser parse: 'blablabla' onError: [42]) equals: 42
%

category: 'test - Float'
method: NumberParserTest
testFloatFromStreamAsNumber
	"This covers parsing in Number>>readFrom:"

	| rs aFloat |
	rs := '10r-12.3456' readStreamPortable.
	aFloat := NumberParser parse: rs.
	self assert: -12.3456 equals: aFloat.
	self assert: rs atEnd.

	rs := '10r-12.3456e2' readStreamPortable.
	aFloat := NumberParser parse: rs.
	self assert: -1234.56 equals: aFloat.
	self assert: rs atEnd.

	rs := '10r-12.3456e2e2' readStreamPortable.
	aFloat := NumberParser parse: rs.
	self assert: -1234.56 equals: aFloat.
	self assert: rs upToEnd equals: 'e2'.

	rs := '10r-12.3456d2' readStreamPortable.
	aFloat := NumberParser parse: rs.
	self assert: -1234.56 equals: aFloat.
	self assert: rs atEnd.

	rs := '10r-12.3456q2' readStreamPortable.
	aFloat := NumberParser parse: rs.
	self assert: -1234.56 equals: aFloat.
	self assert: rs atEnd.

	rs := '-12.3456q2' readStreamPortable.
	aFloat := NumberParser parse: rs.
	self assert: -1234.56 equals: aFloat.
	self assert: rs atEnd.

	rs := '12.3456q2' readStreamPortable.
	aFloat := NumberParser parse: rs.
	self assert: 1234.56 equals: aFloat.
	self assert: rs atEnd.

	rs := '12.3456z2' readStreamPortable.
	aFloat := NumberParser parse: rs.
	self assert: 12.3456 equals: aFloat.
	self assert: rs upToEnd equals: 'z2'.
%

category: 'test - Float'
method: NumberParserTest
testFloatFromStreamWithExponent
	"This covers parsing in Number>>readFrom:"

	| rs aFloat |
	rs := '1.0e-14' readStream.
	aFloat := NumberParser parse: rs.
	self assert: 1.0e-14 equals: aFloat.
	self assert: rs atEnd.

	rs := '1.0e-14 1' readStream.
	aFloat := NumberParser parse: rs.
	self assert: 1.0e-14 equals: aFloat.
	self assert: rs upToEnd equals: ' 1'.

	rs := '1.0e-14eee' readStream.
	aFloat := NumberParser parse: rs.
	self assert: 1.0e-14 equals: aFloat.
	self assert: rs upToEnd equals: 'eee'.

	rs := '1.0e14e10' readStream.
	aFloat := NumberParser parse: rs.
	self assert: 1.0e14 equals: aFloat.
	self assert: rs upToEnd equals: 'e10'.

	rs := '1.0e+14e' readStream. "Plus sign is not parseable"
	aFloat := NumberParser parse: rs.
	self assert: 1.0 equals: aFloat.
	self assert: rs upToEnd equals: 'e+14e'.

	rs := '1.0e' readStream.
	aFloat := NumberParser parse: rs.
	self assert: 1.0 equals: aFloat.
	self assert: rs upToEnd equals: 'e'.
%

category: 'test - Float'
method: NumberParserTest
testFloatGradualUnderflow
	"Gradual underflow are tricky.
	This is a non regression test for http://bugs.squeak.org/view.php?id=6976"

	| float trueFraction str |
	
	"as a preamble, use a base 16 representation to avoid round off error and check that number parsing is correct"
	trueFraction := 16r2D2593D58B4FC4 / (16 raisedTo: 256+13).
	"Parse the number in base 16 if possible - it is impossible if lowercase letter are allowed digits due to exponent letter ambiguity."
	float := self areLowercaseDigitsAllowed
		ifFalse: [NumberParser parse: '16r2.D2593D58B4FC4e-256']
		ifTrue: [trueFraction asFloat].
	self assert: float asTrueFraction equals: trueFraction.
	self assert: float equals: trueFraction asFloat.

	"now print in base 10"
	str := (String new: 32) writeStream.
	float absPrintExactlyOn: str base: 10.
	
	"verify if SqNumberParser can read it back"
	self assert: (NumberParser parse: str contents) equals: float.
%

category: 'test - Float'
method: NumberParserTest
testFloatMaxAndMin
	"This covers parsing in Number>>readFrom:"

	| rs aFloat |
	rs := '2r0.0000000000000000000000000000000000000000000000000001e-1022'
		readStream.
	aFloat := NumberParser parse: rs.
	self assert: Float fminDenormalized equals: aFloat.
	self assert: rs atEnd.
	rs := '-2r0.0000000000000000000000000000000000000000000000000001e-1022'
		readStream.
	aFloat := NumberParser parse: rs.
	self assert: Float fminDenormalized negated equals: aFloat.
	self assert: rs atEnd.
	rs := '2r1.1111111111111111111111111111111111111111111111111111e1023'
		readStream.
	aFloat := NumberParser parse: rs.
	self assert: Float fmax equals: aFloat.
	self assert: rs atEnd
%

category: 'test - Float'
method: NumberParserTest
testFloatmin
	"Note that these are originally tests cases for former bugs of libc dtoa from netlib.
	ref http://www.exploringbinary.com/gays-strtod-returns-zero-for-inputs-just-above-2-1075/
	ref http://gcc.gnu.org/viewcvs/gcc/trunk/gcc/testsuite/gcc.dg/float-exact-1.c?view=markup&pathrev=205119
	They are also non regression for a bug of NumberParser related to incorrect position of last non zero digit.
	ref https://pharo.manuscript.com/f/cases/12642/"
	| halfMin moreThanHalfmin |
	halfMin := NumberParser parse: (Float fmin asTrueFraction / 2 printShowingDecimalPlaces: 1 - Float fmin exponent).
	self assert: halfMin = 0.0 description: 'nearest even of 0.5*Float fmin is zero'.
	moreThanHalfmin := NumberParser parse: (Float fmin asTrueFraction / 2 + (10 raisedTo: Float fmin exponent - 4) printShowingDecimalPlaces: 4 - Float fmin exponent).
	self assert: moreThanHalfmin = Float fmin description: 'nearest Float of a Fraction > 0.5*Float fmin is Float fmin'.
%

category: 'test - Float'
method: NumberParserTest
testFloatPrintString

	" This does not run in GemStone as one cannot use #at:put: with Float"
		
"	| f r bases |
	f := Float basicNew: 2.
	r := Random new seed: 1234567.
	'printing a Float in base other than 10 is broken if lowercase digits are allowed'
	bases := self areLowercaseDigitsAllowed
		ifTrue: [#(10)]
		ifFalse: [#(2 8 10 16)].
	100
		timesRepeat: [f basicAt: 1 put: (r nextInt: 16r100000000)- 1.
			f basicAt: 2 put: (r nextInt: 16r100000000) - 1.
			bases
				do: [:base | | str |
						str := (String new: 64) writeStream.
						f negative ifTrue: [str nextPut: $-].
						str print: base; nextPut: $r.
						f absPrintExactlyOn: str base: base.
						self assert: (NumberParser parse: str contents) = f]].
	'test big num near infinity'
	10
		timesRepeat: [f basicAt: 1 put: 16r7FE00000 + ((r nextInt: 16r100000) - 1).
			f basicAt: 2 put: (r nextInt: 16r100000000) - 1.
			bases
				do: [:base | | str |
						str := (String new: 64) writeStream.
						f negative ifTrue: [str nextPut: $-].
						str print: base; nextPut: $r.
						f absPrintExactlyOn: str base: base.
						self assert: (NumberParser parse: str contents) = f]].
	'test infinitesimal (gradual underflow)'
	10
		timesRepeat: [f basicAt: 1 put: 0 + ((r nextInt: 16r100000) - 1).
			f basicAt: 2 put: (r nextInt: 16r100000000) - 1.
			bases
				do: [:base | | str |
						str := (String new: 64) writeStream.
						f negative ifTrue: [str nextPut: $-].
						str print: base; nextPut: $r.
						f absPrintExactlyOn: str base: base.
						self assert: (NumberParser parse: str contents) = f]].
"
%

category: 'test - Float'
method: NumberParserTest
testFloatReadError
	"This covers parsing in Number>>readFrom:"

	| rs num |
	rs := '1e' readStream.
	num := NumberParser parse: rs.
	self assert: 1 equals: num.
	self assert: rs upToEnd equals: 'e'.
	
	rs := '1s' readStream.
	num := NumberParser parse: rs.
	self assert: 1 equals: num.
	self assert: rs upToEnd equals: ''.

	rs := '1.' readStream.
	num := NumberParser parse: rs.
	self assert: 1 equals: num.
	self assert: num _isInteger.
	self assert: rs upToEnd equals: '.'.
	
	rs := '' readStream.
	self should: [NumberParser parse: rs] raise: Error.
	
	rs := 'foo' readStream.
	self should: [NumberParser parse: rs] raise: Error.

	rs := 'radix' readStream.
	self should: [NumberParser parse: rs] raise: Error.
	
	rs := '.e0' readStream.
	self should: [NumberParser parse: rs] raise: Error.
	
	rs := '-.e0' readStream.
	self should: [NumberParser parse: rs] raise: Error.
	
	rs := '--1' readStream.
	self should: [NumberParser parse: rs] raise: Error.
%

category: 'test - Float'
method: NumberParserTest
testFloatReadWithRadix
	"This covers parsing in Number>>readFrom:
	Note: In most Smalltalk dialects, the radix notation is not used for numbers
	with exponents. In Squeak, a string with radix and exponent can be parsed,
	and the exponent is always treated as base 10 (not the base indicated in the
	radix prefix). I am not sure if this is a feature, a bug, or both, but the
	Squeak behavior is documented in this test. -dtl"
	| rs |
	self assert: (NumberParser parse: '2r1.0101e9') equals: (1.3125 * (2 raisedTo: 9)).
	rs := '2r1.0101e9e9' readStream.
	self assert: (NumberParser parse: rs) equals: 672.0.
	self assert: rs upToEnd equals: 'e9'
%

category: 'tests - Integer'
method: NumberParserTest
testIntegerReadFrom
	"Ensure remaining characters in a stream are not lost when parsing an integer."
	| rs i s |
	rs := '123s could be confused with a ScaledDecimal' readStream.
	i := NumberParser parse: rs.
	self assert: i equals: 123.
	s := rs upToEnd.
	self assert: ' could be confused with a ScaledDecimal' equals: s.
	rs := '123.s could be confused with a ScaledDecimal' readStream.
	i := NumberParser parse: rs.
	self assert: i equals: 123.
	s := rs upToEnd.
	self assert: '.s could be confused with a ScaledDecimal' equals: s
%

category: 'tests - Integer'
method: NumberParserTest
testIntegerReadWithRadix
	"This covers parsing in Number>>readFrom:
	Note: In most Smalltalk dialects, the radix notation is not used for numbers
	with exponents. In Squeak, a string with radix and exponent can be parsed,
	and the exponent is always treated as base 10 (not the base indicated in the
	radix prefix). I am not sure if this is a feature, a bug, or both, but the
	Squeak behavior is documented in this test. -dtl"

	| rs |
	self assert: (NumberParser parse: '2r1e26') equals: (2 raisedTo: 26).
	rs := '2r1e26eee' readStream.
	self assert: (NumberParser parse: rs) equals: 67108864.
	self assert: rs upToEnd equals: 'eee'
%

category: 'test - Float'
method: NumberParserTest
testIntegerWithNegExponentIsAFloat
	"Make sure a float literal like 1e(some possible neg exponent)
	isn't evaluated to the non-literal Fraction"

	| rs aFloat |
	rs := '1e-14' readStreamPortable.
	aFloat := NumberParser parse: rs.
	self assert: aFloat _isFloat.

	rs := '1e-14' readStreamPortable.
	aFloat := (NumberParser on: rs ) nextNumberBase: 10.
	self assert: aFloat _isFloat.
%

category: 'test - Float'
method: NumberParserTest
testIsNumber

	self assert: (NumberParser isNumber: '-1.2') equals: true.
	self assert: (NumberParser isNumber: '2e-2') equals: true.
	
	self assert: (NumberParser isNumber: '') equals: false.
	self assert: (NumberParser isNumber: '2a') equals: false.
	self assert: (NumberParser isNumber: '--1') equals: false.
	self assert: (NumberParser isNumber: '1-') equals: false.
	self assert: (NumberParser isNumber: '1..2') equals: false.
%

category: 'tests - ScaledDecimal'
method: NumberParserTest
testScaledDecimalWithoutScaleSpecification
	self assert: (NumberParser parse: '0.050s') equals: 1/20.
	self assert: (NumberParser parse: '0.050s') scale equals: 3.
%

category: 'tests - ScaledDecimal'
method: NumberParserTest
testScaledDecimalWithTrailingZeroes
	"This is a non regression tests for http://bugs.squeak.org/view.php?id=7169"
	
	self assert: (NumberParser parse: '0.50s2') equals: 1/2.
	self assert: (NumberParser parse: '0.500s3') equals: 1/2.
	self assert: (NumberParser parse: '0.050s3') equals: 1/20.
%

category: 'tests - squeezing'
method: NumberParserTest
testSqueezingOutNumbers
	"test that SqNumberParser squeezeNumberOutOfString finds numbers."
	
	self assert: '123blabla' squeezeOutNumber equals: 123.
	self assert: 'blabla123' squeezeOutNumber equals: 123.
	self assert: 'blabla12blabla' squeezeOutNumber equals: 12.
	self assert: ('12.3bla' squeezeOutNumber -12.3 ) abs < 0.0001.
	self assert: '.1' squeezeOutNumber > 0.
	
	self assert: 'blabla1230' squeezeOutNumber equals: 1230.
%

! Class implementation for 'PathTest'

!		Instance methods for 'PathTest'

category: 'tests'
method: PathTest
testAbsolutePath

	| path |

	self assert: (AbsolutePath new isAbsolute).
	self assert: (Path root isAbsolute).
	
	path := AbsolutePath from: 'parent/child/grandChild' delimiter: $/.
	self assert: path size equals: 3.
	self assert: (path at: 1) equals: 'parent'.
	self assert: (path at: 2) equals: 'child'.
	self assert: (path at: 3) equals: 'grandChild'.
	
	path := AbsolutePath from: '/' delimiter: $/.
	self assert: path equals: Path root.
	
%

category: 'tests'
method: PathTest
testAbsolutePrintString
	
	| path actual |
	path := Path / 'plonk' / 'griffle'.
	actual := path printString.
	self assert: actual equals: 'Path / ''plonk'' / ''griffle'''
%

category: 'tests'
method: PathTest
testAbsoluteWithParents
	| path allPaths |
	path := Path / 'plonk' / 'griffle' / 'nurb'.
	allPaths := path withParents.
	
	self assert: allPaths size equals: 4.
	self assert: allPaths first isRoot.
	self assert: allPaths second basename equals: 'plonk'.
	self assert: allPaths second size equals: 1.
	self assert: (allPaths second isChildOf: allPaths first).
	self assert: allPaths third basename equals: 'griffle'.
	self assert: allPaths third size equals: 2.
	self assert: (allPaths third isChildOf: allPaths second).
	self assert: allPaths fourth basename equals: 'nurb'.
	self assert: allPaths fourth size equals: 3.
	self assert: (allPaths fourth isChildOf: allPaths third).
	
	self assert: allPaths fourth equals: path.
	self assert: allPaths fourth == path
%

category: 'tests'
method: PathTest
testAsReference
	| path reference |
	path := Path * 'plonk'.
	reference := path asFileReference.
	self assert: reference class equals: FileReference.
	self assert: reference path equals: path
%

category: 'tests'
method: PathTest
testBasename
	| path |
	path := Path * 'plonk' / 'griffle'.
	self assert: path basename equals: 'griffle'
%

category: 'tests'
method: PathTest
testBasenameNoParent
	| path |

	path := Path / 'griffle'.
	self assert: path parent basename equals: '/'.

	path := Path * 'griffle'.
	self assert: path parent basename equals: '.'.
%

category: 'tests'
method: PathTest
testBasenameWithoutExtension
	
	| path |
	path := Path * 'plonk' / 'griffle'.
	self assert: path basenameWithoutExtension equals: 'griffle'.
	self assert: (path basenameWithoutExtension: 'griffle') equals: 'griffle'.
	self assert: (path basenameWithoutExtension: 'taz') equals: 'griffle'.
	
	path := Path * 'plonk' / 'griffle.taz'.
	self assert: path basenameWithoutExtension equals: 'griffle'.
	self assert: (path basenameWithoutExtension: 'taz') equals: 'griffle'.
	self assert: (path basenameWithoutExtension: 'griffle.taz') equals: 'griffle.taz'.
	self assert: (path basenameWithoutExtension: 'zork') equals: 'griffle.taz'.
	
	path := Path * 'plonk' / 'griffle.taz.zork'.
	self assert: path basenameWithoutExtension equals: 'griffle.taz'.
	self assert: (path basenameWithoutExtension: 'zork') equals: 'griffle.taz'.
	self assert: (path basenameWithoutExtension: 'taz.zork') equals: 'griffle'.
	self assert: (path basenameWithoutExtension: 'girffle.taz.zork') equals: 'griffle.taz.zork'.
	self assert: (path basenameWithoutExtension: 'taz') equals: 'griffle.taz.zork'.
%

category: 'tests'
method: PathTest
testCanonicalization

	| ref |

	ref := (Path * 'a/b/c') canonicalize.
	self assert: ref segments equals: #('a' 'b' 'c').

	ref := (Path / 'a/b/c') canonicalize.
	self assert: ref segments equals: #('a' 'b' 'c').

	ref := (Path * '../a/b/c') canonicalize.
	self assert: ref segments equals: #('..' 'a' 'b' 'c').

	ref := (Path * 'a/b/c/..') canonicalize.
	self assert: ref segments equals: #('a' 'b').

	ref := (Path / 'a/b/c/..') canonicalize.
	self assert: ref segments equals: #('a' 'b').

	ref := (Path * 'a/b/../c') canonicalize.
	self assert: ref segments equals: #('a' 'c').

	ref := (Path / 'a/b/../c') canonicalize.
	self assert: ref segments equals: #('a' 'c').
%

category: 'tests'
method: PathTest
testCommaAddsExtension
	| path result |
	path := Path * 'plonk' .
	result := path, 'griffle'.
	self assert: result basename equals: 'plonk.griffle'
%

category: 'tests'
method: PathTest
testCommaAddsExtensionAgain
	| path result |
	path := Path * 'plonk.griffle'.
	result := path, 'nurp'.
	self assert: result basename equals: 'plonk.griffle.nurp'
%

category: 'tests'
method: PathTest
testContains
	| ancestor descendent |
	ancestor := Path / 'plonk'.
	descendent := Path / 'plonk' / 'griffle' / 'bork'.
	self assert: (ancestor contains: descendent).
	self deny: (descendent contains: ancestor)
%

category: 'tests'
method: PathTest
testContainsLocator
	| ancestor descendent |
	ancestor := FileLocator imageDirectory resolve path.
	descendent := FileLocator image / 'griffle'.
	self deny: (ancestor contains: descendent).
	self deny: (descendent contains: ancestor)
%

category: 'tests'
method: PathTest
testEqual
	| a b |
	a := Path * 'plonk'.
	b := Path * 'plonk'.
	self deny: a == b.
	self assert: a equals: b.
%

category: 'tests'
method: PathTest
testExtendingPath

	| ref |

	self should: [ '/a/b' asPath / '' ] raise: Error.
	self should: [ '/a/b' asPath / nil ] raise: Error.

	ref := '/a/b/c' asPath / 'd/e'.
	self assert: ref segments equals: #('a' 'b' 'c' 'd' 'e').

	ref := '/a/b/c' asPath / 'd/e'.
	self assert: ref parent segments equals: #('a' 'b' 'c' 'd').

	ref := '/a/b/c' asPath / '../d'.
	self assert: ref segments equals:  #('a' 'b' 'c' '..' 'd').

	ref := '/a/b/c' asPath / 'd/..'.
	self assert: ref segments equals: #('a' 'b' 'c' 'd' '..').

	ref := '/a/b/c' asPath / 'd/../e'.
	self assert: ref segments equals: #('a' 'b' 'c' 'd' '..' 'e').

	ref := '/a/b/c' asPath / './d'.
	self assert: (ref segments = #('a' 'b' 'c' 'd')).

	ref := '/a/b/c' asPath / 'd/.'.
	self assert: (ref segments = #('a' 'b' 'c' 'd')).

	ref := '/a/b/c' asPath / 'd/./e'.
	self assert: ref segments equals: #('a' 'b' 'c' 'd' 'e').

%

category: 'tests'
method: PathTest
testExtensions
	self 
		assertCollection: (Path from: 'foo') extensions asArray
		equals: #().
	self
		assertCollection: (Path from: 'foo.tar') extensions asArray
		equals: #( 'tar' ).
	self
		assertCollection: (Path from: 'foo.tar.gz') extensions asArray
		equals: #( 'tar' 'gz').
	self
		assertCollection: (Path from: 'foo.1.tar.gz') extensions asArray
		equals: #( '1' 'tar' 'gz').
%

category: 'tests'
method: PathTest
testFullName

	| path |

	path := (FileSystem workingDirectory / 'book-result' / 'W01-Welcome')
				relativeToReference: FileSystem workingDirectory.
	self assert: path fullName equals: 'book-result/W01-Welcome'
%

category: 'tests'
method: PathTest
testGrandchildOfPath
	| griffle  nurb |
	griffle := Path / 'griffle'.
	nurb := griffle / 'plonk' / 'nurb'.
	self deny: (griffle isChildOf: nurb).
	self deny: (nurb isChildOf: griffle).
%

category: 'tests'
method: PathTest
testIsAbsolute
	self assert: (Path / 'plonk') isAbsolute
%

category: 'tests'
method: PathTest
testIsAbsoluteWindowsPathReturnsFalseWhenNoWindowsAbsolutePathProvided
  
	self deny: (Path isAbsoluteWindowsPath: 'tmp').
	self deny: (Path isAbsoluteWindowsPath: '/tmp').
	self deny: (Path isAbsoluteWindowsPath: '/tmp/test').
%

category: 'tests'
method: PathTest
testIsAbsoluteWindowsPathReturnsTrueWhenWindowsAbsolutePathProvided
  
	self assert: (Path isAbsoluteWindowsPath: 'A:\').
	self assert: (Path isAbsoluteWindowsPath: 'c:\').
	self assert: (Path isAbsoluteWindowsPath: 'c:\test').
%

category: 'tests'
method: PathTest
testIsChildOfPath
	| parent child |
	parent := Path / 'griffle'.
	child := parent / 'nurb'.
	self assert: (child isChildOf: parent).
	self deny: (parent isChildOf: child)
%

category: 'tests'
method: PathTest
testIsChildOfReference
	| parent child |
	parent := Path / 'griffle'.
	child := FileSystem memory referenceTo: parent / 'nurb'.
	self deny: (child isChildOf: parent).
	self deny: (parent isChildOf: child)
%

category: 'tests'
method: PathTest
testIsEmpty
	self assert: (Path workingDirectory) isEmpty
%

category: 'tests'
method: PathTest
testIsNotAbsolute
	self deny: (Path * 'plonk') isAbsolute
%

category: 'tests'
method: PathTest
testIsNotRelative
	self deny: (Path / 'plonk') isRelative
%

category: 'tests'
method: PathTest
testIsNotRoot
	self deny: (Path / 'plonk') isRoot
%

category: 'tests'
method: PathTest
testIsRelative
	self assert: (Path * 'plonk') isRelative
%

category: 'tests'
method: PathTest
testIsRoot
	self assert: Path root isRoot
%

category: 'tests'
method: PathTest
testMakeRelative
	
	| parent child relative |
	parent := Path / 'griffle' / 'bibb'.
	child := Path / 'griffle' / 'plonk' / 'nurp'.
	relative := parent makeRelative: child.
	self assert: relative equals: (Path parent / 'plonk' / 'nurp')
%

category: 'tests'
method: PathTest
testMakeRelativeFrom2RelativePaths
	"Related to issue: 14846 MakeRelative-method-applied-on-two-relative-paths-is-not-working"

	| parent child relative |
	parent := RelativePath new / 'griffle' / 'bibb'.
	child := RelativePath new / 'griffle' / 'plonk' / 'nurp'.
	relative := parent makeRelative: child.
	self assert: relative equals: Path parent / 'plonk' / 'nurp'
%

category: 'tests'
method: PathTest
testParent
	| path parent |
	path := (Path * 'plonk') / 'griffle'.
	parent := path parent.
	self assert: parent isRelative.
	self assert: (parent at: 1) equals: 'plonk'
%

category: 'tests'
method: PathTest
testParentParent
	| path  |
	path := (Path * '..') parent.
	self assert: path size equals: 2.
	self assert: (path at: 1) equals: '..'.
	self assert: (path at: 2) equals: '..'.
%

category: 'tests'
method: PathTest
testParentResolution
	| base relative absolute |
	base := Path / 'plonk' / 'pinto'.
	relative := Path parent / 'griffle' / 'zonk'.
	absolute := base resolve: relative.
	self assert: absolute isAbsolute.
	self assert: absolute segments equals: #('plonk' 'pinto' '..' 'griffle' 'zonk').
%

category: 'tests'
method: PathTest
testParentUpTo
	| a b c |
	a := Path / 'testParentUpTo' / 'A'.
	b := Path / 'testParentUpTo' / 'A' / 'B'.
	c := Path / 'testParentUpTo' / 'A' / 'B' / 'C'.
	self assert: b equals: (c parentUpTo: 'B').
	self assert: a equals: (c parentUpTo: 'A').
	self assert: Path / 'testParentUpTo' equals: (c parentUpTo: 'testParentUpTo').
	self assert: Path root equals: (c parentUpTo: 'notAParent')
%

category: 'tests'
method: PathTest
testParse

	| path |
	path := Path from: 'parent/child/grandChild' delimiter: $/.
	self assert: path size equals: 3.
	self assert: (path at: 1) equals: 'parent'.
	self assert: (path at: 2) equals: 'child'.
	self assert: (path at: 3) equals: 'grandChild'.
	
%

category: 'tests'
method: PathTest
testParseBogus
	
	| path |
	path := Path from: 'parent?<>~ \child/grandChild' delimiter: $/.
	self assert: path size equals: 2.
	self assert: (path at: 1) equals: 'parent?<>~ \child'.
	self assert: (path at: 2) equals: 'grandChild'.
	
%

category: 'tests'
method: PathTest
testParseTrailingSlash
	| path |
	path := Path from: 'griffle/' delimiter: $/.
	self assert: path size equals: 1.
	self assert: (path at: 1) equals: 'griffle'
%

category: 'tests'
method: PathTest
testParseWindowsPathWithUnixDelimiters
	
	| path |
	path := WindowsStore new pathFromString: 'C:\a/b/c'.
	self assert: path segments size equals: 4.
	self
		assertCollection: path segments
		equals: #('C:' 'a' 'b' 'c')
%

category: 'tests'
method: PathTest
testPathString

	| path |

	path := (FileSystem workingDirectory / 'book-result' / 'W01-Welcome')
				relativeToReference: FileSystem workingDirectory.
	self assert: path isRelative.
	self assert: path pathString equals: 'book-result/W01-Welcome'
%

category: 'tests'
method: PathTest
testPrintPathOn

	| pathString pathSrc path |

	"Test a Relative path"
	pathSrc := 'one/two/three'.
	path := Path from: pathSrc.
	self assert: path isRelative.
	pathString := String streamContents: [ :stream | path printPathOn: stream ].
	self assert: pathSrc equals: pathString.

	"Test an Absolute path"
	pathSrc := '/one/two/three'.
	path := Path from: pathSrc.
	self assert: path isAbsolute.
	pathString := String streamContents: [ :stream | path printPathOn: stream ].
	self assert: pathSrc equals: pathString
%

category: 'tests'
method: PathTest
testPrintPathOnDelimiter

	| pathString pathSrc path |

	"Test a Relative path"
	"Use an unusal delimiter to check that the default isn't hardcoded anywhere"
	pathSrc := 'one|two|three'.
	path := Path from: pathSrc delimiter: $|.
	self assert: path isRelative.
	pathString := String streamContents: [ :stream | path printPathOn: stream delimiter: $| ].
	self assert: pathSrc equals: pathString.

	"Test an Absolute path"
	"Use an unusal delimiter to check that the default isn't hardcoded anywhere"
	pathSrc := '|one|two|three'.
	path := Path from: pathSrc delimiter: $|.
	self assert: path isAbsolute.
	pathString := String streamContents: [ :stream | path printPathOn: stream delimiter: $| ].
	self assert: pathSrc equals: pathString
%

category: 'tests'
method: PathTest
testPrintRelativeWithParent
	| path |
	path := Path parent / 'foo'.
	self assert: (path printWithDelimiter: $/) equals: '../foo'
%

category: 'tests'
method: PathTest
testPrintWithDelimiter
	| path |
	path := (Path * 'plonk') / 'griffle'.
	self assert: (path printWithDelimiter: $%) equals: 'plonk%griffle'
%

category: 'tests'
method: PathTest
testRedundantSeparators

	| ref |

	ref := '/a/b/c' asPath / 'foo/'.
	self assert: ref segments equals: #('a' 'b' 'c' 'foo').

	ref := '/a/b/c' asPath / 'foo//'.
	self assert: ref segments equals: #('a' 'b' 'c' 'foo').

	ref := '/a/b/c' asPath / 'foo//..'.
	self assert: ref segments equals: #('a' 'b' 'c' 'foo' '..').

	ref := '/a/b/c' asPath / '..//foo'.
	self assert: ref segments equals: #('a' 'b' 'c' '..' 'foo').

	ref := '/a/b/c' asPath / 'foo//..//bar'.
	self assert: ref segments equals: #('a' 'b' 'c' 'foo' '..' 'bar')
%

category: 'tests'
method: PathTest
testRelativeFromString

	| path |
	
	path := Path from: 'plonk/griffle'.
	
	self assert: path isRelative.
	self assert: path size equals: 2.
	self assert: (path at: 1) equals: 'plonk'.
	self assert: (path at: 2) equals: 'griffle'.
%

category: 'tests'
method: PathTest
testRelativeFromStringNormalization

	| path |
	
	path := Path from: 'plonk/../griffle'.
	
	self assert: path isRelative.
	self assert: path size equals: 3.
	self assert: path segments equals: #('plonk' '..' 'griffle').
%

category: 'tests'
method: PathTest
testRelativeFromStringNormalizationParent

	| path |
	
	path := Path from: 'plonk/../../griffle'.
	
	self assert: path isRelative.
	self assert: path size equals: 4.
	self assert: path segments equals: #('plonk' '..' '..' 'griffle')
%

category: 'tests'
method: PathTest
testRelativeFromStringParent

	| path |
	
	path := Path from: '../..'.
	
	self assert: path isRelative.
	self assert: path size equals: 2.
	self assert: (path at: 1) equals: '..'.
	self assert: (path at: 2) equals: '..'.
%

category: 'tests'
method: PathTest
testRelativePrintString
	| path actual |
	path := Path * 'plonk' / 'griffle'.
	actual := path printString.
	self assert: actual equals: 'Path * ''plonk'' / ''griffle'''
%

category: 'tests'
method: PathTest
testRelativeTo
	"aPath relativeTo: aParent returns a new path relative to the parent"
	
	| parent child relative |
	parent := Path / 'griffle'.
	child := Path / 'griffle' / 'plonk' / 'nurp'.
	relative := child relativeTo: parent.
	self assert: relative equals: (Path * 'plonk' / 'nurp')
%

category: 'tests'
method: PathTest
testRelativeToBranch
	| parent child relative |
	parent := Path / 'griffle' / 'bibb'.
	child := Path / 'griffle' / 'plonk' / 'nurp'.
	relative := child relativeTo: parent.
	self assert: relative  equals: (Path parent / 'plonk' / 'nurp')
%

category: 'tests'
method: PathTest
testRelativeWithParents
	| path allPaths |
	path := Path * 'plonk' / 'griffle' / 'nurb'.
	allPaths := path withParents.
	
	self assert: allPaths size equals: 3.
	self assert: allPaths first basename equals: 'plonk'.
	self assert: allPaths first size equals: 1.
	self assert: allPaths second basename equals: 'griffle'.
	self assert: allPaths second size equals: 2.
	self assert: (allPaths second isChildOf: allPaths first).
	self assert: allPaths third basename equals: 'nurb'.
	self assert: allPaths third size equals: 3.
	self assert: (allPaths third isChildOf: allPaths second).
	self assert: allPaths third == path
%

category: 'tests'
method: PathTest
testResolveAbsolute
	| path |
	path := Path / 'griffle'.
	self assert: path resolve == path.
	self assert: path isAbsolute
%

category: 'tests'
method: PathTest
testResolvePath
		
	| path |
	path := Path / 'grandfather' / 'father' / 'child'.
	self 
		assert: (path resolvePath: Path / 'grandfather') 
		equals: (Path / 'grandfather').
	self 
		assert: (path resolvePath: Path / 'child')
		equals: (Path / 'child').
	self
		assert: (path resolvePath: Path * 'grandfather')
		equals: (Path / 'grandfather' / 'father' / 'child' / 'grandfather').
	self
		assert: (path resolvePath: Path * 'child')
		equals: (Path / 'grandfather' / 'father' / 'child' / 'child').
	self
		assert: (path resolvePath: Path * 'grandfather')
		equals: (Path / 'grandfather' / 'father' / 'child' / 'grandfather').
	self
		assert: (path resolvePath: Path * 'child')
		equals: (Path / 'grandfather' / 'father' / 'child' / 'child').
	self
		assert:  (path resolvePath: (Path parent) / '..') canonicalize
		equals: (Path / 'grandfather')
%

category: 'tests'
method: PathTest
testResolveRelative
	| path |
	path := Path * 'griffle'.
	self assert: path resolve == path.
	self assert: path isRelative
%

category: 'tests'
method: PathTest
testResolveString	

	| path result |
	path := Path * 'plonk'.
	result := path resolve: 'griffle'.
	self assert: result class equals: path class.
	self assert: result size equals: 2.
	self assert: (result at: 1) equals: 'plonk'.
	self assert: (result at: 2) equals: 'griffle'.
%

category: 'tests'
method: PathTest
testRootParent
	| root |
	root := Path root.
	self assert: root parent == root
%

category: 'tests'
method: PathTest
testRootPrintString
	| path actual |
	path := Path root.
	actual := path printString.
	self assert: actual equals: 'Path root'
%

category: 'tests'
method: PathTest
testSiblingOfPath
	| griffle  nurb |
	griffle := Path / 'griffle'.
	nurb := Path / 'nurb'.
	self deny: (griffle isChildOf: nurb).
	self deny: (nurb isChildOf: griffle).
%

category: 'tests'
method: PathTest
testSimpleResolution
	| base relative absolute |
	base := Path / 'plonk'.
	relative := (Path * 'griffle') / 'zonk'.
	absolute := base resolve: relative.
	self assert: absolute isAbsolute.
	self assert: (absolute at: 1) equals: 'plonk'.
	self assert: (absolute at: 2) equals: 'griffle'.
	self assert: (absolute at: 3) equals: 'zonk'.
%

category: 'tests'
method: PathTest
testSlash
	| path actual |
	path := Path * 'plonk'.
	actual := path / 'griffle'.
	self assert: actual class equals: path class.
	self assert: (actual printWithDelimiter: $/) equals: 'plonk/griffle'
%

category: 'tests'
method: PathTest
testUnequalContent
	| a b |
	a := Path * 'plonk'.
	b := Path * 'griffle'.
	self deny: a = b.
%

category: 'tests'
method: PathTest
testUnequalSize
	| a b |
	a := Path * 'plonk'.
	b := (Path * 'plonk') / 'griffle'.
	self deny: a = b.
%

category: 'tests'
method: PathTest
testUnixAbsolutePathName

	self assert: (Path from: '/test') isAbsolute.
	self assert: (Path from: '/etc/bin') isAbsolute.
%

category: 'tests'
method: PathTest
testWindowsAbsolutePathName
  
	self assert: (Path from: 'A:\') isAbsolute.
	self assert: (Path from: 'c:\') isAbsolute.
	self assert: (Path from: 'c:\test') isAbsolute.
%

category: 'tests'
method: PathTest
testWithExtentionAddsExtension
	| path result |
	path := Path * 'plonk'.
	result := path withExtension: 'griffle'.
	self assert: result basename equals: 'plonk.griffle'
%

category: 'tests'
method: PathTest
testWithExtentionReplacesExtension
	| path result |
	path := Path * 'plonk.griffle'.
	result := path withExtension: 'griffle'.
	self assert: result basename equals: 'plonk.griffle'
%

category: 'tests'
method: PathTest
testWorkingDirectoryParent
	| path |
	path := Path workingDirectory parent.
	self assert: path size equals: 1.
	self assert: (path at: 1) equals: '..'
%

category: 'tests'
method: PathTest
testWorkingDirPrintString
	| path actual |
	path := Path workingDirectory. 
	actual := path printString.
	self assert: actual equals: 'Path workingDirectory'
%

! Class implementation for 'PosixErrorGroupTest'

!		Instance methods for 'PosixErrorGroupTest'

category: 'private'
method: PosixErrorGroupTest
cleanupPaths

	filesToDelete
		select: [ :path | fileSystem exists: path ]
		thenDo: [ :path | fileSystem delete: path ].
	directoriesToDelete
		select: [ :path | fileSystem exists: path ]
		thenDo: [ :path | fileSystem delete: path ]
%

category: 'private'
method: PosixErrorGroupTest
createTemporaryDirectory: aPath	

	| aFileReference |

	aFileReference := fileSystem referenceTo: aPath.
	self markForCleanupDirectory: aPath.
	fileSystem store createDirectory: aPath.
	^aFileReference
%

category: 'private'
method: PosixErrorGroupTest
createTemporaryFile: aPath	containing: aString

	| aFileReference |

	aFileReference := fileSystem referenceTo: aPath.
	self markForCleanupFile: aFileReference.
	aFileReference writeStreamDo: [ :ws | ws nextPutAll: aString ].
	^aFileReference
%

category: 'testing'
method: PosixErrorGroupTest
isWindowsStoreCurrentStore

	^FileSystem disk store class name == #WindowsStore
%

category: 'private'
method: PosixErrorGroupTest
markForCleanupDirectory: aFileReference

	directoriesToDelete add: aFileReference
%

category: 'private'
method: PosixErrorGroupTest
markForCleanupFile: aFileReference

	filesToDelete add: aFileReference
%

category: 'Running'
method: PosixErrorGroupTest
setUp
	super setUp.
	self setupFileSystem.
	filesToDelete := OrderedCollection new.
	directoriesToDelete := OrderedCollection new.
%

category: 'private'
method: PosixErrorGroupTest
setupFileSystem
	self subclassResponsibility
%

category: 'private'
method: PosixErrorGroupTest
setupForChageModeErrorGroup

	errorGroup := ChangeModeErrorGroup new
%

category: 'private'
method: PosixErrorGroupTest
setupForCloseDirectoryErrorGroup

	errorGroup := CloseDirectoryErrorGroup new
%

category: 'private'
method: PosixErrorGroupTest
setupForCloseFileErrorGroup

	errorGroup := CloseFileErrorGroup new
%

category: 'private'
method: PosixErrorGroupTest
setupForCopyFileErrorGroup

	errorGroup := CopyFileErrorGroup new
%

category: 'private'
method: PosixErrorGroupTest
setupForFileControlErrorGroup

	errorGroup := FileControlErrorGroup new
%

category: 'private'
method: PosixErrorGroupTest
setupForFileSeekErrorGroup

	errorGroup := FileSeekErrorGroup new
%

category: 'private'
method: PosixErrorGroupTest
setupForFileSizeErrorGroup

	errorGroup := FileSizeErrorGroup new
%

category: 'private'
method: PosixErrorGroupTest
setupForFlushFileErrorGroup

	errorGroup := FlushFileErrorGroup new
%

category: 'private'
method: PosixErrorGroupTest
setupForLockFileErrorGroup

	errorGroup := LockFileErrorGroup new
%

category: 'private'
method: PosixErrorGroupTest
setupForOpenDirectoryErrorGroup

	errorGroup := OpenDirectoryErrorGroup new
%

category: 'private'
method: PosixErrorGroupTest
setupForOpenFileErrorGroup

	errorGroup := OpenFileErrorGroup new
%

category: 'private'
method: PosixErrorGroupTest
setupForReadDirectoryGroup

	errorGroup := ReadDirectoryErrorGroup new
%

category: 'private'
method: PosixErrorGroupTest
setupForReadFileErrorGroup

	errorGroup := ReadFileErrorGroup new
%

category: 'private'
method: PosixErrorGroupTest
setupForRemoveFileErrorGroup

	errorGroup := RemoveFileErrorGroup new
%

category: 'private'
method: PosixErrorGroupTest
setupForRenameFileErrorGroup

	errorGroup := RenameFileErrorGroup new
%

category: 'private'
method: PosixErrorGroupTest
setupForTouchFileErrorGroup

	errorGroup := TouchFileErrorGroup new
%

category: 'private'
method: PosixErrorGroupTest
setupForUnlockFileErrorGroup

	errorGroup := UnlockFileErrorGroup new
%

category: 'private'
method: PosixErrorGroupTest
setupForWritingErrorGroup

	errorGroup := WritingErrorGroup new
%

category: 'Running'
method: PosixErrorGroupTest
tearDown
	self cleanupPaths.
	super tearDown
%

! Class implementation for 'PosixErrorGroupClientTest'

!		Instance methods for 'PosixErrorGroupClientTest'

category: 'private'
method: PosixErrorGroupClientTest
setupFileSystem
	fileSystem := FileSystem clientDisk
%

category: 'tests'
method: PosixErrorGroupClientTest
testDelegationOfFileOpenError
	| aClass  aFilePosixError|
	fileReference := fileSystem workingDirectory / 't1'.
	self assert: fileSystem store class name equals: #ClientStore.

	fileSystem store store class name = #WindowsStore
		ifTrue: [
			aClass := OpenFileErrorGroup new errorClassForErrorNumber: 19 fileReference: fileReference options: FileOptions newRead.
			self assert: aClass posixName equals: 'EROFS'.

			aFilePosixError := OpenFileErrorGroup new errorForNumber: 19 fileReference: fileReference options: FileOptions newRead.
			self assert: aFilePosixError class posixName equals: 'EROFS'.

		posixErrorClass := ChangeModeErrorGroup new errorClassForErrorNumber: 123 fileReference: fileReference.
		self assert: posixErrorClass posixName equals: 'EACCES'].
%

! Class implementation for 'PosixErrorGroupUnixTest'

!		Instance methods for 'PosixErrorGroupUnixTest'

category: 'private'
method: PosixErrorGroupUnixTest
setupFileSystem
	fileSystem := FileSystem store: UnixStore new
%

category: 'tests - lookup file posix error class'
method: PosixErrorGroupUnixTest
testChageModeErrorGroup

	self setupForChageModeErrorGroup.
	fileReference := fileSystem workingDirectory.

	self should: [ errorGroup errorClassForErrorNumber: 9999 fileReference: fileReference ] raise: Error description: 'Error name is not in valid error list'.

	posixErrorClass := errorGroup errorClassForErrorNumber: FileIOError defaultErrorNumber fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EIO'.

	posixErrorClass := errorGroup errorClassForErrorNumber: FileAccessError defaultErrorNumber fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EACCES'.
	
	posixErrorClass := errorGroup errorClassForErrorNumber: FileReadOnlyFileSystemError defaultErrorNumber fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EROFS'.

	posixErrorClass := errorGroup errorClassForErrorNumber: FilePermissionDeniedError defaultErrorNumber fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EPERM'
%

category: 'tests - lookup file posix error class'
method: PosixErrorGroupUnixTest
testCopyFileErrorGroup
	| directory1 directory2 |

	self setupForCopyFileErrorGroup.
	directory1 := self createTemporaryDirectory: Path * 'plonk1'.
	directory2 := self createTemporaryDirectory: Path * 'plonk2'.
	oldFileReference := self createTemporaryFile: directory1 path / 't1'	containing: 'griffle'.
	fileReference := fileSystem referenceTo: directory2 path / 't2'.

	self should: [ errorGroup errorClassForErrorNumber: 9999 fileReference: fileReference ] raise: Error description: 'Error name is not in valid error list'.

	posixErrorClass := errorGroup errorClassForErrorNumber: FileIOError defaultErrorNumber fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EIO'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 13 newFileReference: fileReference oldFileReference: oldFileReference.
	self assert: posixErrorClass posixName equals: 'EACCES'.
	
"	posixErrorClass := errorGroup errorClassForErrorNumber: FileReadOnlyFileSystemError defaultErrorNumber fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EROFS'.

	posixErrorClass := errorGroup errorClassForErrorNumber: FilePermissionDeniedError defaultErrorNumber fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EPERM'"
%

category: 'tests - create file posix error'
method: PosixErrorGroupUnixTest
testCreationOfFilePosixError
	| fileOptions aPosixError |

	self setupForOpenFileErrorGroup.
	fileOptions := FileOptions newRead.
	fileReference := fileSystem workingDirectory / 't2'.

	errorGroup := OpenFileErrorGroup new.
	aPosixError := errorGroup errorForNumber: FileExistsError defaultErrorNumber fileReference: fileReference options: fileOptions.
	self assert: aPosixError class posixName equals: 'EEXIST'.
	self assert: aPosixError reference equals: fileReference.
	self assert: aPosixError sourceReference isNil.
	self assert: aPosixError options equals: fileOptions.
	self assert: aPosixError errorGroupName = errorGroup class name asString.

	errorGroup := CopyFileErrorGroup new.
	aPosixError := errorGroup errorForNumber: FileExistsError defaultErrorNumber newFileReference: fileReference oldFileReference: fileSystem workingDirectory.
	self assert: aPosixError class posixName equals: 'EEXIST'.
	self assert: aPosixError reference equals: fileReference.
	self assert: aPosixError sourceReference equals: fileSystem workingDirectory.
	self assert: aPosixError options isNil.
	self assert: aPosixError errorGroupName = errorGroup class name asString.

	errorGroup := ChangeModeErrorGroup new.
	aPosixError := errorGroup errorForNumber: FileAccessError defaultErrorNumber fileReference: fileReference.
	self assert: aPosixError class posixName equals: 'EACCES'.
	self assert: aPosixError reference equals: fileReference.
	self assert: aPosixError sourceReference isNil.
	self assert: aPosixError options isNil.
	self assert: aPosixError errorGroupName = errorGroup class name asString
%

category: 'tests - lookup file posix error class'
method: PosixErrorGroupUnixTest
testOpenFileErrorGroup
	| fileOptions |

	self setupForOpenFileErrorGroup.
	fileOptions := FileOptions newRead.
"	directory1 := self createTemporaryDirectory: Path * 'plonk1'.
	oldFileReference := self createTemporaryFile: directory1 path / 't1'	containing: 'griffle'."
	fileReference := fileSystem workingDirectory / 't2'.

	self should: [ errorGroup errorClassForErrorNumber: 9999 fileReference: fileReference ] raise: Error description: 'Error name is not in valid error list'.

	posixErrorClass := errorGroup errorClassForErrorNumber: FileIOError defaultErrorNumber fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EIO'.

	posixErrorClass := errorGroup errorClassForErrorNumber: FileExistsError defaultErrorNumber fileReference: fileReference options: fileOptions.
	self assert: posixErrorClass posixName equals: 'EEXIST'.

	posixErrorClass := errorGroup errorClassForErrorNumber: FileReadOnlyFileSystemError defaultErrorNumber fileReference: fileReference options: fileOptions.
	self assert: posixErrorClass posixName equals: 'EROFS'.

	posixErrorClass := errorGroup errorClassForErrorNumber: FileAccessError defaultErrorNumber fileReference: fileReference options: fileOptions.
	self assert: posixErrorClass posixName equals: 'EACCES'.

	posixErrorClass := errorGroup errorClassForErrorNumber: FileBusyError defaultErrorNumber fileReference: fileReference options: fileOptions.
	self assert: posixErrorClass posixName equals: 'EBUSY'.

	posixErrorClass := errorGroup errorClassForErrorNumber: FileNoSpaceError defaultErrorNumber fileReference: fileReference options: fileOptions.
	self assert: posixErrorClass posixName equals: 'ENOSPC'.

	posixErrorClass := errorGroup errorClassForErrorNumber: FileNoEntryError defaultErrorNumber fileReference: fileReference options: fileOptions.
	self assert: posixErrorClass posixName equals: 'ENOENT'.

	posixErrorClass := errorGroup errorClassForErrorNumber: FileMaxFilesOpenError defaultErrorNumber fileReference: fileReference options: fileOptions.
	self assert: posixErrorClass posixName equals: 'ENFILE'.

	posixErrorClass := errorGroup errorClassForErrorNumber:  FileInvalidOptionError defaultErrorNumber fileReference: fileReference options: fileOptions.
	self assert: posixErrorClass posixName equals: 'EINVAL'
%

! Class implementation for 'PosixErrorGroupWindowsTest'

!		Instance methods for 'PosixErrorGroupWindowsTest'

category: 'private'
method: PosixErrorGroupWindowsTest
nonExistantFileReference
	"This results in a non sensical path which is what this test requires"
	^(FileSystem store: WindowsStore new) workingDirectory
%

category: 'private'
method: PosixErrorGroupWindowsTest
setupFileSystem
	fileSystem := FileSystem store: WindowsStore new
%

category: 'tests - lookup file posix error class'
method: PosixErrorGroupWindowsTest
testChageModeErrorGroup

	self setupForChageModeErrorGroup.
	fileReference := self nonExistantFileReference.

	posixErrorClass := errorGroup errorClassForErrorNumber: 1 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EIO'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 2 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'ENOENT'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 123 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EACCES'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 19 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EROFS'
%

category: 'tests - lookup file posix error class'
method: PosixErrorGroupWindowsTest
testCloseDriectoryErrorGroup

	self setupForCloseDirectoryErrorGroup.
	fileReference := fileSystem workingDirectory / 't2'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 9999 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EIO'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 6 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EBADF'
%

category: 'tests - lookup file posix error class'
method: PosixErrorGroupWindowsTest
testCloseFileErrorGroup

	self setupForCloseFileErrorGroup.
	fileReference := fileSystem workingDirectory / 't2'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 9999 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EIO'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 6 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EBADF'.

	self should: [errorGroup signalErrorNumber: 6 fileReference: fileReference] raise: FileBadFileDescriptorError
%

category: 'tests - lookup file posix error class'
method: PosixErrorGroupWindowsTest
testCopyFileErrorGroup
	| directory1 directory2 |
	
	self setupForCopyFileErrorGroup.
	directory1 := fileSystem workingDirectoryPath.
	directory2 := fileSystem workingDirectoryPath.
	oldFileReference := self createTemporaryFile: directory1 / 't1'	containing: 'griffle'.
	fileReference := fileSystem referenceTo: directory2 / 't2'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 9999 newFileReference: fileReference oldFileReference: oldFileReference.
	self assert: posixErrorClass posixName equals: 'EIO'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 3 newFileReference: fileReference oldFileReference: oldFileReference.
	self assert: posixErrorClass posixName equals: 'EACCES'.

	self isWindowsStoreCurrentStore ifTrue: [
		posixErrorClass := errorGroup errorClassForErrorNumber: 5 newFileReference: directory1 oldFileReference: oldFileReference.
		self assert: posixErrorClass posixName equals: 'EEXIST' description: 'Target file is a directory that already exists'.
		posixErrorClass := errorGroup errorClassForErrorNumber: 5 newFileReference: oldFileReference oldFileReference: oldFileReference.
		self assert: posixErrorClass posixName equals: 'EEXIST' description: 'Target file is a file that already exists'.
		posixErrorClass := errorGroup errorClassForErrorNumber: 5 newFileReference: fileReference oldFileReference: fileReference.
		self assert: posixErrorClass posixName equals: 'ENOENT' description: 'Source file is a does not exists'.
		posixErrorClass := errorGroup errorClassForErrorNumber: 5 newFileReference: fileReference oldFileReference: directory2.
		self assert: posixErrorClass posixName equals: 'EISDIR' description: 'Source file is actually a directory'].

	posixErrorClass := errorGroup errorClassForErrorNumber: 16 newFileReference: fileReference oldFileReference: oldFileReference.
	self assert: posixErrorClass posixName equals: 'EBUSY'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 19 newFileReference: fileReference oldFileReference: oldFileReference.
	self assert: posixErrorClass posixName equals: 'EROFS'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 21 newFileReference: fileReference oldFileReference: oldFileReference.
	self assert: posixErrorClass posixName equals: 'EACCES'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 32 newFileReference: fileReference oldFileReference: oldFileReference.
	self assert: posixErrorClass posixName equals: 'EBUSY'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 33 newFileReference: fileReference oldFileReference: oldFileReference.
	self assert: posixErrorClass posixName equals: 'EBUSY'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 80 newFileReference: fileReference oldFileReference: oldFileReference.
	self assert: posixErrorClass posixName equals: 'EEXIST'.

	self isWindowsStoreCurrentStore ifTrue: [
		posixErrorClass := errorGroup errorClassForErrorNumber: 87 newFileReference: fileReference oldFileReference: fileReference.
		self assert: posixErrorClass posixName equals: 'EEXIST' description: 'Old file does not exist'.
		posixErrorClass := errorGroup errorClassForErrorNumber: 87 newFileReference: fileReference oldFileReference: oldFileReference.
		self assert: posixErrorClass posixName equals: 'EACCES' description: 'Old file exists but not able to access'].


	posixErrorClass := errorGroup errorClassForErrorNumber: 123 newFileReference: fileReference oldFileReference: oldFileReference.
	self assert: posixErrorClass posixName equals: 'EACCES'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 161 newFileReference: fileReference oldFileReference: oldFileReference.
	self assert: posixErrorClass posixName equals: 'ENOENT'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 183 newFileReference: fileReference oldFileReference: oldFileReference.
	self assert: posixErrorClass posixName equals: 'EEXIST'.
%

category: 'tests - create file posix error'
method: PosixErrorGroupWindowsTest
testCreationOfFilePosixError
	| fileOptions aPosixError |

	self setupForOpenFileErrorGroup.
	fileOptions := FileOptions newRead.
	fileReference := fileSystem workingDirectory / 't2'.

	errorGroup := OpenFileErrorGroup new.
	aPosixError := errorGroup errorForNumber: 3 fileReference: fileReference options: fileOptions.
	self assert: aPosixError class posixName equals: 'EACCES'.
	self assert: aPosixError reference equals: fileReference.
	self assert: aPosixError sourceReference isNil.
	self assert: aPosixError options equals: fileOptions.
	self assert: aPosixError errorGroupName = errorGroup class name asString.

	self should: [errorGroup signalErrorNumber: 3 fileReference: fileReference options: fileOptions] raise: FileAccessError.

	errorGroup := CopyFileErrorGroup new.
	aPosixError := errorGroup errorForNumber: 16 newFileReference: fileReference oldFileReference: fileSystem workingDirectory.
	self assert: aPosixError class posixName equals: 'EBUSY'.
	self assert: aPosixError reference equals: fileReference.
	self assert: aPosixError sourceReference equals: fileSystem workingDirectory.
	self assert: aPosixError options isNil.
	self assert: aPosixError errorGroupName = errorGroup class name asString.

	errorGroup := ChangeModeErrorGroup new.
	aPosixError := errorGroup errorForNumber: 123 fileReference: fileReference.
	self assert: aPosixError class posixName equals: 'EACCES'.
	self assert: aPosixError reference equals: fileReference.
	self assert: aPosixError sourceReference isNil.
	self assert: aPosixError options isNil.
	self assert: aPosixError errorGroupName = errorGroup class name asString
%

category: 'tests - lookup file posix error class'
method: PosixErrorGroupWindowsTest
testFileControlErrorGroup

	self setupForFileControlErrorGroup.
	fileReference := fileSystem workingDirectory / 't2'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 9999 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EIO'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 5 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EACCES'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 6 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EBADF'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 7 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EINVAL'
%

category: 'tests - lookup file posix error class'
method: PosixErrorGroupWindowsTest
testFileSeekErrorGroup

	self setupForFileSeekErrorGroup.
	fileReference := fileSystem workingDirectory / 't2'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 9999 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EIO'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 6 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EBADF'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 87 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EINVAL'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 131 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EINVAL'
%

category: 'tests - lookup file posix error class'
method: PosixErrorGroupWindowsTest
testFileSizeErrorGroup

	self setupForFileSizeErrorGroup.
	fileReference := fileSystem workingDirectory / 't2'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 9999 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EIO'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 6 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EBADF'
%

category: 'tests - lookup file posix error class'
method: PosixErrorGroupWindowsTest
testFlushFileErrorGroup

	self setupForFlushFileErrorGroup.
	fileReference := fileSystem workingDirectory / 't2'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 9999 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EIO'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 5 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EACCES'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 6 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EBADF'
%

category: 'tests - lookup file posix error class'
method: PosixErrorGroupWindowsTest
testLockFileErrorGroup

	self setupForLockFileErrorGroup.
	fileReference := fileSystem workingDirectory / 't2'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 9999 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EIO'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 1 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EINVAL'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 5 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EACCES'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 6 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EBADF'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 19 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EROFS'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 32 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EBUSY'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 33 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EBUSY'.
%

category: 'tests - lookup file posix error class'
method: PosixErrorGroupWindowsTest
testOpenDirectoryErrorGroup
	| fileOptions |

	self setupForOpenDirectoryErrorGroup.
	fileOptions := FileOptions newRead.
	fileReference := fileSystem workingDirectory / 't2'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 9999 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EIO'.

	self isWindowsStoreCurrentStore ifTrue: [
		posixErrorClass := errorGroup errorClassForErrorNumber: 2 fileReference: fileReference.
		self assert: posixErrorClass posixName equals: 'ENOENT' description: 'Directory does not exist'].

	posixErrorClass := errorGroup errorClassForErrorNumber: 3 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EACCES'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 5 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EACCES'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 84 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'ENFILE'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 87 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EINVAL'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 123 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EACCES'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 161 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'ENOENT'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 267 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EACCES'
%

category: 'tests - lookup file posix error class'
method: PosixErrorGroupWindowsTest
testOpenFileErrorGroup
	| fileOptions |

	self setupForOpenFileErrorGroup.
	fileOptions := FileOptions newRead.
	fileReference := fileSystem workingDirectory / 't2'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 9999 fileReference: fileReference options: fileOptions.
	self assert: posixErrorClass posixName equals: 'EIO'.

	self isWindowsStoreCurrentStore ifTrue: [
		posixErrorClass := errorGroup errorClassForErrorNumber: 2 fileReference: fileSystem workingDirectory options: fileOptions.
		self assert: posixErrorClass posixName equals: 'EACCES' description: 'Access Error - attempted to open a directory'.
		posixErrorClass := errorGroup errorClassForErrorNumber: 2 fileReference: fileSystem workingDirectory / 't1' options: fileOptions.
		self assert: posixErrorClass posixName equals: 'ENOENT' description: 'No file entry was found'].

	posixErrorClass := errorGroup errorClassForErrorNumber: 3 fileReference: fileReference options: fileOptions.
	self assert: posixErrorClass posixName equals: 'EACCES'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 4 fileReference: fileReference options: fileOptions.
	self assert: posixErrorClass posixName equals: 'ENFILE'.

	self isWindowsStoreCurrentStore ifTrue: [
		posixErrorClass := errorGroup errorClassForErrorNumber: 5 fileReference: fileSystem workingDirectory options: fileOptions.
		self assert: posixErrorClass posixName equals: 'EACCES' description: 'File reference is a directory'.
		posixErrorClass := errorGroup errorClassForErrorNumber: 5 fileReference: fileReference options: FileOptions newWrite.
		self assert: posixErrorClass posixName equals: 'EINVAL' description: 'Attempting to open with a truncate flag'.
		posixErrorClass := errorGroup errorClassForErrorNumber: 5 fileReference: fileReference options: FileOptions newAppend.
		self assert: posixErrorClass posixName equals: 'EACCES' description: 'Attempting to create a file if does not exist'.
		posixErrorClass := errorGroup errorClassForErrorNumber: 5 fileReference: fileReference options: FileOptions read.
		self assert: posixErrorClass posixName equals: 'EACCES' description: 'Attempting to open a file with no access'.
		posixErrorClass := errorGroup errorClassForErrorNumber: 5 fileReference: fileReference options: FileOptions newReadWrite.
		self assert: posixErrorClass posixName equals: 'EINVAL' description: 'Catchall errors - no flags, file does not exist'].

	posixErrorClass := errorGroup errorClassForErrorNumber: 19 fileReference: fileReference options: fileOptions.
	self assert: posixErrorClass posixName equals: 'EROFS'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 32 fileReference: fileReference options: fileOptions.
	self assert: posixErrorClass posixName equals: 'EBUSY'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 33 fileReference: fileReference options: fileOptions.
	self assert: posixErrorClass posixName equals: 'EBUSY'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 36 fileReference: fileReference options: fileOptions.
	self assert: posixErrorClass posixName equals: 'ENFILE'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 80 fileReference: fileReference options: (FileOptions newWrite addExclusiveFlag).
	self assert: posixErrorClass posixName equals: 'EACCES' description: 'Attempting to open with an exlusive flag'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 80 fileReference: fileReference options: FileOptions newWrite.
	self assert: posixErrorClass posixName equals: 'EEXIST' description: 'Attempting to open without an exlusive flag'.

	self isWindowsStoreCurrentStore ifTrue: [
		posixErrorClass := errorGroup errorClassForErrorNumber: 87 fileReference: fileSystem workingDirectory options: fileOptions.
		self assert: posixErrorClass posixName equals: 'EACCES' description: 'File reference is a directory'.
		posixErrorClass := errorGroup errorClassForErrorNumber: 87 fileReference: fileReference options: FileOptions write.
		self assert: posixErrorClass posixName equals: 'EINVAL' description: 'Attempting to open with a truncate flag'.
		posixErrorClass := errorGroup errorClassForErrorNumber: 87 fileReference: fileReference options: FileOptions newAppend.
		self assert: posixErrorClass posixName equals: 'EACCES' description: 'Attempting to create a file if does not exist'.
		posixErrorClass := errorGroup errorClassForErrorNumber: 87 fileReference: fileReference options: FileOptions read.
		self assert: posixErrorClass posixName equals: 'EACCES' description: 'Attempting to open a file with no access'.
		posixErrorClass := errorGroup errorClassForErrorNumber: 87 fileReference: fileReference options: FileOptions readWrite.
		self assert: posixErrorClass posixName equals: 'EINVAL' description: 'Catchall errors - no flags, file does not exist'].

	posixErrorClass := errorGroup errorClassForErrorNumber: 123 fileReference: fileReference options: fileOptions.
	self assert: posixErrorClass posixName equals: 'EACCES'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 161 fileReference: fileReference options: fileOptions.
	self assert: posixErrorClass posixName equals: 'EACCES'
%

category: 'tests - lookup file posix error class'
method: PosixErrorGroupWindowsTest
testReadDirectoryErrorGroup

	self setupForReadDirectoryGroup.
	fileReference := fileSystem referenceTo: fileSystem workingDirectoryPath / 't2'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 9999 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EIO'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 3 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EACCES'
%

category: 'tests - lookup file posix error class'
method: PosixErrorGroupWindowsTest
testReadFileErrorGroup

	self setupForReadFileErrorGroup.
	fileReference := fileSystem referenceTo: fileSystem workingDirectoryPath / 't2'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 9999 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EIO'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 5 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EACCES'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 6 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EBADF'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 33 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EBUSY'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 112 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EACCES'
%

category: 'tests - lookup file posix error class'
method: PosixErrorGroupWindowsTest
testRemoveFileErrorGroup

	self setupForRemoveFileErrorGroup.
	fileReference := fileSystem referenceTo: fileSystem workingDirectoryPath / 't2'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 9999 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EIO'.

	self isWindowsStoreCurrentStore ifTrue: [
		posixErrorClass := errorGroup errorClassForErrorNumber: 2 fileReference: fileSystem workingDirectory.
		self assert: posixErrorClass posixName equals: 'EISDIR' description: 'Access Error - attempted to open a directory'.
		posixErrorClass := errorGroup errorClassForErrorNumber: 2 fileReference: fileSystem workingDirectory / 't1'.
		self assert: posixErrorClass posixName equals: 'ENOENT' description: 'No file entry was found'].

	posixErrorClass := errorGroup errorClassForErrorNumber: 3 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EACCES' description: 'File reference is a directory'.

	self isWindowsStoreCurrentStore ifTrue: [
		posixErrorClass := errorGroup errorClassForErrorNumber: 5 fileReference: fileSystem workingDirectory.
		self assert: posixErrorClass posixName equals: 'EISDIR' description: 'Access Error - attempted to open a directory'.
		posixErrorClass := errorGroup errorClassForErrorNumber: 5 fileReference: fileSystem workingDirectory / 't1'.
		self assert: posixErrorClass posixName equals: 'ENOENT' description: 'No file entry was found'].

	posixErrorClass := errorGroup errorClassForErrorNumber: 19 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EROFS'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 145 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EEXIST'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 161 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'ENOENT'
%

category: 'tests - lookup file posix error class'
method: PosixErrorGroupWindowsTest
testRenameFileErrorGroup

	| directory1 directory2 |

	directory1 := fileSystem workingDirectoryPath.
	directory2 := fileSystem workingDirectoryPath.
	oldFileReference := self createTemporaryFile: directory1 / 't1'	containing: 'griffle'.
	fileReference := fileSystem referenceTo: directory2 / 't2'.

	self setupForRenameFileErrorGroup.
	fileReference := fileSystem workingDirectory / 't2'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 9999 newFileReference: fileReference oldFileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EIO'.

	self should: [errorGroup signalErrorNumber: 9999 newFileReference: fileReference oldFileReference: fileReference] raise: FileIOError.

	self isWindowsStoreCurrentStore ifTrue: [
		posixErrorClass := errorGroup errorClassForErrorNumber: 2 newFileReference: fileReference oldFileReference: fileReference.
		self assert: posixErrorClass posixName equals: 'ENOENT' description: 'New file does not exist'.
		posixErrorClass := errorGroup errorClassForErrorNumber: 2 newFileReference: oldFileReference oldFileReference: fileReference.
		self assert: posixErrorClass posixName equals: 'ENOENT' description: 'Old file does not exist'.
		posixErrorClass := errorGroup errorClassForErrorNumber: 2 newFileReference: oldFileReference oldFileReference: oldFileReference.
		self assert: posixErrorClass posixName equals: 'EEXIST' description: 'New file does not exist - it should not exist for rename to work'.
		posixErrorClass := errorGroup errorClassForErrorNumber: 2 newFileReference: oldFileReference oldFileReference: oldFileReference.
		self assert: posixErrorClass posixName equals: 'EXDEV' description: 'No file entry was found'].

	self isWindowsStoreCurrentStore ifTrue: [
		posixErrorClass := errorGroup errorClassForErrorNumber: 3 newFileReference: oldFileReference oldFileReference: fileReference.
		self assert: posixErrorClass posixName equals: 'EEXIST' description: 'New file already exists'.
		posixErrorClass := errorGroup errorClassForErrorNumber: 3 newFileReference: fileReference oldFileReference: fileSystem workingDirectory.
		self assert: posixErrorClass posixName equals: 'EISDIR' description: 'Old file is a directory'.
		posixErrorClass := errorGroup errorClassForErrorNumber: 5 newFileReference: fileReference oldFileReference: oldFileReference.
		self assert: posixErrorClass posixName equals: 'EACCES' description: 'Catach all'].

	self isWindowsStoreCurrentStore ifTrue: [
		posixErrorClass := errorGroup errorClassForErrorNumber: 5 newFileReference: oldFileReference oldFileReference: fileReference.
		self assert: posixErrorClass posixName equals: 'EEXIST' description: 'New file already exists'.
		posixErrorClass := errorGroup errorClassForErrorNumber: 5 newFileReference: fileReference oldFileReference: fileSystem workingDirectory.
		self assert: posixErrorClass posixName equals: 'EISDIR' description: 'Old file is a directory'.
		posixErrorClass := errorGroup errorClassForErrorNumber: 5 newFileReference: fileReference oldFileReference: oldFileReference.
		self assert: posixErrorClass posixName equals: 'EACCES' description: 'Catach all'].

	self isWindowsStoreCurrentStore ifTrue: [
		posixErrorClass := errorGroup errorClassForErrorNumber: 17 newFileReference: fileSystem workingDirectory oldFileReference: fileReference.
		self assert: posixErrorClass posixName equals: 'EISDIR'.
		posixErrorClass := errorGroup errorClassForErrorNumber: 17 newFileReference: fileSystem workingDirectory / 't1' oldFileReference: fileReference.
		self assert: posixErrorClass posixName equals: 'EXDEV'].

	posixErrorClass := errorGroup errorClassForErrorNumber: 19 newFileReference: fileReference oldFileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EROFS'.

	self isWindowsStoreCurrentStore ifTrue: [
		posixErrorClass := errorGroup errorClassForErrorNumber: 87 newFileReference: oldFileReference oldFileReference: fileReference.
		self assert: posixErrorClass posixName equals: 'EEXIST' description: 'New file already exists'.
		posixErrorClass := errorGroup errorClassForErrorNumber: 87 newFileReference: fileReference oldFileReference: fileSystem workingDirectory.
		self assert: posixErrorClass posixName equals: 'EISDIR' description: 'Old file is a directory'.
		posixErrorClass := errorGroup errorClassForErrorNumber: 87 newFileReference: fileReference oldFileReference: fileReference.
		self assert: posixErrorClass posixName equals: 'ENOENT' description: 'Old file does not exist'.
		posixErrorClass := errorGroup errorClassForErrorNumber: 87 newFileReference: fileReference oldFileReference: oldFileReference.
		self assert: posixErrorClass posixName equals: 'EACCES' description: 'Catach all'].

	posixErrorClass := errorGroup errorClassForErrorNumber: 123 newFileReference: fileReference oldFileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EACCES'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 161 newFileReference: fileReference oldFileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'ENOENT'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 183 newFileReference: fileReference oldFileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EEXIST'
%

category: 'tests - lookup file posix error class'
method: PosixErrorGroupWindowsTest
testTouchFileErrorGroup

	self setupForTouchFileErrorGroup.
	fileReference := fileSystem workingDirectory / 't2'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 9999 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EIO'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 2 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'ENOENT'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 3 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'ENOENT'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 4 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'ENFILE'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 5 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EACCES'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 19 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EROFS'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 123 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EACCES'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 161 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'ENOENT'.
%

category: 'tests - lookup file posix error class'
method: PosixErrorGroupWindowsTest
testUnlockFileErrorGroup

	self setupForUnlockFileErrorGroup.
	fileReference := fileSystem workingDirectory / 't2'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 9999 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EIO'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 1 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EINVAL'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 5 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EACCES'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 6 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EBADF'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 19 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EROFS'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 32 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EBUSY'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 33 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EBUSY'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 158 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EINVAL'.
%

category: 'tests - lookup file posix error class'
method: PosixErrorGroupWindowsTest
testWritingErrorGroup

	self setupForWritingErrorGroup.
	fileReference := fileSystem workingDirectory / 't2'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 9999 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EIO'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 5 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EACCES'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 6 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EBADF'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 19 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EROFS'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 33 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EBUSY'.

	posixErrorClass := errorGroup errorClassForErrorNumber: 112 fileReference: fileReference.
	self assert: posixErrorClass posixName equals: 'EACCES'.
%

! Class implementation for 'StdioStreamTest'

!		Class methods for 'StdioStreamTest'

category: 'tests - manual'
classmethod: StdioStreamTest
manualStdinTest
	"This test can be run headless to manually to check actual stdin.
	It reads the input and confirms that #atEnd answers as expected and the number of characters read.
	
	Example execution (from the shell prompt):

		echo 'hello world' | pharo --headless Pharo.image eval 'StdioStreamTest manualStdinTest'
		
		pharo --headless Pharo.image eval 'StdioStreamTest manualStdinTest' < /proc/cpuinfo
		
		pharo --headless Pharo.image eval 'StdioStreamTest manualStdinTest'
		# Type some text, enter and Ctrl-D"

	| stdin stdout atEndBefore atEndAfter contents |

	stdin := #Stdio stdin.
	stdout := ZnNewLineWriterStream on: #Stdio stdout.
	atEndBefore := stdin atEnd.
	contents := stdin upToEnd.
	atEndAfter := stdin atEnd.
	stdout
		<< '#atEnd (before): ';
		<< atEndBefore displayString;
		cr;
		<< '#atEnd (after): ';
		<< atEndAfter displayString;
		cr;
		<< 'Size: ';
		<< contents size displayString;
		cr;
		<< 'Contents:';
		cr; cr.
	contents hexDumpOn: stdout max: 128.
	stdout cr.
	"SmalltalkImage current snapshot: false andQuit: true."
%

category: 'accessing'
classmethod: StdioStreamTest
resources

	^{ StdioStreamTestResource }.
%

!		Instance methods for 'StdioStreamTest'

category: 'accessing'
method: StdioStreamTest
resource
	"Answer the receiver's resource object"

	^StdioStreamTestResource current
%

category: 'running'
method: StdioStreamTest
setUp
	"A stream is shared amongst all the tests to reduce file create, write, delete cycles.
	Ensure the stream is in the expected state."

	super setUp.
	self resource stdioStream position: 0.
%

category: 'accessing'
method: StdioStreamTest
stdioStream

	^self resource stdioStream
%

category: 'tests'
method: StdioStreamTest
testAtEnd
	"Test that #atEnd isn't incorrectly answering true"

	self deny: self stdioStream atEnd
%

category: 'tests'
method: StdioStreamTest
testChangePosition
	"The shared stream should be put back to the start for each test"

	"| stream |
	
	stream := self stdioStream.
	stream position: 5.
	self assert: stream position equals: 5.
	self assert: stream next equals: $5 asciiValue."
%

category: 'tests'
method: StdioStreamTest
testContents

	"self assert: self stdioStream contents equals: self resource contents asByteArray"
%

category: 'tests'
method: StdioStreamTest
testPeek

"	| stream |

	stream := self stdioStream.
	self assert: stream peek equals: $0 asciiValue.
	self assert: stream peek equals: stream next."
%

category: 'tests'
method: StdioStreamTest
testPosition
	"The shared stream should be put back to the start for each test"
	
	self assert: self stdioStream position equals: 0.
%

category: 'tests'
method: StdioStreamTest
testSize

	"self assert: self stdioStream size equals: self resource contents size"
%

category: 'tests'
method: StdioStreamTest
testUpToEnd

	"| stream contents |

	stream := self stdioStream.
	contents := self resource contents.
	stream position: 5.
	self assert: stream upToEnd 
		equals: (contents copyFrom: 6 to: contents size) asByteArray"
%

! Class implementation for 'StdioTest'

!		Instance methods for 'StdioTest'

category: 'testing'
method: StdioTest
clearCache
	SessionTemps current removeKey: #StdioCreateReadStreamBlock ifAbsent: [].
	SessionTemps current removeKey: #StdioCreateWriteStreamBlock ifAbsent: [].
	self deny: (SessionTemps current includesKey: #StdioCreateReadStreamBlock).
	self deny: (SessionTemps current includesKey: #StdioCreateWriteStreamBlock).
	Stdio cleanStdioHandles.
	self verifyStdInstanceCleared.
%

category: 'testing'
method: StdioTest
testInitialization
	| stdio |
	self clearCache.
	stdio := Stdio stdout.
	self assert: (stdio terminal isKindOf: FileSystem fileClass).
	self assert: stdio isWritable.

	stdio := Stdio stderr.
	self assert: (stdio terminal isKindOf: FileSystem fileClass).
	self assert: stdio isWritable.

	stdio := Stdio stdin.
	self assert: (stdio terminal isKindOf: FileSystem fileClass).
	self deny: stdio isWritable
%

category: 'testing'
method: StdioTest
testUseMemoryStreams
	| stdio |
	self clearCache.
	stdio := Stdio stdout.
	self assert: (stdio terminal isKindOf: FileSystem fileClass).
	self assert: stdio isWritable.

	Stdio useMemoryStreams.
	self verifyStdInstanceCleared.

	stdio := Stdio stdout.
	self assert: (stdio isKindOf: ZnCharacterWriteStream).

	stdio := Stdio stderr.
	self assert: (stdio isKindOf: ZnCharacterWriteStream).

	stdio := Stdio stdin.
	self assert: (stdio isKindOf: ZnCharacterReadStream).
%

category: 'testing'
method: StdioTest
verifyStdInstanceCleared
	#(stdin stdout stderr) do: [:ea | self deny: (SessionTemps current includesKey: ea)].
%

! Class implementation for 'WindowsStoreTest'

!		Instance methods for 'WindowsStoreTest'

category: 'testing'
method: WindowsStoreTest
testAbsoluteFullName
	| filesystem |

	filesystem := FileSystem store: WindowsStore new.

	"Check dropping trailing slash."	
	self assert: (filesystem referenceTo: 'C:\tmp\') fullName equals: 'C:\tmp'.

	"Check round-trip conversion from String to FileReference back to String again."
	#( 	'C:\'		'D:\'		'\'		'C:\tmp'		'D:\tmp'		'C:\tmp\xx'      ) 
		do: [ :pathString | self assert: (filesystem referenceTo: pathString) fullName equals: pathString ].
%

category: 'testing'
method: WindowsStoreTest
testAbsolutePath

	#('c:\' 'C:\temp' 'A:\temp\test' '\test\bar' '\') do: [:each |
		self assert: (WindowsStore current pathFromString: each) isAbsolute ] 
	
%

category: 'testing'
method: WindowsStoreTest
testPrintString
	| filesystem |
	filesystem := FileSystem store: WindowsStore new.
	
	"path"
	self assert: (filesystem referenceTo: 'a/b') printString equals: 'File @ a\b'.
	self assert: (filesystem referenceTo: '\test.txt') printString equals: 'File @ \test.txt'.
	
	"root"
	self assert: (filesystem referenceTo: '/') printString equals: 'File @ \'.
	self assert: (filesystem referenceTo: '\') printString equals: 'File @ \'.
	
	"drive root"
	self assert: (filesystem referenceTo: 'c:\') printString equals: 'File @ c:\'.
	
	"empty"
	self assert: (filesystem referenceTo: './') printString equals: 'File @ .'.
	self assert: (filesystem referenceTo: '.\') printString equals: 'File @ .'
%

category: 'testing'
method: WindowsStoreTest
testRelativeFullName

	"FIleSystem currently does not support MS Windows concept of each drive having its own working directory.
	That is too great a change during Pharo 3 Beta, so for now just drop drive from relative references. 
	Follow up in Case 13094"	

	| filesystem |
	filesystem := FileSystem store: WindowsStore new.
		
	self assert: (filesystem referenceTo: 'C:tmp') fullName equals: (filesystem referenceTo: 'tmp') fullName
%

category: 'testing'
method: WindowsStoreTest
testRelativePath

	#('a' 'bin\foo' 'temp\test' 'C:temp\test') do: [:each |
		self assert: (WindowsStore current pathFromString: each) isRelative ] 
	
%

! Class implementation for 'ZnBase64EncoderTests'

!		Instance methods for 'ZnBase64EncoderTests'

category: 'tests'
method: ZnBase64EncoderTests
testCustomLineBreaking
	| encoder input output charCount |
	encoder := ZnBase64Encoder new.
	encoder breakLinesAt: 16.
	input := (0 to: 255) asByteArray.
	output := encoder encode: input.
	self assert: (encoder decode: output) equals: input.
	charCount := ((256 // 3) + (256 \\ 3) sign) * 4.
	self assert: output size equals: (charCount + (charCount // 16 * String crlf size))
%

category: 'tests'
method: ZnBase64EncoderTests
testDecodingErrors
	| encoder |
	encoder := ZnBase64Encoder new.
	self should: [ encoder decode: 'A' ] raise: ZnCharacterEncodingError.
	self should: [ encoder decode: 'AB' ] raise: ZnCharacterEncodingError.
	self should: [ encoder decode: 'ABC' ] raise: ZnCharacterEncodingError.
	encoder whitespace: #separator.
	self should: [ encoder decode: '*' ] raise: ZnCharacterEncodingError.
	self should: [ encoder decode: '**' ] raise: ZnCharacterEncodingError.
	self should: [ encoder decode: '***' ] raise: ZnCharacterEncodingError.
	self should: [ encoder decode: '****' ] raise: ZnCharacterEncodingError.
	encoder whitespace: nil.
	self should: [ encoder decode: '*' ] raise: ZnCharacterEncodingError.
	self should: [ encoder decode: '**' ] raise: ZnCharacterEncodingError.
	self should: [ encoder decode: '***' ] raise: ZnCharacterEncodingError.
	self should: [ encoder decode: '****' ] raise: ZnCharacterEncodingError.
%

category: 'tests'
method: ZnBase64EncoderTests
testEmpty
	| encoder |
	encoder := ZnBase64Encoder new.
	self 
		assert: (encoder encode: #[])
		equals: ''.
	self
		assert: (encoder decode: '')
		equals: #[]
%

category: 'tests'
method: ZnBase64EncoderTests
testFullAlphabet
	| encoder input output |
	encoder := ZnBase64Encoder new.
	input := encoder alphabet.
	output := encoder decode: input.
	self assert: (encoder encode: output) equals: input.
	encoder breakLines.
	output := encoder decode: input.
	self assert: (encoder encode: output) equals: input
%

category: 'tests'
method: ZnBase64EncoderTests
testFullSpectrum
	| encoder input output |
	encoder := ZnBase64Encoder new.
	input := (0 to: 255) asByteArray , (255 to: 0) asByteArray.
	output := encoder encode: input.
	self assert: (encoder decode: output) equals: input.
	encoder breakLines.
	output := encoder encode: input.
	self assert: (encoder decode: output) equals: input.
%

category: 'tests'
method: ZnBase64EncoderTests
testPadding
	| encoder |
	encoder := ZnBase64Encoder new.
	self assert: (encoder encode: 'M' asByteArray) equals: 'TQ=='.
	self assert: (encoder decode: 'TQ==') equals: 'M' asByteArray.
	self assert: (encoder encode: 'Ma' asByteArray) equals: 'TWE='.
	self assert: (encoder decode: 'TWE=') equals: 'Ma' asByteArray
%

category: 'tests'
method: ZnBase64EncoderTests
testQuote
	| input output encoder break |
	encoder := ZnBase64Encoder new lineEndConvention: #cr; breakLines; yourself.
	input := 'Man is distinguished, not only by his reason, but by this singular passion from other animals, which is a lust of the mind, that by a perseverance of delight in the continued and indefatigable generation of knowledge, exceeds the short vehemence of any carnal pleasure.'.
	break := String with: Character cr.
	output := 'TWFuIGlzIGRpc3Rpbmd1aXNoZWQsIG5vdCBvbmx5IGJ5IGhpcyByZWFzb24sIGJ1dCBieSB0aGlz' , break 
				, 'IHNpbmd1bGFyIHBhc3Npb24gZnJvbSBvdGhlciBhbmltYWxzLCB3aGljaCBpcyBhIGx1c3Qgb2Yg' , break 
				, 'dGhlIG1pbmQsIHRoYXQgYnkgYSBwZXJzZXZlcmFuY2Ugb2YgZGVsaWdodCBpbiB0aGUgY29udGlu' , break 
				, 'dWVkIGFuZCBpbmRlZmF0aWdhYmxlIGdlbmVyYXRpb24gb2Yga25vd2xlZGdlLCBleGNlZWRzIHRo' , break 
				, 'ZSBzaG9ydCB2ZWhlbWVuY2Ugb2YgYW55IGNhcm5hbCBwbGVhc3VyZS4='.
	self 
		assert: (encoder encode: input asByteArray)
		equals: output.
	self 
		assert: (encoder decode: output) 
		equals: input asByteArray
%

category: 'tests'
method: ZnBase64EncoderTests
testSimple
	| encoder |
	encoder := ZnBase64Encoder new.
	self 
		assert: (encoder encode: 'Man' asByteArray)
		equals: 'TWFu'.
	self
		assert: (encoder decode: 'TWFu')
		equals: 'Man' asByteArray
%

category: 'tests'
method: ZnBase64EncoderTests
testWhitespaceAtEnd
	| encoder |
	encoder := ZnBase64Encoder new.
	"whitespace is #any non-alphabet character"
	self assert: (encoder decode: 'TQ==' , String lf) equals: 'M' asByteArray.
	encoder whitespace: #separator.
	self assert: (encoder decode: 'TQ==' , String lf) equals: 'M' asByteArray.
	encoder whitespace: nil.
	self should: [ encoder decode: 'TQ==' , String lf ] raise: ZnCharacterEncodingError
%

! Class implementation for 'ZnBufferedReadStreamTests'

!		Instance methods for 'ZnBufferedReadStreamTests'

category: 'testing'
method: ZnBufferedReadStreamTests
testBuffering
	| stream |
	stream := ZnBufferedReadStream on: '01234567890123456789' readStreamPortable.
	stream sizeBuffer: 8.
	self deny: stream atEnd.
	self assert: (stream next: 10) equals: '0123456789'.
	self deny: stream atEnd.
	self assert: (stream next: 10) equals: '0123456789'.
	self assert: stream atEnd	
%

category: 'testing'
method: ZnBufferedReadStreamTests
testPeek
	| stream |
	stream := ZnBufferedReadStream on: '0123456789' readStreamPortable.
	stream sizeBuffer: 8.
	'0123456789' do: [ :each |
		self deny: stream atEnd.
		self assert: stream peek equals: each.
		self assert: stream next equals: each ].
	self assert: stream atEnd.
	self assert: stream peek isNil.
	self assert: stream next isNil
%

category: 'testing'
method: ZnBufferedReadStreamTests
testReadInto
	| stream buffer count |
	stream := ZnBufferedReadStream on: '0123456789' readStreamPortable.
	stream sizeBuffer: 8.
	buffer := String new: 6 withAll: Character space.
	stream skip: 1.
	stream readInto: buffer startingAt: 4 count: 3.
	self assert: buffer equals: '   123'.
	stream readInto: buffer startingAt: 1 count: 3.
	self assert: buffer equals: '456123'.
	count := stream readInto: buffer startingAt: 1 count: 100.
	self assert: count equals: 3.
	self assert: buffer equals: '789123'	 
%

category: 'testing'
method: ZnBufferedReadStreamTests
testReadIntoLarger
	| stream buffer count |
	stream := ZnBufferedReadStream on: '0123456789' readStreamPortable.
	stream sizeBuffer: 4.
	buffer := String new: 10.
	count := stream readInto: buffer startingAt: 1 count: 10.
	self assert: count equals: 10.
	self assert: buffer equals: '0123456789'	 
%

category: 'testing'
method: ZnBufferedReadStreamTests
testReadUpTo
	| stream |
	stream := ZnBufferedReadStream on: '0123456789' readStreamPortable.
	stream sizeBuffer: 8.
	self assert: (stream upTo: $5) equals: '01234'.
	self assert: stream upToEnd equals: '6789'.
	self assert: stream atEnd
%

category: 'testing'
method: ZnBufferedReadStreamTests
testReadUpToEnd
	| stream |
	stream := ZnBufferedReadStream on: '0123456789' readStreamPortable.
	stream sizeBuffer: 4.
	stream next: 2.
	self assert: stream upToEnd equals: '23456789'.
	self assert: stream atEnd
%

! Class implementation for 'ZnBufferedReadWriteStreamTests'

!		Instance methods for 'ZnBufferedReadWriteStreamTests'

category: 'tests'
method: ZnBufferedReadWriteStreamTests
testBuffering
	| stream |
	stream := ZnBufferedReadWriteStream on: '01234567890123456789' readStreamPortable.
	stream sizeBuffer: 8.
	self deny: stream atEnd.
	self assert: (stream next: 10) equals: '0123456789'.
	self deny: stream atEnd.
	self assert: (stream next: 10) equals: '0123456789'.
	self assert: stream atEnd
%

category: 'tests'
method: ZnBufferedReadWriteStreamTests
testNextPutAllStartingAt
	| string |
	string := String streamContents: [ :stringStream | 
		ZnBufferedReadWriteStream on: stringStream do: [ : bufferedStream |
			bufferedStream sizeBuffer: 8.
			bufferedStream next: 5 putAll: '--012345--' startingAt: 3.
			bufferedStream next: 5 putAll: '0123456789XX' startingAt: 6. 
			bufferedStream next: 5 putAll: '--012345--' startingAt: 3.
			bufferedStream next: 5 putAll: '0123456789XX' startingAt: 6.] ].
	self assert: string equals: '01234567890123456789'
%

category: 'tests'
method: ZnBufferedReadWriteStreamTests
testPeek
	| stream |
	stream := ZnBufferedReadWriteStream on: '0123456789' readStreamPortable.
	stream sizeBuffer: 8.
	'0123456789' do: [ :each |
		self deny: stream atEnd.
		self assert: stream peek equals: each.
		self assert: stream next equals: each ].
	self assert: stream atEnd.
	self assert: stream peek isNil.
	self assert: stream next isNil
%

category: 'tests'
method: ZnBufferedReadWriteStreamTests
testReadInto
	| stream buffer count |
	stream := ZnBufferedReadWriteStream on: '0123456789' readStreamPortable.
	stream sizeBuffer: 8.
	buffer := String new: 6 withAll: Character space.
	stream skip: 1.
	stream readInto: buffer startingAt: 4 count: 3.
	self assert: buffer equals: '   123'.
	stream readInto: buffer startingAt: 1 count: 3.
	self assert: buffer equals: '456123'.
	count := stream readInto: buffer startingAt: 1 count: 100.
	self assert: count equals: 3.
	self assert: buffer equals: '789123'	 
%

category: 'tests'
method: ZnBufferedReadWriteStreamTests
testReadIntoLarger
	| stream buffer count |
	stream := ZnBufferedReadWriteStream on: '0123456789' readStreamPortable.
	stream sizeBuffer: 4.
	buffer := String new: 10.
	count := stream readInto: buffer startingAt: 1 count: 10.
	self assert: count equals: 10.
	self assert: buffer equals: '0123456789'	 
%

category: 'tests'
method: ZnBufferedReadWriteStreamTests
testReadThenWrite
	| stream stringStream |
	
	stringStream := ReadWriteStreamPortable with: '0123456789' copy.
	stringStream reset.
	stream := ZnBufferedReadWriteStream on: stringStream.
	stream sizeBuffer: 8.
	
	stream next: 4.
	self assert: stream position equals: 4.
	
	stream nextPutAll: 'ABCD'.
	self assert: stream position equals: 8.
		
	self assert: stream peek equals: $8. 
	self assert: stream upToEnd equals: '89'.
	self assert: stream atEnd.
	
	self assert: stringStream contents equals: '0123ABCD89'
%

category: 'tests'
method: ZnBufferedReadWriteStreamTests
testReadUpTo
	| stream |
	stream := ZnBufferedReadWriteStream on: '0123456789' readStreamPortable.
	stream sizeBuffer: 8.
	self assert: (stream upTo: $5) equals: '01234'.
	self assert: stream upToEnd equals: '6789'.
	self assert: stream atEnd
%

category: 'tests'
method: ZnBufferedReadWriteStreamTests
testReadUpToEnd
	| stream |
	stream := ZnBufferedReadWriteStream on: '0123456789' readStreamPortable.
	stream sizeBuffer: 4.
	stream next: 2.
	self assert: stream upToEnd equals: '23456789'.
	self assert: stream atEnd
%

category: 'tests'
method: ZnBufferedReadWriteStreamTests
testWriteThenRead
	| stream stringStream |
	
	stringStream := ReadWriteStreamPortable with: '0123456789' copy.
	stringStream reset.
	stream := ZnBufferedReadWriteStream on: stringStream.
	stream sizeBuffer: 8.
	
	stream nextPutAll: 'ABCD'.
	
	self assert: stream peek equals: $4. 
	self assert: stream position equals: 4.
	self assert: stream upToEnd equals: '456789'.
	self assert: stream position equals: 10.
	self assert: stream atEnd.
	
	self assert: stringStream contents equals: 'ABCD456789'
%

category: 'tests'
method: ZnBufferedReadWriteStreamTests
testWriting
	| string |
	string := String streamContents: [ :stringStream | | bufferedStream |
		bufferedStream := ZnBufferedReadWriteStream on: stringStream.
		0 to: 9 do: [ :each | bufferedStream nextPut: (Character digitValue: each) ].
		bufferedStream flush ].
	self assert: string = '0123456789'
%

category: 'tests'
method: ZnBufferedReadWriteStreamTests
testWritingOverflow
	| string |
	string := String streamContents: [ :stringStream | | bufferedStream |
		bufferedStream := ZnBufferedReadWriteStream on: stringStream.
		bufferedStream sizeBuffer: 8.
		0 to: 9 do: [ :each | bufferedStream nextPut: (Character digitValue: each) ].
		bufferedStream nextPutAll: '0123'; nextPutAll: '4567'; nextPutAll: '89'.
		bufferedStream nextPutAll: '0123456789'; nextPutAll: '0123456789'.
		bufferedStream flush ].
	self assert: string = '0123456789012345678901234567890123456789'
%

! Class implementation for 'ZnBufferedStreamByteTests'

!		Instance methods for 'ZnBufferedStreamByteTests'

category: 'accessing'
method: ZnBufferedStreamByteTests
integerEncodingSpec
	^ #(
"<hex-bytes> <integer> <u|s> <be|le> unsigned|signed big-endian|little-endian"
('00' 0 u be)
('00' 0 s be)
('01' 1 u be)
('01' 1 s be)
('FF' -1 s be)
('FF' 255 u be)
('7B' 123 u be)
('7B' 123 s be)
('85' -123 s be)
('7F' 127 u be)
('7F' 127 s be)
('80' -128 s be)
('00' 0 u le)
('00' 0 s le)
('01' 1 u le)
('01' 1 s le)
('FF' -1 s le)
('FF' 255 u le)
('7B' 123 u le)
('7B' 123 s le)
('85' -123 s le)
('7F' 127 u le)
('7F' 127 s le)
('80' -128 s le)
('00000000' 0 u be)
('00000000' 0 s be)
('00000000' 0 u le)
('00000000' 0 s le)
('00000001' 1 u be)
('00000001' 1 s be)
('01000000' 1 u le)
('01000000' 1 s le)
('FFFFFFFF' -1 s be)
('FFFFFFFF' -1 s le)
('000004D2' 1234 u be)
('000004D2' 1234 s be)
('FFFFFB2E' -1234 s be)
('D2040000' 1234 u le)
('D2040000' 1234 s le)
('2EFBFFFF' -1234 s le)
('FFFFFFFF' 4294967295 u be)
('FFFFFFFF' 4294967295 u le)
('7FFFFFFF' 2147483647 u be)
('7FFFFFFF' 2147483647 s be)
('80000000' -2147483648 s be)
('FFFFFF7F' 2147483647 s le)
('00000080' -2147483648 s le)
('499602D2' 1234567890 u be)
('499602D2' 1234567890 s be)
('B669FD2E' -1234567890 s be)
('D2029649' 1234567890 u le)
('D2029649' 1234567890 s le)
('2EFD69B6' -1234567890 s le)
)
%

category: 'testing'
method: ZnBufferedStreamByteTests
testInt16Aliases
	| input writer |
	writer := [ :block | 
		ByteArray streamContents: [ :out | 
			ZnBufferedWriteStream on: out do: block ] ].
	input := ByteArray readHexFrom: '04D2'.
	self assert: (ZnBufferedReadStream on: input readStreamPortable) int16 equals: 1234.
	self assert: (ZnBufferedReadStream on: input readStreamPortable) uint16 equals: 1234.
	self assert: (ZnBufferedReadStream on: input readStreamPortable) nextWord equals: 1234.
	self assert: ((ZnBufferedReadStream on: input readStreamPortable) nextNumber: 2) equals: 1234.
	self assert: ((ZnBufferedReadStream on: input reversed readStreamPortable) nextLittleEndianNumber: 2) equals: 1234.	
	self assert: (writer value: [ :out | out int16: 1234 ]) equals: input.
	self assert: (writer value: [ :out | out uint16: 1234 ]) equals: input.
	self assert: (writer value: [ :out | out nextWordPut: 1234 ]) equals: input.
	self assert: (writer value: [ :out | out nextNumber: 2 put: 1234 ]) equals: input.
	self assert: (writer value: [ :out | out nextLittleEndianNumber: 2 put: 1234 ]) equals: input reversed.
	input := ByteArray readHexFrom: 'FB2E'.
	self assert: (ZnBufferedReadStream on: input readStreamPortable) int16 equals: -1234.
	self assert: (writer value: [ :out | out int16: -1234 ]) equals: input
%

category: 'testing'
method: ZnBufferedStreamByteTests
testInt32Aliases
	| input writer |
	writer := [ :block | 
		ByteArray streamContents: [ :out | 
			ZnBufferedWriteStream on: out do: block ] ].
	input := ByteArray readHexFrom: '499602D2'.
	self assert: (ZnBufferedReadStream on: input readStreamPortable) int32 equals: 1234567890.
	self assert: (ZnBufferedReadStream on: input readStreamPortable) uint32 equals: 1234567890.
	self assert: (ZnBufferedReadStream on: input readStreamPortable) nextInt32 equals: 1234567890.
	self assert: ((ZnBufferedReadStream on: input readStreamPortable) nextNumber: 4) equals: 1234567890.
	self assert: ((ZnBufferedReadStream on: input reversed readStreamPortable) nextLittleEndianNumber: 4) equals: 1234567890.	
	self assert: (writer value: [ :out | out int32: 1234567890 ]) equals: input.
	self assert: (writer value: [ :out | out uint32: 1234567890 ]) equals: input.
	self assert: (writer value: [ :out | out nextInt32Put: 1234567890 ]) equals: input.
	self assert: (writer value: [ :out | out nextNumber: 4 put: 1234567890 ]) equals: input.
	self assert: (writer value: [ :out | out nextLittleEndianNumber: 4 put: 1234567890 ]) equals: input reversed.
	input := ByteArray readHexFrom: 'B669FD2E'.
	self assert: (ZnBufferedReadStream on: input readStreamPortable) int32 equals: -1234567890.
	self assert: (ZnBufferedReadStream on: input readStreamPortable) nextInt32 equals: -1234567890.
	self assert: (writer value: [ :out | out int32: -1234567890 ]) equals: input.
	self assert: (writer value: [ :out | out nextInt32Put: -1234567890 ]) equals: input
%

category: 'testing'
method: ZnBufferedStreamByteTests
testInt8
	| bytes readStream |
	bytes := ByteArray streamContents: [ :out |
		ZnBufferedWriteStream on: out do: [ :bout |
			bout int8: 123; uint8: 123; int8: -123 ] ].
	readStream := ZnBufferedReadStream on: bytes readStreamPortable.
	self assert: readStream peek equals: 123.
	self assert: readStream int8 equals: 123.
	self assert: readStream peek equals: 123.
	self assert: readStream uint8 equals: 123.
	self deny: readStream peek = 123.
	self deny: readStream peek = -123.
	self assert: readStream int8 equals: -123
%

category: 'testing'
method: ZnBufferedStreamByteTests
testNextIntegerOfSizeSignedBigEndian
	self integerEncodingSpec do: [ :spec |
		| input integer |
		input := ByteArray readHexFrom: spec first.
		integer := (ZnBufferedReadStream on: input readStreamPortable) 
			nextIntegerOfSize: input 
			size signed: spec third = #s 
			bigEndian: spec fourth = #be.
		self assert: integer equals: spec second ]
%

category: 'testing'
method: ZnBufferedStreamByteTests
testNextIntegerOfSizeSignedBigEndianPut
	self integerEncodingSpec do: [ :spec |
		| input output |
		input := ByteArray readHexFrom: spec first.
		output := ByteArray streamContents: [ :out |
			ZnBufferedWriteStream on: out do: [ :bout | 
				bout 
					nextIntegerOfSize: input size 
					signed: spec third = #s 
					bigEndian: spec fourth = #be 
					put: spec second ] ].
		self assert: output equals: input ]
%

! Class implementation for 'ZnBufferedWriteStreamTests'

!		Instance methods for 'ZnBufferedWriteStreamTests'

category: 'testing'
method: ZnBufferedWriteStreamTests
testNextPutAllStartingAt
	| string |
	string := String streamContents: [ :stringStream | 
		ZnBufferedWriteStream on: stringStream do: [ : bufferedStream |
			bufferedStream sizeBuffer: 8.
			bufferedStream next: 5 putAll: '--012345--' startingAt: 3.
			bufferedStream next: 5 putAll: '0123456789XX' startingAt: 6. 
			bufferedStream next: 5 putAll: '--012345--' startingAt: 3.
			bufferedStream next: 5 putAll: '0123456789XX' startingAt: 6.] ].
	self assert: string equals: '01234567890123456789'
%

category: 'testing'
method: ZnBufferedWriteStreamTests
testWriting
	| string |
	string := String streamContents: [ :stringStream | | bufferedStream |
		bufferedStream := ZnBufferedWriteStream on: stringStream.
		0 to: 9 do: [ :each | bufferedStream nextPut: (Character digitValue: each) ].
		bufferedStream flush ].
	self assert: string = '0123456789'
%

category: 'testing'
method: ZnBufferedWriteStreamTests
testWritingOverflow
	| string |
	string := String streamContents: [ :stringStream | | bufferedStream |
		bufferedStream := ZnBufferedWriteStream on: stringStream.
		bufferedStream sizeBuffer: 8.
		0 to: 9 do: [ :each | bufferedStream nextPut: (Character digitValue: each) ].
		bufferedStream nextPutAll: '0123'; nextPutAll: '4567'; nextPutAll: '89'.
		bufferedStream nextPutAll: '0123456789'; nextPutAll: '0123456789'.
		bufferedStream flush ].
	self assert: string = '0123456789012345678901234567890123456789'
%

! Class implementation for 'ZnCharacterEncoderTests'

!		Class methods for 'ZnCharacterEncoderTests'

category: 'accessing'
classmethod: ZnCharacterEncoderTests
asciiCharacterSource
	^ ($A to: $Z), ($a to: $z), ($0 to: $9), '.-_/*+=|,;?!$&<>^%#', '    '
%

category: 'accessing'
classmethod: ZnCharacterEncoderTests
latin1CharacterSource
	^ ($A to: $Z), ($a to: $z), ($0 to: $9), '.-_/*+=|,;?!$&<>^%#', '       ', 'éèçüäßñ'
%

category: 'accessing'
classmethod: ZnCharacterEncoderTests
stringOfSize: size fromSource: source
	"self stringOfSize: 1024 fromSource: self unicodeCharacterSource"
	
	^ String new: size streamContents: [ :out | 
		size timesRepeat: [ out nextPut: source atRandom ] ]
%

category: 'accessing'
classmethod: ZnCharacterEncoderTests
unicodeCharacterSource
	^ ($A to: $Z), ($a to: $z), ($0 to: $9), '.-_/*+=|,;?!$&<>^%#', '         ', 'éèçüäßñα', '€∏'
%

!		Instance methods for 'ZnCharacterEncoderTests'

category: 'public'
method: ZnCharacterEncoderTests
assertCharacterCollection: anObject equals: otherObj
	"allow comparison between unitcode and legacy strings in legacy mode"

	self
		assert: (anObject isEquivalent: otherObj)
		description: anObject printString , ' is not equal to ' , otherObj printString.
%

category: 'private'
method: ZnCharacterEncoderTests
decodeBytes: bytes with: encoder

	"GemStone does not support streamed decoding ... hack for tests"
	"^ bytes decodeFromUTF8"

	"Uncomment the following code for decoding"

	| input |
	input := bytes readStream.
	^ String streamContents: [ :stream |
		[ input atEnd ] whileFalse: [ 
			stream nextPut: (encoder nextFromStream: input) ] ]
%

category: 'private'
method: ZnCharacterEncoderTests
encodeString: string with: encoder
"
	^ ByteArray streamContents: [ :stream |
		stream nextPutAll: string encodeAsUTF8 ]
"

	^ ByteArray streamContents: [ :stream |
		string do: [ :each |
			encoder nextPut: each toStream: stream ] ]
%

category: 'testing'
method: ZnCharacterEncoderTests
testAllByteEncoderDomains
	| encoder characterDomain byteDomain encoded |
	ZnByteEncoder knownEncodingIdentifiers do: [ :identifier |
		encoder := ZnCharacterEncoder newForEncoding: identifier.
		self assert: encoder identifier equals: identifier.
		self assert: encoder isStrict.
		self assert: encoder isLenient not.
		self assert: (encoder encodeString: 'hello') equals: #[104 101 108 108 111] .
		self assert: (encoder decodeBytes: #[104 101 108 108 111] ) equals: 'hello'.
		characterDomain := encoder characterDomain.
		byteDomain := encoder byteDomain.
		characterDomain do: [ :each |
			self assert: (encoder encodedByteCountFor: each) equals: 1.
			encoded := ByteArray streamContents: [ :out | encoder nextPut: each toStream: out ].
			self assert: encoded size equals: 1.
			self assert: (byteDomain includes: encoded first) ].
		byteDomain do: [ :each |
			self assert: (characterDomain includes: (encoder nextFromStream: (ByteArray with: each) readStream)) ] ]
%

category: 'testing'
method: ZnCharacterEncoderTests
testBeLenient
	| encoder unmapped |
	encoder := ZnCharacterEncoder newForEncoding: 'iso-8859-1'.
	unmapped := (128 to: 159) asByteArray.
	self should: [ encoder encodeString: unmapped asString ] raise: ZnCharacterEncodingError.
	self should: [ encoder decodeBytes: unmapped ] raise: ZnCharacterEncodingError.
	encoder beLenient.
	self assert: (encoder encodeString: unmapped asString) equals: unmapped.
	self assert: (encoder decodeBytes: unmapped) equals: unmapped asString.
%

category: 'testing'
method: ZnCharacterEncoderTests
testByteDecoding
	| encoder bytes |
	encoder := ZnUTF8Encoder new.
	bytes := encoder encodeString: 'élève en Français'.
	self assert: (bytes decodeWith: encoder) equals: (encoder decodeBytes: bytes).
	self assert: (bytes decodeWith: #utf8) equals: (encoder decodeBytes: bytes).
	self assert: bytes utf8Decoded equals: (encoder decodeBytes: bytes)
%

category: 'testing'
method: ZnCharacterEncoderTests
testCodePointEncodingDecoding
	| encoder input output |
	input := 'Düsseldorf Königsallee' asByteArray asArray.
	self assert: input isCollection.
	self assert: (input allSatisfy: [:ea | ea _isInteger]).
	#(utf8 utf16 utf32 latin1 null) do: [ :each |
		encoder := each asZnCharacterEncoder.
		output := encoder encodeCodePoints: input.
		self assert: output isCollection.
		self assert: (output allSatisfy: [ :e | e _isInteger and: [ e between: 0 and: 255 ] ] ).
		self assert: (encoder encodedByteCountForCodePoints: input) equals: output size.
		self assert: (encoder decodeAsCodePoints: output) equals: input ]
%

category: 'testing'
method: ZnCharacterEncoderTests
testCodePointStreams
	| string codePoints bytes result result2 |
	string := 'Düsseldorf Königsallee'.
	codePoints := string asByteArray asArray.
	self assert: codePoints isCollection.
	self assert: (codePoints allSatisfy: [:ea | ea isKindOf: Integer]).
	#(utf8 utf16 utf32 latin1 null) do: [ :each |
		bytes := ByteArray streamContents: [ :out |
			(ZnCodePointWriteStream on: out encoding: each)
				nextPutAll: codePoints ].
		result2 := each asZnCharacterEncoder encodeString: string.
		self assert: bytes equals: result2.
		result := (ZnCodePointReadStream on: bytes readStream encoding: each) upToEnd.
		self assert: result equals: codePoints.
		self assert: (codePoints collect: [:ea | ea asCharacter] as: String) equals: string ]
%

category: 'testing'
method: ZnCharacterEncoderTests
testConvencienceMethods
	| encoder string |
	encoder := ZnUTF8Encoder new.
	string := 'élève en Français'.
	self assertCharacterCollection: (encoder decodeBytes: (encoder encodeString: string)) equals: string.
	self assert: (encoder encodedByteCountForString: string) = 20.
	
	#( 'ccc' 'ççç' 'c' 'ç' 'çc' 'cç' ) do: [ :each |
		self assertCharacterCollection: (encoder decodeBytes: (encoder encodeString: each)) equals: each ]
%

category: 'testing'
method: ZnCharacterEncoderTests
testDefault
	| string bytes |  
	self assert: ZnCharacterEncoder default equals: ZnUTF8Encoder default.
	string := 'Der Weg zur Hölle ist mit guten Vorsätzen gepflastert.'.
	bytes := ZnUTF8Encoder new encodeString: string.
	ZnDefaultCharacterEncoder 
		value: ZnUTF8Encoder new
		during: [ 
			self 
				assertString: (ZnCharacterEncoder default decodeBytes: bytes) 
				equalsString: string ].
	ZnDefaultCharacterEncoder 
		value: ZnUTF8Encoder new
		during: [ 
			self 
				assertString: ((ZnCharacterEncoder newForEncoding: 'unknown') decodeBytes: bytes) 
				equalsString: string ]
%

category: 'testing'
method: ZnCharacterEncoderTests
testDetectEncoding
	| bytes |
	bytes := 'abc' asByteArray.
	self assert: (ZnCharacterEncoder detectEncoding: bytes) equals: ZnCharacterEncoder ascii.
	bytes := ZnCharacterEncoder iso88591 encodeString: 'Les élèves Français'.
	self assert: (ZnCharacterEncoder detectEncoding: bytes) equals: ZnCharacterEncoder iso88591.
	bytes := ZnCharacterEncoder utf8 encodeString: 'Les élèves Français'.
	self assert: (ZnCharacterEncoder detectEncoding: bytes) equals: ZnCharacterEncoder utf8.
%

category: 'testing'
method: ZnCharacterEncoderTests
testKnownEncodingIdentifiers
	| all minimal asciiString aCollection |
	all := ZnCharacterEncoder knownEncodingIdentifiers asSet.
	minimal := #('utf8' 'latin1' 'null' 'ascii' 'iso88591') asSet.
	"make sure at least a minimal set is present"
	self assert: ( all  select: [ :each | (minimal includes: each) ] ) equals: minimal.
	asciiString := String withAll: ((($a asciiValue to: $z asciiValue) , 
		($A asciiValue to: $Z asciiValue) , ($0 asciiValue to: $9 asciiValue)) collect: [:each | Character codePoint: each ]).
	"make sure that each identifier can be used to instanciate a decoder,
	and that those decoders at least work on a ASCII string in both directions"
	all do: [ :each |
		| encoder bytes |
		encoder := ZnCharacterEncoder newForEncoding: each.
		bytes := encoder encodeString: asciiString.
		self assert: ((encoder decodeBytes: bytes) isEquivalent: asciiString) ]
%

category: 'testing'
method: ZnCharacterEncoderTests
testLatin1Encoder
	| encoder string bytes |
	encoder := ZnCharacterEncoder newForEncoding: 'latin1'.
	string := 'élève en Français'.
	bytes := #(233 108 232 118 101 32 101 110 32 70 114 97 110 231 97 105 115) asByteArray.
	self assert: (encoder encodeString: string) equals: bytes.
	self assert: (encoder decodeBytes: bytes) equals: string
%

category: 'testing'
method: ZnCharacterEncoderTests
testLatin2Encoder
	"Example characters taken from http://en.wikipedia.org/wiki/Latin2"
	
	| encoder inputBytes outputBytes inputString outputString |
	encoder := ZnCharacterEncoder newForEncoding: 'latin2'.
	inputString := String 
		with: (16r0154 asCharacter) with: (16r0110 asCharacter) 
		with: ( 16r0155 asCharacter) with: (16r0111 asCharacter).
	inputBytes := #(192 208 224 240) asByteArray.
	outputBytes := self encodeString: inputString with: encoder.
	self assert: outputBytes = inputBytes.
	outputString := self decodeBytes: inputBytes with: encoder.
	self assert: outputString = inputString
%

category: 'testing'
method: ZnCharacterEncoderTests
testNextPutAllStartingAtToStream
	| encoder |
	encoder := ZnUTF8Encoder new.
	#( 'ccc' 'ççç' 'c' 'ç' 'çc' 'cç' 'çç' ) do: [ :each |
		#( ( '' '' ) ( 'ABC' '' ) ( '' 'ABC' ) ( 'ABC' 'ABC' ) 
			( 'AéC' '' ) ( '' 'AèC' ) ( 'AéC' 'AèC' ) 
			( 'AXC' 'AèC' ) ( 'AéC' 'AXC' ) 
			( 'PRE' 'ç' ) ) do: [ :extra |
				| prefix postfix string bytes |
				prefix := extra first.
				postfix := extra last.
				string := prefix, each, postfix.
				bytes := ByteArray streamContents: [ :out |
					encoder next: each size putAll: string startingAt: prefix size + 1 toStream: out ].
				self assertCharacterCollection: (encoder decodeBytes: bytes) equals: each ] ]
%

category: 'testing'
method: ZnCharacterEncoderTests
testNullEncoder
	| encoder bytes string |
	encoder := ZnNullEncoder new.
	bytes := self encodeString: 'abc' with: encoder.
	self assert: bytes = #(97 98 99) asByteArray.
	string := self decodeBytes: #(65 66 67) asByteArray with: encoder.
	self assert: string = 'ABC'
%

category: 'testing'
method: ZnCharacterEncoderTests
testReadIntoStartingAtCountFromStream
	| encoder |
	encoder := ZnUTF8Encoder new.
	#( 'ccc' 'ççç' 'c' 'ç' 'çc' 'cç' 'çç' ) do: [ :each |
		#( ( '' '' ) ( 'ABC' '' ) ( '' 'ABC' ) ( 'ABC' 'ABC' ) 
			( 'AéC' '' ) ( '' 'AèC' ) ( 'AéC' 'AèC' ) 
			( 'AXC' 'AèC' ) ( 'AéC' 'AXC' ) 
			( 'PRE' 'ç' ) ) do: [ :extra |
				| prefix postfix string bytes buffer read |
				prefix := extra first.
				postfix := extra last.
				string := prefix, each.
				bytes := encoder encodeString: string, postfix.
				buffer := String new: string size.
				read := encoder readInto: buffer startingAt: 1 count: string size fromStream: bytes readStream.
				self assert: buffer equals: string.
				self assert: read equals: string size ] ]
%

category: 'testing'
method: ZnCharacterEncoderTests
testReadIntoStartingAtCountFromStreamAtEnd
	| input encoder bytes readStream string read |
	encoder := ZnUTF8Encoder new.
	input := 'élève'.
	bytes := encoder encodeString: input.
	readStream := bytes readStream.
	string := String new: 5 withAll: $_.
	read := encoder readInto: string startingAt: 1 count: 10 fromStream: readStream.
	self assert: string equals: input.
	self assert: read equals: 5
%

category: 'testing'
method: ZnCharacterEncoderTests
testReadIntoStartingAtCountFromStreamWide
	| encoder |

self flag: 'TODO: WideString does not exist in Gemstone. Using String which can grow to a QuadByteString'.

	encoder := ZnUTF8Encoder new.
	{ 'abc', (String withAll: { 300 asCharacter. 400 asCharacter. 500 asCharacter}), 'xyz' } do: [ :each |
			| bytes buffer notified read |
			bytes := encoder encodeString: each.
			buffer := String new: each size.
			notified := false.
			[ read := encoder readInto: buffer startingAt: 1 count: each size fromStream: bytes readStream ]
				on: ZnByteStringBecameWideString 
				do: [ :notification |
					self deny: notified.
					notified := true.
					buffer := notification wideString.
					notification resume ].
			self assert: notified.
			self assert: buffer equals: each.
			self assert: read equals: each size ]
%

category: 'testing'
method: ZnCharacterEncoderTests
testReadIntoStartingAtCountFromStreamWithOffset
	| input encoder bytes readStream string read |
	encoder := ZnUTF8Encoder new.
	input := '_élève_'.
	bytes := encoder encodeString: input.
	readStream := bytes readStream.
	readStream next.
	string := String new: 7 withAll: $_.
	read := encoder readInto: string startingAt: 2 count: 5 fromStream: readStream.
	self assertCharacterCollection: string equals: input.
	self assert: read equals: 5.
	input := '_Français_'.
	bytes := encoder encodeString: input.
	readStream := bytes readStream.
	readStream next.
	string := String new: 10 withAll: $_.
	read := encoder readInto: string startingAt: 2 count: 8 fromStream: readStream.
	self assert: string equals: input.
	self assert: read equals: 8
%

category: 'testing'
method: ZnCharacterEncoderTests
testStringEncoding
	| encoder string |
	encoder := ZnUTF8Encoder new.
	string := 'élève en Français'.
	self assert: (string encodeWith: encoder) equals: (encoder encodeString: string).
	self assert: (string encodeWith: #utf8) equals: (encoder encodeString: string).
	"self assert: string encodeAsUTF8 asByteArray equals: (encoder encodeString: string)."
	self assert: string utf8Encoded equals: (encoder encodeString: string)
%

category: 'testing'
method: ZnCharacterEncoderTests
testUTF16Back
	| encoder stream |
	encoder := ZnUTF16Encoder new.
	stream := (encoder encodeString: 'Les élèves Françaises') readStream.
	self should: [ encoder backOnStream: stream ] raise: Error.
	4 timesRepeat: [ encoder nextFromStream: stream ].
	self assert: (encoder nextFromStream: stream) equals: $é.
	encoder backOnStream: stream.
	self assert: (encoder nextFromStream: stream) equals: $é.
	10 timesRepeat: [ encoder nextFromStream: stream ].
	13 timesRepeat: [ encoder backOnStream: stream ].
	self assert: (encoder nextFromStream: stream) equals: $s
%

category: 'testing'
method: ZnCharacterEncoderTests
testUTF16EncoderBigEndian
	| string bytes encoder |
	string := 'élève en Français'.
	bytes := ByteArray readHexFrom:
		'00E9006C00E80076006500200065006E0020004600720061006E00E7006100690073'.
	encoder := ZnUTF16Encoder new.
	self assert: encoder isBigEndian.
	self assert: (encoder encodeString: string) equals: bytes.
	self assert: (encoder decodeBytes: bytes) equals: string
%

category: 'testing'
method: ZnCharacterEncoderTests
testUTF16EncoderByteOrderMark
	| string bytes encoder encoded |
	string := 'foo'.
	bytes := ByteArray readHexFrom: 'FEFF0066006f006f'.
	encoder := ZnUTF16Encoder new.
	self assert: encoder isBigEndian.
	encoded := ByteArray streamContents: [ :out |
		encoder nextPutByteOrderMarkToStream: out.
		encoder next: string size putAll: string startingAt: 1 toStream: out ].
	self assert: encoded equals: bytes.
	self assert: (encoder decodeBytes: bytes) equals: string.
	encoder beLittleEndian.
	self assert: encoder isLittleEndian.
	self assert: (encoder decodeBytes: bytes) equals: string.
	self assert: encoder isBigEndian.
	1 to: bytes size by: 2 do: [ :each | bytes swap: each with: each + 1 ].
	self assert: (encoder decodeBytes: bytes) equals: string.
	self assert: encoder isLittleEndian
%

category: 'testing'
method: ZnCharacterEncoderTests
testUTF16EncoderLittleEndian
	| string bytes encoder |
	string := 'élève en Français'.
	bytes := ByteArray readHexFrom:
		'E9006C00E80076006500200065006E0020004600720061006E00E700610069007300'.
	encoder := ZnUTF16Encoder new.
	encoder beLittleEndian.
	self assert: encoder isLittleEndian.
	self assert: (encoder encodeString: string) equals: bytes.
	self assert: (encoder decodeBytes: bytes) equals: string
%

category: 'testing'
method: ZnCharacterEncoderTests
testUTF16EncoderWide1
	| string bytes encoder |
	string := (Character codePoint: 16r1d11e) asString. "MUSICAL SYMBOL G CLEF"
	bytes := ByteArray readHexFrom: 'D834DD1E'.
	encoder := ZnUTF16Encoder new.
	self assert: encoder isBigEndian.
	self assert: (encoder encodeString: string) equals: bytes.
	self assert: (encoder decodeBytes: bytes) equals: string
%

category: 'testing'
method: ZnCharacterEncoderTests
testUTF32EncoderExampleFromD100
	| string bytes encoder |
	string := #(16r0000004D 16r00000430 16r00004E8C 16r00010302) collect: [:ea | ea asCharacter] as: String.
	bytes := ByteArray readHexFrom: '4D000000300400008C4E000002030100'.
	encoder := #utf32 asZnCharacterEncoder.
	encoder beLittleEndian.
	self assert: encoder isLittleEndian.
	self assert: (encoder encodeString: string) equals: bytes.
	self assert: (encoder decodeBytes: bytes) equals: string
%

category: 'testing'
method: ZnCharacterEncoderTests
testUTF32EncoderExampleFromD101
	| string encoder bytesBigEndianWithBom bytesLittleEndianWithBom |
	string := #(16r0000004D 16r00000430 16r00004E8C 16r00010302) collect: [:ea | ea asCharacter] as: String.
	encoder := #utf32 asZnCharacterEncoder.
	encoder beLittleEndian.
	"start with the wrong endianness (LE)"
	bytesBigEndianWithBom := ByteArray readHexFrom: '0000FEFF0000004D0000043000004E8C00010302'.
	"the correct endianness (BE) should be detected"
	self assert: (encoder decodeBytes: bytesBigEndianWithBom) equals: string.
	self assert: encoder isBigEndian.
	"start with the wrong endianness (BE)"
	bytesLittleEndianWithBom := ByteArray readHexFrom: 'FFFE00004D000000300400008C4E000002030100'.
	"the correct endianness (LE) should be detected"
	self assert: (encoder decodeBytes: bytesLittleEndianWithBom) equals: string.
	self assert: encoder isLittleEndian.
%

category: 'testing'
method: ZnCharacterEncoderTests
testUTF32EncoderExampleFromD99
	| string bytes encoder |
	string := #(16r0000004D 16r00000430 16r00004E8C 16r00010302) collect: [:ea | ea asCharacter] as: String.
	bytes := ByteArray readHexFrom: '0000004D0000043000004E8C00010302'.
	encoder := #utf32 asZnCharacterEncoder.
	encoder beBigEndian.
	self assert: encoder isBigEndian.
	self assert: (encoder encodeString: string) equals: bytes.
	self assert: (encoder decodeBytes: bytes) equals: string
%

category: 'testing'
method: ZnCharacterEncoderTests
testUTF32EncoderSimple
	| string bytes encoder |
	string := 'élève en Français'.
	bytes := ByteArray readHexFrom: '000000e90000006c000000e8000000760000006500000020000000650000006e000000200000004600000072000000610000006e000000e7000000610000006900000073'.
	encoder := #utf32 asZnCharacterEncoder.
	self assert: encoder isBigEndian.
	self assert: (encoder encodedByteCountForString: string) equals: 17 * 4.
	self assert: (encoder encodeString: string) equals: bytes.
	self assert: (encoder decodeBytes: bytes) equals: string
%

category: 'testing'
method: ZnCharacterEncoderTests
testUTF32EncoderWide
	| encoder |
	encoder := ZnUTF32Encoder new.
	{ 
		'abc'. 
		'élève en Français'. 
		'Pra-ská' copy at: 4 put: (Character value: 382); yourself. 
		(Character codePoint: 16r1d11e) asString. "MUSICAL SYMBOL G CLEF"
		'' } do: [ :each | 
			| bytes |
			bytes := self encodeString: each with: encoder. 
			self assertString: (encoder decodeBytesIntoWideString: bytes) equalsString: each ]
%

category: 'testing'
method: ZnCharacterEncoderTests
testUTF8Back
	| encoder stream |
	encoder := ZnUTF8Encoder new.
	stream := (encoder encodeString: 'Les élèves Françaises') readStream.
	self should: [ encoder backOnStream: stream ] raise: Error.
	4 timesRepeat: [ encoder nextFromStream: stream ].
	self assert: (encoder nextFromStream: stream) equals: $é.
	encoder backOnStream: stream.
	self assert: (encoder nextFromStream: stream) equals: $é.
	10 timesRepeat: [ encoder nextFromStream: stream ].
	13 timesRepeat: [ encoder backOnStream: stream ].
	self assert: (encoder nextFromStream: stream) equals: $s
%

category: 'testing'
method: ZnCharacterEncoderTests
testUTF8Boundaries
	"Test encoding and decoding of the characters at the boundaries between 1, 2, 3, and 4 multi-byte sequences.
	Values taken from http://en.wikipedia.org/wiki/Utf8#Description with the new RFC 3629 limit"
	
	| utf8Encoder string encoded |
	utf8Encoder := ZnUTF8Encoder new.
	#( (0 16r7f) 
		(16r80 16r07FF) 
		(16r0800 16rFFFF) 
		(16r10000 16r10FFFF) 
	) doWithIndex: [ :boundaries :byteCount |
		boundaries do: [ :each |
			string := String with: each asCharacter.
			encoded := utf8Encoder encodeString: string. 
			self assertCharacterCollection: (utf8Encoder decodeBytes: encoded) equals: string.
			self assert: encoded size equals: byteCount ] ]
%

category: 'testing'
method: ZnCharacterEncoderTests
testUTF8ByteOrderMark
	"The Unicode Byte Order Mark (BOM) should be skipped."
	
	| bom string bytes encoder decodedString |
	encoder := ZnUTF8Encoder new.
	string := 'élève en Français'.
	bytes := encoder encodeStringWithByteOrderMark: string.
	self assertString: (encoder decodeBytes: bytes) equalsString: string.
	
	string := 'Foo'.
	bytes := encoder encodeStringWithByteOrderMark: string.
	decodedString := String new: string size.
	ZnUTF8Encoder new 
		readInto: decodedString startingAt: 1 count: string size fromStream: bytes readStream. 
	self assertString: decodedString equalsString: string.
	
	bom := encoder encodeStringWithByteOrderMark: ''.
	self should: [ encoder decodeBytes: bom ] raise: Error
%

category: 'testing'
method: ZnCharacterEncoderTests
testUTF8Encoder
	"The examples are taken from http://en.wikipedia.org/wiki/UTF-8#Description"
	
	| encoder inputBytes outputBytes inputString outputString |
	encoder := ZnUTF8Encoder new.
	inputString := String with: $$ with: (16r00A2 asCharacter) with: (16r20AC asCharacter) with: (16r024B62 asCharacter).
	inputBytes := #(16r24 16rC2 16rA2 16rE2 16r82 16rAC 16rF0 16rA4 16rAD 16rA2) asByteArray.
	outputBytes := self encodeString: inputString with: encoder.
	self assert: outputBytes = inputBytes.
	outputString := self decodeBytes: inputBytes with: encoder.
	self assertCharacterCollection: outputString equals: inputString
%

category: 'testing'
method: ZnCharacterEncoderTests
testUTF8EncoderAuto
	| encoder inputString bytes outputString |
	encoder := ZnUTF8Encoder new.
	inputString := String withAll: ((1 to: 3072) collect: [ :each | Character codePoint: each ]).
	bytes := self encodeString: inputString with: encoder. 
	outputString := self decodeBytes: bytes with: encoder.
	self assertCharacterCollection: inputString equals: outputString
%

category: 'testing'
method: ZnCharacterEncoderTests
testUTF8EncoderByteCount	
	| encoder |
	encoder := ZnUTF8Encoder new.
	self assert: (encoder encodedByteCountFor: $$) = 1.
	self assert: (encoder encodedByteCountFor: (16r00A2 asCharacter)) = 2.
	self assert: (encoder encodedByteCountFor: (16r20AC asCharacter)) = 3.
	self assert: (encoder encodedByteCountFor: (16r024B62 asCharacter)) = 4
%

category: 'testing'
method: ZnCharacterEncoderTests
testUTF8EncoderIncomplete
	"The examples are taken from http://en.wikipedia.org/wiki/UTF-8#Description"
	
	| encoder |
	encoder := ZnUTF8Encoder new.
	#( (16rC2 16rA2) (16rE2 16r82 16rAC) (16rF0 16rA4 16rAD 16rA2) ) do: [ :each |
		2 to: each size do: [ :count |
			self 
				should: [ encoder decodeBytes: (each allButLast: count - 1) ] 
				raise: ZnCharacterEncodingError ] ]
%

category: 'testing'
method: ZnCharacterEncoderTests
testUTF8EncoderRandom
	#(unicodeCharacterSource latin1CharacterSource asciiCharacterSource) do: [ :source |
		| string bytes result |
		string := self class stringOfSize: 256 fromSource: source.
		bytes := string utf8Encoded.
		result := bytes utf8Decoded.
		self assertString: result equalsString: string ]
%

category: 'testing'
method: ZnCharacterEncoderTests
testUTF8EncoderWide
	| encoder |
	encoder := ZnUTF8Encoder new.
	{ 'abc'. 'élève en Français'. 'Pra-ská' copy at: 4 put: (Character codePoint: 382); yourself. '' }
		do: [ :each | | bytes |
			bytes := self encodeString: each with: encoder. 
"			self assertCharacterCollection: (encoder decodeBytes: bytes) equals: each."
			self assertCharacterCollection: (encoder decodeBytesIntoWideString: bytes) equals: each ]
%

category: 'testing'
method: ZnCharacterEncoderTests
testUTF8Overlong
	"Overlong encoding, aka Non shortest form characters should be rejected.
	See http://en.wikipedia.org/wiki/UTF-8#Overlong_encodings"
	
	self 
		should: [ ZnUTF8Encoder new decodeBytes: #[ 16rF0 16r82 16r82 16rAC ] ] 
		raise: ZnCharacterEncodingError.
	self 
		should: [ ZnUTF8Encoder new decodeBytes: #(193 129 193 130 193 131 ) ] 
		raise: ZnCharacterEncodingError.
%

category: 'testing'
method: ZnCharacterEncoderTests
testUTF8ReadFaultyInput
	"Although not recommended, it is possible to read faulty UTF-8 encoded input by resuming ZnInvalidUTF8"
	
	"illegal leading byte"
	self 
		should: [ #[102 111 111 160 102 111 111] utf8Decoded ] 
		raise: ZnInvalidUTF8.
	self 
		assert: ([ #[102 111 111 160 102 111 111] utf8Decoded ] on: ZnInvalidUTF8 do: [ :exception | exception resume ]) 
		equals: 'foo?foo'.
	self 
		assert: ([ #[102 111 111 160 102 111 111] utf8Decoded ] on: ZnInvalidUTF8 do: [ :exception | exception resume: $_ codePoint ]) 
		equals: 'foo_foo'.
	
	"illegal continuation byte"
	self 
		should: [ #[102 111 111 195 0 102 111 111] utf8Decoded ] 
		raise: ZnInvalidUTF8.
	self 
		assert: ([ #[102 111 111 195 0 102 111 111] utf8Decoded ] on: ZnInvalidUTF8 do: [ :exception | exception resume ]) 
		equals: 'foo?foo'.

	"incomplete input"
	self 
		should: [ #[102 111 111 195 ] utf8Decoded ] 
		raise: ZnIncomplete.
	self 
		assert: ([ #[102 111 111 195 ] utf8Decoded ] on: ZnInvalidUTF8 , ZnIncomplete do: [ :exception | exception resume ]) 
		equals: 'foo?'
%

category: 'testing'
method: ZnCharacterEncoderTests
testUTF8SurrogateCodePointsShouldFail
	| encoder surrogateCodePoint |
	encoder := #utf8 asZnCharacterEncoder.
	surrogateCodePoint := 16rD801.
	self assert: (encoder isSurrogateCodePoint: surrogateCodePoint).
	self 
		should: [ encoder encodeString: surrogateCodePoint asCharacter asString ]
		raise: ZnInvalidUTF8.
	self 
		should: [ encoder decodeBytes: #[237 160 129] ]
		raise: ZnCharacterEncodingError
%

! Class implementation for 'ZnCharacterStreamTests'

!		Instance methods for 'ZnCharacterStreamTests'

category: 'testing'
method: ZnCharacterStreamTests
assertUpToAll: array
	| utf8Stream |
	utf8Stream := self utf8ReadStreamOn: array first.
	self assert: (array first readStream upToAll: array second) equals: array third.
	self assert: (utf8Stream upToAll: array second) equals: array third
%

category: 'private'
method: ZnCharacterStreamTests
decodeBytes: bytes with: encoder
	| input |
	input := bytes readStream.
	^ String streamContents: [ :stream |
		[ input atEnd ] whileFalse: [ 
			stream nextPut: (encoder nextFromStream: input) ] ]
%

category: 'private'
method: ZnCharacterStreamTests
encodeString: string with: encoder
	^ ByteArray streamContents: [ :stream |
		string do: [ :each |
			encoder nextPut: each toStream: stream ] ]
%

category: 'testing'
method: ZnCharacterStreamTests
testNextLine
	| stream |
	stream := ZnCharacterReadStream on: 'abc' asByteArray readStream.
	self assert: stream nextLine equals: 'abc'.
	self assert: stream nextLine  equals: nil.
	stream := ZnCharacterReadStream on: '' asByteArray readStream.
	self assert: stream nextLine equals: nil.
	stream := ZnCharacterReadStream on: ({ 
		$a. Character cr. 
		$b. Character lf. 
		$c } collect: [:ea | ea charCode]) readStream.
	self assert: stream nextLine equals: 'a'.
	self assert: stream nextLine equals: 'b'.
	self assert: stream nextLine equals: 'c'.
	self assert: stream nextLine equals: nil.
	stream := ZnCharacterReadStream on: ( { 
		$a. Character cr. Character lf.  
		$b. Character cr. Character lf. 
		$c. Character cr. Character lf }  collect: [:ea | ea charCode]) readStream.
	self assert: stream nextLine equals: 'a'.
	self assert: stream nextLine equals: 'b'.
	self assert: stream nextLine equals: 'c'.
	self assert: stream nextLine equals: nil.
	stream := ZnCharacterReadStream on: ({ 
		$a. Character cr. Character lf.  
		Character cr. Character lf. 
		$c. Character cr. Character lf }  collect: [:ea | ea charCode]) readStream.
	self assert: stream nextLine equals: 'a'.
	self assert: stream nextLine equals: ''.
	self assert: stream nextLine equals: 'c'.
	self assert: stream nextLine equals: nil.
%

category: 'testing'
method: ZnCharacterStreamTests
testPeek
	| string bytes readStream |
	string := 'élève en Français'.
	bytes := ZnUTF8Encoder new encodeString: string.
	readStream := ZnBufferedReadStream on: (ZnCharacterReadStream on: bytes readStreamPortable).
	self assert: readStream peek equals: 'é' first.
	self assert: readStream peek equals: 'é' first.
	self assert: readStream next equals: 'é' first.
	readStream skip: 'lève ' size.
	self assert: readStream peek equals: $e.
	self assert: (readStream next: 'en Français' size) equals: 'en Français'
%

category: 'testing'
method: ZnCharacterStreamTests
testReadStream
	| stream |
	stream := ZnBufferedReadStream on: (ZnCharacterReadStream on: 'ABC' asByteArray readStreamPortable).
	stream sizeBuffer: 3.
	self deny: stream atEnd.
	self deny: stream isBinary.
	self assert: stream next = $A.
	self deny: stream atEnd.
	self assert: stream peek = $B.
	self deny: stream atEnd.
	self assert: stream peek = $B.
	self deny: stream atEnd.
	self assert: stream next = $B.
	self deny: stream atEnd.
	self assert: stream next = $C.
	self assert: stream atEnd.
	self assert: stream next isNil.
	self assert: stream peek isNil
%

category: 'testing'
method: ZnCharacterStreamTests
testSimpleUTF8ReadStream
	| string bytes stream |
	string := 'élève en Français'.
	bytes := ZnUTF8Encoder new encodeString: string.
	stream := ZnBufferedReadStream on: (ZnCharacterReadStream on: bytes readStreamPortable).
	stream sizeBuffer: string size.
	self 
		assert:stream upToEnd
		equals: string
%

category: 'testing'
method: ZnCharacterStreamTests
testSimpleUTF8WriteStream
	| string bytes stream |
	string := 'élève en Français'.
	bytes := ZnUTF8Encoder new encodeString: string.
	stream := (ZnCharacterWriteStream on: ByteArray new writeStreamPortable).
	stream nextPutAll: string.
	self 
		assert: stream wrappedStream contents
		equals: bytes asByteArray
%

category: 'testing'
method: ZnCharacterStreamTests
testUpToAll
	true ifTrue: [ "upToAll: not supported for buffered streams ... skip test for now" ^ self ].
	#(
		('' '' '')
		('' 'ß' '')
		('' 'ße' '')
		('ß' '' '')
		('ße' '' '')
		('ß' 'ß' '')
		('ße' 'ß' '')
		('ß' 'ße' 'ß')
		('ß' 'e' 'ß')
		('ße' 'e' 'ß')
		('ßen' 'e' 'ß')
		('ßen' 'en' 'ß')
		('ßend' 'en' 'ß')
		('iße' 'e' 'iß')
		('ißen' 'e' 'iß')
		('ißen' 'en' 'iß')
		('ißend' 'en' 'iß')
		('iß' 'ß' 'i')
		('iße' 'ß' 'i')
		('eißen' 'ßend' 'eißen')
		('abcdefgh' 'cd' 'ab')
		('a' '' '')
		('a' 'a' '')
		('a' 'b' 'a')
		('ab' '' '')
		('ab' 'a' '')
		('ab' 'b' 'a')
		('ab' 'c' 'ab')
		('ab' 'ab' '')
		('abc' '' '')
		('abc' 'a' '')
		('abc' 'b' 'a')
		('abc' 'c' 'ab')
		('abc' 'd' 'abc')
		('abc' 'ab' '')
		('abc' 'bc' 'a')
		('abc' 'cd' 'abc')
	) do: [ :array | self assertUpToAll: array ]
%

category: 'testing'
method: ZnCharacterStreamTests
testUpToAllTwice
	| utf8Stream stream |
	true ifTrue: [ "upToAll: not supported for buffered streams ... skip test for now" ^ self ].
	utf8Stream := self utf8ReadStreamOn: 'eißendeße'.
	self assert: (utf8Stream upToAll: 'ße') equals: 'ei'.
	self assert: (utf8Stream upToAll: 'ße') equals: 'nde'.

	stream := 'eißendeße' readStreamPortable.
	self assert: (stream upToAll: 'ße') equals: 'ei'.
	self assert: (stream upToAll: 'ße') equals: 'nde'
%

category: 'testing'
method: ZnCharacterStreamTests
testUTF8ReadStreamReadInto
	| string bytes stream buffer |
	string := 'élève en Français'.
	bytes := ZnUTF8Encoder new encodeString: string.
	stream := ZnCharacterReadStream on: bytes readStream.
	buffer := String new: string size.
	stream next: string size into: buffer. 
	self assert: buffer equals: string.
	self assert: stream atEnd.
	string := 'Czech in Czech is {1}e{2}tina.' format: { 269 asCharacter. 353 asCharacter }.
	bytes := ZnUTF8Encoder new encodeString: string.
	stream := ZnCharacterReadStream on: bytes readStream.
	buffer := String new: string size.
	stream next: string size into: buffer. 
	self assert: buffer equals: string.
	self assert: stream atEnd
%

category: 'private'
method: ZnCharacterStreamTests
utf8ReadStreamOn: string
	| bytes stream |
	bytes := ZnUTF8Encoder new encodeString: string.
	stream := ZnBufferedReadStream on: (ZnCharacterReadStream
		on: bytes readStreamPortable
		encoding: #utf8).
	stream sizeBuffer: string size.
	^stream
%

! Class implementation for 'ZnCrPortableWriteStreamTests'

!		Instance methods for 'ZnCrPortableWriteStreamTests'

category: 'tests'
method: ZnCrPortableWriteStreamTests
testNextPut
	"Ensure that the line ends are written correctly"

	| expectedString stream crStream |
	expectedString := 'a', System lineEnding  asString, 'b'.
	{ String cr.
		String lf.
		String crlf. } do: [ :lineEnd |
			stream := String new writeStream.
			crStream := ZnNewLineWriterStream on: stream.
			crStream
				<< 'a';
				<< lineEnd;
				<< 'b'.
			self assert: stream contents equals: expectedString ]
%

! Class implementation for 'ZnFastLineReaderTests'

!		Instance methods for 'ZnFastLineReaderTests'

category: 'tests'
method: ZnFastLineReaderTests
testLinesDo
	| lines reader |
	lines := #( 'foo' 'bar' 'last').
	reader := ZnFastLineReader on: (Character lf join: lines) readStream.
	self 
		assert: (Array streamContents: [ :out | 
					reader linesDo: [ :line | out nextPut: line ] ])
		equals: lines
%

category: 'tests'
method: ZnFastLineReaderTests
testNextLine
	| reader |
	reader := ZnFastLineReader on: 'abc' readStream.
	self assert: reader nextLine equals: 'abc'.
	self assert: reader atEnd.
	self assert: reader nextLine  equals: nil.
	reader := ZnFastLineReader on: '' readStream.
	self assert: reader nextLine equals: nil.
	self assert: reader atEnd.
	reader := ZnFastLineReader on: (String withAll: { 
		$a. Character cr. 
		$b. Character lf. 
		$c }) readStream.
	self assert: reader nextLine equals: 'a'.
	self assert: reader nextLine equals: 'b'.
	self assert: reader nextLine equals: 'c'.
	self assert: reader nextLine equals: nil.
	reader := ZnFastLineReader on: (String withAll: { 
		$a. Character cr. Character lf.  
		$b. Character cr. Character lf. 
		$c. Character cr. Character lf }) readStream.
	self assert: reader nextLine equals: 'a'.
	self assert: reader nextLine equals: 'b'.
	self assert: reader nextLine equals: 'c'.
	self assert: reader nextLine equals: nil.
	reader := ZnFastLineReader on: (String withAll: { 
		$a. Character cr. Character lf.  
		Character cr. Character lf. 
		$c. Character cr. Character lf }) readStream.
	self assert: reader nextLine equals: 'a'.
	self assert: reader nextLine equals: ''.
	self assert: reader nextLine equals: 'c'.
	self assert: reader atEnd
%

! Class implementation for 'ZnNewLineWriterStreamTests'

!		Instance methods for 'ZnNewLineWriterStreamTests'

category: 'tests'
method: ZnNewLineWriterStreamTests
testClose 

	| string fs fileReference znstream |

	fs := FileSystem memory.
	fileReference := fs / 'test.txt'.
	string := String streamContents: [ :stream |
		stream 
			<< 'abccr';
			cr ].
	znstream := ZnNewLineWriterStream on: fileReference writeStream.
	znstream forLf.
	[ znstream nextPutAll: string ]
		ensure: [ znstream close ].
	string at: string size put: Character lf.
	self assert: fileReference contents equals: string.
%

category: 'tests'
method: ZnNewLineWriterStreamTests
testNextPut
	"Ensure that the line ends are written correctly"

	| expectedString stream crStream |
	expectedString := 'a', System lineEnding , 'b'.
	{ String cr.
		String lf.
		String crlf. } do: [ :lineEnd |
			stream := String new writeStream.
			crStream := ZnNewLineWriterStream on: stream.
			crStream
				<< 'a';
				<< lineEnd;
				<< 'b'.
			self assert: stream contents equals: expectedString ]
%

! Class implementation for 'ZnPercentEncoderTests'

!		Instance methods for 'ZnPercentEncoderTests'

category: 'tests'
method: ZnPercentEncoderTests
testDecodePlusAsSpace
	| encoder |
	encoder := ZnPercentEncoder new.
	self assertString: (encoder decode: '+') equalsString: ' '.
	self assert: encoder decodePlusAsSpace.
	encoder decodePlusAsSpace: false.
	self assertString: (encoder decode: '+') equalsString: '+'.
	self deny: encoder decodePlusAsSpace
%

category: 'tests'
method: ZnPercentEncoderTests
testDecodingErrors
	| encoder |
	encoder := ZnPercentEncoder new.
	self should: [ encoder decode: 'foo%%bar' ] raise: ZnCharacterEncodingError.
	self should: [ encoder decode: 'fooçbar' ] raise: ZnCharacterEncodingError.
	self should: [ encoder decode: 'foo%' ] raise: ZnCharacterEncodingError.
	self should: [ encoder decode: '%XX' ] raise: ZnCharacterEncodingError.
	self should: [ encoder decode: 'foo%F' ] raise: ZnCharacterEncodingError
%

category: 'tests'
method: ZnPercentEncoderTests
testLeadingZero
	| encoder |
	encoder := ZnPercentEncoder new.
	self assertString: (encoder encode: 'foo', Character tab asString, 'bar') equalsString: 'foo%09bar'.
	self assertString: (encoder decode: 'foo%09bar') equalsString: 'foo', Character tab asString, 'bar'.
	self assertString: (encoder encode: 'foo', Character lf asString, 'bar') equalsString: 'foo%0Abar'.
	self assertString: (encoder decode: 'foo%0Abar') equalsString: 'foo', Character lf asString, 'bar'
%

category: 'tests'
method: ZnPercentEncoderTests
testNonAscii
	| encoder |
	encoder := ZnPercentEncoder new.
	self 
		assert: encoder characterEncoder 
		equals: (ZnCharacterEncoder newForEncoding: 'utf-8').
	self 
		assertString: (encoder encode: 'élève en Français') 
		equalsString: '%C3%A9l%C3%A8ve%20en%20Fran%C3%A7ais'.
	self 
		assertString: (encoder decode: '%C3%A9l%C3%A8ve%20en%20Fran%C3%A7ais') 
		equalsString: 'élève en Français'
%

category: 'tests'
method: ZnPercentEncoderTests
testSimple
	| encoder |
	encoder := ZnPercentEncoder new.
	self assertString: (encoder encode: 'foo bar') equalsString: 'foo%20bar'.
	self assertString: (encoder decode: 'foo%20bar') equalsString: 'foo bar'.
	self assertString: (encoder encode: '') equalsString: ''.
	self assertString: (encoder decode: '') equalsString: ''.
	self assertString: (encoder decode: 'foo%25bar') equalsString: 'foo%bar'.
	self assertString: (encoder decode: 'foo+bar') equalsString: 'foo bar'
%

category: 'tests'
method: ZnPercentEncoderTests
testStringUrlDecoded
	self assertString: ('foo%20bar' urlDecoded) equalsString: 'foo bar'
%

category: 'tests'
method: ZnPercentEncoderTests
testStringUrlEncoded
	self assert: ('foo bar' urlEncoded) equals: 'foo%20bar'
%

! Class implementation for 'ZnPositionableReadStreamTests'

!		Instance methods for 'ZnPositionableReadStreamTests'

category: 'tests'
method: ZnPositionableReadStreamTests
buildData
	^ByteArray new: 2000 streamContents: [ :out | 
		2000 timesRepeat: [ out nextPut: 256 atRandom - 1 ] ]
%

category: 'tests'
method: ZnPositionableReadStreamTests
testBulkReading
	| stream buffer |
	stream := ZnPositionableReadStream on: #(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15) readStream.
	buffer := Array new: 6.
	self assert: (stream readInto: buffer startingAt: 1 count: 6) equals: 6.
	self assert: buffer equals: #(0 1 2 3 4 5).
	self assert: (stream readInto: buffer startingAt: 4 count: 3) equals: 3.
	self assert: (stream readInto: buffer startingAt: 1 count: 3) equals: 3.
	self assert: buffer equals: #(9 10 11 6 7 8).
	buffer atAllPut: 0.
	self assert: (stream readInto: buffer startingAt: 1 count: 6) equals: 4.
	self assert: buffer equals: #(12 13 14 15 0 0)
%

category: 'tests'
method: ZnPositionableReadStreamTests
testEmpty
	| stream |
	stream := ZnPositionableReadStream on: '' readStream.
	self assert: stream atEnd.
	self assert: stream peek isNil.
	self assert: stream next isNil.
	self assert: stream position = 0
%

category: 'tests'
method: ZnPositionableReadStreamTests
testNestedExcursion
	| stream |
	stream := ZnPositionableReadStream on: 'abcdef---XYZ!1--------' readStream.
	self assert: (stream next: 3) equals: 'abc'.
	stream savingPositionDo: [ 
		self assert: (stream next: 3) equals: 'def'.
		stream savingPositionDo: [ 
			stream upTo: $!.
			self assert: (stream peekFor: $1) ].
		stream skip: 3.
		self assert: (stream next: 3) equals: 'XYZ' ].
	self assert: (stream next: 3) equals: 'def'
%

category: 'tests'
method: ZnPositionableReadStreamTests
testNew
	| stream |
	stream := ZnPositionableReadStream on: 'abc' readStream.
	self assert: stream position = 0.
	self assert: stream bufferSize equals: stream defaultBufferSize.
	self deny: stream atEnd
%

category: 'tests'
method: ZnPositionableReadStreamTests
testPlainExcursion
	| stream |
	stream := ZnPositionableReadStream on: 'abcdef-----------' readStream.
	self assert: (stream next: 3) equals: 'abc'.
	self assert: (stream savingPositionDo: [ stream next: 3 ]) equals: 'def'.
	self assert: (stream next: 3) equals: 'def'
%

category: 'tests'
method: ZnPositionableReadStreamTests
testPlainNext
	| stream |
	stream := ZnPositionableReadStream on: 'abc' readStream.
	self assert: stream next equals: $a.
	self deny: stream atEnd.
	self assert: stream next equals: $b.
	self assert: stream position equals: 2.
	self assert: stream next equals: $c.
	self assert: stream atEnd
%

category: 'tests'
method: ZnPositionableReadStreamTests
testPlainPeek
	| stream |
	stream := ZnPositionableReadStream on: 'abc' readStream.
	self assert: stream next equals: $a.
	self assert: stream peek equals: $b.
	self assert: stream next equals: $b.
	self assert: stream position equals: 2.
	self assert: stream peek equals: $c.
	self assert: stream next equals: $c.
	self assert: stream atEnd
%

category: 'tests'
method: ZnPositionableReadStreamTests
testPositionErrors
	| data stream |
	data := ByteArray new: 1000 streamContents: [ :out | 
		100 timesRepeat: [ out nextPutAll: #[ 0 1 2 3 4 5 6 7 8 9 ] ] ].
	stream := ZnPositionableReadStream on: data readStream.
	self should: [ stream position: 1 ] raise: SubscriptOutOfBounds.
	stream next: 100.
	self should: [ stream position: -1 ] raise: SubscriptOutOfBounds.
	self should: [ stream position: 101 ] raise: SubscriptOutOfBounds.
	stream next: 500.
	self should: [ stream position: 100 ] raise: SubscriptOutOfBounds.
	self should: [ stream position: 600 - stream bufferSize - 1 ] raise: SubscriptOutOfBounds.
	stream position: 400.
	stream next: 599.
	self assert: stream next equals: 9.
	self assert: stream atEnd
%

category: 'tests'
method: ZnPositionableReadStreamTests
testReadAll
	| data stream |
	data := String new: 200 streamContents: [ :out | 
		200 timesRepeat: [ out nextPut: 'abc' atRandom ] ].
	stream := ZnPositionableReadStream on: data readStream.
	self deny: stream atEnd.
	self assert: stream position equals: 0.
	stream savingPositionDo: [
		self assert: stream upToEnd equals: data.
		self assert: stream atEnd.
		self assert: stream position equals: 200 ].
	self deny: stream atEnd.
	self assert: stream position equals: 0.
	self assert: stream upToEnd equals: data.
	self assert: stream atEnd.
	self assert: stream position equals: 200
%

category: 'tests'
method: ZnPositionableReadStreamTests
testReadAllLargerBuffer
	| data stream |
	data := String new: 500 streamContents: [ :out | 
		500 timesRepeat: [ out nextPut: 'abc' atRandom ] ].
	stream := ZnPositionableReadStream on: data readStream.
	stream sizeBuffer: 500.
	self deny: stream atEnd.
	self assert: stream position equals: 0.
	stream savingPositionDo: [
		self assert: stream upToEnd equals: data.
		self assert: stream atEnd.
		self assert: stream position equals: 500 ].
	self deny: stream atEnd.
	self assert: stream position equals: 0.
	self assert: stream upToEnd equals: data.
	self assert: stream atEnd.
	self assert: stream position equals: 500
%

category: 'tests'
method: ZnPositionableReadStreamTests
testSearch
	| data stream found |
	data := String new: 2000 streamContents: [ :out | 
		2000 timesRepeat: [ out nextPut: 'abc' atRandom ] ].
	data replaceFrom: 1000 to: 1005 with: 'TARGET'.
	stream := ZnPositionableReadStream on: data readStream.
	found := false.
	[ stream atEnd ] whileFalse: [ 
		stream savingPositionDo: [ 
			(stream next: 6) = 'TARGET' 
				ifTrue: [ 
					found := true.
					self assert: stream position equals: 1005 ] ].
		stream next ].
	self assert: found
%

category: 'tests'
method: ZnPositionableReadStreamTests
testSearchBinary
	| data stream pattern found |
	data := self buildData.
	pattern := ByteArray readHexFrom: 'FF77ABAB'.
	data replaceFrom: 1000 to: 1000 + pattern size - 1 with: pattern.
	stream := ZnPositionableReadStream on: data readStream.
	found := false.
	[ stream atEnd ] whileFalse: [ 
		stream savingPositionDo: [ 
			(stream next: pattern size) = pattern 
				ifTrue: [ 
					found := true.
					self assert: stream position equals: 1000 + pattern size - 1 ] ].
		stream next ].
	self assert: found
%

category: 'tests'
method: ZnPositionableReadStreamTests
testSkipAndBack
	| stream |
	stream := ZnPositionableReadStream on: 'abcdef' readStream.
	stream skip: 2.
	self assert: stream next equals: $c.
	stream skip: 1.
	self assert: stream back equals: $d.
	self assert: stream back equals: $c.
	stream skip: -2.
	self assert: stream next equals: $a.
	stream back.
	self assert: stream upToEnd equals: 'abcdef'.
%

category: 'tests'
method: ZnPositionableReadStreamTests
testUTF8
	| data stream |
	data := 'Les élèves Françaises ont 100 '.
	stream := ZnPositionableReadStream on: (ZnCharacterReadStream on: data utf8Encoded readStream).
	self assert: (stream next: 3) equals: 'Les'.
	stream skip: 1.
	stream savingPositionDo: [ 
		self assert: (stream next: 6) equals: 'élèves'.
		self assert: stream next equals: Character space ].
	self assert: (stream next: 6) equals: 'élèves'.
	self assert: (stream peekFor: Character space).
	2 timesRepeat: [ stream upTo: Character space ].
	self assert: (stream upTo: $) trimBoth asNumber equals: 100.
	self assert: stream atEnd
%

! Class implementation for 'DiskFileAttributesTestsResources'

!		Instance methods for 'DiskFileAttributesTestsResources'

category: 'accessing'
method: DiskFileAttributesTestsResources
afterCreationTime
	^ afterCreationTime
%

category: 'accessing'
method: DiskFileAttributesTestsResources
beforeCreationTime
	^ beforeCreationTime
%

category: 'accessing'
method: DiskFileAttributesTestsResources
file
	^ file
%

category: 'running'
method: DiskFileAttributesTestsResources
setUp

	| ws bufferedStream days hours minutes seconds |

	file := FileLocator temp / ('FileAttributesTests-', FastUUIDGenerator next asString, '.txt').
	beforeCreationTime := DateAndTime secondsLocal: DateAndTime secondsSince2001 truncated offset: (Duration seconds: 0).
	beforeCreationTime offset: (Duration seconds: (beforeCreationTime currentTimeZone transitionAtUTC: beforeCreationTime) offsetFromUTC).
	ws := file writeStream.
	bufferedStream := ZnBufferedWriteStream on: ws.
	[
		bufferedStream nextPutAll: 'Created by FileAttributesTestsResources>>setUp '.
		beforeCreationTime printOn: bufferedStream.
	] ensure: [ bufferedStream close ].
	afterCreationTime := DateAndTime now beRounded.
	"Round up to the next second"
	days := afterCreationTime dayOfYear.
	hours := afterCreationTime hour.
	minutes := afterCreationTime minute.
	seconds := afterCreationTime second + 1.
	seconds >= 60
		ifTrue: [ 
			minutes := minutes + 1.
			seconds := seconds - 60.
			minutes >= 60
				ifTrue: [
					hours := hours + 1.
					minutes := minutes - 60.
					hours >=  24
						ifTrue: [ 
							days := days + 1.
							hours := hours - 24 ] ].].
	afterCreationTime := DateAndTime
		year: afterCreationTime year day: days hour:  hours minute: minutes second: seconds
%

category: 'running'
method: DiskFileAttributesTestsResources
tearDown

	file delete.
%

! Class implementation for 'StdioStreamTestResource'

!		Instance methods for 'StdioStreamTestResource'

category: 'accessing'
method: StdioStreamTestResource
contents
	^ contents
%

category: 'accessing'
method: StdioStreamTestResource
fileReference
	^ fileReference
%

category: 'accessing'
method: StdioStreamTestResource
fileStream
	^ fileStream
%

category: 'running'
method: StdioStreamTestResource
setUp
	"Create the temporary file and StdioStream"

	| handle |

	fileReference := FileReference newTempFilePrefix: 'StdioStreamTest.' suffix: '.bin'.
	contents := '01234567890123456789'.
	
	fileStream := fileReference writeRawStream.
	fileStream
		nextPutAll: contents;
		position: 0.
	"NOTE: This makes assumptions about the internal structure of BinaryFileStream.
	This shouldn't be done in general."
	stdioStream := StdioStream on: (FileSystem fileClass stdout).
%

category: 'accessing'
method: StdioStreamTestResource
stdioStream
	^ stdioStream
%

category: 'running'
method: StdioStreamTestResource
tearDown
	"Close the receiver's resources"
	
	stdioStream := nil.
	fileStream close.
	fileReference ensureDelete.
%

! Class implementation for 'MemoryWriteStream'

!		Instance methods for 'MemoryWriteStream'

category: 'testing'
method: MemoryWriteStream
isReadOnly
	
	^ false
%

category: 'accessing'
method: MemoryWriteStream
readOnlyCopy
	
	^ collection readStreamPortable
%

category: 'positioning'
method: MemoryWriteStream
truncate
	collection truncate
%

category: 'positioning'
method: MemoryWriteStream
truncate: anInteger 
	collection truncate: anInteger 
	
%

! Class implementation for 'ZnNewLineWriterStream'

!		Class methods for 'ZnNewLineWriterStream'

category: 'instance creation'
classmethod: ZnNewLineWriterStream
on: aStream

	^ self basicNew
		initialize;
		stream: aStream;
		yourself
%

!		Instance methods for 'ZnNewLineWriterStream'

category: 'open/close'
method: ZnNewLineWriterStream
close 

	^stream close
%

category: 'flushing'
method: ZnNewLineWriterStream
flush
	^ stream flush
%

category: 'accessing'
method: ZnNewLineWriterStream
forCr

	lineEnding := String cr
%

category: 'accessing'
method: ZnNewLineWriterStream
forCrLf

	lineEnding := String crlf
%

category: 'accessing'
method: ZnNewLineWriterStream
forLf

	lineEnding := String lf
%

category: 'accessing'
method: ZnNewLineWriterStream
forPlatformLineEnding
	lineEnding := System lineEnding
%

category: 'initialize'
method: ZnNewLineWriterStream
initialize

	"super initialize."
	cr := Character cr.
	lf := Character lf.
	self forPlatformLineEnding.
%

category: 'accessing'
method: ZnNewLineWriterStream
newLine
	previous := nil.
	stream nextPutAll: lineEnding
%

category: 'accessing'
method: ZnNewLineWriterStream
nextPut: aCharacter
	"Write aCharacter to the receivers stream.
	Convert all line end combinations, i.e cr, lf, crlf, to the platform convention"

	(previous == cr and: [ aCharacter == lf ]) ifFalse: [
		(aCharacter == cr or: [ aCharacter == lf ]) ifTrue: 
			[ self newLine ]
		ifFalse:
			[ stream nextPut: aCharacter ] ].
	previous := aCharacter.
%

category: 'accessing'
method: ZnNewLineWriterStream
stream: aWriteStream 
	stream := aWriteStream
%

! Class extensions for 'AbstractDictionary'

!		Class methods for 'AbstractDictionary'

category: '*filesystem-gemstone-kernel'
classmethod: AbstractDictionary
newFromPairs: anArray
	"Answer an instance of me associating (anArray at: i) to (anArray at: i+1)
	 for each odd i.  anArray must have an even number of entries."

	"Dictionary newFromPairs: {'Red' . Color red . 'Blue' . Color blue . 'Green' . Color green}."

	| newDictionary |
	newDictionary := self new: anArray size / 2.
	1 to: anArray size - 1 by: 2 do: [ :i | newDictionary at: (anArray at: i) put: (anArray at: i + 1) ].
	^ newDictionary
%

!		Instance methods for 'AbstractDictionary'

category: '*filesystem-gemstone-kernel'
method: AbstractDictionary
at: key ifPresent: aBlock
	"Lookup the given key in the receiver. If it is present, answer the
	value of evaluating the given block optionally with the value associated
	with the key.
	Otherwise, answer nil."

	^(self at: key ifAbsent: [])
		ifNotNil: [:aValue | aBlock cull: aValue]
%

category: '*filesystem-gemstone-kernel'
method: AbstractDictionary
fillFrom: aCollection with: aBlock
	"Evaluate aBlock with each of aCollections's elements as the argument.  
	Collect the resulting values into self. Answer self."

	aCollection keysAndValuesDo: [ :key :value |
		self at: key put: (aBlock value: value) ]
%

category: '*filesystem-gemstone-kernel'
method: AbstractDictionary
removeAll

	self removeAllKeys: self keys
%

! Class extensions for 'Array'

!		Instance methods for 'Array'

category: '*filesystem-gemstone-kernel'
method: Array
fillFrom: aCollection with: aBlock
	"Evaluate aBlock with each of aCollections's elements as the argument.  
	Collect the resulting values into self. Answer self."

	| index |
	index := 0.
	aCollection do: [ :each |
		self at: (index := index + 1) put: (aBlock value: each) ]
%

! Class extensions for 'BinaryFloat'

!		Class methods for 'BinaryFloat'

category: '*FileSystem-GemStone-Kernel'
classmethod: BinaryFloat
readFrom: aStream ifFail: aBlock
	"Answer a new Float as described on the stream, aStream."

	^(super readFrom: aStream ifFail: [^aBlock value]) asFloat
%

!		Instance methods for 'BinaryFloat'

category: '*FileSystem-GemStone-Kernel'
method: BinaryFloat
isPowerOfTwo
	"Return true if the receiver is an integral power of two.
	Floats never return true here."
	^false
%

! Class extensions for 'ByteArray'

!		Class methods for 'ByteArray'

category: '*filesystem-gemstone-kernel'
classmethod: ByteArray
readHexFrom: aString
  "Create a byte array from a hexadecimal representation"

  ^ (self new: aString size // 2) readHexFrom: aString readStream
%

!		Instance methods for 'ByteArray'

category: '*filesystem-gemstone-kernel'
method: ByteArray
asString
  "Convert to a String with Characters for each byte"

  ^ String withBytes: self
%

category: '*Zinc-Character-Encoding-GemStone'
method: ByteArray
decodeWith: encoding
	"Produce a String that decodes the receiver, using a specified encoding.
	Encoding is either a ZnCharacterEncoder instance or an identifier for one."
	
	"#[76 101 115 32 195 169 108 195 168 118 101 115 32 102 114 97 110 195 167 97 105 115] decodeWith: #utf8"
	
	^ encoding asZnCharacterEncoder decodeBytes: self
%

category: '*Zinc-Character-Encoding-GemStone'
method: ByteArray
utf8Decoded
	"Produce a String decoding the receiver using UTF-8,
	the recommended encoding for Strings, unless you know what you are doing."

	"#[76 101 115 32 195 169 108 195 168 118 101 115 32 102 114 97 110 195 167 97 105 115] utf8Decoded"
	
	self flag: 'GemStone/Pharo code switched for research'.

	self isGemStoneUtf8Encoding
		ifTrue: [
			^ [self decodeFromUTF8]
				on: ArgumentError
				do: [:s | ZnInvalidUTF8 signal: 'Illegal leading byte for utf-8 encoding'] ]
		ifFalse: [
			^ self decodeWith: ZnCharacterEncoder utf8]
%

! Class extensions for 'Character'

!		Class methods for 'Character'

category: '*filesystem-gemstone-kernel'
classmethod: Character
digitValue: x 
	"Answer the Character whose digit value is x. For example,
	 answer $9 for x=9, $0 for x=0, $A for x=10, $Z for x=35."

	| n |
	n := x asInteger.
	^self withValue: (n < 10 ifTrue: [n + 48] ifFalse: [n + 55])
%

category: '*filesystem-gemstone-kernel'
classmethod: Character
numberParserDigitalValues
	"This is to replicate the Pharo implimentation of Charactr class>>initializeDigitalValues"
		
	^{
			Character numberParserDigitalValuesBlock value.
		} first
%

category: '*filesystem-gemstone-kernel'
classmethod: Character
numberParserDigitalValuesBlock
	"This is to replicate the Pharo implimentation of Charactr class>>initializeDigitalValues"

	| aBlock|
	aBlock := [ | anArray | 
			"Initialize the well known digit value of ascii characters.
			Note that the DigitValues table is 1-based while ascii values are 0-based, thus the offset+1."
			anArray := Array new: 256 withAll: -1.
			"the digits"
			0 to: 9 do: [:i | anArray at: 48 + i + 1 put: i].
			"the uppercase letters"
			10 to: 35 do: [:i | anArray at: 55 + i + 1 put: i].
			"the lowercase letters"
			10 to: 35 do: [:i | anArray at: 87 + i + 1 put: i].
		anArray].
	^aBlock
%

category: '*filesystem-gemstone-kernel'
classmethod: Character
to: other
"Had problems deleting this method. It needs to be removed."
   ^self
%

category: '*filesystem-gemstone-kernel'
classmethod: Character
value: anInteger
	"Compatability with Pharo"
	^self withValue: anInteger
%

!		Instance methods for 'Character'

category: '*filesystem-gemstone-kernel'
method: Character
charCode
	^ self asInteger bitAnd: 4194303
%

category: '*filesystem-gemstone-kernel'
method: Character
isCharacter

	^ true
%

category: '*filesystem-gemstone-kernel'
method: Character
join: aSequenceableCollection
	"Append the elements of the argument, aSequenceableCollection, separating them by the receiver."
	"(Character space join: #('Pharo' 'is' 'cool')) >>> 'Pharo is cool'"
	^ self asString join: aSequenceableCollection
%

category: '*filesystem-gemstone-kernel'
method: Character
numberParserDigitalValue
	self asInteger > 255
		ifTrue: [
			self error: 'Characters > 255 are not yet supported - or something'
			"^ self characterSet digitValueOf: self "].
	^ self class numberParserDigitalValues at: 1 + self asInteger
%

category: '*filesystem-gemstone-kernel'
method: Character
to: other
	"Answer with a collection in ascii order -- $a to: $z"
	^ (self asciiValue to: other asciiValue) collect:
				[:ascii | Character value: ascii]
%

! Class extensions for 'CharacterCollection'

!		Class methods for 'CharacterCollection'

category: '*filesystem-gemstone-kernel'
classmethod: CharacterCollection
crlf
	"Answer a string containing a carriage return and a linefeed."

	^ self with: Character cr with: Character lf
%

!		Instance methods for 'CharacterCollection'

category: '*FileSystem-Client-Core'
method: CharacterCollection
asClientFileReference

	^ FileSystem clientDisk referenceTo: self
%

category: '*FileSystem-Client-Core'
method: CharacterCollection
asClientPath

	^ FileSystem clientDisk resolve: self
%

category: '*filesystem-core'
method: CharacterCollection
asFileReference

	^ FileSystem disk referenceTo: self
%

category: '*filesystem-core'
method: CharacterCollection
asPath
	"convert myself to a path"
	"Examples:
		'.' asPath
		'~/Desktop' asPath
		'/home/foo' asPath
		'../../foo.bar' asPath"
	^ FileSystem disk resolve: self
%

category: '*filesystem-core'
method: CharacterCollection
asPathWith: anObject 
	^ anObject pathFromString: self
%

category: '*filesystem-core'
method: CharacterCollection
asResolvedBy: aFileSystem
	^ aFileSystem resolveString: self
%

category: '*Zinc-Character-Encoding-GemStone'
method: CharacterCollection
asZnCharacterEncoder
	"Return a ZnCharacterEncoder instance using the receiver as identifier"

	self flag: 'GemStone/Pharo code switched for research'.

	self isGemStoneUtf8Encoding
		ifTrue: [
			^ ZnCharacterEncoder newForEncoding: self
			" 'UTF-8' asZnCharacterEncoder 
			((self select: [ :each | each isAlphaNumeric ]) asLowercase) = 'utf8' 
				ifFalse: [ self error: 'Only utf8 encoding supported'].
			^ ZnUTF8Encoder new"]
		ifFalse: [
			^ ZnCharacterEncoder newForEncoding: self]
%

category: '*Zinc-Character-Encoding-GemStone'
method: CharacterCollection
encodeWith: encoding
	"Produce a ByteArray that encodes the receiver, using a specified encoding.
	Encoding is either a ZnCharacterEncoder instance or an identifier for one."
	
	" 'Les élèves français' encodeWith: #utf8 "
	
	^ encoding asZnCharacterEncoder encodeString: self
%

category: '*filesystem-gemstone-kernel'
method: CharacterCollection
fillFrom: aCollection with: aBlock
	"Evaluate aBlock with each of aCollections's elements as the argument.  
	Collect the resulting values into self. Answer self."

	| index |
	index := 0.
	aCollection do: [ :each |
		self at: (index := index + 1) put: (aBlock value: each) ]
%

category: '*filesystem-gemstone-kernel'
method: CharacterCollection
fullName
	^self
%

category: '*Zinc-Character-Encoding-GemStone'
method: CharacterCollection
isByteString

	^false
%

category: '*Zinc-Character-Encoding-GemStone'
method: CharacterCollection
isWideString
	"Answer whether the receiver is a WideString"
	^false
%

category: '*Zinc-Character-Encoding-GemStone'
method: CharacterCollection
urlDecoded
	"URL Decode the receiver and return the resulting String.
	This is an encoding where characters that are illegal in a URL are escaped."

    ^ ZnPercentEncoder new decode: self
%

category: '*Zinc-Character-Encoding-GemStone'
method: CharacterCollection
urlEncoded
	"URL Encode the receiver and return the resulting String.
	This is an encoding where characters that are illegal in a URL are escaped."

    ^ ZnPercentEncoder new encode: self
%

category: '*Zinc-Character-Encoding-GemStone'
method: CharacterCollection
utf8Encoded
	"Produce a ByteArray encoding the receiver using UTF-8,
	the recommended encoding for Strings, unless you know what you are doing."
	
	" 'Les élèves français' utf8Encoded "
	
	^ self encodeWith: ZnCharacterEncoder utf8
%

! Class extensions for 'Collection'

!		Instance methods for 'Collection'

category: '*filesystem-gemstone-kernel'
method: Collection
atRandom
	"Answer a random element of the receiver.  Uses a shared random 
	number generator owned by class Collection.  If you use this a lot, 
	define your own instance of Random and use #atRandom:.  Causes 
	an error if self has no elements."

	^ self atRandom: Random new.

"Examples:
	#('one' 'or' 'the' 'other') atRandom
	(1 to: 10) atRandom
	'Just pick one of these letters at random' atRandom
	#(3 7 4 9 21) asSet atRandom		(just to show it also works for Sets)
"
%

category: '*filesystem-gemstone-kernel'
method: Collection
atRandom: aGenerator
	"Answer a random element of the receiver. Uses aGenerator which
    should be kept by the user in a variable and used every time. Use
    this instead of #atRandom for better uniformity of random numbers because 
	only you use the generator. Causes an error if self has no elements."
	| rand index |

	self emptyCheck.
	rand := aGenerator nextInt: self size.
	index := 1.
	self do: [:each |
		index = rand ifTrue: [^each].
		index := index + 1].
	^ self errorEmptyCollection
%

category: '*filesystem-gemstone-kernel'
method: Collection
collect: aBlock as: aClass
	"Evaluate aBlock with each of the receiver's elements as the argument.  
	Collect the resulting values into an instance of aClass. Answer the resulting collection."

	^(aClass new: self size) fillFrom: self with: aBlock
%

category: '*filesystem-gemstone-kernel'
method: Collection
difference: aCollection
  "Answer the set theoretic difference of two collections."

  ^ self reject: [ :each | aCollection includes: each ]
%

category: '*filesystem-gemstone-kernel'
method: Collection
fillFrom: aCollection with: aBlock
	"Evaluate aBlock with each of aCollections's elements as the argument.  
	Collect the resulting values into self. Answer self."

	aCollection do: [ :each |
		self add: (aBlock value: each) ]
%

category: '*filesystem-gemstone-kernel'
method: Collection
ifEmpty: aBlock
  self size == 0
    ifTrue: [ ^ aBlock value ]
%

category: '*filesystem-gemstone-kernel'
method: Collection
ifEmpty: emptyBlock ifNotEmpty: notEmptyBlock
	"Evaluate emptyBlock if I'm empty, notEmptyBlock otherwise"
	" If the notEmptyBlock has an argument, eval with the receiver as its argument"

	self isEmpty ifTrue: [ ^emptyBlock value ].
	^notEmptyBlock cull: self
%

category: '*filesystem-gemstone-kernel'
method: Collection
ifNotEmpty: notEmptyBlock ifEmpty: emptyBlock
	"Evaluate emptyBlock if I'm empty, notEmptyBlock otherwise
	 If the notEmptyBlock has an argument, eval with the receiver as its argument"

	self isEmpty ifFalse: [ ^notEmptyBlock cull: self ].
	^emptyBlock value
%

category: '*filesystem-gemstone-kernel'
method: Collection
isEmptyOrNil
  "Answer whether the receiver contains any elements, or is nil.  Useful in numerous situations where one wishes the same reaction to an empty collection or to nil"

  ^ self size == 0
%

category: '*filesystem-gemstone-kernel'
method: Collection
noneSatisfy: aBlock
	"Evaluate aBlock with the elements of the receiver. If aBlock returns false for all elements return true. Otherwise return false"

	self do: [:item | (aBlock value: item) ifTrue: [^ false]].
	^ true
%

category: '*filesystem-gemstone-kernel'
method: Collection
removeAll

	self removeAll: self
%

category: '*filesystem-gemstone-kernel'
method: Collection
sort

	"Sort this array into ascending order using the '<=' operator."

	^ self sort: [ :a :b | a <= b ]
%

! Class extensions for 'Dictionary'

!		Instance methods for 'Dictionary'

category: '*filesystem-gemstone-kernel'
method: Dictionary
associations
	"Answer a Collection containing the receiver's associations."

	| result |
	result := WriteStream on: (Array new: self size).
	self associationsDo: [ :assoc | result nextPut: assoc ].
	^ result contents
%

! Class extensions for 'DiskFileSystemTest'

!		Instance methods for 'DiskFileSystemTest'

category: '*filesystem-tests-gemstone'
method: DiskFileSystemTest
testGsDeleteDirectoryOnError

	| path |
	path := Path * 'plonk' / 'griffle'.
	self markForCleanup: path.
	self markForCleanup: path parent.
	filesystem ensureCreateDirectory: path.
	self assert: (filesystem isDirectory: Path * 'plonk').
	self assert: (filesystem isDirectory: path).

	self should: [ filesystem delete: path parent ] raise: Error.
%

! Class extensions for 'FileStream'

!		Class methods for 'FileStream'

category: '*FileSystem-Core'
classmethod: FileStream
onHandle: aFileSystemHandle
	^ self concreteStream new
		open: aFileSystemHandle fullName
		forWrite: aFileSystemHandle isWritable
%

! Class extensions for 'FileSystem'

!		Class methods for 'FileSystem'

category: '*filesystem-disk'
classmethod: FileSystem
* aFileOrDirectoryName
	^ self disk * aFileOrDirectoryName
%

category: '*filesystem-disk'
classmethod: FileSystem
/ aFileOrDirectoryName
	^ self disk / aFileOrDirectoryName
%

category: '*FileSystem-Client-Core'
classmethod: FileSystem
clientDisk
	"Answer a filesystem that represents the 'on-disk' filesystem used by the host operating system."

	^ ClientStore currentFileSystem
%

category: '*FileSystem-Memory'
classmethod: FileSystem
currentMemoryFileSystem
	^ MemoryStore currentFileSystem
%

category: '*filesystem-disk'
classmethod: FileSystem
disk
	"Answer a filesystem that represents the 'on-disk' filesystem used by the host operating system."

	^ DiskStore currentFileSystem
%

category: '*FileSystem-Core-GemStone'
classmethod: FileSystem
fileClass
	^GsFileAdaptor
%

category: '*FileSystem-Memory'
classmethod: FileSystem
memory
	^ self store: MemoryStore new
%

category: '*filesystem-disk'
classmethod: FileSystem
root
	^ self disk root
%

category: '*filesystem-disk'
classmethod: FileSystem
workingDirectory
	^ self disk workingDirectory
%

!		Instance methods for 'FileSystem'

category: '*FileSystem-Disk'
method: FileSystem
isDiskFileSystem
	^ store isDiskFileSystem
%

category: '*FileSystem-Memory'
method: FileSystem
isMemoryFileSystem
	^ store isMemoryFileSystem
%

! Class extensions for 'FileSystemStore'

!		Instance methods for 'FileSystemStore'

category: '*FileSystem-Disk'
method: FileSystemStore
isDiskFileSystem
	^ false
%

category: '*FileSystem-Memory'
method: FileSystemStore
isMemoryFileSystem
	^ false
%

! Class extensions for 'Fraction'

!		Instance methods for 'Fraction'

category: '*FileSystem-GemStone-Kernel'
method: Fraction
isPowerOfTwo
	^ numerator = 1 and: [ denominator isPowerOfTwo ]
%

! Class extensions for 'GsFile'

!		Class methods for 'GsFile'

category: '*FileSystem-Gemstone-Kernel'
classmethod: GsFile
currentWorkingDirectoryPath

	^self _expandEnvVariable: 'PWD' isClient:false
%

category: '*filesystem-gemstone-kernel-35x'
classmethod: GsFile
_contentsOfServerDirectory: aPathName expandPath: aBoolean

	^ self _contentsOfServerDirectory: aPathName expandPath: aBoolean utf8Results: false
%

! Class extensions for 'GsFileAdaptor'

!		Class methods for 'GsFileAdaptor'

category: '*FileSystem-Client-Core'
classmethod: GsFileAdaptor
createClientDirectory: path
	^GsFile createClientDirectory: path
%

category: '*FileSystem-Client-Core'
classmethod: GsFileAdaptor
deleteClientDirectory: path
	^GsFile _removeDirectory: path onClient: true 
%

category: '*FileSystem-Client-Core'
classmethod: GsFileAdaptor
deleteClientFile: aPathName
	^GsFile _removeFile: aPathName onClient: true
%

category: '*FileSystem-Client-Core'
classmethod: GsFileAdaptor
renameOnClient: oldFileFullName to: newFileFullName
	^(GsFile renameFile: oldFileFullName to: newFileFullName) = 0 
		ifTrue: [0] 
		ifFalse: [nil]
%

category: '*FileSystem-Client-Core'
classmethod: GsFileAdaptor
_fileKind: aPathName onClient: aBoolean
	^GsFile _fileKind: aPathName onClient: aBoolean
%

! Class extensions for 'Integer'

!		Class methods for 'Integer'

category: '*FileSystem-GemStone-Kernel'
classmethod: Integer
readFrom: aStringOrStream ifFail: aBlock
	"Answer an instance of one of the concrete subclasses if Integer. 
	Initial minus sign accepted.
	Imbedded radix specifiers not allowed;  use Number 
	class readFrom: for that.
	Execute aBlock if there are no digits."

	^(NumberParser on: aStringOrStream) nextIntegerBase: 10 ifFail: aBlock
%

!		Instance methods for 'Integer'

category: '*filesystem-gemstone-kernel'
method: Integer
<< shiftAmount
	"left shift"
	
	shiftAmount < 0 ifTrue: [self error: 'negative arg'].
	^ self bitShift: shiftAmount
%

category: '*filesystem-gemstone-kernel'
method: Integer
>> shiftAmount
	"right shift"
	
	shiftAmount < 0 ifTrue: [self error: 'negative arg'].
	^ self bitShift: 0 - shiftAmount
%

category: '*filesystem-gemstone-kernel'
method: Integer
atRandom
	"Answer a random integer from 1 to self.  This implementation uses a
	shared generator. Heavy users should their own implementation or use
	Interval>atRandom: directly."

	self = 0 ifTrue: [ ^0 ].
	self < 0 ifTrue: [ ^self negated atRandom negated ].
	^ self atRandom: (Random seed: self)
%

category: '*filesystem-gemstone-kernel'
method: Integer
atRandom: aGenerator
	"Answer a random integer from 1 to self picked from aGenerator."

	^ aGenerator nextInt: self
%

category: '*filesystem-gemstone-kernel'
method: Integer
digitAt: n
	"Answer the value of an apparent byte-indexable field in the receiver,
	 analogous to the large integers, which are organized as bytes."

	n = 1
		ifTrue: [ 
			"Negate carefully in case the receiver is SmallInteger minVal"
			^ self < 0
				ifTrue: [ -256 - self bitAnd: 255 ]
				ifFalse: [ self bitAnd: 255 ] ].
	^ self < 0
		ifTrue: [ (-256 - self bitShift: -8) + 1 digitAt: n - 1 ]
		ifFalse: [ (self bitShift: 8 - (n bitShift: 3)) bitAnd: 255 ]
%

category: '*FileSystem-Core'
method: Integer
humanReadableSIByteSize
	^ String streamContents: [ :s|
		self humanReadableSIByteSizeOn: s ]
%

category: '*FileSystem-Core'
method: Integer
humanReadableSIByteSizeOn: s
	| exponent base |
	"Print a string with an SI binary unit represation of myself."
	base := 1000.
	self < base
		ifTrue: [ ^ s print: self; space; nextPut: $B ].
	exponent := (self log / base log) asInteger.
	(self / (base ** exponent)) printOn: s showingDecimalPlaces: 2.
	s 
		space;
		nextPut: ('kMGTPE' at: exponent);
		nextPut: $B.
%

category: '*filesystem-gemstone-kernel'
method: Integer
isPowerOfTwo
	"Return true if the receiver is an integral power of two."
	^ self ~= 0 and: [(self bitAnd: self-1) = 0]
%

category: '*filesystem-gemstone-kernel'
method: Integer
numberOfDigitsInBase: b 
	"Return how many digits are necessary to print this number in base b.
	This does not count any place for minus sign, radix prefix or whatever.
	Note that this algorithm may cost a few operations on LargeInteger."

	| nDigits q total |
	self negative ifTrue: [^self negated numberOfDigitsInBase: b].
	self < b ifTrue: [^1].
	b isPowerOfTwo ifTrue: [^self highBit + b highBit - 2 quo: b highBit - 1].
	
	"A conversion from base 2 to base b has to be performed.
	This algorithm avoids Float computations like (self log: b) floor + 1,
	1) because they are inexact
	2) because LargeInteger might overflow
	3) because this algorithm might be cheaper than conversion"

	q := self.
	total := 0.
	["Make an initial nDigits guess that is lower than or equal to required number of digits"
	nDigits := b = 10
		ifTrue: [((q highBit - 1) * 1233 >> 12) + 1. "This is because (2 log)/(10 log)*4096 is slightly greater than 1233"]
		ifFalse: [q highBit quo: b highBit].
	total := total + nDigits.
	
	"See how many digits remains above these first nDigits guess"
	(q := q quo: (b raisedToInteger: nDigits)) < b] whileFalse.
	^q = 0
		ifTrue: [total]
		ifFalse: [total + 1]
%

category: '*filesystem-gemstone-kernel'
method: Integer
print: positiveNumberString on: aStream prefix: prefix length: minimum padded: zeroFlag
	| padLength |
	padLength := minimum - positiveNumberString size - prefix size.
	padLength > 0
		ifTrue: [zeroFlag
				ifTrue: [aStream nextPutAll: prefix; nextPutAll: (String new: padLength withAll: $0)]
				ifFalse: [aStream nextPutAll: (String new: padLength withAll: Character space); nextPutAll: prefix]]
		ifFalse: [aStream nextPutAll: prefix].
	aStream nextPutAll: positiveNumberString
%

category: '*filesystem-gemstone-kernel'
method: Integer
printOn: aStream base: base length: minimum padded: zeroFlag
	| prefix |
	
	prefix := self negative ifTrue: ['-'] ifFalse: [ '' ].
	
	self print: (self abs printStringBase: base) on: aStream prefix: prefix length: minimum padded: zeroFlag
%

! Class extensions for 'LargeInteger'

!		Instance methods for 'LargeInteger'

category: '*FileSystem-GemStone-Kernel'
method: LargeInteger
isLarge
	^true
%

! Class extensions for 'Number'

!		Class methods for 'Number'

category: '*FileSystem-GemStone-Kernel'
classmethod: Number
readFrom: stringOrStream ifFail: aBlock
	"Answer a number as described on aStream.  The number may
	be any accepted Smalltalk literal Number format.
	It can include a leading radix specification, as in 16rFADE.
	It can as well be NaN, Infinity or -Infinity for conveniency.
	If input does not represent a valid number, then execute fail block
	and leave the stream positioned before offending character"
	
	^(NumberParser on: stringOrStream) failBlock: aBlock; nextNumber
%

category: '*FileSystem-GemStone-Kernel'
classmethod: Number
squeezeNumberOutOfString: stringOrStream
	"Try and find a number in this string. First, look if the string 
	starts with a number. Then, see if it ends with a number. Then,
	remove a character from the front and see if the remaining 
	string makes a number. Repeat the process until no characters
	are left or the number has been found. As soon as a number is
	found, it is returned. Otherwise, the method fails."
	^ NumberParser squeezeNumberOutOfString: stringOrStream
%

!		Instance methods for 'Number'

category: '*FileSystem-GemStone-Kernel'
method: Number
asNumber
	^self
%

category: '*FileSystem-GemStone-Kernel'
method: Number
printStringBase: base
	^ String streamContents:
		[:strm | self printOn: strm base: base]
%

! Class extensions for 'Object'

!		Instance methods for 'Object'

category: '*filesystem-gemstone-kernel'
method: Object
assert: aBlock
	"Throw an assertion error if aBlock does not evaluates to true.
	We check for true explicitly to make the assertion fail for non booleans"
	self assert: aBlock description: 'Assertion failed'.
%

category: '*filesystem-gemstone-kernel'
method: Object
assert: aBlockOrBoolean description: aStringOrBlock
	"Throw an assertion error if aBlock does not evaluates to true."
	
	(aBlockOrBoolean isKindOf: BlockClosure) 
		ifTrue: [
			aBlockOrBoolean value ifFalse: [ AssertionFailure signal: aStringOrBlock value ] ]
		ifFalse: [
			aBlockOrBoolean ifFalse: [ AssertionFailure signal: aStringOrBlock ] ]
%

category: '*filesystem-gemstone-kernel'
method: Object
flag: aSymbol

	"Send this message, with a relevant symbol as argument, to flag a message for subsequent retrieval.  For example, you might put the following line in a number of messages:
	self flag: #returnHereUrgently
	Then, to retrieve all such messages, browse all senders of #returnHereUrgently."
%

category: '*filesystem-gemstone-kernel'
method: Object
isCharacter

	^ false
%

category: '*Zinc-Character-Encoding-Core'
method: Object
isGemStoneUtf8Encoding
	"This is for testing only"
	self flag: 'GemStone/Pharo code switched for research'.


	" Set to Encoder Type To Pharo Style: ' SessionTemps current at: #ZnEncoding put: #Pharo ' " 
	" Set to Encoder Type To GemStone Style: ' SessionTemps current at: #ZnEncoding put: #GemStone ' " 

	^(SessionTemps current at: #ZnEncoding otherwise: #GemStone) = #GemStone
%

category: '*filesystem-gemstone-kernel'
method: Object
split: aSequenceableCollection
	"Split the argument using the receiver as a separator."
	"optimized version for single delimiters"
	"($/ split: '/foo/bar')>>>#('' 'foo' 'bar') asOrderedCollection"
	"([:c| c isSeparator] split: 'aa bb cc dd')>>> #('aa' 'bb' 'cc' 'dd') asOrderedCollection"
		
	| result |
	result := OrderedCollection new: (aSequenceableCollection size / 2) asInteger.
	self split: aSequenceableCollection do: [ :item |
		result add: item ].
	^ result
%

category: '*filesystem-gemstone-kernel'
method: Object
split: aSequenceableCollection do: aBlock
	"optimized version for single delimiters:
	Example:
		$/ split: '/foo/bar' indicesDo: [ :item | ]"
	self split: aSequenceableCollection indicesDo: [ :start :end | 
		aBlock value: (aSequenceableCollection copyFrom: start to: end) ]
%

category: '*filesystem-gemstone-kernel'
method: Object
split: aSequenceableCollection indicesDo: aBlock
	"Perform an action specified as aBlock (with a start and end argument) to each of the indices of the receiver element that have been identified by splitting the receiver using the splitter argument. optimized version for single delimiters."
	
	"(String streamContents: [:s | Character space split: 'Pharo is cool'  indicesDo: [ :start :end | s << 's:' << start asString << ' ' << 'e:' << end asString << ' ' ]]) >>> 's:1 e:5 s:7 e:8 s:10 e:13 '"
		
		
		
	|  position oldPosition |
	
	position := 1.
	oldPosition := position.
	
	position := aSequenceableCollection indexOf: self startingAt: position.
	[ position > 0 ] whileTrue: [
		aBlock value: oldPosition value: position - 1.
		position := position + 1.
		oldPosition := position.
		position := aSequenceableCollection indexOf: self startingAt: position.
	].

	aBlock value: oldPosition value: aSequenceableCollection size.
%

! Class extensions for 'OrderedCollection'

!		Instance methods for 'OrderedCollection'

category: '*filesystem-gemstone-kernel'
method: OrderedCollection
withIndexCollect: elementAndIndexBlock 
	"Just like with:collect: except that the iteration index supplies the second argument to the block. Override superclass in order to use addLast:, not at:put:."

	| newCollection |
	newCollection := self species new: self size.
	1 to: self size do:
		[:index |
		newCollection addLast: (
			elementAndIndexBlock
				value: (self at: index)
				value: index)].
	^ newCollection
%

! Class extensions for 'Path'

!		Instance methods for 'Path'

category: '*FileSystem-Core'
method: Path
asFileReference
	^ FileSystem disk referenceTo: self
%

category: '*FileSystem-Core'
method: Path
relativeToReference: aReference 
	^ self relativeToPath: aReference path
%

! Class extensions for 'PositionableStream'

!		Instance methods for 'PositionableStream'

category: '*filesystem-gemstone-kernel'
method: PositionableStream
back
	"Go back one element and return it."

	self position = 0 ifTrue: [self positionError].
	self skip: -1.
	^ self peek
%

category: '*filesystem-gemstone-kernel'
method: PositionableStream
collectionSpecies

	^self _collection species
%

category: '*filesystem-gemstone-kernel'
method: PositionableStream
positionError
	"Since I am not necessarily writable, it is up to my subclasses to override 
	position: if expanding the collection is preferrable to giving this error."

	self error: 'Attempt to set the position of a PositionableStream out of bounds'
%

category: '*filesystem-gemstone-kernel'
method: PositionableStream
setFrom: newStart to: newStop

	position := newStart - 1.
	readLimit := newStop
%

! Class extensions for 'PositionableStreamPortable'

!		Instance methods for 'PositionableStreamPortable'

category: '*filesystem-gemstone-kernel'
method: PositionableStreamPortable
collectionSpecies

	^self collection species
%

category: '*filesystem-gemstone-kernel'
method: PositionableStreamPortable
isBinary
	"Return true if the receiver is a binary byte stream"
	^collection class == ByteArray
%

category: '*filesystem-gemstone-kernel'
method: PositionableStreamPortable
nextInto: aCollection
	"Read the next elements of the receiver into aCollection.
	Return aCollection or a partial copy if less than aCollection
	size elements have been read."
	^self next: aCollection size into: aCollection startingAt: 1.
%

category: '*filesystem-gemstone-kernel'
method: PositionableStreamPortable
nextMatchAll: aColl
    "Answer true if next N objects are the ones in aColl,
     else false.  Advance stream of true, leave as was if false."
    | save |
    save := self position.
    aColl do: [:each |
       (self next) = each ifFalse: [
            self position: save.
            ^ false]
        ].
    ^ true
%

category: '*filesystem-gemstone-kernel'
method: PositionableStreamPortable
setFrom: newStart to: newStop

	position := newStart - 1.
	readLimit := newStop
%

! Class extensions for 'QuadByteString'

!		Class methods for 'QuadByteString'

category: '*FileSystem-GemStone-Kernel'
classmethod: QuadByteString
from: aString 

	| newString |
	(aString isMemberOf: self)
		ifTrue: [^ aString copy].
	newString := self new: aString size.
	1 to: aString size do: [:index | newString at: index put: (aString at: index)].
	^ newString
%

!		Instance methods for 'QuadByteString'

category: '*Zinc-Character-Encoding-GemStone'
method: QuadByteString
isWideString
	"Answer whether the receiver is a WideString"
	^true
%

! Class extensions for 'Random'

!		Instance methods for 'Random'

category: '*FileSystem-GemStone-Kernel'
method: Random
nextInt: anInteger
	"Answer a random integer in the interval [1, anInteger].
	Handle large numbers too (for cryptography)."
	
	"This is Pharo code that is not implemented at the moment"

	"anInteger strictlyPositive ifFalse: [ self error: 'Range must be positive' ].
	anInteger asFloat isInfinite
		ifTrue: [^(self privateNextValue asFraction * anInteger) truncated + 1]."

	^ (self next * anInteger) truncated + 1
%

! Class extensions for 'ReadStreamPortable'

!		Instance methods for 'ReadStreamPortable'

category: '*filesystem-gemstone-kernel'
method: ReadStreamPortable
readInto: aCollection startingAt: startIndex count: n
	"Read n objects into the given collection. 
	Return number of elements that have been read."
	
	| max |
	max := (readLimit - position) min: n.
	aCollection 
		replaceFrom: startIndex 
		to: startIndex + max - 1
		with: collection
		startingAt: position + 1.
	position := position + max.
	^ max
%

! Class extensions for 'ReadWriteStreamPortable'

!		Instance methods for 'ReadWriteStreamPortable'

category: '*filesystem-gemstone-kernel'
method: ReadWriteStreamPortable
readInto: aCollection startingAt: startIndex count: n
	"Read n objects into the given collection. 
	Return number of elements that have been read."
	
	| max |
	max := (readLimit - position) min: n.
	aCollection 
		replaceFrom: startIndex 
		to: startIndex + max - 1
		with: collection
		startingAt: position + 1.
	position := position + max.
	^ max
%

! Class extensions for 'SequenceableCollection'

!		Instance methods for 'SequenceableCollection'

category: '*filesystem-gemstone-kernel'
method: SequenceableCollection
allButFirst
  "Answer a copy of the receiver containing all but the first
	element. Raise an error if there are not enough elements."

  ^ self allButFirst: 1
%

category: '*filesystem-gemstone-kernel'
method: SequenceableCollection
allButFirst: n
	"Answer a copy of the receiver containing all but the first n
	elements. Raise an error if there are not enough elements."

	^ self copyFrom: n + 1 to: self size
%

category: '*filesystem-gemstone-kernel'
method: SequenceableCollection
atRandom: aGenerator
	"Answer a random element of the receiver.  Uses aGenerator which
	should be kept by the user in a variable and used every time. Use
	this instead of #atRandom for better uniformity of random numbers 
	because only you use the generator.  Causes an error if self has no 
	elements."

	^ self at: (aGenerator nextInt: self size)
%

category: '*filesystem-gemstone-kernel'
method: SequenceableCollection
copyAfter: anElement
	"Answer a copy of the receiver from after the first occurence
	of anElement up to the end. If no such element exists, answer 
	an empty copy."

	^ self allButFirst: (self indexOf: anElement ifAbsent: [^ self copyEmpty])
%

category: '*filesystem-gemstone-kernel'
method: SequenceableCollection
copyAfterLast: anElement
	"Answer a copy of the receiver from after the last occurence
	of anElement up to the end. If no such element exists, answer 
	an empty copy."

	^ self allButFirst: (self lastIndexOf: anElement ifAbsent: [^ self copyEmpty])
%

category: '*filesystem-gemstone-kernel'
method: SequenceableCollection
copyFrom: start to: stop 
	"Answer a copy of a subset of the receiver, starting from element at 
	index start until element at index stop."

	| newSize |
	newSize := stop - start + 1.
	^(self species new: newSize)
		replaceFrom: 1
		to: newSize
		with: self
		startingAt: start
%

category: '*filesystem-gemstone-kernel'
method: SequenceableCollection
copyLast: n
	"Answer the last n elements of the receiver.  
	Raise an error if there are not enough elements."

	| size |
	size := self size.
	^ self copyFrom: size - n + 1 to: size
%

category: '*filesystem-gemstone-kernel'
method: SequenceableCollection
copyUpThrough: anElement
    "Answer all elements up to and including anObject. If there
     is no such object, answer a copy of the receiver."

	^self first: (self indexOf: anElement ifAbsent: [^ self copy])
%

category: '*filesystem-gemstone-kernel'
method: SequenceableCollection
copyUpToLast: anElement
  "Answer a copy of the receiver from index 1 to the last occurrence of 
	anElement, not including anElement."

	| n |
	n :=  (self lastIndexOf: anElement ifAbsent: [ ^ self copy ]) - 1.
  ^ self copyFrom: 1 to: n
%

category: '*filesystem-gemstone-kernel'
method: SequenceableCollection
copyWithFirst: newElement 
	"Answer a copy of the receiver that is 1 bigger than the receiver with newElement as the first element."

	| newIC |
	newIC := self species new: self size + 1.
	newIC 
		replaceFrom: 2
		to: self size + 1
		with: self
		startingAt: 1.
	newIC at: 1 put: newElement.
	^ newIC
%

category: '*filesystem-gemstone-kernel'
method: SequenceableCollection
endsWith: aSequenceableCollection
	"Answer true if the receiver ends with the argument collection"
	
	| start |
	(aSequenceableCollection isEmpty or: [self size < aSequenceableCollection size]) ifTrue: [^false].
	start := self size - aSequenceableCollection size.
	aSequenceableCollection withIndexDo: [:each :index | (self at: start + index) ~= each ifTrue: [^false]].
	^true
%

category: '*filesystem-gemstone-kernel'
method: SequenceableCollection
grownBy: length
	"Answer a copy of receiver collection with size grown by length"

	| newCollection size |
	size := self size.
	newCollection := self species new: size + length.
	newCollection replaceFrom: 1 to: size with: self startingAt: 1.
	^ newCollection
%

category: '*filesystem-gemstone-kernel'
method: SequenceableCollection
lastIndexOf: anElement ifAbsent: exceptionBlock
  "Answer the index of the last occurence of anElement within the  
	receiver. If the receiver does not contain anElement, answer the
	result of evaluating the argument, exceptionBlock."

  ^ self lastIndexOf: anElement startingAt: self size ifAbsent: exceptionBlock
%

category: '*filesystem-gemstone-kernel'
method: SequenceableCollection
lastIndexOf: anElement startingAt: lastIndex ifAbsent: exceptionBlock
  "Answer the index of the last occurence of anElement within the  
	receiver. If the receiver does not contain anElement, answer the
	result of evaluating the argument, exceptionBlock."

  lastIndex to: 1 by: -1 do: [ :index | 
    (self at: index) = anElement
      ifTrue: [ ^ index ] ].
  ^ exceptionBlock ~~ nil
    ifTrue: [ exceptionBlock value ]
    ifFalse: [ 0 ]
%

category: '*filesystem-gemstone-kernel'
method: SequenceableCollection
readStreamPortable

	^ ReadStreamPortable on: self
%

category: '*filesystem-gemstone-kernel'
method: SequenceableCollection
removeAll

	self removeFrom: 1 to: self size
%

category: '*filesystem-gemstone-kernel'
method: SequenceableCollection
replaceAll: oldObject with: newObject
  "Replace all occurences of oldObject with newObject"

  | index |
  index := self indexOf: oldObject startingAt: 1 ifAbsent: [ 0 ].
  [ index = 0 ]
    whileFalse: [ 
      self at: index put: newObject.
      index := self indexOf: oldObject startingAt: index + 1 ifAbsent: [ 0 ] ]
%

category: '*filesystem-gemstone-kernel'
method: SequenceableCollection
reversed
	"Answer a copy of the receiver with element order reversed."
	"Example: 'frog' reversed"

	| n result src |
	n := self size.
	result := self species new: n.
	src := n + 1.
	1 to: n do: [:i | result at: i put: (self at: (src := src - 1))].
	^ result
%

category: '*filesystem-gemstone-kernel'
method: SequenceableCollection
swap: oneIndex with: anotherIndex 
	"Move the element at oneIndex to anotherIndex, and vice-versa."

	| element |
	element := self at: oneIndex.
	self at: oneIndex put: (self at: anotherIndex).
	self at: anotherIndex put: element
%

category: '*filesystem-gemstone-kernel'
method: SequenceableCollection
withIndexCollect: elementAndIndexBlock 
	"Just like with:collect: except that the iteration index supplies the second argument to the block. Override superclass in order to use addLast:, not at:put:."

	| newCollection |
	newCollection := self species new: self size.
	1 to: self size do:
		[:index |
		newCollection at: index put: (
			elementAndIndexBlock
				value: (self at: index)
				value: index)].
	^ newCollection
%

category: '*filesystem-gemstone-kernel'
method: SequenceableCollection
writeStream
	^ WriteStream on: self
%

! Class extensions for 'Set'

!		Instance methods for 'Set'

category: '*FileSystem-GemStone-Kernel'
method: Set
atRandom: aGenerator
	"Answer a random element of the receiver. Uses aGenerator which
    should be kept by the user in a variable and used every time. Use
    this instead of #atRandom for better uniformity of random numbers because 
	only you use the generator. Causes an error if self has no elements."
	| rand |

	self isEmpty ifTrue: [self error: 'No items in collection - unable to find a random item'].
	rand := aGenerator nextInt: self size.
	self doWithIndex:[:each :ind |
		ind == rand ifTrue:[^each]].
	^self error: 'No items in collection - unable to find a random item'
%

! Class extensions for 'SmallInteger'

!		Instance methods for 'SmallInteger'

category: '*FIleSystem-GemStone-Kernel'
method: SmallInteger
isLarge
	^false
%

category: '*FIleSystem-GemStone-Kernel'
method: SmallInteger
numberOfDigitsInBase: b 
	"Return how many digits are necessary to print this number in base b.
	Mostly same as super but an optimized version for base 10 case"
	
	b = 10 ifFalse: [^super numberOfDigitsInBase: b].
	self < 0 ifTrue: [^self negated numberOfDigitsInBase: b].
	^self decimalDigitLength
%

category: '*FIleSystem-GemStone-Kernel'
method: SmallInteger
printOn: stream base: base length: minimumLength padded: padWithZeroes

	| n numberOfDigits totalLength divisor |
	self < 0
		ifTrue: [
			n := self negated.
			totalLength := 1 ]
		ifFalse: [
			n := self.
			totalLength := 0 ].
	numberOfDigits := n numberOfDigitsInBase: base.
	totalLength := totalLength + numberOfDigits.
	padWithZeroes ifFalse: [
		[ totalLength < minimumLength ] whileTrue: [
			stream space.
			totalLength := totalLength + 1 ] ].
	n = self ifFalse: [ stream nextPut: $- ].
	padWithZeroes ifTrue: [
		[ totalLength < minimumLength ] whileTrue: [
			stream nextPut: $0.
			totalLength := totalLength + 1 ] ].
	divisor := (base raisedToInteger: numberOfDigits - 1).
	[ divisor > 0 ] whileTrue: [
		| digit |
		digit := n // divisor.
		stream nextPut: ('0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ' at: digit + 1).
		n := n - (digit * divisor).
		divisor := divisor // base ]
%

category: '*FIleSystem-GemStone-Kernel'
method: SmallInteger
printStringBase: b 
	"Return a String representation of this number in base b.
	For SmallIntegers, it is more efficient to print directly in a String,
	rather than using a Stream like super."

	self < 0
		ifTrue: [^ '-'
				, (self negated printStringBase: b)].
	self < b
		ifTrue: [^ String
				with: (Character digitValue: self)].
	^ self printStringBase: b nDigits: (self numberOfDigitsInBase: b)
%

! Class extensions for 'Stream'

!		Instance methods for 'Stream'

category: '*filesystem-gemstone-kernel'
method: Stream
isBinary
	^false
%

! Class extensions for 'String'

!		Class methods for 'String'

category: '*FileSystem-GemStone-Kernel'
classmethod: String
cr
	"Answer a string containing a single carriage return character."

	^ self with: Character cr
%

category: '*FileSystem-GemStone-Kernel'
classmethod: String
crlf
	"Answer a string containing a carriage return and a linefeed."

	^ self with: Character cr with: Character lf
%

category: '*FileSystem-GemStone-Kernel'
classmethod: String
from: aString

	^self withAll: aString
%

category: '*FileSystem-GemStone-Kernel'
classmethod: String
lf
	"Answer a string containing a single carriage return character."

	^ self with: Character lf
%

!		Instance methods for 'String'

category: '*FileSystem-Gemstone-Kernel'
method: String
asValidSelector
	"Returns a symbol that is a vlida selector: remove any space or forbiddent characters"

	| result |
	result := String new.
	self first isAlphaNumeric ifFalse: [self error: 'Not able to create selector - must start with a character'].
	result add: self first asLowercase.
	2 to: self size do: [:idx | result add: (self at: idx)].
	^result
%

category: '*FileSystem-Gemstone-Kernel'
method: String
format: collection 
	"Format the receiver by interpolating elements from collection, as in the following examples:  
	'Today is {1}.' format: {Date today}.
	'Today is {date}.' format: (Dictionary with: #date->Date today). 
	'In {1} you can escape \{ by prefixing it with \\' format: {'strings'}.  "
	
	^ self class new: self size streamContents: [ :result | | stream |
		stream := self readStream.
		[ stream atEnd ] whileFalse: [ | currentChar | 
			(currentChar := stream next) == ${
				ifTrue: [ | expression index | 
					expression := stream upTo: $}.
					index := Integer readFrom: expression ifFail: [ expression ].
					result nextPutAll: (collection at: index) asString ]
				ifFalse: [
					currentChar == $\
						ifTrue: [ stream atEnd ifFalse: [ result nextPut: stream next ] ]
						ifFalse: [ result nextPut: currentChar ] ] ] ]
%

category: '*FileSystem-Gemstone-Kernel'
method: String
includesSubstring: substring
	"Returns whether the receiver contains the argument."
	"('abcdefgh' includesSubstring: 'de') >>> true"
	
	^ substring isEmpty or: [ (self findString: substring startingAt: 1) > 0 ]
%

category: '*FileSystem-Gemstone-Kernel'
method: String
squeezeOutNumber
	"Try to find a number somewhere in this string, as explained in Number>readFrom:
	
	this method returns the first number found"
	
	"'th is is29 a stRI4' squeezeOutNumber >>> 29"
	"'th is is2 9 a stRI4' squeezeOutNumber >>> 2"
	
	^ Number squeezeNumberOutOfString: self
%

! Class extensions for 'System'

!		Class methods for 'System'

category: '*FileSystem-GemStone-Kernel'
classmethod: System
clientOsName
	^self clientVersionReport at: 'osName'
%

category: '*FileSystem-GemStone-Kernel'
classmethod: System
lineEnding
  "Answer the os-specific line endings"

  ^ String with: Character lf
%

category: '*FileSystem-GemStone-Kernel'
classmethod: System
osName
	^self gemVersionAt: 'osName'
%

! Class extensions for 'TestAsserter'

!		Instance methods for 'TestAsserter'

category: '*filesystem-gemstone-kernel'
method: TestAsserter
assert: anObject equals: otherObj description: aString
	self
		assert: anObject = otherObj
		description: anObject printString , ' is not equal to ' , otherObj printString , ' - ' , aString.
%

category: '*filesystem-gemstone-kernel'
method: TestAsserter
assertCollection: actual equals: expected
	"Specialized test method that generates a proper error message for collection"
	^ self
		assert: expected = actual
		description: [ self comparingCollectionBetween: actual and: expected ]
%

category: '*filesystem-gemstone-kernel'
method: TestAsserter
assertCollectionOfStrings: actual equalsCollectionOfStrings: expected
	"Specialized test method that generates a proper error message for collection"
	actual with: expected do: [:a :b |
		self
			assert: (self isString: a equalToString: b)
			description: a printString , ' is  equal to ' , b printString]
%

category: '*filesystem-gemstone-kernel'
method: TestAsserter
assertString: actual equalsString: expected
	"Specialized test method that generates a proper error message for collection"
	^ self
		assert: (actual isEquivalent: expected)
		description: actual printString , ' is not equal to ' , expected printString
%

category: '*filesystem-gemstone-kernel'
method: TestAsserter
comparingCollectionBetween: left and: right
	| additionalLeft additionalRight sortBlock|
	
	"use a very slow sort block"
	sortBlock := [ :a :b | a asString <= b asString ].
	additionalLeft := (left difference: right) sort: sortBlock.
	additionalRight := (right difference: left) sort: sortBlock. 
	
	^ String streamContents: [:stream |
		stream
			nextPutAll: 'Given Collections do not match. Got '; lf;
			tab; nextPutAll: 'left := '; print: left; nextPut: $.; lf;
			nextPutAll: ' instead of ';
			tab; nextPutAll: ' right :='; print: right; nextPut: $.; lf.
		left size = right size
			ifFalse: [ 
				stream 
					nextPutAll: 'Collection size does not match: left='; 
					print: left size;
					nextPutAll: ' vs. right=';
					print: right size; lf ].
		additionalLeft isEmpty
			ifFalse: [ 
				stream 
					nextPutAll: 'Got ';
					print: additionalLeft size;
					nextPutAll: ' additional element(s) in the left collection: ';
					tab; print: additionalLeft  ].
		additionalRight isEmpty
			ifFalse: [ 
				stream 
					nextPutAll: 'Got ';
					print: additionalRight size;
					nextPutAll: ' additional element(s) in the right collection: ';
					tab; print: additionalRight  ]]
%

category: '*filesystem-gemstone-kernel'
method: TestAsserter
isString: actual comparedToDifferentStringClass: expected
	^(actual isKindOf: String) and: [
		(expected isKindOf: String) and: [
			actual class name ~~ expected class name]]
%

category: '*filesystem-gemstone-kernel'
method: TestAsserter
isString: actual equalToString: expected
	"Support testing strings in two different classes have the same contents"
	^(self isString: actual comparedToDifferentStringClass: expected)
		ifTrue: [
			actual size = expected size ifFalse: [^false].
			actual with: expected do: [:a :b | a == b ifFalse: [^false] ].
			true]
		ifFalse: [
			actual = expected]
%

! Class extensions for 'UndefinedObject'

!		Instance methods for 'UndefinedObject'

category: '*filesystem-gemstone-kernel'
method: UndefinedObject
isEmptyOrNil
  "Answer whether the receiver contains any elements, or is nil.  Useful in numerous situations where one wishes the same reaction to an empty collection or to nil"

  ^ true
%

! Class extensions for 'Utf8'

!		Instance methods for 'Utf8'

category: '*filesystem-gemstone-kernel'
method: Utf8
asByteArray

	^ ByteArray streamContents: [ :stream |
		self do: [ :each |
			stream nextPut: each ] ]
%

! Class extensions for 'WriteStreamPortable'

!		Instance methods for 'WriteStreamPortable'

category: '*FileSystem-GemStone-Kernel'
method: WriteStreamPortable
<< anObject
	"A more readable, shorter alternative to #nextPutAll: that also
	accepts non-Collection arguments"

	anObject class == collection class
		ifTrue: [ self nextPutAll: anObject ]
		ifFalse: [ anObject putOn: self ]
%

! Class Initialization

run
ClientStore initialize.
DiskStore initialize.
FastUUIDGenerator initialize.
ProcessSpecificVariable initialize.
ZnBase64Encoder initialize.
ZnSimplifiedByteEncoder initialize.
ZnUTF8Encoder initialize.
true
%
