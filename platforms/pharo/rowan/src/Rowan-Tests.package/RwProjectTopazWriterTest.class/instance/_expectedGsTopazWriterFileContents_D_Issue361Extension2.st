private
_expectedGsTopazWriterFileContents_D_Issue361Extension2

| percent|
percent := '%'.	"a method containing % in the first column cannot be loaded into GemStone using topaz"
	^ '! Class extensions for ''Issue361Class1''

!		Instance methods for ''Issue361Class1''

category: ''*issue361-extension2''
method: Issue361Class1
method5 ^5
', percent, '

! Class extensions for ''Issue361Class2''

!		Class methods for ''Issue361Class2''

category: ''*issue361-extension2''
classmethod: Issue361Class2
initialize "another noop"
', percent, '

! Class Initialization

run
Issue361Class2 initialize.
', percent, '

'