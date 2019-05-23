private
_expectedGsTopazWriterFileContents_MULTI_B_Issue361Extension2

| percent|
percent := '%'.	"a method containing % in the first column cannot be loaded into GemStone using topaz"
	^ '! Class extensions for ''Issue361Class1_1''

!		Instance methods for ''Issue361Class1_1''

category: ''*issue361-extension2_1''
method: Issue361Class1_1
method5 ^5
', percent, '

! Class extensions for ''Issue361Class1_2''

!		Instance methods for ''Issue361Class1_2''

category: ''*issue361-extension2_2''
method: Issue361Class1_2
method5 ^5
', percent, '

! Class extensions for ''Issue361Class2_1''

!		Class methods for ''Issue361Class2_1''

category: ''*issue361-extension2_1''
classmethod: Issue361Class2_1
initialize "another noop"
', percent, '

! Class extensions for ''Issue361Class2_2''

!		Class methods for ''Issue361Class2_2''

category: ''*issue361-extension2_2''
classmethod: Issue361Class2_2
initialize "another noop"
', percent, '

! Class Initialization

run
Issue361Class2_1 initialize.
Issue361Class2_2 initialize.
', percent, '

'