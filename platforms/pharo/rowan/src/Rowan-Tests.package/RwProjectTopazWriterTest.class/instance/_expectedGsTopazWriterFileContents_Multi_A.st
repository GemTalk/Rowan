private
_expectedGsTopazWriterFileContents_Multi_A

| percent|
percent := '%'.	"a method containing % in the first column cannot be loaded into GemStone using topaz"
	^ '! Class Declarations

doit
(Array
	subclass: ''Issue361Class2_1''
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: CypressTestData
	options: #())
		category: ''Issue361-Core_1'';
		comment: '''';
		immediateInvariant.
true.
', percent, '

doit
(Array
	subclass: ''Issue361Class2_2''
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: CypressTestData
	options: #())
		category: ''Issue361-Core_2'';
		comment: '''';
		immediateInvariant.
true.
', percent, '

doit
(Issue361Class1
	subclass: ''Issue361Class3_1''
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: CypressTestData
	options: #())
		category: ''Issue361-Core_1'';
		comment: '''';
		immediateInvariant.
true.
', percent, '

doit
(Issue361Class1
	subclass: ''Issue361Class3_2''
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: CypressTestData
	options: #())
		category: ''Issue361-Core_2'';
		comment: '''';
		immediateInvariant.
true.
', percent, '

doit
(Object
	subclass: ''Issue361Class1_1''
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: CypressTestData
	options: #())
		category: ''Issue361-Core_1'';
		comment: '''';
		immediateInvariant.
true.
', percent, '

doit
(Object
	subclass: ''Issue361Class1_2''
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: CypressTestData
	options: #())
		category: ''Issue361-Core_2'';
		comment: '''';
		immediateInvariant.
true.
', percent, '

! Class implementation for ''Issue361Class2_1''

!		Instance methods for ''Issue361Class2_1''

category: ''accessing''
method: Issue361Class2_1
method3 ^3
', percent, '

! Class implementation for ''Issue361Class2_2''

!		Instance methods for ''Issue361Class2_2''

category: ''accessing''
method: Issue361Class2_2
method3 ^3
', percent, '

! Class implementation for ''Issue361Class3_1''

!		Class methods for ''Issue361Class3_1''

category: ''accessing''
classmethod: Issue361Class3_1
method4 ^4
', percent, '

! Class implementation for ''Issue361Class3_2''

!		Class methods for ''Issue361Class3_2''

category: ''accessing''
classmethod: Issue361Class3_2
method4 ^4
', percent, '

! Class implementation for ''Issue361Class1_1''

!		Class methods for ''Issue361Class1_1''

category: ''initialization''
classmethod: Issue361Class1_1
initialize "noop"
', percent, '

category: ''accessing''
classmethod: Issue361Class1_1
method2 ^2
', percent, '

! Class implementation for ''Issue361Class1_2''

!		Class methods for ''Issue361Class1_2''

category: ''initialization''
classmethod: Issue361Class1_2
initialize "noop"
', percent, '

category: ''accessing''
classmethod: Issue361Class1_2
method2 ^2
', percent, '

! Class extensions for ''Issue361Class1_1''

!		Instance methods for ''Issue361Class1_1''

category: ''*issue361-extension1_1''
method: Issue361Class1_1
method1 ^1
', percent, '

category: ''*issue361-extension2_1''
method: Issue361Class1_1
method5 ^5
', percent, '

! Class extensions for ''Issue361Class1_2''

!		Instance methods for ''Issue361Class1_2''

category: ''*issue361-extension1_2''
method: Issue361Class1_2
method1 ^1
', percent, '

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
Issue361Class1_1 initialize.
Issue361Class1_2 initialize.
Issue361Class2_1 initialize.
Issue361Class2_2 initialize.
', percent, '

'