private
_expectedGsTopazWriterFileContents_MULTI_B_Issue361Core

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

! Class Initialization

run
Issue361Class1_1 initialize.
Issue361Class1_2 initialize.
', percent, '

'