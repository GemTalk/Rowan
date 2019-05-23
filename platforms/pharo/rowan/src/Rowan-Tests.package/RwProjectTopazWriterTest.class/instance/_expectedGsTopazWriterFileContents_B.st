private
_expectedGsTopazWriterFileContents_B

| percent|
percent := '%'.	"a method containing % in the first column cannot be loaded into GemStone using topaz"
	^ '! Class Declarations

doit
(Array
	subclass: ''Issue361Class2''
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: CypressTestData
	options: #())
		category: ''Issue361-Core'';
		comment: '''';
		immediateInvariant.
true.
', percent, '

doit
(Object
	subclass: ''Issue361Class1''
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: CypressTestData
	options: #())
		category: ''Issue361-Core'';
		comment: '''';
		immediateInvariant.
true.
', percent, '

doit
(Issue361Class1
	subclass: ''Issue361Class3''
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: CypressTestData
	options: #())
		category: ''Issue361-Core'';
		comment: '''';
		immediateInvariant.
true.
', percent, '

! Class implementation for ''Issue361Class2''

!		Instance methods for ''Issue361Class2''

category: ''accessing''
method: Issue361Class2
method3 ^3
', percent, '

! Class implementation for ''Issue361Class1''

!		Class methods for ''Issue361Class1''

category: ''accessing''
classmethod: Issue361Class1
method2 ^2
', percent, '

! Class implementation for ''Issue361Class3''

!		Class methods for ''Issue361Class3''

category: ''accessing''
classmethod: Issue361Class3
method4 ^4
', percent, '

! Class extensions for ''Issue361Class1''

!		Instance methods for ''Issue361Class1''

category: ''*issue361-extension1''
method: Issue361Class1
method1 ^1
', percent, '

category: ''*issue361-extension2''
method: Issue361Class1
method5 ^5
', percent, '

'