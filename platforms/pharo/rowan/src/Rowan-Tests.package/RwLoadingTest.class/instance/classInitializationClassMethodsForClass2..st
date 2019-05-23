support - classInitialization definitions
classInitializationClassMethodsForClass2: className
  ^ self
    methodDefsFromSpec:
      {
			{#'initialize'.
      		'initialization'.
      		('initialize ClassVar1 := classInstVar1 := 1. classInstVar2 := 2' )}.
			{#'classVar'.
      		'accessing'.
      		('classVar ^ ClassVar1' )}.
			{#'classInstVar1'.
      		'accessing'.
      		('classInstVar1 ^ classInstVar1' )}.
			{#'classInstVar2'.
      		'accessing'.
      		('classInstVar2 ^ classInstVar2' )}.
		}