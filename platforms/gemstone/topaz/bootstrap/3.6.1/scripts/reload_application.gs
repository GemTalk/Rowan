#!/usr/local/bin/smalltalk/gemstone/topaz

	omit pushonly
	display classoops

  set user SystemUser p swordfish
  login

run
   {
   	'file:$ROWAN_PROJECTS_HOME/application/Project_Missing/rowan/specs/Project_Missing.ston'.
   	'file:$ROWAN_PROJECTS_HOME/application/Project_Main/rowan/specs/Project_Main.ston'.
   	'file:$ROWAN_PROJECTS_HOME/application/Project_GemStoneExtensions/rowan/specs/Project_GemStoneExtensions.ston'.
   	'file:$ROWAN_PROJECTS_HOME/application/Project_TestsGemStoneExtensions/rowan/specs/Project_TestsGemStoneExtensions.ston'.
   	'file:$ROWAN_PROJECTS_HOME/application/Project_TestsMain/rowan/specs/Project_TestsMain.ston'.
   } do: [:specUrl |
   	Rowan projectTools upgrade upgradeProjectFromSpecUrl: specUrl.
   	System commit ].
%
