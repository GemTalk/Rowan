omit pushonly
display classoops

set user SystemUser pass swordfish
login

run
   {
   	'file:$ROWAN_PROJECTS_HOME/Example_Project_Missing/rowan/specs/Project_Missing.ston'.
   	'file:$ROWAN_PROJECTS_HOME/Example_Project_Main/rowan/specs/Project_Main.ston'.
   	'file:$ROWAN_PROJECTS_HOME/Example_Project_GemStoneExtensions/rowan/specs/Project_GemStoneExtensions.ston'.
   	'file:$ROWAN_PROJECTS_HOME/Example_Project_TestsGemStoneExtensions/rowan/specs/Project_TestsGemStoneExtensions.ston'.
   	'file:$ROWAN_PROJECTS_HOME/Example_Project_TestsMain/rowan/specs/Project_TestsMain.ston'
   } do: [:specUrl |
   	Rowan projectTools upgrade upgradeProjectFromSpecUrl: specUrl.
   	System commit ].
%
