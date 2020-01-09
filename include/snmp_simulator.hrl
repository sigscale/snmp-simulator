%%% snmp_simulator.hrl

-define(Unsigned32, 0..4294967295).
-define(Unsigned64, 0..18446744073709551615).
-define(Integer32, -2147483648..2147483647).

-record(alarmModelTable,
		{key :: {AlarmListName :: string(),
				AlarmModelIndex :: ?Unsigned32,
				AlarmModelState :: ?Unsigned32},
		alarmModelNotificationId = [0, 0] :: snmpa:oid(),
		alarmModelVarbindIndex = 0 :: ?Unsigned32,
		alarmModelVarbindValue = 0 :: ?Integer32,
		alarmModelDescription = "" :: string(),
		alarmModelSpecificPointer = [0, 0] :: snmpa:oid(),
		alarmModelVarbindSubtree = [0, 0] :: snmpa:oid(),
		alarmModelResourcePrefix = [0, 0] :: snmpa:oid(),
		alarmModelRowStatus = 3 :: 1..6}).

-record(alarmActiveTable,
		{key :: {AlarmListName :: string(),
				AlarmActiveDateAndTime :: string(),
				AlarmActiveIndex :: ?Unsigned32},
		alarmActiveEngineID :: [byte()],
		alarmActiveEngineAddressType :: 0..4 | 16,
		alarmActiveEngineAddress :: [byte()],
		alarmActiveContextName :: string(),
		alarmActiveVariables :: ?Unsigned32,
		alarmActiveNotificationID :: snmpa:oid(),
		alarmActiveResourceId :: snmpa:oid(),
		alarmActiveDescription :: string(),
		alarmActiveLogPointers :: snmpa:oid(),
		alarmActiveModelPointer :: snmpa:oid(),
		alarmActiveSpecificPointer :: snmpa:oid()}).

-record(alarmActiveVariableTable,
		{key :: {AlarmListName :: string(),
				AlarmActiveIndex :: ?Unsigned32,
				AlarmActiveVariableIndex :: ?Unsigned32},
		alarmActiveVariableID :: snmpa:oid(),
		alarmActiveVariableValueType :: 1..9,
		alarmActiveVariableCounter32Val :: ?Unsigned32,
		alarmActiveVariableUnsigned32Val :: ?Unsigned32,
		alarmActiveVariableTimeTicksVal :: ?Unsigned32,
		alarmActiveVariableInteger32Val :: ?Integer32,
		alarmActiveVariableOctetStringVal :: [byte()],
		alarmActiveVariableIpAddressVal :: string(),
		alarmActiveVariableOidVal :: snmpa:oid(),
		alarmActiveVariableCounter64Val :: ?Unsigned64,
		alarmActiveVariableOpaqueVal :: [byte()]}).

-record(alarmActiveStatsTable,
		{key :: (AlarmListName :: string()),
		alarmActiveStatsActiveCurrent :: ?Unsigned32,
		alarmActiveStatsActives :: ?Unsigned32,
		alarmActiveStatsLastRaise :: ?Unsigned32,
		alarmActiveStatsLastClear :: ?Unsigned32}).

-record(alarmClearTable,
		{key :: {AlarmListName :: string(),
				AlarmClearDateAndTime :: string(),
				AlarmClearIndex :: ?Unsigned32},
		alarmClearDateAndTime :: string(),
		alarmClearEngineID :: [byte()],
		alarmClearEngineAddressType :: 0..4 | 16,
		alarmClearEngineAddress :: [byte()],
		alarmClearContextName :: string(),
		alarmClearNotificationID :: snmpa:oid(),
		alarmClearResourceId :: snmpa:oid(),
		alarmClearLogIndex :: ?Unsigned32,
		alarmClearModelPointer :: snmpa:oid()}).

