* User name
PARAMETERS p_user TYPE syuname DEFAULT sy-uname OBLIGATORY. "User name parameter

* Flag checkbox
PARAMETERS p_flag TYPE abap_bool DEFAULT abap_true AS CHECKBOX. "Enable flag

PARAMETERS p_len TYPE i LENGTH 10 DECIMALS 2. "Number format
PARAMETERS p_memory TYPE string MEMORY ID memid. "Remembered value
PARAMETERS p_group TYPE i GROUP grp1 NO-DISPLAY. "Hidden in group
