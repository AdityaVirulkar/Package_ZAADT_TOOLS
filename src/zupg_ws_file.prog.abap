*&---------------------------------------------------------------------*
*& Report ZUPG_WS_FILE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZUPG_WS_FILE.
PARAMETERS: p_file TYPE localfile.

AT SELECTION-SCREEN on VALUE-REQUEST FOR p_file.


CALL FUNCTION 'WS_FILENAME_GET'

EXPORTING

def_path = 'C:\'

mask = ',.,..'

mode = 'O'

title = 'Select input file name '(003)

IMPORTING

filename = p_file

EXCEPTIONS

inv_winsys = 1

no_batch = 2

selection_cancel = 3

selection_error = 4

OTHERS = 5.

IF sy-subrc <> 0.

MESSAGE i036(zs) WITH text-e04.

ENDIF.
