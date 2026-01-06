# ABAP Flow Diagram (Excel VBA)

## Yeu cau
- Windows + Microsoft Excel
- Python 3.x (de chay script `xlwings`)
- Excel cho phep script truy cap VBA Project:
  - `File -> Options -> Trust Center -> Trust Center Settings -> Macro Settings`
  - tick: `Trust access to the VBA project object model`
  - Hoac chay script (set registry cho user hien tai): `powershell -ExecutionPolicy Bypass -File scripts\\enable_vbom.ps1`

## Cai dat
- Chay: `powershell -ExecutionPolicy Bypass -File scripts\\setup.ps1`

## Tao/Sync workbook (.xlsm)
- Import code tu `vba/` vao workbook: `powershell -ExecutionPolicy Bypass -File scripts\\sync_vba.ps1 -Import`
- Export code tu workbook ra `vba/`: `powershell -ExecutionPolicy Bypass -File scripts\\sync_vba.ps1 -Export`
- Neu gap loi "Programmatic access to Visual Basic Project is not trusted": `powershell -ExecutionPolicy Bypass -File scripts\\sync_vba.ps1 -Import -EnableVBOM`

Workbook mac dinh: `excel\\AbapFlowDiagram.xlsm`

## Chay macro
- Paste ABAP vao `Sheet1`
- Chay macro: `BuildAbapFlowDiagram` (se ve diagram len `Sheet2`)
- Macro cung tao `Objects` de liet ke toan bo object + global DATA/CONSTANTS

## ABAP sample de test
- File: `samples\\ZABAP_FLOW_PARSER_TEST.abap`
