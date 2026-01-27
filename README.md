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

## Web (Offline HTML/JS)
- Mo `web\\index.html` bang Edge/Chrome (khong can server, khong can thu vien ngoai).
- Paste ABAP vao o ben trai -> bam `Analyze`.
- Tab `Objects`: xem danh sach FORM/EVENT + description/params/DATA/CONSTANTS/calls.
- Tab `Diagram`: flow diagram (SVG) va highlight cycle (mau do).
- Tab `Sequence`: sequence diagram theo luong chay (columns = steps, rows = event/form), click param -> `Trace`.
- Tab `Trace`: truy vet bien (CHANGING/TABLES + global writes).

## Excel Template Tool (ABAP Objects XML -> filled template)
- Workbook: `excel\\AbapTemplateTool.xlsm`
- Muc tieu: export ABAP Objects XML tu tab `Templates` (web) -> Excel VBA doc XML -> gen template tren sheet `Template` (du lieu da fill theo context)

Workflow:
1) Mo `web\\index.html` -> Analyze -> tab `Templates`
2) Tick chon template can export -> `Export` -> `Copy` hoac `Download` (XML)
3) Mo `excel\\AbapTemplateTool.xlsm`
   - Sheet `Input`: paste XML vao cot A tu `A2` tro xuong (1 dong XML / 1 row) (hoac bam `Load XML file`)
   - Bam `Generate all`
4) Sheet `Template`: template da duoc copy tu `Template config` va fill placeholder tu XML

Ghi chu:
- Sheet `Template config`: chua cac template mau, moi template duoc khoanh vung bang 4 marker o 4 goc (`__ABAPFLOW_TPL_<templateId>_{TL|TR|BL|BR}__`).
- Co the them template moi bang cach copy 1 block trong `Template config` va doi marker + noi dung ben trong.
