# Chỉnh Template Form bằng Excel

## Mục đích

Tính năng Excel round-trip cho phép copy một Template Key từ Viewer sang Excel, sửa nội dung hoặc định dạng, sau đó paste trở lại Template Form.

Placeholder được giữ ở dạng thô, ví dụ `{values.target.finalDesc}`. Viewer chỉ lưu thay đổi khi người dùng bấm **Apply**.

## Khi dùng

Dùng tính năng này khi cần sửa một template có nhiều ô hoặc nhiều vùng định dạng. Excel phù hợp hơn Editable Grid cho các thao tác như đổi màu, font, căn lề và điều chỉnh nhiều ô cùng lúc.

Tính năng hiện không đọc hoặc ghi file `.xlsx`. Dữ liệu được trao đổi qua clipboard HTML của Excel.

## Điều kiện trước khi làm

- Mở `viewer/index.html` hoặc `viewer/index.inline.html` bằng Chrome.
- Viewer phải hiển thị phiên bản `v2026.07.26-r80` trở lên.
- Máy có Excel hoặc ứng dụng bảng tính cung cấp clipboard `text/html`.
- Đã parse ABAP và mở tab **Template**.

Nếu Viewer đã mở từ trước khi cập nhật, nhấn `Ctrl+F5` để nạp lại JavaScript và CSS.

## Bước làm

1. Bấm **Template Form** trên thanh công cụ.
2. Chọn Template Key cần sửa, ví dụ `READ_TABLE`.
3. Bấm **Copy to Excel**.
4. Paste vào một vùng trống trong Excel.
5. Sửa text, placeholder hoặc định dạng được hỗ trợ.
6. Chọn đúng toàn bộ vùng vừa paste và nhấn `Ctrl+C`.
7. Quay lại Viewer, bấm **Paste from Excel**.
8. Click vùng có nội dung **Click here, then press Ctrl+V**, sau đó nhấn `Ctrl+V`.
9. Kiểm tra kích thước bảng, số range, số merge và các cảnh báo trong preview.
10. Bấm **Replace selected key** để thay ranges trong bản nháp của modal.
11. Bấm **Apply** để lưu Template Form vào Viewer.

Bấm nút đóng của trang **Template Form** trước bước 11 sẽ bỏ bản nháp vừa import; `localStorage` chỉ được cập nhật khi bấm **Apply**.

## Format được hỗ trợ

Viewer nhập các thuộc tính sau:

- Text và placeholder.
- Màu nền và màu chữ.
- Font và cỡ chữ.
- Bold, italic và underline.
- Align, valign và wrap.
- Merge thật từ `rowspan` hoặc `colspan`.
- Border ngoài khép kín, lưu bằng token `outside-thin`.

Khi copy sang Excel, border `outside-thin` được xuất thành đường `0.5pt`.

Các thuộc tính trình bày phụ do Excel tự sinh như `width`, `height`, `padding-*`, `background-*` và `text-wrap-mode` không được lưu vào template.

## Kiểm tra

Sau khi paste vào `.template-excel-paste-zone`, kiểm tra panel **Paste Excel into `<Template Key>`**:

1. Kiểm tra dòng trạng thái phải ghi `Excel preview created successfully.` hoặc thông báo có cảnh báo.
2. Kiểm tra số hàng và cột trong preview đúng với vùng đã copy.
3. Kiểm tra placeholder vẫn còn dấu `{}` và chưa bị resolve.
4. Kiểm tra từng outline, đặc biệt các border range nằm liền nhau.
5. Bấm **Replace selected key**, sau đó kiểm tra Editable Grid.
6. Bấm **Apply**, đóng rồi mở lại Template Form để xác nhận cấu hình đã được lưu.

Với template mẫu `READ_TABLE`, kiểm tra tự động hiện dùng bảng 4 hàng × 80 cột và xác nhận đủ 12 border range sau vòng Copy → Excel → Copy → Paste.

Từ thư mục `F:\MyGitProject\Abap`, chạy:

```powershell
node tests/run.js viewer --focus template-excel-roundtrip
npm run test:fast
uv run python scripts/build-inline-viewer.py --check
```

## Lỗi thường gặp

### Chỉ còn border đầu tiên

Excel có thể lưu cạnh trái của range sau dưới dạng cạnh phải của ô đứng trước. Viewer `v2026.07.26-r80` trở lên sẽ mirror cạnh dùng chung trước khi dựng lại từng outline.

Nếu vẫn dùng Viewer cũ, nhấn `Ctrl+F5`, copy template sang một vùng Excel mới rồi thực hiện lại. Không dùng vùng Excel đã nhận border sai từ phiên bản cũ.

### Ô chứa placeholder có border riêng

Text và placeholder được lưu bằng range overlay, ví dụ `A1`, trong khi format có thể nằm ở `A1:T1`. Viewer mới không cho overlay text thu hẹp phạm vi border.

Nếu Excel đã nhận border riêng quanh `A1` từ phiên bản cũ, phải copy lại template sau khi reload Viewer.

### Chỉ nhận text, mất format

Clipboard không có `text/html` nên Viewer dùng TSV fallback. Panel sẽ báo rằng format, merge và border đã mất.

Thực hiện `Ctrl+C` trực tiếp trên vùng Excel. Không copy text từ formula bar hoặc từ ứng dụng trung gian.

### Có cảnh báo cạnh border rời

Schema template chỉ hỗ trợ outline khép kín. Các cạnh đơn lẻ không tạo thành hình chữ nhật sẽ bị bỏ qua và được ghi trong danh sách cảnh báo.

Trong Excel, chọn chính range đang import, ví dụ `A1:T1`, nhấn `Ctrl+1`, chọn **Border → Outline → OK**, rồi nhấn `Ctrl+C` lại.

### Paste vượt giới hạn

Hàm `parseClipboardPayload` từ chối bảng vượt một trong các giới hạn:

- 500 hàng.
- 200 cột.
- 20.000 ô.
- 2 MB clipboard HTML.
- 250 KB CSS hoặc 5.000 CSS rule.

Giảm vùng Excel xuống tối đa 500 hàng × 200 cột và không quá 20.000 ô trước khi nhấn `Ctrl+C`.

## Link nguồn

- [Mã nguồn Excel round-trip](../viewer/app/template/00-excel-roundtrip.js) — codec clipboard HTML/TSV, border reconstruction và giới hạn import.
- [Template Form](../viewer/app/template/01-path-resolver.js) — panel paste, preview, Replace và Apply.
- [Regression test](../tests/viewer-contracts.template.test.js) — các trường hợp placeholder, CSS Excel, merge và shared border edges.
- [GitHub repository](https://github.com/TimTini/Abap) — lịch sử commit và mã nguồn đã đồng bộ.
