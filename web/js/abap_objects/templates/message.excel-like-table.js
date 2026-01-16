(function (ns) {
    "use strict";

    const messageExcelLikeTable = {
        type: "excel-like-table",
        compact: {
            removeEmptyRows: true,
        },
        grid: {
            rows: 8,
            cols: 6,
            colWidths: {
                A: 260,
                B: 360,
                C: 220,
                D: 420,
                E: 220,
                F: 260,
            },
            rowHeights: {
                1: 30,
                2: 30,
                3: 30,
                4: 30,
                5: 30,
                6: 30,
                7: 30,
                8: 30,
            },
        },
        css: {
            header: "background:#9dc3e6;font-weight:700;color:#111;",
            cell: "border:1px solid #222;padding:6px 8px;vertical-align:middle;background:#fff;color:#111;",
            mono: "font-family:Consolas,monospace;",
            wrap: "white-space:normal;line-height:1.25;",
            center: "text-align:center;",
        },
        cells: [
            {
                addr: "A1",
                text: "{labels.msgClass}",
                class: ["cell", "header"],
            },
            {
                addr: "B1",
                text: "{msgClass.description}",
                class: ["cell", "wrap"],
            },
            {
                addr: "C1",
                text: "{labels.msgNo}",
                class: ["cell", "header"],
            },
            {
                addr: "D1",
                text: "{msgNo.description}",
                class: ["cell", "wrap"],
            },
            {
                addr: "E1",
                text: "{labels.displayLike}",
                class: ["cell", "header"],
            },
            {
                addr: "F1",
                text: "{displayLike.description}",
                class: ["cell", "wrap"],
            },
            {
                addr: "A2",
                text: "{labels.messageText}",
                class: ["cell", "header"],
            },
            {
                addr: "B2",
                text: "{messageText.description}",
                class: ["cell", "wrap"],
            },
            {
                addr: "A3",
                text: "{labels.with1}",
                class: ["cell", "header"],
            },
            {
                addr: "B3",
                text: "{with[0].description}",
                class: ["cell", "wrap"],
            },
            {
                addr: "A4",
                text: "{labels.with2}",
                class: ["cell", "header"],
            },
            {
                addr: "B4",
                text: "{with[1].description}",
                class: ["cell", "wrap"],
            },
            {
                addr: "A5",
                text: "{labels.with3}",
                class: ["cell", "header"],
            },
            {
                addr: "B5",
                text: "{with[2].description}",
                class: ["cell", "wrap"],
            },
            {
                addr: "A6",
                text: "{labels.with4}",
                class: ["cell", "header"],
            },
            {
                addr: "B6",
                text: "{with[3].description}",
                class: ["cell", "wrap"],
            },
            {
                addr: "A7",
                text: "{labels.into}",
                class: ["cell", "header"],
            },
            {
                addr: "B7",
                text: "{into.description}",
                class: ["cell", "wrap"],
            },
            {
                addr: "A8",
                text: "{labels.raising}",
                class: ["cell", "header"],
            },
            {
                addr: "B8",
                text: "{raising.description}",
                class: ["cell", "wrap"],
            },
        ],
    };

    ns.abapObjects?.defineTemplate?.("message.excel-like-table", messageExcelLikeTable);
})(window.AbapFlow);
