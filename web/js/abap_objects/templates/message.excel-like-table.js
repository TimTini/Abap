(function (ns) {
    "use strict";

    const messageExcelLikeTable = {
        type: "excel-like-table",
        compact: {
            removeEmptyRows: true,
        },
        grid: {
            rows: 8,
            cols: 20,
            colWidths: {
                A: 420,
                G: 340,
                K: 240,
                Q: 640,
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
        merges: [
            {
                start: "A1",
                rowspan: 1,
                colspan: 6,
            },
            {
                start: "G1",
                rowspan: 1,
                colspan: 4,
            },
            {
                start: "K1",
                rowspan: 1,
                colspan: 6,
            },
            {
                start: "Q1",
                rowspan: 1,
                colspan: 4,
            },
            {
                start: "A2",
                rowspan: 1,
                colspan: 6,
            },
            {
                start: "G2",
                rowspan: 1,
                colspan: 4,
            },
            {
                start: "K2",
                rowspan: 1,
                colspan: 6,
            },
            {
                start: "Q2",
                rowspan: 1,
                colspan: 4,
            },
            {
                start: "A3",
                rowspan: 1,
                colspan: 6,
            },
            {
                start: "G3",
                rowspan: 1,
                colspan: 14,
            },
            {
                start: "A4",
                rowspan: 1,
                colspan: 6,
            },
            {
                start: "G4",
                rowspan: 1,
                colspan: 14,
            },
            {
                start: "A5",
                rowspan: 1,
                colspan: 6,
            },
            {
                start: "G5",
                rowspan: 1,
                colspan: 14,
            },
            {
                start: "A6",
                rowspan: 1,
                colspan: 6,
            },
            {
                start: "G6",
                rowspan: 1,
                colspan: 14,
            },
            {
                start: "A7",
                rowspan: 1,
                colspan: 6,
            },
            {
                start: "G7",
                rowspan: 1,
                colspan: 14,
            },
            {
                start: "A8",
                rowspan: 1,
                colspan: 6,
            },
            {
                start: "G8",
                rowspan: 1,
                colspan: 14,
            },
        ],
        cells: [
            {
                addr: "A1",
                text: "{labels.msgClass}",
                class: ["cell", "header"],
            },
            {
                addr: "G1",
                text: "{msgClass.description}",
                class: ["cell", "wrap"],
            },
            {
                addr: "K1",
                text: "{labels.msgNo}",
                class: ["cell", "header"],
            },
            {
                addr: "Q1",
                text: "{msgNo.description}",
                class: ["cell", "wrap"],
            },
            {
                addr: "A2",
                text: "{labels.displayLike}",
                class: ["cell", "header"],
            },
            {
                addr: "G2",
                text: "{displayLike.description}",
                class: ["cell", "wrap"],
            },
            {
                addr: "K2",
                text: "{labels.messageText}",
                class: ["cell", "header"],
            },
            {
                addr: "Q2",
                text: "{messageText.description}",
                class: ["cell", "wrap"],
            },
            {
                addr: "A3",
                text: "{labels.with1}",
                class: ["cell", "header"],
            },
            {
                addr: "G3",
                text: "{with[0].description}",
                class: ["cell", "wrap"],
            },
            {
                addr: "A4",
                text: "{labels.with2}",
                class: ["cell", "header"],
            },
            {
                addr: "G4",
                text: "{with[1].description}",
                class: ["cell", "wrap"],
            },
            {
                addr: "A5",
                text: "{labels.with3}",
                class: ["cell", "header"],
            },
            {
                addr: "G5",
                text: "{with[2].description}",
                class: ["cell", "wrap"],
            },
            {
                addr: "A6",
                text: "{labels.with4}",
                class: ["cell", "header"],
            },
            {
                addr: "G6",
                text: "{with[3].description}",
                class: ["cell", "wrap"],
            },
            {
                addr: "A7",
                text: "{labels.into}",
                class: ["cell", "header"],
            },
            {
                addr: "G7",
                text: "{into.description}",
                class: ["cell", "wrap"],
            },
            {
                addr: "A8",
                text: "{labels.raising}",
                class: ["cell", "header"],
            },
            {
                addr: "G8",
                text: "{raising.description}",
                class: ["cell", "wrap"],
            },
        ],
    };

    ns.abapObjects?.defineTemplate?.("message.excel-like-table", messageExcelLikeTable);
})(window.AbapFlow);
