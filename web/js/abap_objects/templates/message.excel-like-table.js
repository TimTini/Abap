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
                A: 260,
                E: 360,
                I: 220,
                L: 420,
                P: 220,
                R: 260,
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
                colspan: 4,
            },
            {
                start: "E1",
                rowspan: 1,
                colspan: 4,
            },
            {
                start: "I1",
                rowspan: 1,
                colspan: 3,
            },
            {
                start: "L1",
                rowspan: 1,
                colspan: 4,
            },
            {
                start: "P1",
                rowspan: 1,
                colspan: 2,
            },
            {
                start: "R1",
                rowspan: 1,
                colspan: 3,
            },
            {
                start: "A2",
                rowspan: 1,
                colspan: 4,
            },
            {
                start: "E2",
                rowspan: 1,
                colspan: 16,
            },
            {
                start: "A3",
                rowspan: 1,
                colspan: 4,
            },
            {
                start: "E3",
                rowspan: 1,
                colspan: 16,
            },
            {
                start: "A4",
                rowspan: 1,
                colspan: 4,
            },
            {
                start: "E4",
                rowspan: 1,
                colspan: 16,
            },
            {
                start: "A5",
                rowspan: 1,
                colspan: 4,
            },
            {
                start: "E5",
                rowspan: 1,
                colspan: 16,
            },
            {
                start: "A6",
                rowspan: 1,
                colspan: 4,
            },
            {
                start: "E6",
                rowspan: 1,
                colspan: 16,
            },
            {
                start: "A7",
                rowspan: 1,
                colspan: 4,
            },
            {
                start: "E7",
                rowspan: 1,
                colspan: 16,
            },
            {
                start: "A8",
                rowspan: 1,
                colspan: 4,
            },
            {
                start: "E8",
                rowspan: 1,
                colspan: 16,
            },
        ],
        cells: [
            {
                addr: "A1",
                text: "{labels.msgClass}",
                class: ["cell", "header"],
            },
            {
                addr: "E1",
                text: "{msgClass.description}",
                class: ["cell", "wrap"],
            },
            {
                addr: "I1",
                text: "{labels.msgNo}",
                class: ["cell", "header"],
            },
            {
                addr: "L1",
                text: "{msgNo.description}",
                class: ["cell", "wrap"],
            },
            {
                addr: "P1",
                text: "{labels.displayLike}",
                class: ["cell", "header"],
            },
            {
                addr: "R1",
                text: "{displayLike.description}",
                class: ["cell", "wrap"],
            },
            {
                addr: "A2",
                text: "{labels.messageText}",
                class: ["cell", "header"],
            },
            {
                addr: "E2",
                text: "{messageText.description}",
                class: ["cell", "wrap"],
            },
            {
                addr: "A3",
                text: "{labels.with1}",
                class: ["cell", "header"],
            },
            {
                addr: "E3",
                text: "{with[0].description}",
                class: ["cell", "wrap"],
            },
            {
                addr: "A4",
                text: "{labels.with2}",
                class: ["cell", "header"],
            },
            {
                addr: "E4",
                text: "{with[1].description}",
                class: ["cell", "wrap"],
            },
            {
                addr: "A5",
                text: "{labels.with3}",
                class: ["cell", "header"],
            },
            {
                addr: "E5",
                text: "{with[2].description}",
                class: ["cell", "wrap"],
            },
            {
                addr: "A6",
                text: "{labels.with4}",
                class: ["cell", "header"],
            },
            {
                addr: "E6",
                text: "{with[3].description}",
                class: ["cell", "wrap"],
            },
            {
                addr: "A7",
                text: "{labels.into}",
                class: ["cell", "header"],
            },
            {
                addr: "E7",
                text: "{into.description}",
                class: ["cell", "wrap"],
            },
            {
                addr: "A8",
                text: "{labels.raising}",
                class: ["cell", "header"],
            },
            {
                addr: "E8",
                text: "{raising.description}",
                class: ["cell", "wrap"],
            },
        ],
    };

    ns.abapObjects?.defineTemplate?.("message.excel-like-table", messageExcelLikeTable);
})(window.AbapFlow);
