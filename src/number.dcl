NMBRINS : dialog {
    label = "Insert";
    : spacer {
        height = 0;
    }
    : row {
        : popup_list {
            label = "&Name:";
            key = "name_list";
            edit_width = 30;
            list = "";
        }
        : button {
            label = "Browse...";
            key = "browse";
            fixed_width = true;
            width = 14;
        }
    } /*End row for getting insert name*/
    : row {
        : text {
            label = "Path:";
        }
        : text {
            key = "path";
            width = 45;
            value = "";
        }
    }
    : spacer {
        height = 0;
    }
    : boxed_column {
        label = "Scale";
        : toggle {
            label = "&Set to dimscale";
            key = "dimsc";
            fixed_width = true;
            width = 15;
        }
        : edit_box {
            label = "&Other:";
            key = "user_dimsc";
            edit_width = 10;
        }
        : spacer {
            height = 0;
        }
    } /*End boxed column for scale*/
    : spacer {
        height = 0;
    }
    ok_cancel_err;
}