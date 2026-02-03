% Licensed under the Creative Commons Attribution 4.0 International License (http://creativecommons.org/licenses/by/4.0/).

implement statusLine
    open vpiToolbar, resourceIdentifiers

clauses
    create(Parent) :-
        _ = vpiToolbar::create(style, Parent, controlList).

% This code is maintained automatically, do not update it manually. 22:14:02-24.4.2007
constants
    style : vpiToolbar::style = tb_bottom().
    controlList : vpiToolbar::control_list =
        [
            tb_text(idt_help_line, tb_context(), 200, 0, 4, 8, 0x0, ""),
            tb_text(idt_help_line_1, tb_static(), 49, 1, 4, 8, 0x0, "Stack:"),
            tb_text(idt_help_stack, tb_static(), 64, 0, 4, 8, 0x0, "Static text"),
            tb_text(idt_help_line_4, tb_static(), 60, 1, 4, 8, 0x0, "Heap:"),
            tb_text(idt_help_memory, tb_static(), 120, 0, 4, 8, 0x0, "Memory:"),
            tb_text(idt_help_pause, tb_static(), 50, 0, 4, 8, 0x80, ""),
            tb_text(idt_help_trace, tb_static(), 50, 0, 4, 8, 0x80, "")
        ].
    % end of automatic code

end implement statusLine