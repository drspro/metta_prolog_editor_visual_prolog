% Licensed under the Creative Commons Attribution 4.0 International License (http://creativecommons.org/licenses/by/4.0/).

implement aboutDialog inherits dialog
    open vpiDomains

clauses
    new(Parent) :-
        dialog::new(Parent),
        generatedInitialize().

% This code is maintained automatically, do not update it manually.
%  11:18:10-21.4.2025

facts
    idc_about_dialog_1 : groupBox.
    idct_web_httpwwwpdcdk : textControl.
    idct_internet_email : textControl.
    idct_hjholstvej : textControl.
    idc_dlg_about_st_prj : textControl.
    version_ctl : versioncontrol.
    version1_ctl : versioncontrol.
    version2_ctl : versioncontrol.
    version3_ctl : versioncontrol.

predicates
    generatedInitialize : ().
clauses
    generatedInitialize() :-
        setText("About"),
        setRect(rct(122, 26, 354, 170)),
        setModal(true),
        setDecoration(titlebar([closeButton])),
        setState([wsf_NoClipSiblings]),
        ButtonOk = button::newOk(This),
        ButtonOk:setText("&OK"),
        ButtonOk:setPosition(8, 116),
        ButtonOk:setWidth(56),
        ButtonOk:defaultHeight := true,
        idc_about_dialog_1 := groupBox::new(This),
        idc_about_dialog_1:setText(""),
        idc_about_dialog_1:setPosition(72, 7),
        idc_about_dialog_1:setSize(150, 130),
        version_ctl := versioncontrol::new(idc_about_dialog_1),
        version_ctl:setPosition(40, 18),
        version_ctl:setSize(100, 10),
        version_ctl:setText("File Description: {FileDescription}"),
        idc_dlg_about_st_prj := textControl::new(idc_about_dialog_1),
        idc_dlg_about_st_prj:setText("Metta interpreter"),
        idc_dlg_about_st_prj:setPosition(8, -2),
        idc_dlg_about_st_prj:setSize(130, 16),
        idc_dlg_about_st_prj:setAlignment(alignCenter),
        idct_web_httpwwwpdcdk := textControl::new(idc_about_dialog_1),
        idct_web_httpwwwpdcdk:setText("http://www.healthsoftware.nl"),
        idct_web_httpwwwpdcdk:setPosition(7, 97),
        idct_web_httpwwwpdcdk:setSize(130, 10),
        idct_web_httpwwwpdcdk:setAlignment(alignCenter),
        idct_internet_email := textControl::new(idc_about_dialog_1),
        idct_internet_email:setText("rvvessum@gmail.com"),
        idct_internet_email:setPosition(7, 87),
        idct_internet_email:setSize(130, 10),
        idct_internet_email:setAlignment(alignCenter),
        idct_hjholstvej := textControl::new(idc_about_dialog_1),
        idct_hjholstvej:setText("Biddinghuizen the Netherlands"),
        idct_hjholstvej:setPosition(7, 77),
        idct_hjholstvej:setSize(130, 10),
        idct_hjholstvej:setAlignment(alignCenter),
        version1_ctl := versioncontrol::new(idc_about_dialog_1),
        version1_ctl:setPosition(40, 30),
        version1_ctl:setSize(100, 10),
        version1_ctl:setText("File Version: {FileVersionA}.{FileVersionB}.{FileVersionC}.{FileVersionD}"),
        version2_ctl := versioncontrol::new(idc_about_dialog_1),
        version2_ctl:setPosition(39, 41),
        version2_ctl:setSize(100, 10),
        version2_ctl:setText("Copyright: {LegalCopyright}"),
        version3_ctl := versioncontrol::new(idc_about_dialog_1),
        version3_ctl:setPosition(40, 54),
        version3_ctl:setSize(100, 10),
        version3_ctl:setText("Company Name: Singularity.io"),
        IconProject = iconControl::new(This),
        IconProject:setIcon(application_icon),
        IconProject:setPosition(28, 44).
    % end of automatic code

end implement aboutDialog
