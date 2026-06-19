package com.logicaldoc.gui.frontend.client.whatsapp;

import java.util.ArrayList;
import java.util.List;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.types.AutoFitWidthApproach;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;

/**
 * This popup window is used to display the results of Whatsapp's templates
 * preparation
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.3
 */
public class TemplatesPreparationResults extends Window {

    private static final String DETAILS = "details";
    private static final String LANGUAGE = "language";
    private static final String TEMPLATE = "template";
    private static final String STATUS = "status";
    private List<TemplateResult> results;

    public TemplatesPreparationResults(List<TemplateResult> results) {
        super();
        this.results = results;
        setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
        setTitle(I18N.message("results"));
        setCanDragResize(true);
        setIsModal(true);
        setShowModalMask(true);
        centerInPage();
        setMinWidth(480);
        setMinHeight(350);
        setAutoSize(true);
    }

    @Override
    protected void onDraw() {
        ListGridField status = new ListGridField(STATUS, I18N.message(STATUS));
        status.setAutoFitWidth(true);
        status.setAutoFitWidthApproach(AutoFitWidthApproach.BOTH);
        status.setCellFormatter((value, gridRecord, rowNum, colNum) -> "<span style='color: "
                + ("success".equals(value.toString().toLowerCase()) ? "green" : "red") + "'>" + I18N.message(value.toString()) + "</span>");

        ListGridField template = new ListGridField(TEMPLATE, I18N.message(TEMPLATE));
        template.setAutoFitWidth(true);
        template.setAutoFitWidthApproach(AutoFitWidthApproach.BOTH);
        
        ListGridField language = new ListGridField(LANGUAGE, I18N.message(LANGUAGE));
        language.setAutoFitWidth(true);
        language.setAutoFitWidthApproach(AutoFitWidthApproach.BOTH);
        
        ListGridField details = new ListGridField(DETAILS, I18N.message(DETAILS));
        details.setWidth("*");
        
        ListGrid list = new ListGrid();
        list.setEmptyMessage(I18N.message("notitemstoshow"));
        list.setCanFreezeFields(true);
        list.setAutoFetchData(true);
        list.setWidth100();
        list.setHeight100();    
        list.setFields(status, template, language, details);

        List<ListGridRecord> records = new ArrayList<>();
        for (TemplateResult result : results) {
            ListGridRecord rec = new ListGridRecord();
            rec.setAttribute(STATUS, result.isSuccess() ? "success" : "failure");
            rec.setAttribute(TEMPLATE, result.getTemplate());
            rec.setAttribute(LANGUAGE, result.getLanguage());
            rec.setAttribute(DETAILS, result.getDetails());
            records.add(rec);
        }
        list.setData(records.toArray(new ListGridRecord[0]));
        
        addItem(list);
    }

    @Override
    public boolean equals(Object other) {
        return super.equals(other);
    }

    @Override
    public int hashCode() {
        return super.hashCode();
    }
}