package com.logicaldoc.gui.frontend.client.ai.model;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.EmptyAsyncCallback;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.Upload;
import com.logicaldoc.gui.frontend.client.ai.AIService;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.data.RecordList;
import com.smartgwt.client.types.AutoFitWidthApproach;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.FloatItem;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This popup window is used to input data and obtain a prediction from the AI
 * model.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
public class QueryDialog extends Window {

    private static final String PAYLOAD = "payload";

    private static final String NAME = "name";

    private static final String VALUE = "value";

    private static final String SCORE = "score";

    private DynamicForm form = new DynamicForm();

    private GUIModel model;

    private ListGrid resultGrid;

    private ToolStripButton queryButton;

    public QueryDialog(GUIModel model) {
        this.model = model;

        setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);

        setTitle(I18N.message("querymodel2", model.getLabel()));
        setWidth(500);
        setHeight(350);
        setCanDragResize(true);
        setIsModal(true);
        setShowModalMask(true);
        centerInPage();

        DocumentService.Instance.get().cleanUploadedFileFolder(new EmptyAsyncCallback<>());

        List<FormItem> items = new ArrayList<>();
        for (GUIFeatureDescriptor feature : model.getFeatureDescriptors()) {
            if (feature.getType().toLowerCase().contains("string")) {
                TextItem item = ItemFactory.newTextItem(feature.getName(), null);
                item.setRequired(true);
                items.add(item);
            } else if (feature.getType().toLowerCase().contains("float")) {
                FloatItem item = ItemFactory.newFloatItem(feature.getName(), null);
                item.setRequired(true);
                items.add(item);
            }
        }

        form.setNumCols(2);
        form.setTitleOrientation(TitleOrientation.TOP);
        form.setFields(items.toArray(new FormItem[0]));
        form.setShowResizeBar(true);

        VLayout upload = new VLayout();
        for (GUIFeatureDescriptor feature : model.getFeatureDescriptors().stream()
                .filter(fd -> fd.getType().toLowerCase().contains("image")).collect(Collectors.toList())) {
            HLayout row = new HLayout();
            row.setHeight(50);
            row.addMember(new Label(feature.getName() + ":"));
            row.addMember(new Upload());
            upload.addMember(row);
        }

        queryButton = new ToolStripButton(I18N.message("querymodel"));
        queryButton.setAutoFit(true);
        queryButton.addClickHandler(click -> onQuery());

        ToolStrip buttons = new ToolStrip();
        buttons.setWidth100();
        buttons.addButton(queryButton);
        buttons.addFill();

        prepareResultGrid();

        VLayout content = new VLayout();
        content.setWidth100();
        if (items.isEmpty())
            content.setMembers(upload, buttons, resultGrid);
        else
            content.setMembers(form, upload, buttons, resultGrid);

        addItem(content);
    }

    private void prepareResultGrid() {
        resultGrid = new ListGrid();
        resultGrid.setEmptyMessage(I18N.message("noresults"));
        resultGrid.setShowAllRecords(true);
        resultGrid.setAutoFetchData(true);
        resultGrid.setWidth100();
        resultGrid.setHeight100();
        resultGrid.setSelectionType(SelectionStyle.SINGLE);
        resultGrid.setShowRecordComponents(true);
        resultGrid.setShowRecordComponentsByCell(true);
        resultGrid.setCanFreezeFields(true);
        resultGrid.setFilterOnKeypress(true);

        ListGridField name = new ListGridField(NAME, I18N.message(NAME));
        name.setAutoFitWidth(true);
        name.setAutoFitWidthApproach(AutoFitWidthApproach.BOTH);

        ListGridField score = new ListGridField(SCORE, I18N.message(SCORE));
        score.setAutoFitWidth(true);
        score.setAutoFitWidthApproach(AutoFitWidthApproach.TITLE);

        ListGridField value = new ListGridField(VALUE, I18N.message(VALUE));
        value.setWidth(150);

        ListGridField payload = new ListGridField(PAYLOAD, I18N.message(PAYLOAD));
        payload.setWidth("*");

        resultGrid.setFields(name, value, score, payload);
    }

    private void onQuery() {
        queryButton.setDisabled(true);
        LD.contactingServer();
        AIService.Instance.get().query(model.getId(),
                model.getFeatureDescriptors().stream()
                        .filter(f -> f.getType().toLowerCase().contains("string")
                                || f.getType().toLowerCase().contains("float"))
                        .map(f -> form.getValueAsString(f.getName())).collect(Collectors.toList()),
                new DefaultAsyncCallback<>() {

                    @Override
                    public void onFailure(Throwable caught) {
                        super.onFailure(caught);
                        queryButton.setDisabled(false);
                    }

                    @Override
                    public void handleSuccess(List<GUIQueryResult> answers) {
                        resultGrid.setData(new RecordList(answers.stream().map(answer -> {
                            ListGridRecord rec = new ListGridRecord();
                            rec.setAttribute(NAME, answer.getName());
                            rec.setAttribute(SCORE, answer.getScore());
                            rec.setAttribute(VALUE, answer.getValue());
                            if (answer.getPayload() != null && !answer.getPayload().isEmpty())
                                rec.setAttribute(PAYLOAD, "<img src='" + answer.getPayload() + "' />");
                            return rec;
                        }).collect(Collectors.toList())));

                        queryButton.setDisabled(false);
                    }

                });
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