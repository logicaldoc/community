package com.logicaldoc.gui.frontend.client.document.summary;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.automation.HtmlItemEditor;
import com.logicaldoc.gui.common.client.beans.GUIAccessControlEntry;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIValue;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.frontend.client.ai.AIService;
import com.logicaldoc.gui.frontend.client.ai.model.ModelsDS;
import com.logicaldoc.gui.frontend.client.services.ChatGPTService;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.HTMLFlow;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.fields.DoubleItem;
import com.smartgwt.client.widgets.form.fields.RadioGroupItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This pop-up window is used to display, generate and edit a document's summary
 * 
 * @author Giuseppe Desiato - LogicalDOC
 * @since 9.3.1
 */
public class SummaryDialog extends Window {

    private RadioGroupItem engine;

    private SelectItem modelSelector;

    private TextItem chatGPTModel;

    private SpinnerItem sentencesItem;

    private DoubleItem mmrlambdaItem;

    private GUIDocument document;

    private String fileVersion;

    private HTMLFlow html;

    private String summary;

    private GUIAccessControlEntry ace;

    private final boolean readOnly;

    public SummaryDialog(GUIDocument document) {
        this(document, null);
    }

    public SummaryDialog(GUIDocument document, String fileVersion) {

        this.document = document;
        this.fileVersion = fileVersion;

        this.readOnly = fileVersion != null && !fileVersion.equals(document.getFileVersion());

        setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);

        setTitle(I18N.message("summary"));

        setWidth100();
        setHeight100();
        setCanDragResize(true);
        setIsModal(true);
        setShowModalMask(true);
        centerInPage();
    }

    @Override
    protected void onDraw() {
        LD.contactingServer();
        DocumentService.Instance.get().getAllowedPermissions(Arrays.asList(document.getId()),
                new DefaultAsyncCallback<>() {

                    @Override
                    public void handleSuccess(GUIAccessControlEntry ace) {

                        SummaryDialog.this.ace = ace;

                        DocumentService.Instance.get().getSummary(document.getId(),
                                fileVersion != null ? fileVersion : document.getFileVersion(),
                                new DefaultAsyncCallback<>() {

                                    @Override
                                    public void handleSuccess(String result) {
                                        summary = result;
                                        if (summary == null || summary.trim().isEmpty())
                                            summary = "";
                                        initGUI();
                                    }
                                });
                    }
                });
    }

    public void onConfirm() {
        DocumentService.Instance.get().saveSummary(document.getId(),
                fileVersion != null ? fileVersion : document.getFileVersion(), summary, new DefaultAsyncCallback<>() {

                    @Override
                    public void handleSuccess(Void result) {
                        destroy();
                    }
                });
    }

    private void initGUI() {
        ToolStrip toolStrip = new ToolStrip();
        toolStrip.setHeight(20);
        toolStrip.setWidth100();
        toolStrip.addSpacer(2);

        ToolStripButton summarize = new ToolStripButton();
        summarize.setTitle(I18N.message("summarize"));
        summarize.addClickHandler(event -> onSummarize());

        ToolStripButton edit = new ToolStripButton();
        edit.setTitle(I18N.message("edit"));
        edit.addClickHandler(event -> onEdit());

        ToolStripButton save = new ToolStripButton();
        save.setTitle(I18N.message("save"));
        save.addClickHandler(event -> onConfirm());

        ToolStripButton close = new ToolStripButton();
        close.setTitle(I18N.message("close"));
        close.addClickHandler(event -> destroy());

        engine = ItemFactory.newRadioGroup("engine", "engine");

        HashMap<String, String> engineOptions = new HashMap<>();
        engineOptions.put("logicaldoc", "logicaldoc");
        engineOptions.put("chatgpt", "chatgpt");

        engine.setValueMap(engineOptions);
        engine.setValue("logicaldoc");
        engine.setWrap(false);

        modelSelector = ItemFactory.newSelectItem("model");
        modelSelector.setValueField("id");
        modelSelector.setDisplayField("name");
        modelSelector.setOptionDataSource(new ModelsDS("summarizer"));
        modelSelector.addDataArrivedHandler(dataArrived -> modelSelector
                .setValue(dataArrived.getData().get(dataArrived.getStartRow()).getAttributeAsString("id")));

        sentencesItem = ItemFactory.newSpinnerItem("sentences", 5);
        sentencesItem.setMin(5);
        sentencesItem.setStep(5);
        sentencesItem.setWrapTitle(false);

        mmrlambdaItem = ItemFactory.newDoubleItem("mmrlambda", 0.7);
        mmrlambdaItem.setWrapTitle(false);

        chatGPTModel = ItemFactory.newTextItem("model", "model");
        chatGPTModel.setVisible(false);

        ChatGPTService.Instance.get().loadSettings(new DefaultAsyncCallback<>() {

            @Override
            public void handleSuccess(List<GUIValue> settings) {
                String model = GUIValue.getValue("model", settings);
                chatGPTModel.setValue(model != null ? model : "gpt-4o");
            }
        });

        engine.addChangedHandler(changed -> {

            boolean logicaldocEngineSelected = "logicaldoc".equals(changed.getValue());

            modelSelector.setVisible(logicaldocEngineSelected);
            modelSelector.setRequired(logicaldocEngineSelected);
            sentencesItem.setVisible(logicaldocEngineSelected);
            mmrlambdaItem.setVisible(logicaldocEngineSelected);
            chatGPTModel.setVisible(!logicaldocEngineSelected);

            toolStrip.markForRedraw();
        });

        toolStrip.addFormItem(engine);
        toolStrip.addFormItem(modelSelector);
        toolStrip.addFormItem(chatGPTModel);
        toolStrip.addFormItem(sentencesItem);
        toolStrip.addFormItem(mmrlambdaItem);
        toolStrip.addSeparator();
        toolStrip.addButton(summarize);
        toolStrip.addButton(edit);
        toolStrip.addButton(save);
        toolStrip.addButton(close);

        html = new HTMLFlow();
        html.setContents(summary);
        html.setWidth100();
        html.setHeight100();

        if (ace.isWrite() && !readOnly)
            addItem(toolStrip);
        addItem(html);
    }

    private void onSummarize() {
        Integer sentences = sentencesItem.getValueAsInteger();
        Double lambda = mmrlambdaItem.getValueAsDouble();

        Long modelId = null;
        String modelSpec = null;
        if ("logicaldoc".equals(engine.getValueAsString())) {
            if (modelSelector.getValue() != null) {
                modelId = Long.valueOf(modelSelector.getValueAsString());
            } else {
                SC.warn(I18N.message("selectamodel"));
                return;
            }

        } else
            modelSpec = chatGPTModel.getValueAsString();

        LD.contactingServer();
        AIService.Instance.get().summarize(document.getId(), document.getFileVersion(), modelId, sentences, lambda,
                modelSpec, new DefaultAsyncCallback<>() {

                    @Override
                    public void handleSuccess(String result) {
                        summary = result != null ? result : "";
                        html.setContents(summary);
                    }
                });
    }

    private void onEdit() {
        new HtmlItemEditor(html, event -> {
            summary = html.getContents();
        }).show();
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