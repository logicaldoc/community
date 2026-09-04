package com.logicaldoc.gui.frontend.client.document.summary;

import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedHashMap;
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
import com.logicaldoc.gui.frontend.client.ai.model.GUIModel;
import com.logicaldoc.gui.frontend.client.services.ChatGPTService;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.HTMLFlow;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.fields.RadioGroupItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
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

        ToolStripButton generate = new ToolStripButton();
        generate.setTitle(I18N.message("generate"));
        generate.addClickHandler(event -> onGenerate());

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
        modelSelector.setRequired(false);

        chatGPTModel = ItemFactory.newTextItem("model", "model");
        chatGPTModel.setVisible(false);

        ChatGPTService.Instance.get().loadSettings(new DefaultAsyncCallback<>() {

            @Override
            public void handleSuccess(List<GUIValue> settings) {

                String model = GUIValue.getValue("model", settings);

                if (model != null && !model.trim().isEmpty())
                    chatGPTModel.setValue(model);
            }
        });

        // Load all available summarizer models
        AIService.Instance.get().getModels(new DefaultAsyncCallback<>() {

            @Override
            public void handleSuccess(List<GUIModel> models) {

                LinkedHashMap<String, String> values = new LinkedHashMap<>();

                for (GUIModel model : models) {
                    if ("summarizer".equals(model.getType()))
                        values.put(Long.toString(model.getId()), model.getName());
                }

                modelSelector.setValueMap(values);

                if (!values.isEmpty())
                    modelSelector.setValue(values.keySet().iterator().next());
            }
        });

        engine.addChangedHandler(event -> {

            boolean logicalDoc = "logicaldoc".equals(event.getValue());

            modelSelector.setVisible(logicalDoc);
            chatGPTModel.setVisible(!logicalDoc);

            toolStrip.markForRedraw();
        });

        toolStrip.addFormItem(engine);
        toolStrip.addFormItem(modelSelector);
        toolStrip.addFormItem(chatGPTModel);
        toolStrip.addSeparator();
        toolStrip.addButton(generate);
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

    private void onGenerate() {

        Long modelId = null;
        String modelSpec = null;

        if ("logicaldoc".equals(engine.getValueAsString())) {

            if (modelSelector.getValue() != null)
                modelId = Long.valueOf(modelSelector.getValueAsString());
        } else
            modelSpec = chatGPTModel.getValueAsString();

        LD.contactingServer();

        AIService.Instance.get().summarize(document.getId(), document.getFileVersion(), modelId, modelSpec,
                new DefaultAsyncCallback<>() {

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