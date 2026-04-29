package com.logicaldoc.gui.frontend.client.ai.autofill;

import java.util.LinkedHashMap;

import com.logicaldoc.gui.common.client.grid.IdListGridField;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.ai.embedding.EmbeddingSchemesDS;
import com.logicaldoc.gui.frontend.client.ai.model.ModelsDS;
import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.data.Criterion;
import com.smartgwt.client.types.AutoFitWidthApproach;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.types.OperatorId;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.SectionStack;
import com.smartgwt.client.widgets.layout.SectionStackSection;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;

/**
 * Shows filler's standard properties and read-only data
 * 
 * @author Giuseppe Desiato - LogicalDOC
 * @since 9.2.3
 */
public class FillerProperties extends FillerDetailsTab {

    private static final String METRIC = "metric";

    private static final String OPERAND = "operand";

    private static final String OPERATOR = "operator";

    private static final String ATTRIBUTE = "attribute";

    private static final String CANDIDATE = "candidate";

    private static final String TEMPLATE = "template";

    private static final String TAG = "tag";

    private static final String RETRIEVAL = "retrieval";

    private static final String LANGUAGE = "language";

    private static final String MODEL = "model";

    private static final String ID = "id";

    private static final String NAME = "name";

    private static final String LABEL = "label";

    private static final String DESCRIPTION = "description";

    private static final String TYPE = "type";

    private static final String MODEL_ID = "modelId";

    private static final String THRESHOLD = "threshold";

    private static final String STRATEGY = "strategy";

    private static final String EMBEDDING_SCHEME = "embeddingscheme";

    private static final String CHAIN = "chain";

    private static final String OVERWRITE = "overwrite";

    private static final String FILL_ON_CHECKIN = "filloncheckin";

    private DynamicForm form = new DynamicForm();

    private HLayout container = new HLayout();

    private ListGrid chainGrid;

    private ListGrid criteriaGrid;

    private SectionStack chainStack = new SectionStack();

    private SectionStack criteriaStack = new SectionStack();

    public FillerProperties(GUIFiller filler, ChangedHandler changedHandler) {
        super(filler, changedHandler);
        setWidth100();
        setHeight100();

        setMembers(container);

        refresh();
    }

    private void refresh() {
        form.clearValues();
        form.clearErrors(false);
        form.destroy();

        if (Boolean.TRUE.equals(container.contains(form)))
            container.removeChild(form);

        form = new DynamicForm();
        form.setNumCols(4);
        form.setTitleOrientation(TitleOrientation.TOP);

        // ID
        StaticTextItem id = ItemFactory.newStaticTextItem(ID, Long.toString(filler.getId()));
        id.setVisible(filler.getId() != 0L);

        // Name
        TextItem name = ItemFactory.newSimpleTextItem(NAME, filler.getName());
        name.setRequired(true);
        name.addChangedHandler(changedHandler);

        // Label
        TextItem label = ItemFactory.newTextItem(LABEL, filler.getLabel());
        label.addChangedHandler(changedHandler);

        // Overwrite flag
        CheckboxItem overwrite = new CheckboxItem(OVERWRITE, I18N.message("overwrite"));
        overwrite.setValue(filler.isOverwrite());
        overwrite.addChangedHandler(changedHandler);

        AdvancedCriteria overwriteVisible = new AdvancedCriteria(TYPE, OperatorId.NOT_EQUAL, CHAIN);
        overwrite.setVisibleWhen(overwriteVisible);

        // On check-in flag
        CheckboxItem onCheckin = new CheckboxItem(FILL_ON_CHECKIN, I18N.message("filloncheckin"));
        onCheckin.setValue(filler.isOnCheckin());
        onCheckin.addChangedHandler(changedHandler);

        // Description
        TextAreaItem description = ItemFactory.newTextAreaItem(DESCRIPTION, filler.getDescription());
        description.addChangedHandler(changedHandler);
        description.setColSpan(4);
        description.setWidth(400);

        // Model selector
        SelectItem modelSelector = ItemFactory.newSelectItem(MODEL_ID, MODEL);
        modelSelector.setValueField("id");
        modelSelector.setDisplayField("name");
        modelSelector.setRequired(true);
        modelSelector.addChangedHandler(changedHandler);

        // Type selector
        SelectItem type = ItemFactory.newSelectItem(TYPE);
        type.setOptionDataSource(new FillerTypesDS());
        type.setValueField("id");
        type.setDisplayField(LABEL);
        type.setRequired(true);
        type.setValue(filler.getType());
        type.addChangedHandler(changedHandler);
        type.addChangedHandler(event -> {
            String selectedType = event.getValue().toString();
            modelSelector.clearValue();
            modelSelector.setOptionDataSource(new ModelsDS(modelTypesSuitableForFiller(selectedType)));
            modelSelector.fetchData();

            chainStack.setVisible(CHAIN.equals(selectedType));
            criteriaStack.setVisible(ATTRIBUTE.equals(selectedType));
        });

        if (filler.getType() != null) {
            modelSelector.setOptionDataSource(new ModelsDS(modelTypesSuitableForFiller(filler.getType())));
            if (filler.getModelId() != null)
                modelSelector.setValue(filler.getModelId());
        }

        // Strategy selector
        SelectItem strategy = ItemFactory.newSelectItem(STRATEGY);
        LinkedHashMap<String, String> strategyMap = new LinkedHashMap<>();
        strategyMap.put(MODEL, I18N.message("strategy.model"));
        strategyMap.put(RETRIEVAL, I18N.message("strategy.retrieval"));
        strategy.setValueMap(strategyMap);
        strategy.setRequired(true);

        if (TAG.equals(filler.getType())) {
            if (filler.getModelId() == null) {
                strategy.setValue(RETRIEVAL);
            } else {
                strategy.setValue(MODEL);
            }
        }

        // EmbeddingScheme selector
        SelectItem embeddingSelector = ItemFactory.newSelectItem(EMBEDDING_SCHEME);
        embeddingSelector.setValueField("id");
        embeddingSelector.setDisplayField("name");
        embeddingSelector.setOptionDataSource(new EmbeddingSchemesDS());
        embeddingSelector.addChangedHandler(changedHandler);
        if (filler.getEmbeddingSchemeId() != null)
            embeddingSelector.setValue(filler.getEmbeddingSchemeId());

        // A threshold used by AI models (UI as percentage 0–100)
        SpinnerItem threshold = new SpinnerItem(THRESHOLD);
        threshold.setTitle(I18N.message("threshold"));
        threshold.setMin(0);
        threshold.setMax(100);
        threshold.setStep(1);
        threshold.setRequired(true);
        threshold.addChangedHandler(changedHandler);

        // Set initial value
        if (filler.getThreshold() != null) {
            int percent = (int) Math.round(filler.getThreshold() * 100);
            threshold.setValue(percent);
        }

        // Name of candidate label
        TextItem candidate = ItemFactory.newTextItem(CANDIDATE, "candidatelabel", filler.getCandidate());
        candidate.addChangedHandler(changedHandler);

        // Name of attribute to fill
        SelectItem attribute = ItemFactory.newAttributeSelector();
        attribute.setValue(filler.getAttribute());
        attribute.addChangedHandler(changedHandler);

        // Format to treat the label's content
        TextItem format = ItemFactory.newTextItem("format", filler.getFormat());
        format.addChangedHandler(changedHandler);

        TextItem decimalSep = ItemFactory.newTextItem("decimalseparator", filler.getDecimalSeparator());
        decimalSep.setWidth(80);
        decimalSep.addChangedHandler(changedHandler);

        TextItem groupingSep = ItemFactory.newTextItem("groupingseparator", filler.getGroupingSeparator());
        groupingSep.setWidth(80);
        groupingSep.addChangedHandler(changedHandler);

        // Criteria
        AdvancedCriteria thresholdCriteria = new AdvancedCriteria(OperatorId.OR, new Criterion[] {
                new Criterion(TYPE, OperatorId.EQUALS, TAG), new Criterion(TYPE, OperatorId.EQUALS, TEMPLATE) });

        threshold.setVisibleWhen(thresholdCriteria);
        threshold.setRequiredWhen(thresholdCriteria);

        AdvancedCriteria modelVisible = new AdvancedCriteria(OperatorId.OR,
                new Criterion[] { new Criterion(TYPE, OperatorId.EQUALS, LANGUAGE),
                        new Criterion(TYPE, OperatorId.EQUALS, ATTRIBUTE),
                        new AdvancedCriteria(OperatorId.AND,
                                new Criterion[] { new Criterion(TYPE, OperatorId.EQUALS, TAG),
                                        new Criterion(STRATEGY, OperatorId.EQUALS, MODEL) }) });

        modelSelector.setVisibleWhen(modelVisible);
        modelSelector.setRequiredWhen(modelVisible);

        AdvancedCriteria strategyVisible = new AdvancedCriteria(TYPE, OperatorId.EQUALS, TAG);

        strategy.setVisibleWhen(strategyVisible);
        strategy.setRequiredWhen(strategyVisible);

        AdvancedCriteria embeddingVisible = new AdvancedCriteria(OperatorId.OR,
                new Criterion[] {
                        new AdvancedCriteria(OperatorId.AND,
                                new Criterion[] { new Criterion(TYPE, OperatorId.EQUALS, TAG),
                                        new Criterion(STRATEGY, OperatorId.EQUALS, RETRIEVAL) }),
                        new Criterion(TYPE, OperatorId.EQUALS, TEMPLATE) });

        embeddingSelector.setVisibleWhen(embeddingVisible);
        embeddingSelector.setRequiredWhen(embeddingVisible);

        AdvancedCriteria attributeSelected = new AdvancedCriteria(TYPE, OperatorId.EQUALS, ATTRIBUTE);
        candidate.setVisibleWhen(attributeSelected);
        candidate.setRequiredWhen(attributeSelected);
        attribute.setVisibleWhen(attributeSelected);
        attribute.setRequiredWhen(attributeSelected);
        format.setVisibleWhen(attributeSelected);
        decimalSep.setVisibleWhen(attributeSelected);
        groupingSep.setVisibleWhen(attributeSelected);

        form.setItems(id, type, strategy, name, label, overwrite, onCheckin, modelSelector, embeddingSelector,
                threshold, candidate, attribute, format, decimalSep, groupingSep, description);

        container.addMember(form);

        prepareChain();

        prepareCriteria();
    }

    private String modelTypesSuitableForFiller(String fillerType) {
        return switch (fillerType) {
            case TAG -> "zeroshot,classifier";
            case LANGUAGE -> LANGUAGE;
            default -> null;
        };
    }

    public boolean validate() {
        if (form.validate()) {
            filler.setName(form.getValueAsString(NAME));
            filler.setLabel(form.getValueAsString(LABEL));
            filler.setDescription(form.getValueAsString(DESCRIPTION));
            filler.setType(form.getValueAsString(TYPE));
            filler.setCandidate(form.getValueAsString(CANDIDATE));
            filler.setAttribute(form.getValueAsString("attribute"));
            filler.setFormat(form.getValueAsString("format"));
            filler.setDecimalSeparator(form.getValueAsString("decimalseparator"));
            filler.setGroupingSeparator(form.getValueAsString("groupingseparator"));

            Boolean overwriteVal = (Boolean) form.getValue(OVERWRITE);
            filler.setOverwrite(Boolean.TRUE.equals(overwriteVal));

            Boolean onCheckinVal = (Boolean) form.getValue(FILL_ON_CHECKIN);
            filler.setOnCheckin(Boolean.TRUE.equals(onCheckinVal));

            validateAI();

            // Chain handling
            if (CHAIN.equals(filler.getType())) {
                if (chainGrid == null || chainGrid.getRecordList().isEmpty()) {
                    GuiLog.error("fillerchainempty");
                    return false;
                }

                filler.getChain().clear();

                com.smartgwt.client.data.Record[] chainRecords = chainGrid.getRecordList().toArray();
                for (com.smartgwt.client.data.Record chainRecord : chainRecords) {
                    Long id = chainRecord.getAttributeAsLong(ID);
                    String name = chainRecord.getAttribute(NAME);
                    filler.getChain().add(new GUIFiller(id, name));
                }
            }

            // Criteria handling
            if (ATTRIBUTE.equals(filler.getType())) {
                filler.getCriteria().clear();

                com.smartgwt.client.data.Record[] criteriaRecords = criteriaGrid.getRecordList().toArray();
                for (com.smartgwt.client.data.Record criterionRecord : criteriaRecords)
                    filler.getCriteria().add(new GUICriterion(criterionRecord.getAttribute("operator"),
                            criterionRecord.getAttribute(OPERAND), criterionRecord.getAttributeAsDouble(METRIC)));
            }
        }

        return !form.hasErrors();
    }

    private void validateAI() {
        String strategyValue = form.getValueAsString(STRATEGY);
        String modelId = form.getValueAsString(MODEL_ID);
        String embeddingId = form.getValueAsString(EMBEDDING_SCHEME);

        Integer thresholdValue = (Integer) form.getValue(THRESHOLD);
        if (thresholdValue != null) {
            filler.setThreshold(thresholdValue / 100d);
        } else {
            filler.setThreshold(null);
        }

        if (filler.getType().equals(TEMPLATE)) {
            filler.setModelId(null);
            filler.setEmbeddingSchemeId(Long.parseLong(embeddingId));
        } else {
            if (RETRIEVAL.equals(strategyValue)) {
                // Retrieval strategy
                filler.setModelId(null);
                filler.setEmbeddingSchemeId(embeddingId != null ? Long.parseLong(embeddingId) : null);

            } else {
                // Model strategy
                filler.setEmbeddingSchemeId(null);
                filler.setModelId(modelId != null ? Long.parseLong(modelId) : null);
            }
        }
    }

    private void prepareCriteria() {
        criteriaGrid = new ListGrid();
        criteriaGrid.setEmptyMessage(I18N.message("notitemstoshow"));
        criteriaGrid.setWidth100();
        criteriaGrid.setHeight100();
        criteriaGrid.setEmptyMessage(I18N.message("norecords"));
        criteriaGrid.setCanSort(true);
        criteriaGrid.setCanFreezeFields(false);
        criteriaGrid.setCanGroupBy(false);
        criteriaGrid.setLeaveScrollbarGap(false);
        criteriaGrid.setShowHeader(true);
        criteriaGrid.setSelectionType(SelectionStyle.MULTIPLE);
        criteriaGrid.setCanEdit(false);
        criteriaGrid.setShowRowNumbers(true);
        criteriaGrid.setCanReorderRecords(true);
        criteriaGrid.setAutoFetchData(true);
        criteriaGrid.setShowRecordComponents(true);
        criteriaGrid.setShowRecordComponentsByCell(true);
        criteriaGrid.addDropCompleteHandler(dropCompleted -> changedHandler.onChanged(null));
        criteriaGrid.addCellContextClickHandler(event -> {
            showChainContextMenu();
            event.cancel();
        });

        SelectItem operatorSelector = ItemFactory.newSelectItem(OPERATOR);
        operatorSelector.setValueMap("INCLUDED", "LEFT", "RIGHT", "ABOVE", "BELOW", "LEFTMOST", "RIGHTMOST", "TOPMOST",
                "BOTTOMMOST", "MAXDIST");

        ListGridField operator = new ListGridField(OPERATOR, I18N.message(OPERATOR));
        operator.setEditorProperties(operatorSelector);
        operator.setCanEdit(true);
        operator.setAutoFitWidth(true);
        operator.setAutoFitWidthApproach(AutoFitWidthApproach.BOTH);
    
        ListGridField operand = new ListGridField(OPERAND, I18N.message(OPERAND));
        operand.setCanEdit(true);
        operand.setAutoFitWidth(true);
        operand.setAutoFitWidthApproach(AutoFitWidthApproach.BOTH);

        ListGridField metric = new ListGridField(METRIC, I18N.message(METRIC));
        metric.setType(ListGridFieldType.FLOAT);
        metric.setCanEdit(true);
        metric.setAutoFitWidth(true);
        metric.setAutoFitWidthApproach(AutoFitWidthApproach.BOTH);

        criteriaGrid.setFields(operator, operand, metric);

        // Initialize the criteria grid with existing criteria
        for (GUICriterion criterion : filler.getCriteria()) {
            ListGridRecord rec = new ListGridRecord();
            rec.setAttribute(OPERATOR, criterion.getOperator());
            rec.setAttribute(OPERAND, criterion.getOperand());
            rec.setAttribute(METRIC, criterion.getMetric());
            criteriaGrid.addData(rec);
        }

        IButton addCriterion = new IButton("addcriterion");
        addCriterion.addClickHandler(click -> {
            GUICriterion newEntry = new GUICriterion();
            filler.getCriteria().add(newEntry);
            ListGridRecord rec = new ListGridRecord();
            rec.setAttribute(OPERATOR, newEntry.getOperator());
            rec.setAttribute(OPERAND, newEntry.getOperand());
            rec.setAttribute(METRIC, newEntry.getMetric());
            criteriaGrid.addData(rec);
        });

        // Configure the chain stack
        criteriaStack.setHeight100();
        criteriaStack.setVisible(ATTRIBUTE.equals(filler.getType()));

        // Section that holds the grid + selector
        SectionStackSection section = new SectionStackSection("<b>" + I18N.message("criteria") + "</b>");

        section.setCanCollapse(false);
        section.setExpanded(true);
        section.setItems(criteriaGrid, addCriterion);

        // Attach section to stack
        criteriaStack.setSections(section);
        criteriaStack.draw();

        // Attach chain UI to main container
        container.addMember(criteriaStack);
    }

    private void prepareChain() {
        chainGrid = new ListGrid();
        chainGrid.setEmptyMessage(I18N.message("notitemstoshow"));
        chainGrid.setWidth100();
        chainGrid.setHeight100();
        chainGrid.setEmptyMessage(I18N.message("norecords"));
        chainGrid.setCanSort(false);
        chainGrid.setCanFreezeFields(false);
        chainGrid.setCanGroupBy(false);
        chainGrid.setLeaveScrollbarGap(false);
        chainGrid.setShowHeader(true);
        chainGrid.setSelectionType(SelectionStyle.MULTIPLE);
        chainGrid.setCanEdit(false);
        chainGrid.setShowRowNumbers(true);
        chainGrid.setCanReorderRecords(true);
        chainGrid.setAutoFetchData(true);
        chainGrid.setShowRecordComponents(true);
        chainGrid.setShowRecordComponentsByCell(true);
        chainGrid.addDropCompleteHandler(dropCompleted -> changedHandler.onChanged(null));
        chainGrid.addCellContextClickHandler(event -> {
            showChainContextMenu();
            event.cancel();
        });

        ListGridField id = new IdListGridField();

        ListGridField name = new ListGridField(NAME, I18N.message(NAME));
        name.setCanEdit(false);
        name.setCanSort(false);
        name.setAutoFitWidth(true);
        name.setAutoFitWidthApproach(AutoFitWidthApproach.BOTH);

        ListGridField type = new ListGridField(TYPE, I18N.message(TYPE));
        type.setCanEdit(false);
        type.setCanSort(false);
        type.setAutoFitWidth(true);
        type.setAutoFitWidthApproach(AutoFitWidthApproach.BOTH);

        ListGridField description = new ListGridField(DESCRIPTION, I18N.message(DESCRIPTION));
        description.setCanEdit(false);
        description.setCanSort(false);
        description.setAutoFitWidth(true);
        description.setAutoFitWidthApproach(AutoFitWidthApproach.BOTH);

        chainGrid.setFields(id, name, type, description);

        SelectItem addFiller = prepareFillerSelector();

        // Initialize the chain grid with existing fillers
        for (GUIFiller chainedFiller : filler.getChain()) {
            ListGridRecord rec = new ListGridRecord();
            rec.setAttribute(ID, chainedFiller.getId());
            rec.setAttribute(NAME, chainedFiller.getName());
            rec.setAttribute(TYPE, chainedFiller.getType());
            rec.setAttribute(DESCRIPTION, chainedFiller.getDescription());
            chainGrid.addData(rec);
        }

        // Form that contains the selector
        DynamicForm buttonsForm = new DynamicForm();
        buttonsForm.setItems(addFiller);

        // Configure the chain stack
        chainStack.setHeight100();
        chainStack.setVisible(CHAIN.equals(filler.getType()));

        // Section that holds the grid + selector
        SectionStackSection section = new SectionStackSection("<b>" + I18N.message(CHAIN) + "</b>");

        section.setCanCollapse(false);
        section.setExpanded(true);
        section.setItems(chainGrid, buttonsForm);

        // Attach section to stack
        chainStack.setSections(section);
        chainStack.draw();

        // Attach chain UI to main container
        container.addMember(chainStack);
    }

    private SelectItem prepareFillerSelector() {
        SelectItem addFiller = new FillerSelector(true, null);
        addFiller.addChangedHandler(changed -> {
            ListGridRecord selection = addFiller.getSelectedRecord();

            boolean alreadyInChain = chainGrid
                    .find(new AdvancedCriteria(ID, OperatorId.EQUALS, selection.getAttributeAsString(ID))) != null;

            if (!alreadyInChain && selection.getAttributeAsLong(ID) != filler.getId()) {
                ListGridRecord rec = new ListGridRecord();
                rec.setAttribute(ID, selection.getAttributeAsLong(ID));
                rec.setAttribute(NAME, selection.getAttribute(NAME));
                rec.setAttribute(TYPE, selection.getAttribute(TYPE));
                rec.setAttribute(DESCRIPTION, selection.getAttribute(DESCRIPTION));
                chainGrid.getRecordList().add(rec);
            }
        });
        addFiller.addChangedHandler(changedHandler);

        return addFiller;
    }

    private void showChainContextMenu() {
        Menu contextMenu = new Menu();

        MenuItem delete = new MenuItem();
        delete.setTitle(I18N.message("ddelete"));

        delete.addClickHandler(click -> {
            chainGrid.removeSelectedData();
            changedHandler.onChanged(null);
        });

        contextMenu.setItems(delete);
        contextMenu.showContextMenu();
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