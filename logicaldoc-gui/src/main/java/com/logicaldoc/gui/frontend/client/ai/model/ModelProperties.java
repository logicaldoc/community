package com.logicaldoc.gui.frontend.client.ai.model;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.stream.Collectors;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.Util;
import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.data.Criterion;
import com.smartgwt.client.types.AutoFitWidthApproach;
import com.smartgwt.client.types.OperatorId;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.DoubleItem;
import com.smartgwt.client.widgets.form.fields.MultiComboBoxItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.form.validator.FloatRangeValidator;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.SectionStack;
import com.smartgwt.client.widgets.layout.SectionStackSection;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;

/**
 * Shows model's standard properties and read-only data
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
public class ModelProperties extends ModelDetailsTab {

    private static final String CATEGORIES = "categories";

    private static final String FEATURES = "features";

    private static final String THRESHOLD = "threshold";

    private static final String MINCHUNKSIZE = "minchunksize";

    private static final String TOKENS = "tokens";

    private static final String MOMENTUM = "momentum";

    private static final String EPSILON = "epsilon";

    private static final String LEARNINGRATE = "learningrate";

    private static final String LABEL = "label";

    private static final String UPDATER = "updater";

    private static final String FUNCTION = "function";

    private static final String LANGUAGE = "language";

    private static final String SEED = "seed";

    private static final String BATCH = "batch";

    private static final String NEURAL = "neural";

    private static final String YOLO = "yolo";

    private static final String EMBEDDER = "embedder";

    private static final String ACTIVATION = "activation";

    private static final String OUTPUTNODES = "outputnodes";

    private static final String ID = "id";

    private static final String NAME = "name";

    private static final String TYPE = "type";

    private static final AdvancedCriteria NEURAL_CRITERIA = new AdvancedCriteria(TYPE, OperatorId.EQUALS, NEURAL);

    private static final String CLASSIFIER = "classifier";

    private static final String MINI_EMBEDDER = "miniembedder";

    private static final String SUMMARIZER = "summarizer";

    private DynamicForm form = new DynamicForm();

    private HLayout container = new HLayout();

    private ListGrid layers;

    private SectionStack layersStack = new SectionStack();

    public ModelProperties(GUIModel model, final ChangedHandler changedHandler) {
        super(model, changedHandler);
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

        /*
         * Pre-compute the model type flags used to control the visibility of
         * the various property fields in the form.
         */
        boolean neural = NEURAL.equals(model.getType());
        boolean embedder = EMBEDDER.equals(model.getType());
        boolean miniEmbedder = MINI_EMBEDDER.equals(model.getType());
        boolean anyembedder = embedder || miniEmbedder;
        boolean nlp = CLASSIFIER.equals(model.getType()) || TOKENS.equals(model.getType());
        boolean yolo = YOLO.equals(model.getType());
        boolean summarizer = SUMMARIZER.equals(model.getType());

        boolean trainable = neural || embedder;

        TextItem name = ItemFactory.newSimpleTextItem(NAME, model.getName());
        name.addChangedHandler(changedHandler);
        name.setRequired(true);

        TextItem label = ItemFactory.newTextItem(LABEL, model.getLabel());
        label.addChangedHandler(changedHandler);

        TextAreaItem description = ItemFactory.newTextAreaItem("description", model.getDescription());
        description.addChangedHandler(changedHandler);
        description.setColSpan(4);
        description.setWidth("*");

        SpinnerItem windowSize = ItemFactory.newSpinnerItem("windowsize", model.getWindowSize());
        windowSize.setMin(2);
        windowSize.setStep(1);
        windowSize.addChangedHandler(changedHandler);
        windowSize.setVisible(embedder);
        windowSize.setRequired(embedder);

        StaticTextItem type = ItemFactory.newStaticTextItem(TYPE, TYPE, I18N.message("aimodeltype." + model.getType()));

        StaticTextItem id = ItemFactory.newStaticTextItem(ID, Long.toString(model.getId()));
        id.setVisible(model.getId() != 0L);

        String[] faturesArray = model.getFeatureDescriptors().stream().map(fd -> fd.getName())
                .collect(Collectors.toList()).toArray(new String[0]);

        MultiComboBoxItem features = ItemFactory.newMultiComboBoxItem(FEATURES, FEATURES, null, faturesArray);
        features.setShowPending(true);
        features.setAddUnknownValues(true);
        features.setColSpan(4);
        features.setWidth("*");
        features.addChangedHandler(changedHandler);
        features.setValueMap(faturesArray);
        features.setVisible(neural);
        features.setRequired(trainable && neural);

        SelectItem activationSelector = activationSeletor();
        activationSelector.setVisible(neural);
        activationSelector.setRequired(neural);
        activationSelector.addChangedHandler(changedHandler);

        SelectItem weightInit = weightInitSeletor();
        weightInit.setVisible(neural);
        weightInit.setRequired(neural);

        SelectItem loss = lossSeletor();
        loss.setVisible(neural);
        loss.setRequired(neural);

        SelectItem updater = updaterSelector();
        updater.setVisible(neural);
        updater.setRequired(neural);

        DoubleItem learningRate = ItemFactory.newDoubleItem(LEARNINGRATE, LEARNINGRATE,
                model.getUpdater().getLearningRate());
        learningRate.addChangedHandler(changedHandler);
        learningRate.setVisibleWhen(
                new AdvancedCriteria(OperatorId.AND, new Criterion[] { NEURAL_CRITERIA, new AdvancedCriteria(UPDATER,
                        OperatorId.IN_SET, new String[] { GUIUpdater.ADAGRAD, GUIUpdater.NESTEROV }) }));

        DoubleItem epsilon = ItemFactory.newDoubleItem(EPSILON, EPSILON, model.getUpdater().getEpsilon());
        epsilon.addChangedHandler(changedHandler);
        epsilon.setVisibleWhen(new AdvancedCriteria(OperatorId.AND, new Criterion[] { NEURAL_CRITERIA,
                new AdvancedCriteria(UPDATER, OperatorId.EQUALS, GUIUpdater.ADAGRAD) }));

        DoubleItem momentum = ItemFactory.newDoubleItem(MOMENTUM, MOMENTUM, model.getUpdater().getMomentum());
        momentum.addChangedHandler(changedHandler);
        momentum.setVisibleWhen(new AdvancedCriteria(OperatorId.AND, new Criterion[] { NEURAL_CRITERIA,
                new AdvancedCriteria(UPDATER, OperatorId.EQUALS, GUIUpdater.NESTEROV) }));

        SpinnerItem batch = ItemFactory.newSpinnerItem(BATCH, model.getBatch());
        batch.setMin(1);
        batch.addChangedHandler(changedHandler);
        batch.setVisible(neural);
        batch.setRequired(neural);

        SpinnerItem seed = ItemFactory.newSpinnerItem(SEED, SEED, model.getSeed());
        seed.setMin(1);
        seed.setStep(1);
        seed.addChangedHandler(changedHandler);
        boolean neuralOrEmbedder = neural || anyembedder;
        seed.setVisible(neuralOrEmbedder);
        seed.setRequired(neuralOrEmbedder);

        SelectItem language = ItemFactory.newLanguageSelector(LANGUAGE, true, false);
        language.setValue(model.getLanguage());
        language.addChangedHandler(changedHandler);
        language.setVisible(nlp);
        language.setRequired(nlp);

        SpinnerItem cutoff = ItemFactory.newSpinnerItem("cutoff", model.getCutoff());
        cutoff.setMin(1);
        cutoff.addChangedHandler(changedHandler);
        cutoff.setVisible(nlp);
        cutoff.setRequired(nlp);

        SpinnerItem ngramMin = ItemFactory.newSpinnerItem("ngrammin", model.getNgramMin());
        ngramMin.setMin(2);
        ngramMin.addChangedHandler(changedHandler);
        ngramMin.setVisible(nlp);
        ngramMin.setRequired(nlp);

        SpinnerItem ngramMax = ItemFactory.newSpinnerItem("ngrammax", model.getNgramMax());
        ngramMax.setMin(2);
        ngramMax.addChangedHandler(changedHandler);
        ngramMax.setVisible(nlp);
        ngramMax.setRequired(nlp);

        SpinnerItem vectorSize = ItemFactory.newSpinnerItem("vectorsize", model.getVectorSize());
        vectorSize.setMin(100);
        vectorSize.setStep(50);
        vectorSize.addChangedHandler(changedHandler);
        vectorSize.setVisible(embedder);
        vectorSize.setRequired(embedder);

        SpinnerItem minWordFrequency = ItemFactory.newSpinnerItem("minwordfrequency", model.getMinWordFrequency());
        minWordFrequency.setMin(1);
        minWordFrequency.setStep(1);
        minWordFrequency.addChangedHandler(changedHandler);
        minWordFrequency.setVisible(embedder);
        minWordFrequency.setRequired(embedder);

        SpinnerItem chunkSize = ItemFactory.newSpinnerItem("chunksize", model.getChunking().getChunkSize());
        chunkSize.setHint(I18N.message(TOKENS).toLowerCase());
        chunkSize.setMin(1);
        chunkSize.setStep(10);
        chunkSize.addChangedHandler(changedHandler);
        chunkSize.setVisible(anyembedder);
        chunkSize.setRequired(anyembedder);

        SpinnerItem minChunkSize = ItemFactory.newSpinnerItem(MINCHUNKSIZE, model.getChunking().getMinChunkSize());
        minChunkSize.setHint(I18N.message(TOKENS).toLowerCase());
        minChunkSize.setMin(1);
        minChunkSize.setStep(5);
        minChunkSize.addChangedHandler(changedHandler);
        minChunkSize.setVisible(anyembedder);
        minChunkSize.setRequired(anyembedder);

        SpinnerItem minChunkSizeChars = ItemFactory.newSpinnerItem("minchunksizechars", MINCHUNKSIZE,
                model.getChunking().getMinChunkSizeChars());
        minChunkSizeChars.setHint(I18N.message("chars").toLowerCase());
        minChunkSizeChars.setMin(1);
        minChunkSizeChars.setStep(10);
        minChunkSizeChars.addChangedHandler(changedHandler);
        minChunkSizeChars.setVisible(anyembedder);
        minChunkSizeChars.setRequired(anyembedder);

        SpinnerItem maxChunks = ItemFactory.newSpinnerItem("maxchunks", model.getChunking().getMaxChunks());
        maxChunks.setMin(1);
        maxChunks.setStep(10);
        maxChunks.addChangedHandler(changedHandler);
        maxChunks.setVisible(anyembedder);
        maxChunks.setRequired(anyembedder);

        SpinnerItem workers = ItemFactory.newSpinnerItem("workers", model.getWorkers());
        workers.setMin(1);
        workers.setStep(1);
        workers.addChangedHandler(changedHandler);
        workers.setVisible(embedder);
        workers.setRequired(embedder);

        FloatRangeValidator rangevalidator = new FloatRangeValidator();
        rangevalidator.setMin(0);
        rangevalidator.setMax(1);

        DoubleItem alpha = ItemFactory.newDoubleItem("alpha", model.getAlpha());
        alpha.setValidators(rangevalidator);
        alpha.setWidth(60);
        alpha.addChangedHandler(changedHandler);
        alpha.setVisible(embedder);
        alpha.setRequired(embedder);

        DoubleItem minAlpha = ItemFactory.newDoubleItem("minalpha", model.getMinAlpha());
        minAlpha.setValidators(rangevalidator);
        minAlpha.setWidth(60);
        minAlpha.addChangedHandler(changedHandler);
        minAlpha.setVisible(embedder);
        minAlpha.setRequired(embedder);

        MultiComboBoxItem categories = ItemFactory.newMultiComboBoxItem(CATEGORIES, CATEGORIES, null,
                model.getCategoriesArray());
        categories.setShowPending(true);
        categories.setAddUnknownValues(true);
        categories.setColSpan(4);
        categories.setWidth("*");
        categories.addChangedHandler(changedHandler);
        categories.setValueMap(model.getCategoriesArray());
        categories.setVisible(neural || yolo);
        categories.setRequired(neural || yolo);

        SpinnerItem trainingImagesWidth = ItemFactory.newSpinnerItem("trainingimageswidth",
                model.getTrainingImagesWidth());
        trainingImagesWidth.setHint("px");
        trainingImagesWidth.setMin(1);
        trainingImagesWidth.setStep(1);
        trainingImagesWidth.addChangedHandler(changedHandler);
        trainingImagesWidth.setVisible(yolo);
        trainingImagesWidth.setRequired(yolo);

        SpinnerItem trainingImagesHeight = ItemFactory.newSpinnerItem("trainingimagesheight",
                model.getTrainingImagesHeight());
        trainingImagesHeight.setHint("px");
        trainingImagesHeight.setMin(1);
        trainingImagesHeight.setStep(1);
        trainingImagesHeight.addChangedHandler(changedHandler);
        trainingImagesHeight.setVisible(yolo);
        trainingImagesHeight.setRequired(yolo);

        SpinnerItem threshold = ItemFactory.newSpinnerItem(THRESHOLD, Math.round(model.getThreshold() * 100));
        threshold.setMin(0);
        threshold.setMax(100);
        threshold.setStep(1);
        threshold.addChangedHandler(changedHandler);
        threshold.setVisible(yolo);
        threshold.setRequired(yolo);

        SpinnerItem sentences = ItemFactory.newSpinnerItem("sentences", model.getSummarySentences());

        sentences.setMin(1);
        sentences.setVisible(summarizer);
        sentences.setRequired(summarizer);
        sentences.addChangedHandler(changedHandler);

        DoubleItem mmrLambda = ItemFactory.newDoubleItem("mmrlambda", model.getMmrLambda());

        FloatRangeValidator validator = new FloatRangeValidator();
        validator.setMin(0);
        validator.setMax(1);

        mmrLambda.setValidators(validator);
        mmrLambda.setVisible(summarizer);
        mmrLambda.setRequired(summarizer);
        mmrLambda.addChangedHandler(changedHandler);

        form.setItems(id, type, name, label, features, categories, activationSelector, weightInit, loss, updater,
                learningRate, epsilon, momentum, batch, seed, cutoff, ngramMin, ngramMax, language, vectorSize,
                minWordFrequency, windowSize, chunkSize, minChunkSize, minChunkSizeChars, maxChunks, workers, alpha,
                minAlpha, trainingImagesWidth, trainingImagesHeight, threshold, sentences, mmrLambda, description);

        container.setMembersMargin(3);
        container.addMember(form);

        prepareLayers();
    }

    boolean validate() {
        if (!form.validate())
            return false;

        model.setName(form.getValueAsString(NAME));
        model.setLabel(form.getValueAsString(LABEL));
        model.setDescription(form.getValueAsString("description"));
        model.setLanguage(form.getValueAsString(LANGUAGE));

        validateCategories();

        validateFeatures();

        model.setActivation(form.getValueAsString(ACTIVATION));
        model.setWeightInit(form.getValueAsString("weightInit"));
        model.setLoss(form.getValueAsString("loss"));
        model.setBatch(Integer.parseInt(form.getValueAsString(BATCH)));
        model.setSeed(Long.parseLong(form.getValueAsString(SEED)));
        model.setVectorSize(Integer.parseInt(form.getValueAsString("vectorsize")));
        model.setMinWordFrequency(Integer.parseInt(form.getValueAsString("minwordfrequency")));
        model.setWindowSize(Integer.parseInt(form.getValueAsString("windowsize")));
        model.setWorkers(Integer.parseInt(form.getValueAsString("workers")));
        model.setAlpha(Double.parseDouble(form.getValueAsString("alpha")));
        model.setMinAlpha(Double.parseDouble(form.getValueAsString("minalpha")));

        model.getChunking().setChunkSize(Integer.parseInt(form.getValueAsString("chunksize")));
        model.getChunking().setMinChunkSize(Integer.parseInt(form.getValueAsString(MINCHUNKSIZE)));
        model.getChunking().setMinChunkSizeChars(Integer.parseInt(form.getValueAsString("minchunksizechars")));
        model.getChunking().setMaxChunks(Integer.parseInt(form.getValueAsString("maxchunks")));

        model.getUpdater().setUpdateAlgorithm(form.getValueAsString(UPDATER));

        String val = form.getValueAsString(LEARNINGRATE);
        model.getUpdater().setLearningRate(val != null ? Double.parseDouble(val) : null);

        val = form.getValueAsString(EPSILON);
        model.getUpdater().setEpsilon(val != null ? Double.parseDouble(val) : null);

        val = form.getValueAsString(MOMENTUM);
        model.getUpdater().setMomentum(val != null ? Double.parseDouble(val) : null);

        model.setTrainingImagesWidth(Integer.parseInt(form.getValueAsString("trainingimageswidth")));
        model.setTrainingImagesHeight(Integer.parseInt(form.getValueAsString("trainingimagesheight")));

        Integer thresholdValue = (Integer) form.getValue(THRESHOLD);

        if (thresholdValue != null) {
            model.setThreshold(thresholdValue / 100d);
        } else {
            model.setThreshold(0);
        }

        if (NEURAL.equals(model.getType())) {
            com.smartgwt.client.data.Record[] layerRecords = layers.getRecordList().toArray();
            if (layerRecords.length < 2) {
                GuiLog.error(I18N.message("modulelayersnotenough"));
                return false;
            }

            model.getLayers().clear();
            for (com.smartgwt.client.data.Record layerRecord : layerRecords)
                model.getLayers().add(new GUINeuralNetworkLayer(layerRecord.getAttribute(NAME),
                        layerRecord.getAttributeAsInt(OUTPUTNODES), layerRecord.getAttribute(ACTIVATION)));
        } else {
            model.setLanguage(form.getValueAsString(LANGUAGE));
            model.setCutoff(Integer.parseInt(form.getValueAsString("cutoff")));
            model.setNgramMin(Integer.parseInt(form.getValueAsString("ngrammin")));
            model.setNgramMax(Integer.parseInt(form.getValueAsString("ngrammax")));
        }

        if ("summarizer".equals(model.getType())) {
            model.setSummarySentences(Integer.parseInt(form.getValueAsString("sentences")));
            model.setMmrLambda(Double.parseDouble(form.getValueAsString("mmrlambda")));
        }

        return !form.hasErrors();
    }

    private void validateFeatures() {
        List<String> features = new ArrayList<>();
        for (Object cat : (Object[]) form.getValue(FEATURES)) {
            String str = cat.toString();
            if (str.contains(",")) {
                String[] subcats = str.split(",");
                for (String subcat : subcats)
                    features.add(subcat.trim());
            } else {
                features.add(str.trim());
            }
        }
        model.setFeatureNames(features.stream().collect(Collectors.joining(",")));
    }

    private void validateCategories() {
        List<String> categories = new ArrayList<>();
        for (Object cat : (Object[]) form.getValue(CATEGORIES)) {
            String str = cat.toString();
            if (str.contains(",")) {
                String[] subcats = str.split(",");
                for (String subcat : subcats)
                    categories.add(subcat.trim());
            } else {
                categories.add(str.trim());
            }
        }
        model.setCategoriesArray(categories.toArray(new String[0]));
    }

    private SelectItem updaterSelector() {
        SelectItem item = ItemFactory.newSelectItem(UPDATER, UPDATER);
        item.addChangedHandler(changedHandler);

        LinkedHashMap<String, String> map = new LinkedHashMap<>();
        map.put(GUIUpdater.ADAGRAD, "Adaptive Gradient");
        map.put(GUIUpdater.NESTEROV, "Nesterov Accelerated Gradient");
        item.setValueMap(map);

        item.setValue(model.getUpdater().getUpdateAlgorithm());

        return item;
    }

    private SelectItem lossSeletor() {
        SelectItem item = ItemFactory.newSelectItem("loss", "lossfunction");
        item.addChangedHandler(changedHandler);

        LinkedHashMap<String, String> map = new LinkedHashMap<>();
        map.put("MSE", "MSE");
        map.put("XENT", "XENT");
        map.put("MCXENT", "MCXENT");
        map.put("SQUARED_LOSS", "SQUARED_LOSS");
        map.put("NEGATIVELOGLIKELIHOOD", "NEGATIVELOGLIKELIHOOD");
        item.setValueMap(map);

        item.setValue(model.getLoss());

        return item;
    }

    private SelectItem weightInitSeletor() {
        SelectItem item = ItemFactory.newSelectItem("weightInit", "weightinitscheme");
        item.addChangedHandler(changedHandler);

        LinkedHashMap<String, String> map = new LinkedHashMap<>();
        map.put("DISTRIBUTION", "DISTRIBUTION");
        map.put("NORMALIZED", "NORMALIZED");
        map.put("RELU", "RELU");
        map.put("SIZE", "SIZE");
        map.put("UNIFORM", "UNIFORM");
        map.put("VI", "VI");
        map.put("ZERO", "ZERO");
        map.put("XAVIER", "XAVIER");

        item.setValueMap(map);
        item.setValue(model.getWeightInit());

        return item;
    }

    private SelectItem activationSeletor() {
        SelectItem item = ItemFactory.newSelectItem(ACTIVATION, "activationfunction");
        LinkedHashMap<String, String> map = new LinkedHashMap<>();
        map.put("CUBE", "CUBE");
        map.put("ELU", "ELU");
        map.put("HARDSIGMOID", "HARDSIGMOID");
        map.put("HARDTANH", "HARDTANH");
        map.put("IDENTITY", "IDENTITY");
        map.put("LEAKYRELU", "LEAKYRELU");
        map.put("RATIONALTANH", "RATIONALTANH");
        map.put("RELU", "RELU");
        map.put("RELU6", "RELU6");
        map.put("RRELU", "RRELU");
        map.put("SIGMOID", "SIGMOID");
        map.put("SOFTMAX", "SOFTMAX");
        map.put("SOFTPLUS", "SOFTPLUS");
        map.put("SOFTSIGN", "SOFTSIGN");
        map.put("TANH", "TANH");
        map.put("RECTIFIEDTANH", "RECTIFIEDTANH");
        map.put("SELU", "SELU");
        map.put("SWISH", "SWISH");
        map.put("THRESHOLDEDRELU", "THRESHOLDEDRELU");
        map.put("GELU", "GELU");
        map.put("MISH", "MISH");
        item.setValueMap(map);
        item.setValue(model.getActivation());

        ListGridField functionField = new ListGridField(FUNCTION, I18N.message(FUNCTION));
        ListGridField graphField = new ListGridField("graph", I18N.message("graph"));
        graphField.setCellFormatter((value, rcd, rowNum, colNum) -> Util
                .imageHTML("AI/activation/" + rcd.getAttributeAsString(FUNCTION) + ".png", null, 80, null));

        item.setValueField(FUNCTION);
        item.setDisplayField(FUNCTION);
        item.setPickListWidth(450);
        item.setPickListFields(functionField, graphField);

        return item;
    }

    private void prepareLayers() {
        layers = new ListGrid();
        layers.setEmptyMessage(I18N.message("notitemstoshow"));
        layers.setWidth100();
        layers.setHeight100();
        layers.setEmptyMessage(I18N.message("norecords"));
        layers.setCanSort(false);
        layers.setCanFreezeFields(false);
        layers.setCanGroupBy(false);
        layers.setLeaveScrollbarGap(false);
        layers.setShowHeader(true);
        layers.setSelectionType(SelectionStyle.MULTIPLE);
        layers.setCanEdit(true);
        layers.setEditByCell(true);
        layers.setShowRowNumbers(true);
        layers.setCanReorderRecords(true);
        layers.setAutoFetchData(true);
        layers.setShowRecordComponents(true);
        layers.setShowRecordComponentsByCell(true);
        layers.addDropCompleteHandler(dropCompleted -> changedHandler.onChanged(null));
        layers.addEditCompleteHandler(editCompleted -> changedHandler.onChanged(null));
        layers.addCellContextClickHandler(event -> {
            showContextMenu();
            event.cancel();
        });

        ListGridField name = new ListGridField(NAME, I18N.message(NAME));
        name.setCanEdit(true);
        name.setCanSort(false);
        name.setAutoFitWidth(true);
        name.setMinWidth(150);
        name.setAutoFitWidthApproach(AutoFitWidthApproach.BOTH);

        ListGridField outputNodes = new ListGridField(OUTPUTNODES, I18N.message(OUTPUTNODES));
        outputNodes.setCanEdit(true);
        outputNodes.setCanSort(false);
        outputNodes.setAutoFitWidth(true);
        outputNodes.setAutoFitWidthApproach(AutoFitWidthApproach.BOTH);
        SpinnerItem editor = ItemFactory.newSpinnerItem(OUTPUTNODES, 1);
        editor.setMin(1);
        outputNodes.setEditorProperties(editor);

        ListGridField activationField = new ListGridField(ACTIVATION, I18N.message("activationfunction"));
        activationField.setCanEdit(true);
        activationField.setCanSort(false);
        activationField.setAutoFitWidth(true);
        activationField.setAutoFitWidthApproach(AutoFitWidthApproach.BOTH);
        activationField.setEditorProperties(activationSeletor());

        layers.setFields(name, outputNodes, activationField);

        // Initialize the layers grid
        for (GUINeuralNetworkLayer layer : model.getLayers()) {
            ListGridRecord rec = new ListGridRecord();
            rec.setAttribute(NAME, layer.getName());
            rec.setAttribute(OUTPUTNODES, layer.getOutputNodes());
            rec.setAttribute(ACTIVATION, layer.getActivation());
            layers.addData(rec);
        }

        layersStack.setHeight100();
        layersStack.setVisible(NEURAL.equals(model.getType()));

        IButton addLayer = new IButton(I18N.message("addlayer"));
        addLayer.addClickHandler(click -> {
            ListGridRecord rec = new ListGridRecord();
            rec.setAttribute(NAME, "new_layer");
            rec.setAttribute(OUTPUTNODES, 3);
            rec.setAttribute(ACTIVATION, form.getValueAsString(ACTIVATION));
            layers.addData(rec);
            changedHandler.onChanged(null);
        });

        SectionStackSection section = new SectionStackSection("<b>" + I18N.message("layers") + "</b>");
        section.setCanCollapse(false);
        section.setExpanded(true);
        section.setItems(layers, addLayer);

        layersStack.setSections(section);
        layersStack.draw();

        container.addMember(layersStack);
    }

    private void showContextMenu() {
        Menu contextMenu = new Menu();
        MenuItem delete = new MenuItem();
        delete.setTitle(I18N.message("ddelete"));
        delete.addClickHandler(click -> {
            layers.removeSelectedData();
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