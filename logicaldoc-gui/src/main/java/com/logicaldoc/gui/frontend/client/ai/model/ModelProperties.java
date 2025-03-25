package com.logicaldoc.gui.frontend.client.ai.model;

import java.util.LinkedHashMap;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.types.AutoFitWidthApproach;
import com.smartgwt.client.types.OperatorId;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.IntegerItem;
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
 * Shows model's standard properties and read-only data
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
public class ModelProperties extends ModelDetailsTab {

	private static final String SEED = "seed";

	private static final String BATCH = "batch";

	private static final String NEURAL = "neural";

	private static final String ACTIVATION = "activation";

	private static final String OUTPUTNODES = "outputnodes";

	private static final String ID = "id";

	private static final String NAME = "name";

	private static final String VALUE = "value";

	private static final String TYPE = "type";

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

		TextItem name = ItemFactory.newSimpleTextItem(NAME, model.getName());
		name.addChangedHandler(changedHandler);
		name.setRequired(true);

		TextItem label = ItemFactory.newTextItem("label", model.getLabel());
		label.addChangedHandler(changedHandler);

		TextAreaItem description = ItemFactory.newTextAreaItem("description", model.getDescription());
		description.addChangedHandler(changedHandler);
		description.setColSpan(4);
		description.setWidth("*");

		SelectItem type = ItemFactory.newSelectItem(TYPE);
		type.setOptionDataSource(new ModelTypeDS());
		type.setValueField(VALUE);
		type.setDisplayField(VALUE);
		type.setValue(model.getType());
		type.addChangedHandler(changedHandler);
		type.addChangedHandler(changed -> layersStack.setVisible(NEURAL.equals(type.getValueAsString())));
		type.setRequired(true);
		type.setDisabled(model.getId() != 0L);
		type.setVisible(model.getId() == 0L);

		StaticTextItem typeValue = ItemFactory.newStaticTextItem("typeValue", TYPE, model.getType());
		typeValue.setVisible(model.getId() != 0L);

		StaticTextItem id = ItemFactory.newStaticTextItem(ID, Long.toString(model.getId()));
		id.setVisible(model.getId() != 0L);

		TextItem features = ItemFactory.newTextItem("features", model.getFeatures());
		features.addChangedHandler(changedHandler);
		features.setColSpan(4);
		features.setWidth(400);
		features.setHint(I18N.message("valuescommaseparated"));
		features.setShowHintInField(true);
		features.setValue(model.getFeatures());
		setNeuralNetworkVisibility(features);

		TextItem categories = ItemFactory.newTextItem("categories", model.getCategories());
		categories.addChangedHandler(changedHandler);
		categories.setColSpan(4);
		categories.setValue(model.getCategories());
		categories.setWidth(400);
		categories.setHint(I18N.message("valuescommaseparated"));
		categories.setShowHintInField(true);
		setNeuralNetworkVisibility(categories);

		SelectItem activation = activationSeletor();
		setNeuralNetworkVisibility(activation);
		activation.addChangedHandler(changedHandler);

		SelectItem weightInit = weightInitSeletor();

		SelectItem loss = lossSeletor();

		SpinnerItem batch = ItemFactory.newSpinnerItem(BATCH, model.getBatch());
		batch.setMin(1);
		batch.addChangedHandler(changedHandler);
		setNeuralNetworkVisibility(batch);

		IntegerItem seed = ItemFactory.newIntegerItem(SEED, SEED, model.getSeed());
		setNeuralNetworkVisibility(seed);

		form.setItems(id, typeValue, type, name, label, features, categories, activation, weightInit, loss, batch, seed,
				description);

		container.setMembersMargin(3);
		container.addMember(form);

		prepareLayers();
	}

	private void setNeuralNetworkVisibility(FormItem activation) {
		activation.setVisibleWhen(new AdvancedCriteria(TYPE, OperatorId.EQUALS, NEURAL));
		activation.setRequiredWhen(new AdvancedCriteria(TYPE, OperatorId.EQUALS, NEURAL));
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

		setNeuralNetworkVisibility(item);
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

		setNeuralNetworkVisibility(item);
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
		return item;
	}

	boolean validate() {
		if (form.validate()) {
			model.setName(form.getValueAsString(NAME));
			model.setLabel(form.getValueAsString("label"));
			model.setDescription(form.getValueAsString("description"));
			model.setType(form.getValueAsString(TYPE));
			model.setFeatures(form.getValueAsString("features"));
			model.setCategories(form.getValueAsString("categories"));
			model.setActivation(form.getValueAsString(ACTIVATION));
			model.setWeightInit(form.getValueAsString("weightInit"));
			model.setLoss(form.getValueAsString("loss"));
			model.setBatch(Integer.parseInt(form.getValueAsString(BATCH)));
			model.setSeed(Long.parseLong(form.getValueAsString(SEED)));

			if (NEURAL.equals(model.getType())) {
				com.smartgwt.client.data.Record[] layerRecords = layers.getRecordList().toArray();
				if (layerRecords.length < 2) {
					GuiLog.error("modulelayersnotenough");
					return false;
				}

				model.getLayers().clear();
				for (com.smartgwt.client.data.Record layerRecord : layerRecords)
					model.getLayers().add(new GUINeuralNetworkLayer(layerRecord.getAttribute(NAME),
							layerRecord.getAttributeAsInt(OUTPUTNODES), layerRecord.getAttribute(ACTIVATION)));
			}

		}
		return !form.hasErrors();
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

		ListGridField activation = new ListGridField(ACTIVATION, I18N.message("activationfunction"));
		activation.setCanEdit(true);
		activation.setCanSort(false);
		activation.setAutoFitWidth(true);
		activation.setAutoFitWidthApproach(AutoFitWidthApproach.BOTH);
		activation.setEditorProperties(activationSeletor());

		layers.setFields(name, outputNodes, activation);

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
			rec.setAttribute(ACTIVATION, "RELU");
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