package com.logicaldoc.gui.frontend.client.ai.model;

import java.util.LinkedHashMap;

import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.widgets.DocumentSelector;
import com.logicaldoc.i18n.I18N;
import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.types.OperatorId;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;

/**
 * Shows model's standard properties and read-only data
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
public class ModelProperties extends ModelDetailsTab {

	private static final String ID = "id";

	private static final String NAME = "name";

	private static final String VALUE = "value";

	private static final String TYPE = "type";

	private DynamicForm form = new DynamicForm();

	private HLayout container = new HLayout();

	private DocumentSelector documentSelector;

	public ModelProperties(GUIModel model, final ChangedHandler changedHandler) {
		super(model, changedHandler);
		setWidth100();
		setHeight100();

		setMembers(container);

		documentSelector = new DocumentSelector("document", null);
		documentSelector.setWidth(250);

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
		type.setRequired(true);
		type.setDisabled(model.getId() != 0L);
		type.setVisible(model.getId() == 0L);

		StaticTextItem typeValue = ItemFactory.newStaticTextItem("typeValue", TYPE, model.getType());
		typeValue.setVisible(model.getId() != 0L);

		StaticTextItem id = ItemFactory.newStaticTextItem(ID, Long.toString(model.getId()));
		id.setVisible(model.getId() != 0L);
		documentSelector.addDocumentChangeListener(document -> changedHandler.onChanged(null));

		TextItem features = ItemFactory.newTextItem("features", model.getFeatures());
		features.addChangedHandler(changedHandler);
		features.setHint(I18N.message("valuescommaseparated"));
		features.setVisibleWhen(new AdvancedCriteria(TYPE, OperatorId.EQUALS, "neural"));
		features.setRequiredWhen(new AdvancedCriteria(TYPE, OperatorId.EQUALS, "neural"));

		TextItem categories = ItemFactory.newTextItem("categories", model.getCategories());
		categories.addChangedHandler(changedHandler);
		categories.setHint(I18N.message("valuescommaseparated"));
		categories.setVisibleWhen(new AdvancedCriteria(TYPE, OperatorId.EQUALS, "neural"));
		categories.setRequiredWhen(new AdvancedCriteria(TYPE, OperatorId.EQUALS, "neural"));

		SelectItem activation = activationSeletor();

		SelectItem weightInit = weightInitSeletor();

		SelectItem loss = lossSeletor();

		form.setItems(id, type, name, typeValue, label, features, categories, activation, weightInit, loss,
				description);

		container.setMembersMargin(3);
		container.addMember(form);
	}

	private SelectItem lossSeletor() {
		SelectItem item = ItemFactory.newSelectItem("loss", "lossfunction");
		LinkedHashMap<String, String> map = new LinkedHashMap<>();
		map.put("MSE", "MSE");
		map.put("XENT", "XENT");
		map.put("MCXENT", "MCXENT");
		map.put("SQUARED_LOSS", "SQUARED_LOSS");
		map.put("NEGATIVELOGLIKELIHOOD", "NEGATIVELOGLIKELIHOOD");
		item.setValueMap(map);

		item.setVisibleWhen(new AdvancedCriteria(TYPE, OperatorId.EQUALS, "neural"));
		item.setRequiredWhen(new AdvancedCriteria(TYPE, OperatorId.EQUALS, "neural"));

		return item;
	}

	private SelectItem weightInitSeletor() {
		SelectItem item = ItemFactory.newSelectItem("weightInit", "weightinitscheme");
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

		item.setVisibleWhen(new AdvancedCriteria(TYPE, OperatorId.EQUALS, "neural"));
		item.setRequiredWhen(new AdvancedCriteria(TYPE, OperatorId.EQUALS, "neural"));
		return item;
	}

	private SelectItem activationSeletor() {
		SelectItem item = ItemFactory.newSelectItem("activation", "activationfunction");
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

		item.setVisibleWhen(new AdvancedCriteria(TYPE, OperatorId.EQUALS, "neural"));
		item.setRequiredWhen(new AdvancedCriteria(TYPE, OperatorId.EQUALS, "neural"));
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
			model.setActivation(form.getValueAsString("activation"));
			model.setWeightInit(form.getValueAsString("weightInit"));
		}
		return !form.hasErrors();
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