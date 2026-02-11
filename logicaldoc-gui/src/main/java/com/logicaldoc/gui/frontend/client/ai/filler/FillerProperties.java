package com.logicaldoc.gui.frontend.client.ai.filler;

import java.util.LinkedHashMap;

import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.ai.model.ModelsDS;
import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.types.OperatorId;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.DoubleItem;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.form.validator.FloatRangeValidator;
import com.smartgwt.client.widgets.layout.HLayout;

/**
 * Shows robot's standard properties and read-only data
 * 
 * @author Giuseppe Desiato - LogicalDOC
 * @since 9.2.3
 */
public class FillerProperties extends FillerDetailsTab {

	private static final String ID = "id";

	private static final String NAME = "name";

	private static final String LABEL = "label";

	private static final String DESCRIPTION = "description";

	private static final String TYPE = "type";

	private static final String MODEL = "modelId";

	private static final String THRESHOLD = "threshold";

	private DynamicForm form = new DynamicForm();

	private HLayout container = new HLayout();

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
		form = new DynamicForm();
		form.setNumCols(4);
		form.setTitleOrientation(TitleOrientation.TOP);

		if (Boolean.TRUE.equals(container.contains(form)))
			container.removeChild(form);

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

		// Description
		TextAreaItem description = ItemFactory.newTextAreaItem(DESCRIPTION, filler.getDescription());
		description.setColSpan(4);
		description.setWidth("*");
		description.addChangedHandler(changedHandler);

		// Type selector
		SelectItem type = ItemFactory.newSelectItem(TYPE);
		LinkedHashMap<String, String> map = new LinkedHashMap<>();
		map.put("tag", "Tag Filler");
		map.put("language", "Language Filler");
		type.setValueMap(map);
		type.setRequired(true);
		type.setValue(filler.getType());
		type.addChangedHandler(changedHandler);

		// Model selector
		boolean editing = filler.getId() != 0L;

		FormItem modelItem;

		if (!editing) {

			SelectItem modelSelector = new SelectItem(MODEL);
			modelSelector.setTitle("Model");
			modelSelector.setOptionDataSource(new ModelsDS());
			modelSelector.setValueField("id");
			modelSelector.setDisplayField("name");
			modelSelector.setRequired(true);
			modelSelector.setValue(filler.getModelId());
			modelSelector.addChangedHandler(changedHandler);

			modelItem = modelSelector;

		} else {

			StaticTextItem modelName = ItemFactory.newStaticTextItem("modelName", "Model", filler.getModel());

			modelItem = modelName;
		}

		// Threshold (only for tag)
		FloatRangeValidator validator = new FloatRangeValidator();
		validator.setMin(0);
		validator.setMax(1);

		DoubleItem threshold = ItemFactory.newDoubleItem(THRESHOLD, filler.getThreshold());
		threshold.setValidators(validator);
		threshold.addChangedHandler(changedHandler);

		AdvancedCriteria tagCriteria = new AdvancedCriteria(TYPE, OperatorId.EQUALS, "tag");

		threshold.setVisibleWhen(tagCriteria);
		threshold.setRequiredWhen(tagCriteria);

		form.setItems(id, type, name, label, modelItem, threshold, description);

		container.addMember(form);
	}

	public boolean validate() {

		if (!form.validate())
			return false;

		filler.setName(form.getValueAsString(NAME));
		filler.setLabel(form.getValueAsString(LABEL));
		filler.setDescription(form.getValueAsString(DESCRIPTION));
		filler.setType(form.getValueAsString(TYPE));

		String modelId = form.getValueAsString(MODEL);
		filler.setModelId(modelId != null ? Long.parseLong(modelId) : null);

		String threshold = form.getValueAsString(THRESHOLD);
		filler.setThreshold(threshold != null ? Double.parseDouble(threshold) : null);

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