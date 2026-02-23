package com.logicaldoc.gui.frontend.client.ai.autofill;

import java.util.LinkedHashMap;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.ai.embedding.EmbeddingSchemesDS;
import com.logicaldoc.gui.frontend.client.ai.model.ModelsDS;
import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.data.Criterion;
import com.smartgwt.client.types.OperatorId;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.DoubleItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.form.validator.FloatRangeValidator;
import com.smartgwt.client.widgets.layout.HLayout;

/**
 * Shows filler's standard properties and read-only data
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

	private static final String MODEL_ID = "modelId";

	private static final String THRESHOLD = "threshold";

	private static final String STRATEGY = "strategy";

	private static final String EMBEDDING = "embeddingscheme";

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

		// Description
		TextAreaItem description = ItemFactory.newTextAreaItem(DESCRIPTION, filler.getDescription());
		description.addChangedHandler(changedHandler);
		description.setColSpan(4);
		description.setWidth(400);

		// Model selector
		SelectItem modelSelector = ItemFactory.newSelectItem(MODEL_ID, "model");
		modelSelector.setValueField("id");
		modelSelector.setDisplayField("name");
		modelSelector.setRequired(true);
		modelSelector.addChangedHandler(changedHandler);

		// Type selector
		SelectItem type = ItemFactory.newSelectItem(TYPE);
		LinkedHashMap<String, String> map = new LinkedHashMap<>();
		map.put("tag", I18N.message("fillertype.tag"));
		map.put("language", I18N.message("fillertype.language"));
		type.setValueMap(map);
		type.setRequired(true);
		type.setValue(filler.getType());
		type.addChangedHandler(changedHandler);
		type.addChangedHandler(event -> {
			String selectedType = event.getValue().toString();
			modelSelector.clearValue();
			modelSelector.setOptionDataSource(new ModelsDS(modelTypesSuitableForFiller(selectedType)));
			modelSelector.fetchData();
		});

		if (filler.getType() != null) {
			modelSelector.setOptionDataSource(new ModelsDS(modelTypesSuitableForFiller(filler.getType())));
			if (filler.getModelId() != null)
				modelSelector.setValue(filler.getModelId());
		}

		// Threshold (only for tag)
		FloatRangeValidator validator = new FloatRangeValidator();
		validator.setMin(0);
		validator.setMax(1);

		// Strategy selector
		SelectItem strategy = ItemFactory.newSelectItem(STRATEGY);
		LinkedHashMap<String, String> strategyMap = new LinkedHashMap<>();
		strategyMap.put("model", I18N.message("strategy.model"));
		strategyMap.put("retrieval", I18N.message("strategy.retrieval"));
		strategy.setValueMap(strategyMap);
		strategy.setRequired(true);

		if ("tag".equals(filler.getType())) {
			if (filler.getModelId() == null) {
				strategy.setValue("retrieval");
			} else {
				strategy.setValue("model");
			}
		}

		// EmbeddingScheme selector
		SelectItem embeddingSelector = ItemFactory.newSelectItem(EMBEDDING, I18N.message("embeddingscheme"));

		embeddingSelector.setValueField("id");
		embeddingSelector.setDisplayField("name");
		embeddingSelector.setOptionDataSource(new EmbeddingSchemesDS());
		embeddingSelector.addChangedHandler(changedHandler);
		if (filler.getEmbeddingSchemeId() != null)
			embeddingSelector.setValue(filler.getEmbeddingSchemeId());

		DoubleItem threshold = ItemFactory.newDoubleItem(THRESHOLD, filler.getThreshold());
		threshold.setValidators(validator);
		threshold.addChangedHandler(changedHandler);

		// Criteria
		AdvancedCriteria tagCriteria = new AdvancedCriteria(TYPE, OperatorId.EQUALS, "tag");

		threshold.setVisibleWhen(tagCriteria);
		threshold.setRequiredWhen(tagCriteria);

		AdvancedCriteria modelVisible = new AdvancedCriteria(OperatorId.OR,
				new Criterion[] { new Criterion(TYPE, OperatorId.EQUALS, "language"),
						new AdvancedCriteria(OperatorId.AND,
								new Criterion[] { new Criterion(TYPE, OperatorId.EQUALS, "tag"),
										new Criterion(STRATEGY, OperatorId.EQUALS, "model") }) });

		modelSelector.setVisibleWhen(modelVisible);
		modelSelector.setRequiredWhen(modelVisible);

		AdvancedCriteria strategyVisible = new AdvancedCriteria(TYPE, OperatorId.EQUALS, "tag");

		strategy.setVisibleWhen(strategyVisible);
		strategy.setRequiredWhen(strategyVisible);

		AdvancedCriteria embeddingVisible = new AdvancedCriteria(OperatorId.AND,
				new Criterion[] { new Criterion(TYPE, OperatorId.EQUALS, "tag"),
						new Criterion(STRATEGY, OperatorId.EQUALS, "retrieval") });

		embeddingSelector.setVisibleWhen(embeddingVisible);
		embeddingSelector.setRequiredWhen(embeddingVisible);

		form.setItems(id, type, strategy, name, label, modelSelector, embeddingSelector, threshold, description);

		container.addMember(form);
	}

	private String modelTypesSuitableForFiller(String fillerType) {
		return switch (fillerType) {
			case "tag" -> "zeroshot,classifier";
			case "language" -> "language";
			default -> null;
		};
	}

	public boolean validate() {
		if (!form.validate())
			return false;
		filler.setName(form.getValueAsString(NAME));
		filler.setLabel(form.getValueAsString(LABEL));
		filler.setDescription(form.getValueAsString(DESCRIPTION));
		filler.setType(form.getValueAsString(TYPE));

		String strategyValue = form.getValueAsString(STRATEGY);
		String modelId = form.getValueAsString(MODEL_ID);
		String embeddingId = form.getValueAsString(EMBEDDING);

		if ("retrieval".equals(strategyValue)) {
			// Retrieval strategy
			filler.setModelId(null);
			filler.setEmbeddingSchemeId(embeddingId != null ? Long.parseLong(embeddingId) : null);

		} else {
			// Model strategy
			filler.setEmbeddingSchemeId(null);
			filler.setModelId(modelId != null ? Long.parseLong(modelId) : null);
		}
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