package com.logicaldoc.gui.frontend.client.ai.embedding;

import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.types.OperatorId;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;

/**
 * Shows embedding schemes' standard properties and read-only data
 * 
 * @author Giuseppe Desiato - LogicalDOC
 * @since 9.2.2
 */
public class EmbeddingSchemeProperties extends EmbeddingSchemeDetailsTab {

	private static final String VECTORSIZE = "vectorsize";

	private static final String BATCH = "batch";

	private static final String CHUNKSBATCH = "chunksbatch";

	private static final String LABEL = "label";

	private static final String EMBEDDINGMODEL = "embeddingmodel";

	private DynamicForm form;

	private SelectItem model;

	private HLayout container = new HLayout();

	public EmbeddingSchemeProperties(EmbeddingSchemesPanel schemesPanel, GUIEmbeddingScheme scheme,
			ChangedHandler changedHandler) {
		super(schemesPanel, scheme, changedHandler);
		setWidth100();
		setHeight100();

		setMembers(container);
		refresh();
	}

	private void refresh() {
		// Destroy previous form
		if (form != null) {
			form.destroy();
			if (Boolean.TRUE.equals(container.contains(form)))
				container.removeMember(form);
		}

		form = new DynamicForm();
		form.setNumCols(2);
		form.setTitleOrientation(TitleOrientation.TOP);
		form.setWidth100();

		StaticTextItem id = ItemFactory.newStaticTextItem("id", Long.toString(embeddingScheme.getId()));
		id.setVisible(embeddingScheme.getId() != 0L);

		StaticTextItem modelName = ItemFactory.newStaticTextItem("modelName", EMBEDDINGMODEL,
				embeddingScheme.getModel());
		modelName.setVisible(embeddingScheme.getId() != 0L);

		StaticTextItem embeddings = ItemFactory.newStaticTextItem("embeddings",
				Long.toString(embeddingScheme.getEmbeddings()));
		embeddings.setVisible(embeddingScheme.getId() != 0L);

		// Name
		TextItem name = ItemFactory.newSimpleTextItem("name", embeddingScheme.getName());
		name.setRequired(true);
		name.addChangedHandler(changedHandler);

		// Label
		TextItem label = ItemFactory.newTextItem(LABEL, embeddingScheme.getLabel());
		label.addChangedHandler(changedHandler);

		// Embedding model selector
		model = ItemFactory.newSelectItem(EMBEDDINGMODEL);
		model.setOptionDataSource(new EmbeddingModelsDS());
		model.setRequired(true);
		model.setValue(embeddingScheme.getModelId());
		model.setValueField("id");
		model.setDisplayField(LABEL);
		model.addChangedHandler(changedHandler);
		model.setDisabled(embeddingScheme.getId() != 0L);
		model.setVisible(embeddingScheme.getId() == 0L);

		// Model Spec
		TextItem modelSpec = ItemFactory.newTextItem("modelspec", embeddingScheme.getModelSpec());
		modelSpec.setWidth(300);
		modelSpec.setColSpan(2);
		modelSpec.addChangedHandler(changedHandler);
		modelSpec.setVisibleWhen(new AdvancedCriteria(EMBEDDINGMODEL, OperatorId.EQUALS, "0"));

		// API Key
		TextItem apiKey = ItemFactory.newTextItem("apikey", embeddingScheme.getApiKey());
		apiKey.setWidth(300);
		apiKey.setColSpan(2);
		apiKey.addChangedHandler(changedHandler);
		apiKey.setVisibleWhen(new AdvancedCriteria(EMBEDDINGMODEL, OperatorId.EQUALS, "0"));

		// Chunk Batch Size
		SpinnerItem chunksBatch = ItemFactory.newSpinnerItem(CHUNKSBATCH, embeddingScheme.getChunksBatch());
		chunksBatch.setMin(1);
		chunksBatch.setStep(10);
		chunksBatch.setRequired(true);
		chunksBatch.addChangedHandler(changedHandler);

		// Batch Size
		SpinnerItem batch = ItemFactory.newSpinnerItem(BATCH, embeddingScheme.getBatch());
		batch.setMin(1);
		batch.setStep(100);
		batch.setRequired(true);
		batch.addChangedHandler(changedHandler);

		// Vector Size
		SpinnerItem vectorSize = ItemFactory.newSpinnerItem(VECTORSIZE, embeddingScheme.getVectorSize());
		vectorSize.setMin(1);
		vectorSize.setStep(1);
		vectorSize.setRequired(true);
		vectorSize.addChangedHandler(changedHandler);
		vectorSize.setDisabled(embeddingScheme.getId() != 0L);

		form.setItems(id, modelName, embeddings, name, label, model, modelSpec, apiKey, vectorSize, batch, chunksBatch);

		container.setMembersMargin(5);
		container.addMember(form);
	}

	public boolean validate() {
		if (form.validate()) {
			embeddingScheme.setName(form.getValueAsString("name"));
			embeddingScheme.setLabel(form.getValueAsString(LABEL));
			embeddingScheme.setModelId(Long.parseLong(form.getValueAsString(EMBEDDINGMODEL)));
			embeddingScheme.setModel(model.getSelectedRecord().getAttributeAsString("name"));

			embeddingScheme.setModelSpec(form.getValueAsString("modelspec"));
			embeddingScheme.setApiKey(form.getValueAsString("apikey"));
			embeddingScheme.setChunksBatch(
					form.getValue(CHUNKSBATCH) != null ? Integer.parseInt(form.getValueAsString(CHUNKSBATCH)) : 50);
			embeddingScheme.setBatch(
					form.getValue(BATCH) != null ? Integer.parseInt(form.getValueAsString(BATCH)) : 10000);
			embeddingScheme.setVectorSize(
					form.getValue(VECTORSIZE) != null ? Integer.parseInt(form.getValueAsString(VECTORSIZE)) : 300);
		}
		return !form.hasErrors();
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((container == null) ? 0 : container.hashCode());
		result = prime * result + ((form == null) ? 0 : form.hashCode());
		return prime * result + ((model == null) ? 0 : model.hashCode());
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (getClass() != obj.getClass())
			return false;
		EmbeddingSchemeProperties other = (EmbeddingSchemeProperties) obj;
		if (container == null) {
			if (other.container != null)
				return false;
		} else if (!container.equals(other.container))
			return false;
		if (form == null) {
			if (other.form != null)
				return false;
		} else if (!form.equals(other.form))
			return false;
		if (model == null) {
			if (other.model != null)
				return false;
		} else if (!model.equals(other.model))
			return false;
		return true;
	}
}