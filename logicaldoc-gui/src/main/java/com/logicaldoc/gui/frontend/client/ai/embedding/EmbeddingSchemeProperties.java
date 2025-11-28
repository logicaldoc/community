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

	private DynamicForm form;

	private SelectItem model;

	private HLayout container = new HLayout();

	public EmbeddingSchemeProperties(GUIEmbeddingScheme scheme, ChangedHandler changedHandler) {
		super(scheme, changedHandler);
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

		StaticTextItem modelName = ItemFactory.newStaticTextItem("modelName", "embeddingmodel",
				embeddingScheme.getModel());
		modelName.setVisible(embeddingScheme.getId() != 0L);

		// Name
		TextItem name = ItemFactory.newSimpleTextItem("name", embeddingScheme.getName());
		name.setRequired(true);
		name.addChangedHandler(changedHandler);

		// Label
		TextItem label = ItemFactory.newTextItem("label", embeddingScheme.getLabel());
		label.addChangedHandler(changedHandler);

		// Embedding model selector
		model = ItemFactory.newSelectItem("embeddingmodel");
		model.setOptionDataSource(new EmbeddingModelsDS());
		model.setRequired(true);
		model.setValue(embeddingScheme.getModelId());
		model.setValueField("id");
		model.setDisplayField("label");
		model.addChangedHandler(changedHandler);
		model.setDisabled(embeddingScheme.getId() != 0L);
		model.setVisible(embeddingScheme.getId() == 0L);

		// Model Spec
		TextItem modelSpec = ItemFactory.newTextItem("modelspec", embeddingScheme.getModelSpec());
		modelSpec.setWidth(300);
		modelSpec.setColSpan(2);
		modelSpec.addChangedHandler(changedHandler);
		modelSpec.setVisibleWhen(new AdvancedCriteria("embeddingmodel", OperatorId.EQUALS, "0"));

		// API Key
		TextItem apiKey = ItemFactory.newTextItem("apikey", embeddingScheme.getApiKey());
		apiKey.setWidth(300);
		apiKey.setColSpan(2);
		apiKey.addChangedHandler(changedHandler);
		apiKey.setVisibleWhen(new AdvancedCriteria("embeddingmodel", OperatorId.EQUALS, "0"));

		// Chunk Batch Size
		SpinnerItem chunksBatch = ItemFactory.newSpinnerItem("chunksbatch", embeddingScheme.getChunksBatch());
		chunksBatch.setMin(1);
		chunksBatch.setStep(10);
		chunksBatch.setRequired(true);
		chunksBatch.addChangedHandler(changedHandler);

		// Batch Size
		SpinnerItem batch = ItemFactory.newSpinnerItem("batch", embeddingScheme.getBatch());
		batch.setMin(1);
		batch.setStep(100);
		batch.setRequired(true);
		batch.addChangedHandler(changedHandler);

		// Vector Size
		SpinnerItem vectorSize = ItemFactory.newSpinnerItem("vectorsize", embeddingScheme.getVectorSize());
		vectorSize.setMin(1);
		vectorSize.setStep(1);
		vectorSize.setRequired(true);
		vectorSize.addChangedHandler(changedHandler);
		vectorSize.setDisabled(embeddingScheme.getId() != 0L);

		form.setItems(id, modelName, name, label, model, modelSpec, apiKey, vectorSize, batch, chunksBatch);

		container.setMembersMargin(5);
		container.addMember(form);
	}

	public boolean validate() {
		if (form.validate()) {
			embeddingScheme.setName(form.getValueAsString("name"));
			embeddingScheme.setLabel(form.getValueAsString("label"));
			embeddingScheme.setModelId(Long.parseLong(form.getValueAsString("embeddingmodel")));
			embeddingScheme.setModel(model.getSelectedRecord().getAttributeAsString("name"));

			embeddingScheme.setModelSpec(form.getValueAsString("modelspec"));
			embeddingScheme.setApiKey(form.getValueAsString("apikey"));
			embeddingScheme.setChunksBatch(Integer.parseInt(form.getValueAsString("chunksbatch")));
			embeddingScheme.setBatch(Integer.parseInt(form.getValueAsString("batch")));
			embeddingScheme.setVectorSize(Integer.parseInt(form.getValueAsString("vectorsize")));
		}
		return !form.hasErrors();
	}
}
