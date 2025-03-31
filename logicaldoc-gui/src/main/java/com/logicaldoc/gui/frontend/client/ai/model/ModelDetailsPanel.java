package com.logicaldoc.gui.frontend.client.ai.model;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.widgets.EditingTabSet;
import com.logicaldoc.gui.frontend.client.ai.AIService;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.tab.Tab;

/**
 * This panel collects details about a model
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
public class ModelDetailsPanel extends VLayout {
	private static final String EVALUATION = "evaluation";

	private GUIModel model;

	private Layout propertiesTabPanel;

	private ModelProperties propertiesPanel;

	private Layout trainingTabPanel;

	private ModelTraining trainingPanel;

	private Layout evaluationTabPanel;

	private ModelEvaluation evaluationPanel;

	private EditingTabSet tabSet;

	private ModelsPanel modelsPanel;

	public ModelDetailsPanel(ModelsPanel samplersPanel) {
		super();

		this.modelsPanel = samplersPanel;
		setHeight100();
		setWidth100();
		setMembersMargin(10);

		tabSet = new EditingTabSet(saveEvent -> onSave(), cancelEvent -> {
			if (model.getId() != 0) {
				AIService.Instance.get().getModel(model.getId(), new DefaultAsyncCallback<>() {

					@Override
					public void onSuccess(GUIModel sampler) {
						setModel(model);
					}

				});
			} else {
				setModel(new GUIModel());
			}
			tabSet.hideSave();
		});

		propertiesTabPanel = new HLayout();
		propertiesTabPanel.setWidth100();
		propertiesTabPanel.setHeight100();
		Tab propertiesTab = new Tab(I18N.message("properties"));
		propertiesTab.setPane(propertiesTabPanel);
		tabSet.addTab(propertiesTab);

		trainingTabPanel = new HLayout();
		trainingTabPanel.setWidth100();
		trainingTabPanel.setHeight100();
		Tab trainingTab = new Tab(I18N.message("training"));
		trainingTab.setPane(trainingTabPanel);
		tabSet.addTab(trainingTab);

		addMember(tabSet);
	}

	private void refresh() {
		tabSet.hideSave();

		/*
		 * Prepare the standard properties tab
		 */
		if (propertiesPanel != null) {
			propertiesPanel.destroy();
			if (Boolean.TRUE.equals(propertiesTabPanel.contains(propertiesPanel)))
				propertiesTabPanel.removeMember(propertiesPanel);
		}

		if (trainingPanel != null) {
			trainingPanel.destroy();
			if (Boolean.TRUE.equals(trainingTabPanel.contains(trainingPanel)))
				propertiesTabPanel.removeMember(trainingPanel);
		}

		if (evaluationPanel != null) {
			evaluationPanel.destroy();
			if (Boolean.TRUE.equals(evaluationTabPanel.contains(evaluationPanel)))
				evaluationTabPanel.removeMember(evaluationPanel);
		}

		propertiesPanel = new ModelProperties(model, event -> onModified());
		propertiesTabPanel.addMember(propertiesPanel);

		trainingPanel = new ModelTraining(model, event -> onModified());
		trainingTabPanel.addMember(trainingPanel);
		
		if (model.isNeuralNetwork()) {
			evaluationPanel = new ModelEvaluation(model, event -> onModified());
			evaluationTabPanel.addMember(evaluationPanel);
		} else {
			tabSet.removeTab(EVALUATION);
		}
	}

	public GUIModel getModel() {
		return model;
	}

	public void setModel(GUIModel model) {
		this.model = model;

		if (model.isNeuralNetwork()) {
			evaluationTabPanel = new HLayout();
			evaluationTabPanel.setWidth100();
			evaluationTabPanel.setHeight100();
			Tab evaluationTab = new Tab(I18N.message(EVALUATION));
			evaluationTab.setID(EVALUATION);
			evaluationTab.setPane(evaluationTabPanel);
			tabSet.addTab(evaluationTab);
		}

		refresh();
	}

	public void onModified() {
		tabSet.displaySave();
	}

	private boolean validate() {
		boolean valid = propertiesPanel.validate();
		if (!valid)
			tabSet.selectTab(0);

		valid = trainingPanel.validate();
		if (!valid)
			tabSet.selectTab(1);

		return valid;
	}

	public void onSave() {
		if (validate()) {
			AIService.Instance.get().saveModel(model, new DefaultAsyncCallback<>() {
				@Override
				public void onSuccess(GUIModel model) {
					tabSet.hideSave();
					if (model != null) {
						modelsPanel.updateRecord(model);
						modelsPanel.showModelDetails(model);
					}
				}
			});
		}
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