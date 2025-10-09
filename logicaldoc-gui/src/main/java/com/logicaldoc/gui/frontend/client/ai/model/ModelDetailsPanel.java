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

	private Layout statsTabPanel;

	private ModelStats statsPanel;

	private Layout historyTabPanel;

	private ModelHistoryPanel historyPanel;

	private EditingTabSet tabSet;

	private ModelsPanel modelsPanel;

	public ModelDetailsPanel(ModelsPanel modelsPanel) {
		super();

		this.modelsPanel = modelsPanel;
		setHeight100();
		setWidth100();
		setMembersMargin(10);

		tabSet = new EditingTabSet(saveEvent -> onSave(), cancelEvent -> {
			if (model.getId() != 0) {
				AIService.Instance.get().getModel(model.getId(), new DefaultAsyncCallback<>() {

					@Override
					public void handleSuccess(GUIModel sampler) {
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

		evaluationTabPanel = new HLayout();
		evaluationTabPanel.setWidth100();
		evaluationTabPanel.setHeight100();
		Tab evaluationTab = new Tab(I18N.message(EVALUATION));
		evaluationTab.setID(EVALUATION);
		evaluationTab.setPane(evaluationTabPanel);
		tabSet.addTab(evaluationTab);

		historyTabPanel = new HLayout();
		historyTabPanel.setWidth100();
		historyTabPanel.setHeight100();
		Tab historyTab = new Tab(I18N.message("history"));
		historyTab.setPane(historyTabPanel);
		tabSet.addTab(historyTab);

		statsTabPanel = new HLayout();
		statsTabPanel.setWidth100();
		statsTabPanel.setHeight100();
		Tab statsTab = new Tab(I18N.message("stats"));
		statsTab.setPane(statsTabPanel);
		tabSet.addTab(statsTab);

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

		if (historyPanel != null) {
			historyPanel.destroy();
			if (Boolean.TRUE.equals(historyTabPanel.contains(historyPanel)))
				historyTabPanel.removeMember(historyPanel);
		}

		if (evaluationPanel != null) {
			evaluationPanel.destroy();
			if (Boolean.TRUE.equals(evaluationTabPanel.contains(evaluationPanel)))
				evaluationTabPanel.removeMember(evaluationPanel);
		}

		if (statsPanel != null) {
			statsPanel.destroy();
			if (Boolean.TRUE.equals(statsTabPanel.contains(statsPanel)))
				statsTabPanel.removeMember(statsPanel);
		}

		propertiesPanel = new ModelProperties(model, event -> onModified());
		propertiesTabPanel.addMember(propertiesPanel);

		trainingPanel = new ModelTraining(model, event -> onModified());
		trainingTabPanel.addMember(trainingPanel);

		evaluationPanel = new ModelEvaluation(model, event -> onModified());
		evaluationTabPanel.addMember(evaluationPanel);

		statsPanel = new ModelStats(model);
		statsTabPanel.addMember(statsPanel);

		toggleEvaluationTab();

		historyPanel = new ModelHistoryPanel(model.getId());
		historyTabPanel.addMember(historyPanel);
	}

	protected void toggleEvaluationTab() {
		if (model.isNeuralNetwork())
			tabSet.showTab(EVALUATION);
		else
			tabSet.hideTab(EVALUATION);
	}

	public GUIModel getModel() {
		return model;
	}

	public void setModel(GUIModel model) {
		this.model = model;
		refresh();
	}

	public void onModified() {
		tabSet.displaySave();
	}

	private boolean validate() {
		boolean valid = propertiesPanel.validate();
		if (!valid) {
			tabSet.selectTab(0);
			return valid;
		}

		valid = trainingPanel.validate();
		if (!valid)
			tabSet.selectTab(1);
		return valid;
	}

	public void onSave() {
		if (validate()) {
			AIService.Instance.get().saveModel(model, new DefaultAsyncCallback<>() {
				@Override
				public void handleSuccess(GUIModel model) {
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